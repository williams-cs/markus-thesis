open Core
open Async
open Rpc
open Rpc_common

module Query = struct
  type t =
    { host : string
    ; port : int option
    ; remote_port : int
    }
  [@@deriving bin_io, fields]
end

module Close_query = struct
  type t = Close [@@deriving bin_io]
end

module Response = struct
  type t =
    | Write_callback of Receiver_query.t
    | Close_callback of unit Or_error.t
  [@@deriving bin_io]
end

module State = struct
  type t =
    { verbose : bool
    ; close_ivar : unit Ivar.t
    }
  [@@deriving fields]

  let create ~verbose = { verbose; close_ivar = Ivar.create () }
end

type t = Connection.t

let create = Fn.id

(* let verbose_println ~verbose host str =
  if verbose then print_endline (sprintf "[Shard_rpc_local_receiver-%s] " host ^ str)
;; *)

let rpc =
  Pipe_rpc.create
    ~name:"shard_ssh_local_receiver_dispatch"
    ~version:1
    ~bin_query:Query.bin_t
    ~bin_response:Response.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let close_rpc =
  Rpc.create
    ~name:"shard_ssh_local_receiver_close"
    ~version:1
    ~bin_query:Close_query.bin_t
    ~bin_response:Close_query.bin_t
;;

let handle_query state query =
  Deferred.Or_error.return
    (Pipe.create_reader ~close_on_exception:true (fun pipe ->
         let%bind `Reader read_fd, `Writer write_fd =
           Unix.pipe (Info.of_string "local_receiver_pipe")
         in
         let reader = Reader.create read_fd in
         let writer = Writer.create write_fd in
         let reader_sexp_pipe = Reader.read_sexps reader in
         let reader_deferred =
           Pipe.iter_without_pushback reader_sexp_pipe ~f:(fun sexp ->
               let receiver_data = Receiver_query.t_of_sexp sexp in
               Pipe.write_without_pushback pipe (Response.Write_callback receiver_data))
         in
         let%bind () =
           In_thread.run (fun () ->
               let host = Query.host query in
               let port = Query.port query in
               let remote_port = Query.remote_port query in
               let verbose = State.verbose state in
               let res =
                 Remote_ssh.remote_run_receiver
                   ~host
                   ~port
                   ~remote_port
                   ~verbose
                   ~write_callback:(fun b len ->
                     let b = Bytes.copy b in
                     Writer.write_bytes ~len writer b)
                   ~close_callback:(fun () ->
                     Pipe.write_without_pushback pipe (Response.Close_callback (Ok ())))
               in
               Or_error.iter_error res ~f:(fun err ->
                   Pipe.write_without_pushback pipe (Response.Close_callback (Error err))))
         in
         let%bind () = reader_deferred in
         Pipe.close pipe;
         return ()))
;;

let handle_close state query =
  let ivar = State.close_ivar state in
  Ivar.fill_if_empty ivar ();
  return query
;;

let implementations =
  Implementations.create_exn
    ~implementations:
      [ Pipe_rpc.implement rpc handle_query; Rpc.implement close_rpc handle_close ]
    ~on_unknown_rpc:`Raise
;;

let run_server state =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr _conn -> state)
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ()
;;

let start_local_receiver ~verbose =
  let state = State.create ~verbose in
  let%bind tcp = run_server state in
  (* Signal to the client that the connection is ready *)
  let port = Tcp.Server.listening_on tcp in
  print_endline (Int.to_string port);
  let ivar = State.close_ivar state in
  Ivar.read ivar
;;

let dispatch conn ~host ~port ~remote_port =
  let query = { Query.host; port; remote_port } in
  let%map response = Pipe_rpc.dispatch rpc conn query in
  Or_error.join response
;;

let dispatch' conn ~host ~port ~remote_port =
  (* potential race conditions? *)
  let%bind.Deferred.Or_error pipe, metadata = dispatch conn ~host ~port ~remote_port in
  let writers = Hashtbl.create (module String) in
  let readers = Hashtbl.create (module String) in
  let lazy_reader ~id =
    match Hashtbl.find readers id with
    | Some reader -> return reader
    | None ->
      let%bind `Reader read_fd, `Writer write_fd =
        Unix.pipe (Info.of_string (sprintf "read_pipe_id_%s" id))
      in
      let reader = Reader.create read_fd in
      let writer = Writer.create write_fd in
      Hashtbl.set readers ~key:id ~data:reader;
      (match Hashtbl.find writers id with
      | Some ivar -> Ivar.fill ivar writer
      | None ->
        let ivar = Ivar.create_full writer in
        Hashtbl.set writers ~key:id ~data:ivar);
      return reader
  in
  let lazy_writer ~id =
    match Hashtbl.find writers id with
    | Some ivar -> Ivar.read ivar
    | None ->
      let ivar = Ivar.create () in
      Hashtbl.set writers ~key:id ~data:ivar;
      Ivar.read ivar
  in
  let deferred =
    let%bind deferreds =
      Pipe.fold pipe ~init:[] ~f:(fun accum resp ->
          match resp with
          | Write_callback query ->
            (match query with
            | Receiver_query.Data { Receiver_data.id; data } ->
              let deferred =
                let%bind writer = lazy_writer ~id in
                (* printf "dat: %s\n" data; *)
                Writer.write writer data;
                Deferred.return ()
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum)
            | Receiver_query.Close id ->
              let deferred =
                let%bind writer = lazy_writer ~id in
                Writer.close writer
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum))
          | Close_callback x -> return (return x :: accum))
    in
    let%bind or_errors = Deferred.all deferreds in
    return (Or_error.combine_errors or_errors |> Or_error.map ~f:ignore)
  in
  Deferred.Or_error.return (lazy_reader, metadata, deferred)
;;

let dispatch_close conn =
  let query = Close_query.Close in
  Rpc.dispatch close_rpc conn query |> Deferred.Or_error.ignore_m
;;
