open Core
open Async
open Rpc

module Query = struct
  type t = unit [@@deriving bin_io]
end

module Response = struct
  type t =
    | Header of Rpc_common.Header.t
    | Message of string
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

(* let verbose_println ~verbose str =
  if verbose then print_endline (sprintf "[Shard_rpc_remote_receiver] " ^ str)
;; *)

let rpc =
  Pipe_rpc.create
    ~name:"shard_ssh_remote_receiver_dispatch"
    ~version:1
    ~bin_query:Query.bin_t
    ~bin_response:Response.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let handle_query state () =
  Deferred.Or_error.return
    (Pipe.create_reader ~close_on_exception:true (fun pipe ->
         let%bind res =
           Async.try_with (fun () ->
               let stdin = force Reader.stdin in
               let%bind header_sexp = Reader.read_sexp stdin in
               match header_sexp with
               | `Eof -> raise_s [%message "Bad header"]
               | `Ok header_sexp ->
                 let header = Rpc_common.Header.t_of_sexp header_sexp in
                 let%bind () = Pipe.write pipe (Response.Header header) in
                 let stdin_pipe = Reader.pipe stdin in
                 let%bind () =
                   Pipe.iter stdin_pipe ~f:(fun s -> Pipe.write pipe (Response.Message s))
                 in
                 Pipe.close pipe;
                 return ())
         in
         let close_ivar = State.close_ivar state in
         Ivar.fill close_ivar ();
         match res with
         | Ok () -> return ()
         | Error exn -> raise exn))
;;

let implementations =
  Implementations.create_exn
    ~implementations:[ Pipe_rpc.implement rpc handle_query ]
    ~on_unknown_rpc:`Raise
;;

let run_server state =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr _conn -> state)
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ()
;;

let start_remote_receiver ~verbose =
  let state = State.create ~verbose in
  (* verbose_println ~verbose "Remote receiver started!"; *)
  let%bind tcp = run_server state in
  (* Signal to the client that the connection is ready *)
  let port = Tcp.Server.listening_on tcp in
  print_endline (Int.to_string port);
  let ivar = State.close_ivar state in
  Ivar.read ivar
;;

let dispatch conn =
  let query = () in
  let%map response = Pipe_rpc.dispatch rpc conn query in
  Or_error.join response
;;
