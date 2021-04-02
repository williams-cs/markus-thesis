open Core
open Async
open Rpc
open Rpc_common

module Query = struct
  type t = unit [@@deriving bin_io]
end

module Response = struct
  type t = Sender_query.t [@@deriving bin_io]
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
               let sexp_pipe = Reader.read_sexps stdin in
               Pipe.iter sexp_pipe ~f:(fun sexp ->
                   let sender_sexp = Sender_query.t_of_sexp sexp in
                   let id = Sender_query.id sender_sexp in
                   let sz =
                     match Sender_query.data sender_sexp with
                     | Header h ->
                       let env_image = Header.env_image h in
                       let public_image = Env.Image.to_public env_image in
                       let assign = Env_image.get_assignments public_image in
                       (match String.Map.find assign "in" with
                       | None -> sprintf "[Header (%s)]" id
                       | Some map_in -> sprintf "[Header: %s (%s)]" map_in id)
                     | Message m -> sprintf "[Message: %s (%s)]" (Bytes.to_string m) id
                     | Close -> sprintf "[Close (%s)]" id
                   in
                   fprintf (force Writer.stderr) "Send: %s\n" sz;
                   Pipe.write pipe sender_sexp))
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
