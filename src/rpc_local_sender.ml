open Core
open Async
open Rpc
open Rpc_common

module Open_query = struct
  type t =
    { host : string
    ; port : int option
    ; header : Header.t
    }
  [@@deriving bin_io, fields]
end

module Write_query = struct
  type t = { buf : bytes } [@@deriving bin_io, fields]
end

module Close_query = struct
  type t = Close [@@deriving bin_io]
end

module Open_response = struct
  type t = int Or_error.t [@@deriving bin_io]
end

module Response = struct
  type t = unit Or_error.t [@@deriving bin_io]
end

module State = struct
  type t =
    { read_fd : Core.Unix.File_descr.t
    ; write_fd : Core.Unix.File_descr.t
    ; verbose : bool
    ; close_ivar : unit Ivar.t
    }
  [@@deriving fields]

  let create ~read_fd ~write_fd ~verbose =
    { read_fd; write_fd; verbose; close_ivar = Ivar.create () }
  ;;
end

type t = Connection.t

let create = Fn.id

let write_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_write"
    ~version:1
    ~bin_query:Write_query.bin_t
    ~bin_response:Response.bin_t
;;

let handle_write state query =
  let fd = State.write_fd state in
  let buf = Write_query.buf query in
  In_thread.run (fun () ->
      Or_error.try_with (fun () -> Core.Unix.single_write fd ~buf |> ignore))
;;

let open_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_open"
    ~version:1
    ~bin_query:Open_query.bin_t
    ~bin_response:Open_response.bin_t
;;

let handle_open state query =
  let fd = State.read_fd state in
  let verbose = State.verbose state in
  let host = Open_query.host query in
  let port = Open_query.port query in
  let header = Open_query.header query |> Header.sexp_of_t |> Sexp.to_string_mach in
  let ivar : int Or_error.t Ivar.t = Ivar.create () in
  let _x =
    In_thread.run (fun () ->
        let res =
          Remote_ssh.remote_run_sender
            ~host
            ~port
            ~verbose
            ~header
            ~port_callback:(fun port -> Ivar.fill ivar (Ok port))
            ~read_callback:(fun buf len -> Core.Unix.read fd ~len ~buf)
        in
        match res with
        | Ok () -> ()
        | Error err ->
          (match Ivar.is_empty ivar with
          | true -> Ivar.fill_if_empty ivar (Error err)
          | false ->
            fprintf
              (force Writer.stderr)
              "Local sender error: %s"
              (Error.to_string_hum err)))
  in
  Ivar.read ivar
;;

let close_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_close"
    ~version:1
    ~bin_query:Close_query.bin_t
    ~bin_response:Response.bin_t
;;

let handle_close state _query =
  let close_ivar = State.close_ivar state in
  Ivar.fill close_ivar ();
  Deferred.Or_error.return ()
;;

let implementations =
  Implementations.create_exn
    ~implementations:
      [ Rpc.implement write_rpc handle_write
      ; Rpc.implement open_rpc handle_open
      ; Rpc.implement close_rpc handle_close
      ]
    ~on_unknown_rpc:`Raise
;;

let run_server state =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr _conn -> state)
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ()
;;

let start_local_sender ~verbose =
  let read_fd, write_fd = Core.Unix.pipe () in
  let state = State.create ~read_fd ~write_fd ~verbose in
  let%bind tcp = run_server state in
  (* Signal to the client that the connection is ready *)
  let port = Tcp.Server.listening_on tcp in
  print_endline (Int.to_string port);
  let ivar = State.close_ivar state in
  Ivar.read ivar
;;

let dispatch_open conn ~host ~port ~program ~env =
  let env_image = Env.Image.of_env env in
  let header = { Header.program; env_image } in
  let query = { Open_query.host; port; header } in
  let%map response = Rpc.dispatch open_rpc conn query in
  Or_error.join response
;;

let dispatch_write conn ~buf ~amt =
  let buf = Bytes.sub ~len:amt ~pos:0 buf in
  let query = { Write_query.buf } in
  let%map response = Rpc.dispatch write_rpc conn query in
  Or_error.join response
;;

let dispatch_close conn =
  let query = Close_query.Close in
  let%map response = Rpc.dispatch close_rpc conn query in
  Or_error.join response
;;
