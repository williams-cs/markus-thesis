open Core
open Async
open Rpc
open Rpc_common

module Open_query = struct
  type t =
    { host : string
    ; port : int option
    ; user : string option
    }
  [@@deriving bin_io, fields]
end

module Header_query = struct
  type t = Header.t [@@deriving sexp, bin_io]

  let to_sender_query t ~id = { Sender_query.id; data = Sender_data.Header t }
end

module Write_query = struct
  type t =
    { id : string
    ; buf : bytes
    }
  [@@deriving sexp, bin_io, fields]

  let to_sender_query { id; buf } = { Sender_query.id; data = Sender_data.Message buf }
end

module Close_single_query = struct
  type t = string [@@deriving bin_io]

  let to_sender_query id = { Sender_query.id; data = Sender_data.Close }
end

module Close_query = struct
  type t = Close [@@deriving bin_io]
end

module Keepalive_query = struct
  type t = Keepalive [@@deriving bin_io]

  let to_sender_query _t =
    { Sender_query.id = "N/A"; data = Sender_data.Process_keepalive }
  ;;
end

module Open_response = struct
  type t = int Or_error.t [@@deriving bin_io]
end

module Header_response = struct
  type t = string Or_error.t [@@deriving bin_io]
end

module Response = struct
  type t = unit Or_error.t [@@deriving bin_io]
end

module State = struct
  type t =
    { ids : string Hash_set.t
    ; read_fd : Core.Unix.File_descr.t
    ; writer : Writer.t
    ; verbose : bool
    ; close_ivar : unit Ivar.t
    ; keepalive : Keepalive.Chain.t
    }
  [@@deriving fields]

  let create ~read_fd ~writer ~verbose =
    { ids = Hash_set.create (module String)
    ; verbose
    ; read_fd
    ; writer
    ; close_ivar = Ivar.create ()
    ; keepalive =
        Keepalive.Chain.create
          (Clock.after (Time.Span.of_ms (Int.to_float Keepalive.initial_delay)))
    }
  ;;
end

type t = Connection.t

let write_sender_query sender_query writer =
  let bufsize = 1024 in
  let buf = Buffer.create bufsize in
  let sexp = Sender_query.sexp_of_t sender_query in
  Sexp.to_buffer_mach ~buf sexp;
  let bs = Buffer.contents_bytes buf in
  Writer.write_bytes writer bs;
  Writer.flushed writer
;;

let header_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_header"
    ~version:1
    ~bin_query:Header_query.bin_t
    ~bin_response:Header_response.bin_t
;;

let handle_header state query =
  let ids = State.ids state in
  let id = Util.generate_uuid_hash_set ids in
  let writer = State.writer state in
  let { Header.program; env_image } = query in
  let env_image =
    Env.Image.to_public env_image
    |> Env_image.add_assignment ~key:"id" ~data:id
    |> Env.Image.of_public
  in
  let query = { Header.program; env_image } in
  let sender_query = Header_query.to_sender_query query ~id in
  let%map () = write_sender_query sender_query writer in
  id |> Or_error.return
;;

let write_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_write"
    ~version:1
    ~bin_query:Write_query.bin_t
    ~bin_response:Response.bin_t
;;

let handle_write state query =
  let writer = State.writer state in
  let sender_query = Write_query.to_sender_query query in
  write_sender_query sender_query writer |> Deferred.map ~f:Or_error.return
;;

let open_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_open"
    ~version:1
    ~bin_query:Open_query.bin_t
    ~bin_response:Open_response.bin_t
;;

let handle_open state query =
  let verbose = State.verbose state in
  let read_fd = State.read_fd state in
  let host = Open_query.host query in
  let port = Open_query.port query in
  let user = Open_query.user query in
  let ivar : int Or_error.t Ivar.t = Ivar.create () in
  let _x =
    let%bind () =
      In_thread.run (fun () ->
          let res =
            Remote_ssh.remote_run_sender
              ~host
              ~port
              ~user
              ~verbose
              ~port_callback:(fun port -> Ivar.fill ivar (Ok port))
              ~read_callback:(fun buf len -> Core.Unix.read read_fd ~len ~buf)
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
    let close_ivar = State.close_ivar state in
    Ivar.fill close_ivar ();
    return ()
  in
  let%bind.Deferred.Or_error port = Ivar.read ivar in
  port |> Deferred.Or_error.return
;;

let close_single_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_close_single"
    ~version:1
    ~bin_query:Close_single_query.bin_t
    ~bin_response:Response.bin_t
;;

let handle_close_single state query =
  let writer = State.writer state in
  let sender_query = Close_single_query.to_sender_query query in
  write_sender_query sender_query writer |> Deferred.map ~f:Or_error.return
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

let keepalive_rpc =
  Rpc.create
    ~name:"shard_ssh_local_sender_keepalive"
    ~version:1
    ~bin_query:Keepalive_query.bin_t
    ~bin_response:Response.bin_t
;;

let handle_keepalive state query =
  if Keepalive.enable_keepalive
  then (
    let keepalive = State.keepalive state in
    Keepalive.Chain.append
      keepalive
      (Clock.after (Time.Span.of_ms (Int.to_float Keepalive.local_delay)))
    |> ignore;
    let writer = State.writer state in
    let sender_query = Keepalive_query.to_sender_query query in
    write_sender_query sender_query writer |> Deferred.map ~f:Or_error.return)
  else Deferred.Or_error.return ()
;;

let implementations =
  Implementations.create_exn
    ~implementations:
      [ Rpc.implement header_rpc handle_header
      ; Rpc.implement write_rpc handle_write
      ; Rpc.implement open_rpc handle_open
      ; Rpc.implement close_single_rpc handle_close_single
      ; Rpc.implement close_rpc handle_close
      ; Rpc.implement keepalive_rpc handle_keepalive
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
  let writer =
    Fd.create Fd.Kind.Fifo write_fd (Info.of_string "sender_writer") |> Writer.create
  in
  let state = State.create ~read_fd ~writer ~verbose in
  let%bind tcp = run_server state in
  (* Signal to the client that the connection is ready *)
  let port = Tcp.Server.listening_on tcp in
  print_endline (Int.to_string port);
  let ivar = State.close_ivar state in
  let keepalive = State.keepalive state in
  Deferred.any
    [ Ivar.read ivar
    ; (if Keepalive.enable_keepalive
      then Keepalive.Chain.wait keepalive
      else Deferred.never ())
    ]
;;

let dispatch_open conn ~host ~port ~user =
  let query = { Open_query.host; port; user } in
  let%map response = Rpc.dispatch open_rpc conn query in
  Or_error.join response
;;

let dispatch_header conn ~program ~env_image =
  let env_image = Env.Image.of_public env_image in
  let header = { Header.program; env_image } in
  let%map response = Rpc.dispatch header_rpc conn header in
  Or_error.join response
;;

let dispatch_write conn ~id ~buf ~amt =
  let buf = Bytes.sub ~len:amt ~pos:0 buf in
  let query = { Write_query.id; buf } in
  let%map response = Rpc.dispatch write_rpc conn query in
  Or_error.join response
;;

let dispatch_close_single conn ~id =
  let query = id in
  let%map response = Rpc.dispatch close_single_rpc conn query in
  Or_error.join response
;;

let dispatch_close conn =
  let query = Close_query.Close in
  let%map response = Rpc.dispatch close_rpc conn query in
  Or_error.join response
;;

let dispatch_keepalive conn =
  let query = Keepalive_query.Keepalive in
  Rpc.dispatch keepalive_rpc conn query |> Deferred.Or_error.ignore_m
;;

let create conn =
  Keepalive.schedule
    ~dispatch:(fun () -> dispatch_keepalive conn)
    ~on_error:(fun () -> Async.Rpc.Connection.close conn);
  conn
;;
