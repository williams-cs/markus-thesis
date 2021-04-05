open Core
open Async

module Connection = struct
  type t =
    { sender_conn : Rpc_local_sender.t
    ; receiver_conn : Rpc_local_receiver.t
    ; remote_port : int
    ; reader_map : id:string -> Reader.t Deferred.t
    }
  [@@deriving fields]
end

type t =
  { connections : Connection.t Deferred.t String.Table.t
  ; target_list : Remote_target.t list ref
  ; remote_target_index : int ref
  }
[@@deriving fields]

let create () =
  { connections = String.Table.create ()
  ; target_list = ref []
  ; remote_target_index = ref 0
  }
;;

let get_conn { connections; target_list = _; remote_target_index = _ } conn =
  String.Table.find connections conn
;;

let add_conn
    { connections; target_list; remote_target_index = _ }
    conn
    target
    sender_receiver_deferred
  =
  String.Table.add_exn connections ~key:conn ~data:sender_receiver_deferred;
  target_list := target :: !target_list
;;

let remote_target_string target =
  let host = Remote_target.host target in
  let port = Remote_target.port target in
  let port_string =
    match port with
    | None -> ""
    | Some p -> Int.to_string p
  in
  sprintf "%s%s" host port_string
;;

let init_targets t ~targets ~stderr ~verbose ~job =
  Deferred.Or_error.List.iter ~how:`Parallel targets ~f:(fun target ->
      let target_string = remote_target_string target in
      match get_conn t target_string with
      | Some conn_deferred ->
        let%bind _wait_for_conn = conn_deferred in
        Deferred.Or_error.return ()
      | None ->
        let ivar = Ivar.create () in
        add_conn t target_string target (Ivar.read ivar);
        let host = Remote_target.host target in
        let port = Remote_target.port target in
        let%bind.Deferred.Or_error sender_conn, receiver_conn =
          Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job
        in
        let%bind.Deferred.Or_error remote_port =
          Rpc_local_sender.dispatch_open sender_conn ~host ~port
        in
        let%map.Deferred.Or_error resp =
          Rpc_local_receiver.dispatch' receiver_conn ~host ~port ~remote_port
        in
        let reader_map, _metadata, _deferred = resp in
        Ivar.fill ivar { Connection.sender_conn; receiver_conn; remote_port; reader_map })
;;

let dispatch_command_raw connection program send_lines env_image =
  let sender_conn = Connection.sender_conn connection in
  let bufsize = 1024 in
  let resp_buf = Buffer.create bufsize in
  let%bind.Deferred.Or_error id =
    Rpc_local_sender.dispatch_header sender_conn ~program ~env_image
  in
  let send =
    let%bind _res =
      Deferred.List.fold send_lines ~init:(Or_error.return ()) ~f:(fun accum s ->
          match accum with
          | Error error -> return (Error error)
          | Ok () ->
            let buf = Bytes.of_string (s ^ "\n") in
            Rpc_local_sender.dispatch_write sender_conn ~id ~buf ~amt:(Bytes.length buf))
    in
    Rpc_local_sender.dispatch_close_single sender_conn ~id
  in
  let reader_map = Connection.reader_map connection in
  let%bind reader = reader_map ~id in
  let reader_pipe = Reader.pipe reader in
  let%bind () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun s -> Buffer.add_string resp_buf s)
  in
  let%bind _send = send in
  let _end_time = Time_ns.now () in
  (* TODO debug*)
  (* fprintf
        (force Writer.stderr)
        "%f\n"
        (Time_ns.diff end_time start_time |> Time_ns.Span.to_sec); *)
  Deferred.Or_error.return resp_buf
;;

let run_task t ~target ~program ~env_image ~send_lines =
  let remote_target =
    match target with
    | `Any ->
      let remote_target_index = remote_target_index t in
      let target = List.nth_exn !(target_list t) !remote_target_index in
      let next_remote_target =
        (!remote_target_index + 1) % String.Table.length (connections t)
      in
      remote_target_index := next_remote_target;
      target
    | `Specific target -> target
  in
  let%bind connection =
    Option.value_exn (get_conn t (remote_target_string remote_target))
  in
  let%map.Deferred.Or_error res_raw =
    dispatch_command_raw connection program send_lines env_image
  in
  Buffer.contents res_raw
;;
