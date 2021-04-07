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

module Init_target_fields = struct
  type t =
    { stderr : Writer.t
    ; verbose : bool
    }
  [@@deriving fields]
end

type t =
  { connections : Connection.t Deferred.t String.Table.t
  ; target_list : Remote_target.t list ref
  ; remote_target_index : int ref
  ; open_tasks : int ref
  ; open_tasks_cache : int ref
  ; last_activity : Time_ns.t ref
  ; init_target_fields : Init_target_fields.t Ivar.t ref
  }
[@@deriving fields]

let create () =
  { connections = String.Table.create ()
  ; target_list = ref []
  ; remote_target_index = ref 0
  ; open_tasks = ref 0
  ; open_tasks_cache = ref 0
  ; last_activity = ref (Time_ns.now ())
  ; init_target_fields = ref (Ivar.create ())
  }
;;

let get_conn { connections; _ } conn = String.Table.find connections conn

let add_conn { connections; target_list; _ } conn target sender_receiver_deferred =
  String.Table.set connections ~key:conn ~data:sender_receiver_deferred;
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

let init_target t ~target ~force_create =
  let itf = !(init_target_fields t) in
  let%bind { Init_target_fields.stderr; verbose } = Ivar.read itf in
  let target_string = remote_target_string target in
  let old_conn = if force_create then None else get_conn t target_string in
  match old_conn with
  | Some conn_deferred ->
    let%bind _wait_for_conn = conn_deferred in
    Deferred.Or_error.return ()
  | None ->
    let ivar = Ivar.create () in
    add_conn t target_string target (Ivar.read ivar);
    let host = Remote_target.host target in
    let port = Remote_target.port target in
    let%bind.Deferred.Or_error sender_conn, receiver_conn =
      Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job:None
    in
    let%bind.Deferred.Or_error remote_port =
      Rpc_local_sender.dispatch_open sender_conn ~host ~port
    in
    let%map.Deferred.Or_error resp =
      Rpc_local_receiver.dispatch' receiver_conn ~host ~port ~remote_port
    in
    let reader_map, _metadata, _deferred = resp in
    Ivar.fill ivar { Connection.sender_conn; receiver_conn; remote_port; reader_map }
;;

let init_targets t ~targets ~stderr ~verbose =
  let itf = { Init_target_fields.stderr; verbose } in
  let itf_ivar =
    let itf_old = !(init_target_fields t) in
    if Ivar.is_full itf_old
    then Ivar.create_full itf
    else (
      Ivar.fill itf_old itf;
      itf_old)
  in
  init_target_fields t := itf_ivar;
  Deferred.Or_error.List.iter ~how:`Parallel targets ~f:(fun target ->
      init_target t ~target ~force_create:false)
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
  let%bind _send = send in
  (* fprintf (force Writer.stderr) "send\n"; *)
  let reader_map = Connection.reader_map connection in
  let%bind reader = reader_map ~id in
  let reader_pipe = Reader.pipe reader in
  let%bind () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun s -> Buffer.add_string resp_buf s)
  in
  let _end_time = Time_ns.now () in
  (* TODO debug*)
  (* fprintf
        (force Writer.stderr)
        "%f\n"
        (Time_ns.diff end_time start_time |> Time_ns.Span.to_sec); *)
  Deferred.Or_error.return resp_buf
;;

let dispatch_command t ~target ~program ~env_image ~send_lines =
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
  let%bind res_raw = dispatch_command_raw connection program send_lines env_image in
  match res_raw with
  | Ok res -> Deferred.Or_error.return (Buffer.contents res)
  | Error err ->
    (* TODO debug *)
    (* fprintf (force Writer.stderr) "rt\n"; *)
    let%bind itf_res = init_target t ~target:remote_target ~force_create:true in
    (match itf_res with
    | Ok () -> return (Error err)
    | Error err2 -> return (Error (Error.of_list [ err; err2 ])))
;;

let idle_time t =
  let last_activity = last_activity t in
  let now = Time_ns.now () in
  let span = Time_ns.diff now !last_activity in
  Time_ns.Span.to_int_ms span
;;

let refresh_idle_time t =
  let last_activity = last_activity t in
  last_activity := Time_ns.now ()
;;

let run_task t ~target ~program ~env_image ~send_lines =
  let open_tasks = open_tasks t in
  let open_tasks_cache = open_tasks_cache t in
  open_tasks := !open_tasks + 1;
  open_tasks_cache := !open_tasks;
  let max_tries = 100 in
  let max_tries_inner = 120 in
  let rec loop i err deferreds =
    if i > max_tries
    then (
      match err with
      | Some err -> return (Error err)
      | None -> Deferred.Or_error.errorf "failed for max tries (%d)" max_tries)
    else (
      let deferred = dispatch_command t ~target ~program ~env_image ~send_lines in
      let uuid = Util.generate_uuid_list (Map.keys deferreds) in
      let new_deferreds = Map.add_exn deferreds ~key:uuid ~data:deferred in
      let rec inner_loop j =
        let%bind timeout =
          Clock_ns.with_timeout
            (Time_ns.Span.of_int_ms 500)
            (Deferred.choose
               (List.map (Map.to_alist new_deferreds) ~f:(fun (id, def) ->
                    Deferred.choice def (fun x -> id, x))))
        in
        match timeout with
        | `Result (uuid, res) ->
          (match res with
          | Ok s -> Deferred.Or_error.return s
          | Error err ->
            let new_deferreds = Map.remove new_deferreds uuid in
            loop (i + 1) (Some err) new_deferreds)
        | `Timeout ->
          if j > max_tries_inner
             || (idle_time t > 100 && !open_tasks < !open_tasks_cache)
             || idle_time t > 1000
          then (
            (* TODO debug *)
            (* fprintf (force Writer.stderr) "timeout %d %d\n" i !open_tasks; *)
            refresh_idle_time t;
            open_tasks_cache := !open_tasks;
            loop (i + 1) err new_deferreds)
          else inner_loop (j + 1)
      in
      inner_loop 0)
  in
  let%map.Deferred.Or_error out = loop 0 None String.Map.empty in
  open_tasks := !open_tasks - 1;
  refresh_idle_time t;
  out
;;
