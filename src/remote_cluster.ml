open Core
open Async
open Rpc_common

module Reader_info = struct
  type t =
    { reader : Reader.t
    ; next_heartbeat : (unit, read_write) Bvar.t
    }
  [@@deriving fields]

  let create ~reader = { reader; next_heartbeat = Bvar.create () }
end

module Writer_info = struct
  type t =
    { writer : Writer.t
    ; future_tasks : unit Ivar.t Int.Table.t
    ; next_sequence_number : int ref
    }
  [@@deriving fields]

  let create ~writer =
    { writer; future_tasks = Int.Table.create (); next_sequence_number = ref 0 }
  ;;
end

module Connection = struct
  type t =
    { sender_conn : Rpc_local_sender.t
    ; receiver_conn : Rpc_local_receiver.t
    ; remote_port : int
    ; reader_map : id:string -> Reader_info.t Deferred.t
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

let use_sequence_numbers = true

let dispatch_reader conn ~host ~port ~remote_port =
  (* potential race conditions? *)
  let%bind.Deferred.Or_error pipe, metadata =
    Rpc_local_receiver.dispatch conn ~host ~port ~remote_port
  in
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
      let reader_info = Reader_info.create ~reader in
      let writer = Writer.create write_fd in
      let writer_info = Writer_info.create ~writer in
      Hashtbl.set readers ~key:id ~data:reader_info;
      (match Hashtbl.find writers id with
      | Some ivar -> Ivar.fill ivar writer_info
      | None ->
        let ivar = Ivar.create_full writer_info in
        Hashtbl.set writers ~key:id ~data:ivar);
      return reader_info
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
            let { Receiver_query.id; sequence_number; data } = query in
            let perform_future_task writer_info fn =
              match use_sequence_numbers with
              | false ->
                let writer = Writer_info.writer writer_info in
                fn writer
              | true ->
                let fn_augmented () =
                  let writer = Writer_info.writer writer_info in
                  let%bind () = fn writer in
                  Writer_info.next_sequence_number writer_info := sequence_number + 1;
                  (match
                     Int.Table.find
                       (Writer_info.future_tasks writer_info)
                       (sequence_number + 1)
                   with
                  | Some ivar -> Ivar.fill ivar ()
                  | None -> ());
                  return ()
                in
                if sequence_number = !(Writer_info.next_sequence_number writer_info)
                then fn_augmented ()
                else (
                  let ivar = Ivar.create () in
                  Int.Table.add_exn
                    (Writer_info.future_tasks writer_info)
                    ~key:sequence_number
                    ~data:ivar;
                  let%bind () = Ivar.read ivar in
                  fn_augmented ())
            in
            (match data with
            | Receiver_data.Message str ->
              let deferred =
                let%bind writer_info = lazy_writer ~id in
                let perform_write writer =
                  Writer.write writer str;
                  return ()
                in
                (* printf "dat: %s\n" str; *)
                perform_future_task writer_info perform_write
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum)
            | Receiver_data.Close ->
              let deferred =
                let%bind writer_info = lazy_writer ~id in
                let perform_close writer = Writer.close writer in
                perform_future_task writer_info perform_close
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum)
            | Receiver_data.Heartbeat _index ->
              (* TODO heartbeat *)
              let deferred =
                let%bind writer_info = lazy_writer ~id in
                let perform_heartbeat _writer = return () in
                perform_future_task writer_info perform_heartbeat
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum))
          | Close_callback x -> return (return x :: accum))
    in
    let%bind or_errors = Deferred.all deferreds in
    return (Or_error.combine_errors or_errors |> Or_error.map ~f:ignore)
  in
  Deferred.Or_error.return (lazy_reader, metadata, deferred)
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
      dispatch_reader receiver_conn ~host ~port ~remote_port
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
  let%bind reader_info = reader_map ~id in
  let { Reader_info.reader; next_heartbeat } = reader_info in
  let reader_pipe = Reader.pipe reader in
  let resp =
    let%bind () =
      Pipe.iter_without_pushback reader_pipe ~f:(fun s -> Buffer.add_string resp_buf s)
    in
    let _end_time = Time_ns.now () in
    (* TODO debug*)
    (* fprintf
        (force Writer.stderr)
        "%f\n"
        (Time_ns.diff end_time start_time |> Time_ns.Span.to_sec); *)
    return resp_buf
  in
  Deferred.Or_error.return (next_heartbeat, resp)
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
  | Ok (next_heartbeat, res) ->
    let contents =
      let%bind buf = res in
      return (Buffer.contents buf)
    in
    Deferred.Or_error.return (next_heartbeat, contents)
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
  let inner_timeout = 1000 in
  let first_heartbeat_timeout = 10000 in
  let heartbeat_timeout = 2000 in
  let idle_heartbeat_timeout = 2000 in
  let rec loop i err deferreds =
    if i > max_tries
    then (
      match err with
      | Some err -> return (Error err)
      | None -> Deferred.Or_error.errorf "failed for max tries (%d)" max_tries)
    else (
      let deferred =
        let%bind.Deferred.Or_error next_heartbeat, res =
          dispatch_command t ~target ~program ~env_image ~send_lines
        in
        let rec heartbeat_loop timeout =
          let wait_for_heartbeat () = Bvar.wait next_heartbeat in
          let%bind timeout_or_heartbeat_or_output =
            Clock_ns.with_timeout
              (Time_ns.Span.of_int_ms timeout)
              (Deferred.choose
                 [ Deferred.choice (wait_for_heartbeat ()) (fun () -> `Heartbeat)
                 ; Deferred.choice res (fun str -> `Output str)
                 ])
          in
          match timeout_or_heartbeat_or_output with
          | `Timeout -> Deferred.Or_error.return (`Heartbeat_timeout res)
          | `Result heartbeat_or_output ->
            (match heartbeat_or_output with
            | `Heartbeat ->
              refresh_idle_time t;
              heartbeat_loop heartbeat_timeout
            | `Output str -> Deferred.Or_error.return (`Output str))
        in
        heartbeat_loop first_heartbeat_timeout
      in
      let live_uuid = Util.generate_uuid_list (Map.keys deferreds) in
      let deferreds = Map.add_exn deferreds ~key:live_uuid ~data:deferred in
      let rec inner_loop j deferreds =
        let%bind timeout =
          Clock_ns.with_timeout
            (Time_ns.Span.of_int_ms inner_timeout)
            (Deferred.choose
               (List.map (Map.to_alist deferreds) ~f:(fun (id, def) ->
                    Deferred.choice def (fun x -> id, x))))
        in
        match timeout with
        | `Result (uuid, res) ->
          (match res with
          | Ok res ->
            (match res with
            | `Heartbeat_timeout deferred ->
              fprintf (force Writer.stderr) "heartbeat_timeout %d %d\n" i j;
              let no_more_timeout =
                let%bind s = deferred in
                Deferred.Or_error.return (`Output s)
              in
              let deferreds = Map.set deferreds ~key:uuid ~data:no_more_timeout in
              if String.equal uuid live_uuid
              then loop (i + 1) err deferreds
              else inner_loop (j + 1) deferreds
            | `Output s -> Deferred.Or_error.return s)
          | Error err ->
            let deferreds = Map.remove deferreds uuid in
            if String.equal uuid live_uuid
            then loop (i + 1) (Some err) deferreds
            else inner_loop (j + 1) deferreds)
        | `Timeout ->
          if j > max_tries_inner
             (* || (idle_time t > 100 && !open_tasks < !open_tasks_cache) *)
             || idle_time t > idle_heartbeat_timeout
          then (
            (* TODO debug *)
            (* fprintf (force Writer.stderr) "timeout %d %d\n" i !open_tasks; *)
            refresh_idle_time t;
            (* open_tasks_cache := !open_tasks; *)
            loop (i + 1) err deferreds)
          else inner_loop (j + 1) deferreds
      in
      inner_loop 0 deferreds)
  in
  let%map.Deferred.Or_error out = loop 0 None String.Map.empty in
  open_tasks := !open_tasks - 1;
  refresh_idle_time t;
  out
;;
