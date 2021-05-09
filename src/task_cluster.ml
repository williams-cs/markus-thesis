open Core
open Async
open Rpc_common

let use_sequence_numbers = true
let use_local_throttle = true
let use_beta_sampling = true
let enable_logging = true

(* Retry config *)
(* TODO dynamic *)
let force_create_cooldown = 500
let max_tries = 100
let max_tries_inner = 120
let inner_timeout = 1000
let initial_timeout = 10000
let heartbeat_timeout = 2000
let idle_heartbeat_timeout = 2000
let beta_overcapacity_threshold = 0.9
let beta_fail_threshold = 0.25
let beta_stage_two_random_chance = 0.1
let beta_recency_cap = 1000

module Reader_info = struct
  type t =
    { reader : Reader.t
    ; next_heartbeat : (unit, read_write) Bvar.t
    }
  [@@deriving fields]

  let create ~reader ~heartbeat = { reader; next_heartbeat = heartbeat }
end

module Writer_info = struct
  type t =
    { writer : Writer.t
    ; future_tasks : unit Ivar.t Int.Table.t
    ; next_sequence_number : int ref
    ; next_heartbeat : (unit, read_write) Bvar.t
    }
  [@@deriving fields]

  let create ~writer ~heartbeat =
    { writer
    ; future_tasks = Int.Table.create ()
    ; next_sequence_number = ref 0
    ; next_heartbeat = heartbeat
    }
  ;;
end

module Connection = struct
  type t =
    { remote_target : Remote_target.t
    ; sender_conn : Rpc_local_sender.t
    ; sender_throttle : unit Throttle.t
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
  { connections : (Connection.t Ivar.t * Time_ns.t) String.Table.t
  ; target_list : Remote_target.t list ref
  ; remote_target_index : int ref
  ; open_tasks : int ref
  ; open_tasks_cache : int ref
  ; last_activity : Time_ns.t ref
  ; init_target_fields : Init_target_fields.t Ivar.t ref
  ; beta_sampler : Beta_sampler.t
  ; beta_sampler_key_of_uuid : string String.Table.t
  ; log_start_time : Time_ns.t ref
  ; log_id : string ref
  ; log_task_counter : int ref
  }
[@@deriving fields]

let start_to_id start = start |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string

let create () =
  let start = Time_ns.now () in
  { connections = String.Table.create ()
  ; target_list = ref []
  ; remote_target_index = ref 0
  ; open_tasks = ref 0
  ; open_tasks_cache = ref 0
  ; last_activity = ref (Time_ns.now ())
  ; init_target_fields = ref (Ivar.create ())
  ; beta_sampler = Beta_sampler.create ()
  ; beta_sampler_key_of_uuid = String.Table.create ()
  ; log_start_time = ref start
  ; log_id = ref (start_to_id start)
  ; log_task_counter = ref 0
  }
;;

let get_conn { connections; _ } conn = String.Table.find connections conn

let add_conn
    { connections; target_list; _ }
    conn
    target
    sender_receiver_deferred
    timestamp
  =
  String.Table.set connections ~key:conn ~data:(sender_receiver_deferred, timestamp);
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

let dispatch_reader conn ~host ~port ~user ~remote_port =
  (* potential race conditions? *)
  let%bind.Deferred.Or_error pipe, metadata =
    Rpc_local_receiver.dispatch conn ~host ~port ~user ~remote_port
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
      let heartbeat = Bvar.create () in
      let reader = Reader.create read_fd in
      let reader_info = Reader_info.create ~reader ~heartbeat in
      let writer = Writer.create write_fd in
      let writer_info = Writer_info.create ~writer ~heartbeat in
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
              (* Heartbeat does not use sequence number *)
              let deferred =
                let%bind writer_info = lazy_writer ~id in
                let heartbeat = Writer_info.next_heartbeat writer_info in
                Bvar.broadcast heartbeat ();
                return ()
                (* let perform_heartbeat _writer heartbeat =
                  Bvar.broadcast heartbeat ();
                  return ()
                in
                perform_future_task writer_info perform_heartbeat *)
              in
              return (Deferred.map deferred ~f:Or_error.return :: accum))
          | Close_callback x -> return (return x :: accum))
    in
    let%bind or_errors = Deferred.all deferreds in
    return (Or_error.combine_errors or_errors |> Or_error.map ~f:ignore)
  in
  Deferred.Or_error.return (lazy_reader, metadata, deferred)
;;

let init_target_raw t ~target ~force_create =
  let itf = !(init_target_fields t) in
  let%bind { Init_target_fields.stderr; verbose } = Ivar.read itf in
  let target_string = remote_target_string target in
  if use_beta_sampling
  then (
    let sampler = beta_sampler t in
    Beta_sampler.add sampler ~key:target_string);
  let old_conn = get_conn t target_string in
  let do_create () =
    let ivar = Ivar.create () in
    let timestamp = Time_ns.now () in
    add_conn t target_string target ivar timestamp;
    let host = Remote_target.host target in
    let port = Remote_target.port target in
    let user = Remote_target.user target in
    let%bind.Deferred.Or_error sender_conn, receiver_conn =
      Remote_rpc.setup_rpc_service ~host ~port ~user ~stderr ~verbose ~job:None
    in
    let%bind.Deferred.Or_error remote_port =
      Rpc_local_sender.dispatch_open sender_conn ~host ~port ~user
    in
    let%map.Deferred.Or_error resp =
      dispatch_reader receiver_conn ~host ~port ~user ~remote_port
    in
    let reader_map, _metadata, _deferred = resp in
    let sender_throttle =
      Throttle.create
        ~continue_on_error:true
        ~max_concurrent_jobs:(Rpc_remote_sender.max_concurrent_jobs * 2)
    in
    Ivar.fill
      ivar
      { Connection.remote_target = target
      ; sender_conn
      ; sender_throttle
      ; receiver_conn
      ; remote_port
      ; reader_map
      }
  in
  match old_conn with
  | Some (conn_deferred, timestamp) ->
    let actual_force_create =
      match force_create with
      | true ->
        let time_passed = Time_ns.diff (Time_ns.now ()) timestamp in
        Time_ns.Span.( >= ) time_passed (Time_ns.Span.of_int_ms force_create_cooldown)
      | false -> false
    in
    (match actual_force_create with
    | true -> do_create ()
    | false ->
      let%bind _wait_for_conn = Ivar.read conn_deferred in
      Deferred.Or_error.return ())
  | None -> do_create ()
;;

let init_target_timeout = Time_ns.Span.of_int_ms 5000

let init_target t ~target ~force_create =
  let itf_res = init_target_raw t ~target ~force_create in
  let%bind itf_res = Clock_ns.with_timeout init_target_timeout itf_res in
  match itf_res with
  | `Result r -> return r
  | `Timeout -> return (Or_error.error_s [%message "Init target timeout"])
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
    let%bind.Deferred.Or_error () =
      Deferred.List.fold send_lines ~init:(Or_error.return ()) ~f:(fun accum s ->
          match accum with
          | Error error -> return (Error error)
          | Ok () ->
            let buf = Bytes.of_string (s ^ "\n") in
            Rpc_local_sender.dispatch_write sender_conn ~id ~buf ~amt:(Bytes.length buf))
    in
    Rpc_local_sender.dispatch_close_single sender_conn ~id
  in
  let%bind.Deferred.Or_error () = send in
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

let dispatch_command_throttle connection program send_lines env_image ~throttle_timeout =
  let throttle = Connection.sender_throttle connection in
  Throttle.enqueue throttle (fun () ->
      let%bind.Deferred.Or_error next_heartbeat, resp =
        dispatch_command_raw connection program send_lines env_image
      in
      let%bind () =
        Deferred.any
          [ Bvar.wait next_heartbeat
          ; Deferred.map ~f:ignore resp
          ; Clock_ns.after throttle_timeout
          ]
      in
      Deferred.Or_error.return (next_heartbeat, resp))
;;

let choose_target t =
  let choose_next () =
    let remote_target_index = remote_target_index t in
    let target = List.nth_exn !(target_list t) !remote_target_index in
    let next_remote_target =
      (!remote_target_index + 1) % String.Table.length (connections t)
    in
    remote_target_index := next_remote_target;
    target
  in
  if use_beta_sampling
  then (
    (* First stage: Sample all that are not over capacity, find highest. *)
    let conns = connections t in
    let all_targets = String.Table.keys conns in
    let target_at_max_capacity key =
      let ivar, _timestamp = String.Table.find_exn conns key in
      match Ivar.peek ivar with
      | Some conn ->
        if use_local_throttle
        then (
          let throttle = Connection.sender_throttle conn in
          let max_capacity = Throttle.max_concurrent_jobs throttle in
          let current_capacity = Throttle.num_jobs_running throttle in
          (* If over 90% capacity, exclude it from sampling *)
          Float.( >= )
            (Int.to_float current_capacity)
            (Int.to_float max_capacity *. beta_overcapacity_threshold))
        else false
      | None -> true
    in
    let full_targets = List.filter all_targets ~f:target_at_max_capacity in
    let sampler = beta_sampler t in
    let sample res fn =
      match res with
      | Some (key, v) ->
        if Float.( < ) v beta_fail_threshold
        then fn ()
        else (
          let ivar, _timestamp = String.Table.find_exn conns key in
          match Ivar.peek ivar with
          | Some conn -> Connection.remote_target conn
          | None -> fn ())
      | None -> fn ()
    in
    let second_stage () =
      (* Second stage: 90% chance: sample all, including over capacity, find highest.
      10% chance: Fallback to default order. *)
      if Float.( <= )
           (Random.State.float (Util.random_state ()) 1.0)
           beta_stage_two_random_chance
      then choose_next ()
      else sample (Beta_sampler.choose sampler) choose_next
    in
    sample (Beta_sampler.choose sampler ~excluding:full_targets) second_stage)
  else choose_next ()
;;

let dispatch_command t ~target ~program ~env_image ~send_lines ~send_timeout ~uuid =
  let remote_target =
    match target with
    | `Any -> choose_target t
    | `Specific target -> target
  in
  let rts = remote_target_string remote_target in
  if use_beta_sampling
  then (
    let table = beta_sampler_key_of_uuid t in
    String.Table.set table ~key:uuid ~data:rts);
  let connection_res =
    let connection_deferred, _timestamp = Option.value_exn (get_conn t rts) in
    Ivar.read connection_deferred
  in
  let refresh_remote_connection err =
    let%bind itf_res = init_target t ~target:remote_target ~force_create:true in
    match itf_res with
    | Ok () -> return (Error err)
    | Error err2 -> return (Error (Error.of_list [ err; err2 ]))
  in
  let%bind connection_timeout =
    Clock_ns.with_timeout init_target_timeout connection_res
  in
  match connection_timeout with
  | `Result connection ->
    let dispatch =
      if use_local_throttle
      then dispatch_command_throttle ~throttle_timeout:send_timeout
      else dispatch_command_raw
    in
    let res_throttle = dispatch connection program send_lines env_image in
    let%bind res_throttle_timeout = Clock_ns.with_timeout send_timeout res_throttle in
    let post_process res_raw =
      match res_raw with
      | Ok (next_heartbeat, res) ->
        let contents =
          let%bind buf = res in
          return (Buffer.contents buf)
        in
        Deferred.Or_error.return (next_heartbeat, contents)
      | Error err -> refresh_remote_connection err
    in
    (match res_throttle_timeout with
    | `Result r -> return (post_process r)
    | `Timeout -> return (Deferred.bind ~f:(fun r -> post_process r) res_throttle))
  | `Timeout ->
    return (refresh_remote_connection (Error.create_s [%message "Connection not found"]))
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

let log_time_passed t =
  let now = Time_ns.now () in
  let start = !(log_start_time t) in
  let elapsed = Time_ns.diff now start in
  Time_ns.Span.to_sec elapsed
;;

(* TODO implement log *)
let log_task_completed t =
  if enable_logging
  then (
    let complete_count = !(log_task_counter t) + 1 in
    log_task_counter t := complete_count + 1;
    let base_id = !(log_id t) in
    let log = Util.log ~id:base_id in
    Log.printf log "%f,%d" (log_time_passed t) complete_count)
;;

let log_reset t =
  if enable_logging
  then (
    let start = Time_ns.now () in
    log_start_time t := start;
    log_id t := start_to_id start;
    log_task_counter t := 0)
;;

let beta_sampler_outcome t uuid outcome =
  if use_beta_sampling
  then (
    let sampler = beta_sampler t in
    let table = beta_sampler_key_of_uuid t in
    match String.Table.find table uuid with
    | Some key ->
      String.Table.remove table uuid;
      Beta_sampler.update ~cap:beta_recency_cap sampler ~key ~outcome
    | None -> ())
;;

let beta_sampler_success t uuid = beta_sampler_outcome t uuid `Success
let beta_sampler_failure t uuid = beta_sampler_outcome t uuid `Failure
let jc = ref 0

let _print_beta_sampler t =
  (* TODO debug *)
  let sampler = beta_sampler t in
  let keys = Beta_sampler.keys sampler in
  fprintf (force Writer.stderr) "Job %d complete\n" !jc;
  jc := !jc + 1;
  List.iter keys ~f:(fun key ->
      let a, b = Option.value_exn (Beta_sampler.get sampler ~key) in
      fprintf (force Writer.stderr) "%s:%d,%d\n" key a b)
;;

let run_task t ~target ~program ~env_image ~send_lines =
  let open_tasks = open_tasks t in
  let open_tasks_cache = open_tasks_cache t in
  open_tasks := !open_tasks + 1;
  open_tasks_cache := !open_tasks;
  let rec loop i err deferreds =
    if i > max_tries
    then (
      match err with
      | Some err -> return (Error err)
      | None -> Deferred.Or_error.errorf "failed for max tries (%d)" max_tries)
    else (
      let live_uuid = Util.generate_uuid_list (Map.keys deferreds) in
      let%bind throttle_result =
        dispatch_command
          t
          ~target
          ~program
          ~env_image
          ~send_lines
          ~send_timeout:(Time_ns.Span.of_int_ms initial_timeout)
          ~uuid:live_uuid
      in
      let deferred =
        let%bind.Deferred.Or_error next_heartbeat, res = throttle_result in
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
        let first_heartbeat_timeout =
          if use_local_throttle then heartbeat_timeout else initial_timeout
        in
        heartbeat_loop first_heartbeat_timeout
      in
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
              (* TODO debug *)
              fprintf (force Writer.stderr) "heartbeat_timeout %d %d\n" i j;
              let no_more_timeout =
                let%bind s = deferred in
                Deferred.Or_error.return (`Output s)
              in
              let deferreds = Map.set deferreds ~key:uuid ~data:no_more_timeout in
              if String.equal uuid live_uuid
              then (
                beta_sampler_failure t uuid;
                loop (i + 1) err deferreds)
              else inner_loop (j + 1) deferreds
            | `Output s ->
              if String.equal uuid live_uuid then beta_sampler_success t uuid;
              Deferred.Or_error.return s)
          | Error err ->
            let deferreds = Map.remove deferreds uuid in
            if String.equal uuid live_uuid
            then (
              beta_sampler_failure t uuid;
              loop (i + 1) (Some err) deferreds)
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
            beta_sampler_failure t live_uuid;
            loop (i + 1) err deferreds)
          else inner_loop (j + 1) deferreds
      in
      inner_loop 0 deferreds)
  in
  let%map.Deferred.Or_error out = loop 0 None String.Map.empty in
  open_tasks := !open_tasks - 1;
  refresh_idle_time t;
  log_task_completed t;
  (* print_beta_sampler t; *)
  out
;;
