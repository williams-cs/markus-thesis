open Core
open Async

let clusters : Task_cluster.t String.Table.t = String.Table.create ()

module Application_class_impl = struct
  let deferred_or_error_swap v =
    match v with
    | Ok x ->
      let%map x = x in
      Or_error.return x
    | Error err -> return (Result.fail err)
  ;;

  let remote_run_single
      ~cluster_id
      ~remote_target
      ~setting
      ~program
      ~env_image
      ~verbose
      ~stdin:_
      ~stdout
      ~stderr
      ~job_group
    =
    let _cluster_id = cluster_id in
    let _setting = setting in
    let job = Job.create ~groups:[ job_group ] () in
    let sconn = ref None in
    let rconn = ref None in
    let host = Remote_target.host remote_target in
    let port = Remote_target.port remote_target in
    let user = Remote_target.user remote_target in
    let%bind res1d =
      (let%bind.Deferred.Or_error sender_conn, receiver_conn =
         Remote_rpc.setup_rpc_service ~host ~port ~user ~stderr ~verbose ~job:(Some job)
       in
       sconn := Some sender_conn;
       rconn := Some receiver_conn;
       let%bind.Deferred.Or_error remote_port =
         Rpc_local_sender.dispatch_open sender_conn ~host ~port ~user
       in
       let%bind.Deferred.Or_error _id =
         Rpc_local_sender.dispatch_header sender_conn ~program ~env_image
       in
       let%map.Deferred.Or_error resp =
         Rpc_local_receiver.dispatch receiver_conn ~host ~port ~user ~remote_port
       in
       let reader, _metadata = resp in
       (* let _send =
         let s = Reader.read stdin in
         let buf = Bytes.of_string s in
         Rpc_local_sender.dispatch_write sender_conn ~id ~buf ~amt:(Bytes.length buf)
       in *)
       (* Reader.pipe stdin
         |> Pipe.fold ~init:(Or_error.return ()) ~f:(fun accum s ->
                match accum with
                | Error error -> return (Error error)
                | Ok () ->
                  let buf = Bytes.of_string s in
                  Rpc_local_sender.dispatch_write sender_conn ~buf ~amt:(Bytes.length buf)) *)
       (* in *)
       let close_ivar = Ivar.create () in
       let _resp =
         let%bind () =
           Pipe.iter reader ~f:(fun response ->
               match response with
               | Write_callback receiver_query ->
                 (* placeholder, sequence number handling? *)
                 let { Rpc_common.Receiver_query.id = _; sequence_number = _; data } =
                   receiver_query
                 in
                 (match data with
                 | Message str ->
                   Writer.write stdout str;
                   return ()
                 | Close i ->
                   Ivar.fill close_ivar i;
                   return ()
                 | Heartbeat _id -> (* placeholder, heartbeat handling ? *) return ())
               | Close_callback _ ->
                 (* placeholder , error handling ??? *)
                 Ivar.fill_if_empty close_ivar 0;
                 return ())
         in
         return (Ivar.fill_if_empty close_ivar 0)
       in
       Job.complete job;
       let%bind code = Ivar.read close_ivar in
       match code with
       | 0 -> Deferred.Or_error.return ()
       | i -> Deferred.Or_error.error_string (sprintf "Remote exited with code %d" i))
      |> Deferred.map ~f:deferred_or_error_swap
      |> Deferred.join
      |> Deferred.map ~f:Or_error.join
    in
    let res2 =
      Option.map !rconn ~f:(fun conn -> Rpc_local_receiver.dispatch_close conn)
      |> Option.value ~default:Deferred.Or_error.ok_unit
    in
    let res3 =
      Option.map !sconn ~f:(fun conn -> Rpc_local_sender.dispatch_close conn)
      |> Option.value ~default:Deferred.Or_error.ok_unit
    in
    let%bind res2d = res2
    and res3d = res3 in
    Or_error.combine_errors [ res1d; res2d; res3d ] |> Or_error.map ~f:ignore |> return
  ;;

  let _remote_run
      ~cluster_id
      ~remote_targets
      ~setting
      ~program
      ~env_image
      ~verbose
      ~stdin
      ~stdout
      ~stderr
      ~job_group
    =
    let%map errors =
      Deferred.List.map ~how:`Parallel remote_targets ~f:(fun remote_target ->
          remote_run_single
            ~cluster_id
            ~remote_target
            ~setting
            ~program
            ~env_image
            ~verbose
            ~stdin
            ~stdout
            ~stderr
            ~job_group)
    in
    Or_error.combine_errors errors |> Or_error.map ~f:ignore
  ;;

  let remote_run
      ~cluster_id
      ~remote_targets
      ~setting:_
      ~program
      ~env_image
      ~verbose
      ~stdin:_
      ~stdout
      ~stderr
      ~job_group
    =
    let job = Job.create ~groups:[ job_group ] () in
    let res =
      (* Set up connection to hosts in cluster as necessary*)
      let cluster_info = String.Table.find clusters cluster_id in
      let cluster_info =
        match cluster_info with
        | None ->
          let info = Task_cluster.create () in
          String.Table.add_exn clusters ~key:cluster_id ~data:info;
          info
        | Some info -> info
      in
      let%bind.Deferred _res =
        Task_cluster.init_targets cluster_info ~targets:remote_targets ~verbose ~stderr
      in
      (* Reset log *)
      Task_cluster.log_reset cluster_info;
      (* let rec loop lines =
        let%bind line = Reader.read_line stdin in
        match line with
        | `Eof -> return (List.rev lines)
        | `Ok line -> loop (line :: lines)
      in
      let%bind send_lines = loop [] in *)
      let send_lines = [] in
      let%map or_errors =
        Deferred.List.map ~how:`Parallel remote_targets ~f:(fun remote_target ->
            let%bind.Deferred.Or_error res =
              Task_cluster.run_task
                cluster_info
                ~target:(`Specific remote_target)
                ~program
                ~env_image
                ~send_lines
            in
            Writer.write stdout res;
            let%bind () = Writer.flushed stdout in
            Deferred.Or_error.return ())
      in
      let%map.Or_error units = Or_error.combine_errors or_errors in
      ignore units
    in
    let cancelled_res =
      let%bind () = Job.wait_for_cancel job in
      Deferred.Or_error.error_string "Remote job cancelled"
    in
    Deferred.any [ res; cancelled_res ]
  ;;
end

let create () = (module Application_class_impl : Application_class.Backend)
