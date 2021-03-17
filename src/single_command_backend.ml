open Core
open Async

module Application_class_impl = struct
  type 'a env_type = 'a Env.t

  let deferred_or_error_swap v =
    match v with
    | Ok x ->
      let%map x = x in
      Or_error.return x
    | Error err -> return (Result.fail err)
  ;;

  let remote_run ~remote_target ~program ~env ~verbose ~stdin ~stdout ~stderr =
    let job = Job.create () in
    let sconn = ref None in
    let rconn = ref None in
    (* UUID key should be unique *)
    let host = Remote_target.host remote_target in
    let port = Remote_target.port remote_target in
    let%bind res1d =
      (let%bind.Deferred.Or_error sender_conn, receiver_conn =
         Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job
       in
       sconn := Some sender_conn;
       rconn := Some receiver_conn;
       let%bind.Deferred.Or_error remote_port =
         Rpc_local_sender.dispatch_open sender_conn ~host ~port ~program ~env
       in
       let%map.Deferred.Or_error resp =
         Rpc_local_receiver.dispatch receiver_conn ~host ~port ~remote_port
       in
       let reader, _metadata = resp in
       let _send =
         Reader.pipe stdin
         |> Pipe.fold ~init:(Or_error.return ()) ~f:(fun accum s ->
                match accum with
                | Error error -> return (Error error)
                | Ok () ->
                  let buf = Bytes.of_string s in
                  Rpc_local_sender.dispatch_write sender_conn ~buf ~amt:(Bytes.length buf))
       in
       let%bind maybe_error =
         Pipe.fold reader ~init:(Ok ()) ~f:(fun accum response ->
             match response with
             | Write_callback (b, len) ->
               let write_callback b len = return (Writer.write_bytes stdout b ~len) in
               let%bind.Deferred () = write_callback b len in
               return accum
             | Close_callback err ->
               let close_callback () = return () in
               let%bind.Deferred () = close_callback () in
               return err)
       in
       Job.complete job;
       return maybe_error)
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
end

let create () = (module Application_class_impl : Application_class.Backend)
