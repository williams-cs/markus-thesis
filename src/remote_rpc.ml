open Core
open Async
open Rpc

let run_client host port =
  let conn =
    Connection.client
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
  in
  Deferred.Result.map_error ~f:(fun exn -> Error.of_exn exn) conn
;;

let rpc_create_child ~name ~prog ~args ~host ~port ~stderr ~verbose ~job =
  let continue_if_should_connect f =
    if Job.should_connect job
    then f ()
    else Deferred.Or_error.error_string "Should not connect on attempted connection!"
  in
  (* run copied executable with command line argument, which runs the server *)
  continue_if_should_connect (fun () ->
      Util.verbose_println
        ~name:"Shard_rpc"
        ~stderr
        ~verbose
        ~host
        ~port
        (sprintf "Starting %s..." name);
      let args = args @ if verbose then [ "-V" ] else [] in
      let%bind.Deferred.Or_error sender_process =
        Process.create ~prog ~args ~stdin:"" ()
      in
      Job.attach job ~process:sender_process;
      let sender_stdout = Process.stdout sender_process in
      let sender_stderr = Process.stderr sender_process in
      let _err_deferred = Util.glue' ~reader:sender_stderr ~writer:stderr in
      let%bind ready_string = Reader.read_line sender_stdout in
      match ready_string with
      | `Eof -> return (Or_error.error_string "EOF reached when waiting for ready port")
      | `Ok port_string ->
        Util.verbose_println
          ~name:"Shard_rpc"
          ~stderr
          ~verbose
          ~host
          ~port
          (sprintf "%s port: %s" name port_string);
        let port = Int.of_string port_string in
        let _out_deferred = Util.glue' ~reader:sender_stdout ~writer:stderr in
        Deferred.Or_error.return port)
;;

let setup_rpc_service ~host ~port ~stderr ~verbose ~job =
  let continue_if_should_connect f =
    if Job.should_connect job
    then f ()
    else Deferred.Or_error.error_string "Should not connect on attempted connection!"
  in
  continue_if_should_connect (fun () ->
      Util.verbose_println
        ~name:"Shard_rpc"
        ~stderr
        ~verbose
        ~host
        ~port
        "Copying local executable...";
      let%bind local_path =
        (* copy executable to shard folder *)
        In_thread.run (fun () ->
            let local_path, _hash = Remote_ssh.local_copy ~verbose ~host ~port in
            local_path)
      in
      Job.connect job;
      let%bind.Deferred.Or_error sender_port =
        rpc_create_child
          ~name:"RPC local sender"
          ~prog:local_path
          ~args:[ "-Rls" ]
          ~host
          ~port
          ~stderr
          ~verbose
          ~job
      in
      let lhost = "localhost" in
      Util.verbose_println
        ~name:"Shard_rpc"
        ~stderr
        ~verbose
        ~host
        ~port
        "Starting sender RPC client...";
      let%bind.Deferred.Or_error sender_client = run_client lhost sender_port in
      let%bind.Deferred.Or_error receiver_port =
        rpc_create_child
          ~name:"RPC local receiver"
          ~prog:local_path
          ~args:[ "-Rlr" ]
          ~host
          ~port
          ~stderr
          ~verbose
          ~job
      in
      Util.verbose_println
        ~name:"Shard_rpc"
        ~stderr
        ~verbose
        ~host
        ~port
        "Starting receiver RPC client...";
      let%map.Deferred.Or_error receiver_client = run_client lhost receiver_port in
      Rpc_local_sender.create sender_client, Rpc_local_receiver.create receiver_client)
;;

let deferred_or_error_swap v =
  match v with
  | Ok x ->
    let%map x = x in
    Or_error.return x
  | Error err -> return (Result.fail err)
;;

let remote_run ~host ~port ~program ~env ~verbose ~stdin ~stdout ~stderr =
  let job = Job.create () in
  let sconn = ref None in
  let rconn = ref None in
  (* UUID key should be unique *)
  let%bind res1d =
    (let%bind.Deferred.Or_error sender_conn, receiver_conn =
       setup_rpc_service ~host ~port ~stderr ~verbose ~job
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

let start_local_sender = Rpc_local_sender.start_local_sender
let start_local_receiver = Rpc_local_receiver.start_local_receiver
let start_remote_sender = Rpc_remote_sender.start_remote_sender
let start_remote_receiver = Rpc_remote_receiver.start_remote_receiver
