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

let continue_if_should_connect job f =
  let should_connect =
    match job with
    | None -> true
    | Some job -> Job.should_connect job
  in
  if should_connect
  then f ()
  else Deferred.Or_error.error_string "Should not connect on attempted connection!"
;;

let rpc_create_child ~name ~prog ~args ~host ~port ~user:_ ~stderr ~verbose ~job =
  (* run copied executable with command line argument, which runs the server *)
  continue_if_should_connect job (fun () ->
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
      (match job with
      | Some job -> Job.attach job ~process:sender_process
      | None -> ());
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

let setup_rpc_service ~host ~port ~user ~stderr ~verbose ~job =
  continue_if_should_connect job (fun () ->
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
            let local_path, _hash = Remote_ssh.local_copy ~verbose ~host ~port ~user in
            local_path)
      in
      (match job with
      | Some job -> Job.connect job
      | None -> ());
      let%bind.Deferred.Or_error sender_port =
        rpc_create_child
          ~name:"RPC local sender"
          ~prog:local_path
          ~args:[ "-Rls" ]
          ~host
          ~port
          ~user
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
          ~user
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

let start_local_sender = Rpc_local_sender.start_local_sender
let start_local_receiver = Rpc_local_receiver.start_local_receiver
let start_remote_sender = Rpc_remote_sender.start_remote_sender
let start_remote_receiver = Rpc_remote_receiver.start_remote_receiver
