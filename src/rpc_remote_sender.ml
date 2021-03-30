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

let start_remote_sender
    (type a)
    (module Provider : Application_class.Provider with type t = a)
    ~verbose
    ~remote_port
    ~runner
  =
  Async.try_with (fun () ->
      let%bind `Reader read_fd, `Writer write_fd =
        Unix.pipe (Info.of_string "remote_rpc")
      in
      let reader = Reader.create read_fd in
      let writer = Writer.create write_fd in
      let host = "localhost" in
      let port = remote_port in
      let%bind.Deferred.Or_error conn = run_client host port in
      let stdout = force Writer.stdout in
      let stderr = force Writer.stderr in
      let%bind.Deferred.Or_error pipe_reader, _metadata =
        Rpc_remote_receiver.dispatch conn
      in
      let header : Rpc_common.Header.t Ivar.t = Ivar.create () in
      let write_deferred =
        Pipe.iter pipe_reader ~f:(fun x ->
            match x with
            | Header h ->
              Ivar.fill header h;
              return ()
            | Message s ->
              Writer.write writer s;
              return ())
      in
      let%bind header = Ivar.read header in
      let prog = Rpc_common.Header.program header in
      let env_image = Rpc_common.Header.env_image header in
      let%bind cwd = Unix.getcwd () in
      let env = Env.Image.to_env (module Provider) ~working_directory:cwd env_image in
      let run =
        runner ~verbose ~prog ~env ~eval_args_stdin:(Some reader) ~stdout ~stderr
      in
      let%bind () = write_deferred in
      run)
  |> Deferred.map ~f:(fun x -> Result.map_error x ~f:Error.of_exn |> Or_error.join)
;;
