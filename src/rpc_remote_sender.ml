open Core
open Async
open Rpc
open Rpc_common

let run_client host port =
  let conn =
    Connection.client
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
  in
  Deferred.Result.map_error ~f:(fun exn -> Error.of_exn exn) conn
;;

let debug_forced_failure_rate = 0.1
let heartbeat_span_ns = Time_ns.Span.of_int_ms 500

let start_remote_sender
    (type a)
    (module Provider : Application_class.Provider with type t = a)
    ~verbose
    ~remote_port
    ~runner
  =
  Async.try_with (fun () ->
      let rand = Util.random_state () in
      let writers = Hashtbl.create (module String) in
      let host = "localhost" in
      let port = remote_port in
      let%bind.Deferred.Or_error conn = run_client host port in
      let global_stdout = force Writer.stdout in
      let global_stderr = force Writer.stderr in
      let%bind.Deferred.Or_error pipe_reader, _metadata =
        Rpc_remote_receiver.dispatch conn
      in
      let throttle = Throttle.create ~continue_on_error:true ~max_concurrent_jobs:64 in
      let%bind write_deferred =
        Pipe.fold pipe_reader ~init:[] ~f:(fun accum query ->
            let id = Sender_query.id query in
            let data = Sender_query.data query in
            let res =
              match data with
              | Header header ->
                Throttle.enqueue throttle (fun () ->
                    if Float.( < ) (Random.State.float rand 1.0) debug_forced_failure_rate
                    then (
                      fprintf (force Writer.stderr) "FAILURE PROC %s\n" id;
                      Deferred.Or_error.return ())
                    else (
                      fprintf (force Writer.stderr) "TRY PROC %s\n" id;
                      let make_pipe_and_get_writer info_string output_writer =
                        let%bind `Reader tmp_read_fd, `Writer tmp_write_fd =
                          Unix.pipe (Info.of_string info_string)
                        in
                        let tmp_writer = Writer.create tmp_write_fd in
                        let tmp_reader = Reader.create tmp_read_fd in
                        let glued =
                          Async.try_with (fun () ->
                              Util.glue_transform
                                ~reader:tmp_reader
                                ~writer:output_writer
                                ~transform:(fun s ->
                                  fprintf (force Writer.stderr) "[%s:%s]\n" s id;
                                  let rdata = { Receiver_data.id; data = s } in
                                  let rquery = Receiver_query.Data rdata in
                                  let sexp = Receiver_query.sexp_of_t rquery in
                                  let sstr = Sexp.to_string sexp in
                                  sstr ^ "\n"))
                        in
                        return (tmp_reader, tmp_writer, glued)
                      in
                      let close_for_id output_writer =
                        let rquery = Receiver_query.Close id in
                        let sexp = Receiver_query.sexp_of_t rquery in
                        Writer.write_sexp output_writer sexp
                      in
                      let%bind `Reader read_fd, `Writer write_fd =
                        Unix.pipe (Info.of_string "remote_rpc")
                      in
                      let reader = Reader.create read_fd in
                      let writer = Writer.create write_fd in
                      (match Hashtbl.find writers id with
                      | Some ivar -> Ivar.fill ivar writer
                      | None ->
                        let ivar = Ivar.create_full writer in
                        Hashtbl.add_exn writers ~key:id ~data:ivar);
                      let%bind stdout_reader_for_close, stdout, _glue_wait_stdout =
                        make_pipe_and_get_writer "stdout_pipe" global_stdout
                      in
                      let%bind stderr_reader_for_close, stderr, _glue_wait_stderr =
                        make_pipe_and_get_writer "stderr_pipe" global_stderr
                      in
                      let prog = Rpc_common.Header.program header in
                      let env_image = Rpc_common.Header.env_image header in
                      let%bind cwd = Unix.getcwd () in
                      let env =
                        Env.Image.to_env
                          (module Provider)
                          ~working_directory:cwd
                          env_image
                      in
                      let res =
                        fprintf
                          (force Writer.stderr)
                          "PROC %s %s\n"
                          (Sexp.to_string prog)
                          id;
                        let res =
                          runner
                            ~verbose
                            ~prog
                            ~env
                            ~eval_args_stdin:(Some reader)
                            ~stdout
                            ~stderr
                        in
                        let index = ref 0 in
                        Clock_ns.every
                          ~stop:(Deferred.ignore_m res)
                          heartbeat_span_ns
                          (fun () ->
                            let rquery = Receiver_query.Heartbeat (id, !index) in
                            index := !index + 1;
                            let sexp = Receiver_query.sexp_of_t rquery in
                            Writer.write_sexp global_stdout sexp);
                        let%bind res = res in
                        (* let%bind () = Writer.fsync stdout in
                      let%bind () = Writer.fsync stderr in
                      let%bind () = Writer.fsync global_stdout in
                      let%bind () = Writer.fsync global_stderr in *)
                        let%bind () = Writer.flushed stdout in
                        let%bind () = Writer.flushed stderr in
                        let%bind () = Writer.flushed global_stdout in
                        let%bind () = Writer.flushed global_stderr in
                        (* close all related fds *)
                        let%bind () = Writer.close stdout in
                        let%bind () = Writer.close stderr in
                        let%bind () = Reader.close reader in
                        let%bind () = Reader.close stdout_reader_for_close in
                        let%bind () = Reader.close stderr_reader_for_close in
                        close_for_id global_stdout;
                        fprintf (force Writer.stderr) "PROC DONE %s\n" id;
                        return res
                      in
                      (* let%bind () = glue_wait_stdout in
              let%bind () = glue_wait_stderr in *)
                      res))
              | Message s ->
                fprintf (force Writer.stderr) "TRY MESSAGE %s\n" id;
                let ivar =
                  match Hashtbl.find writers id with
                  | None ->
                    let ivar = Ivar.create () in
                    Hashtbl.add_exn writers ~key:id ~data:ivar;
                    ivar
                  | Some ivar -> ivar
                in
                let%bind writer = Ivar.read ivar in
                Writer.write_bytes writer s;
                fprintf (force Writer.stderr) "OK MESSAGE %s\n" id;
                Deferred.Or_error.return ()
              | Close ->
                fprintf (force Writer.stderr) "TRY CLOSE %s\n" id;
                let ivar =
                  match Hashtbl.find writers id with
                  | None ->
                    let ivar = Ivar.create () in
                    Hashtbl.add_exn writers ~key:id ~data:ivar;
                    ivar
                  | Some ivar -> ivar
                in
                let%bind writer = Ivar.read ivar in
                let%bind () = Writer.close writer in
                fprintf (force Writer.stderr) "OK CLOSE %s\n" id;
                Deferred.Or_error.return ()
            in
            return (res :: accum))
      in
      Deferred.all write_deferred
      |> Deferred.map ~f:(fun x -> Or_error.combine_errors x |> Or_error.map ~f:ignore))
  |> Deferred.map ~f:(fun x -> Result.map_error x ~f:Error.of_exn |> Or_error.join)
;;
