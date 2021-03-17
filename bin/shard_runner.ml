open! Core
open Async

type rpc_type =
  | Local_receiver
  | Local_sender
  | Remote_receiver
  | Remote_sender of int

let run_command =
  Command.async_or_error
    ~summary:"The Shard ad-hoc reliable distributed shell"
    Command.Let_syntax.(
      let%map_open sexp_mode =
        flag "-s" no_arg ~doc:"Use s-expressions instead of the grammar for input."
      and rpc =
        Command.Param.choose_one
          ~if_nothing_chosen:Return_none
          [ flag
              "-Rlr"
              no_arg
              ~doc:
                "Set up an rpc local receiver for remote program on an open port \
                 (internal use)."
            |> Command.Param.map ~f:(fun x -> if x then Some Local_receiver else None)
          ; flag
              "-Rls"
              no_arg
              ~doc:
                "Set up an rpc local sender for remote program on an open port (internal \
                 use)."
            |> Command.Param.map ~f:(fun x -> if x then Some Local_sender else None)
          ; flag
              "-Rrr"
              no_arg
              ~doc:
                "Set up an rpc remote receiver for remote program on an open port \
                 (internal use)."
            |> Command.Param.map ~f:(fun x -> if x then Some Remote_receiver else None)
          ; flag
              "-Rrs"
              (optional int)
              ~doc:
                "Set up an rpc remote sender for remote program on an open port \
                 (internal use)."
            |> Command.Param.map ~f:(fun x ->
                   Option.map x ~f:(fun port -> Remote_sender port))
          ]
      and verbose = flag "-V" no_arg ~doc:"Verbose mode."
      and filename = anon (maybe ("filename" %: string)) in
      fun () ->
        let log_remote rerror =
          let%map.Deferred () =
            match rerror with
            | Ok _ -> Deferred.return ()
            | Error error ->
              Writer.with_file ~append:true "/tmp/shard/rlog.txt" ~f:(fun writer ->
                  Writer.write writer (Error.to_string_hum error);
                  Deferred.return ())
          in
          rerror
        in
        match rpc with
        | Some rpc_type ->
          (match rpc_type with
          | Local_receiver ->
            Shard.Remote_rpc.start_local_receiver ~verbose
            |> Deferred.map ~f:Or_error.return
          | Local_sender ->
            Shard.Remote_rpc.start_local_sender ~verbose
            |> Deferred.map ~f:Or_error.return
          | Remote_receiver ->
            let%bind.Deferred rerror =
              Shard.Remote_rpc.start_remote_receiver ~verbose
              |> Deferred.map ~f:Or_error.return
            in
            log_remote rerror
          | Remote_sender remote_port ->
            let (module M) = Shard.Cluster_type.provider in
            let%bind.Deferred rerror =
              Shard.Remote_rpc.start_remote_sender
                (module M)
                ~verbose
                ~remote_port
                ~runner:(fun ~verbose ~prog ~env ~eval_args_stdin ~stdout ~stderr ->
                  Shard.run_with_io
                    ~verbose
                    ~prog_input:(Shard.Eval.Prog_input.Sexp prog)
                    ~env
                    ~eval_args_stdin
                    ~stdout
                    ~stderr
                    ~isatty:false
                    ()
                  |> Deferred.map ~f:(fun x ->
                         match x with
                         | 0 -> Or_error.return ()
                         | _ -> Or_error.errorf "Exit with code %d\n" x))
            in
            log_remote rerror)
        | None ->
          Shard.run ~sexp_mode ?filename ~verbose ()
          |> Deferred.map ~f:(fun x ->
                 match x with
                 | 0 -> Or_error.return ()
                 | _ -> Or_error.errorf "Exit with code %d" x))
;;

let () = Command.run run_command
