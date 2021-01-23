open! Core
open Async

let run_command =
  Command.async
    ~summary:"The Shard accessible reliable distributed shell"
    Command.Let_syntax.(
      let%map_open sexp_mode =
        flag "-s" no_arg ~doc:"Use s-expressions instead of the grammar for input."
      and verbose = flag "-V" no_arg ~doc:"Verbose mode."
      and filename = anon (maybe ("filename" %: string)) in
      fun () -> Shard.run ~sexp_mode ?filename ~verbose ())
;;

let () = Command.run run_command
