open! Core
open Async

let run_command =
  Command.async
    ~summary:"The Shard accessible reliable distributed shell"
    Command.Let_syntax.(
      let%map_open sexp_mode =
        flag "-s" no_arg ~doc:"Use s-expressions instead of the grammar for input."
      in
      fun () -> Shard.run ~sexp_mode ())
;;

let () = Command.run run_command
