open Core
open Async
open Eval
module Ast = Ast
module Env = Env
module Eval = Eval

let run ?sexp_mode () =
  let stdin = force Reader.stdin in
  let stdout = force Writer.stdout in
  let stderr = force Writer.stderr in
  let%bind cwd = Unix.getcwd () in
  let env = Env.create ~working_directory:cwd in
  let read_enviornment_variables ~env =
    Array.iter (Unix.environment ()) ~f:(fun assignment ->
        Builtin.export_single assignment ~env)
  in
  read_enviornment_variables ~env;
  let%bind isatty = Unix.isatty (Fd.stdin ()) in
  let%map exit_code =
    eval_lines
      ?sexp_mode
      ~interactive:isatty
      ~stdin
      ~stdout
      ~stderr
      ~env
      ~maybe_eval_args:None
      ()
  in
  shutdown exit_code
;;
