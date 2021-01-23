open Core
open Async
open Eval
module Ast = Ast
module Env = Env
module Eval = Eval

let setup_signal_handlers () = ()

(* Signal.handle [ Signal.int ] ~f:(fun _signal -> print_endline "cancel") *)

let run ?sexp_mode ?filename () =
  setup_signal_handlers ();
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
  let filename =
    match filename with
    | None -> None
    | Some "" -> None
    | s -> s
  in
  let stdin, isatty =
    match filename with
    | None -> return stdin, Unix.isatty (Fd.stdin ())
    | Some name -> Reader.open_file name, return false
  in
  let%bind stdin = stdin in
  let%bind isatty = isatty in
  let eval_args = Eval_args.create ~env ~stdin:None ~stdout ~verbose:false in
  let%map exit_code =
    eval_lines ?sexp_mode ~interactive:isatty ~stdin ~stdout ~stderr ~eval_args ()
  in
  shutdown exit_code
;;
