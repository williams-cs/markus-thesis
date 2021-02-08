open Core
open Async
open Eval
module Ast = Ast
module Env = Env
module Eval = Eval
module Remote_rpc = Remote_rpc
module Remote_ssh = Remote_ssh
module Util = Util

let active_job_group : Job.Job_group.t option ref = ref None

let setup_signal_handlers () =
  Signal.handle [ Signal.int ] ~f:(fun _signal ->
      match !active_job_group with
      | Some group -> Job.Job_group.cancel group
      | None -> ())
;;

let run ?sexp_mode ?filename ?verbose () =
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
  let verbose =
    match verbose with
    | None -> false
    | Some b -> b
  in
  let eval_args = Eval_args.create ~env ~stdin:None ~stdout ~stderr ~verbose in
  active_job_group := Some (Env.job_group env);
  let%map exit_code =
    eval_lines ?sexp_mode ~interactive:isatty ~stdin ~stdout ~stderr ~eval_args ()
  in
  shutdown exit_code
;;
