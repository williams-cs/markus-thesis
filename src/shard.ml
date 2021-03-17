open Core
open Async
open Eval
module Ast = Ast
module Cluster_type = Cluster_type
module Env = Env
module Eval = Eval
module Remote_rpc = Remote_rpc
module Remote_ssh = Remote_ssh
module Util = Util

let active_job_group : Job.Job_group.t option ref = ref None

let setup_signal_handlers ~interactive =
  Signal.handle [ Signal.int ] ~f:(fun _signal ->
      let maybe_group =
        match interactive with
        | true -> !active_job_group
        | false -> Some Job.all_jobs
      in
      match maybe_group with
      | Some group -> Job.Job_group.cancel group
      | None -> ())
;;

let run_with_io ?verbose ~prog_input ~env ~eval_args_stdin ~stdout ~stderr ~isatty () =
  let read_enviornment_variables ~env =
    Array.iter (Unix.environment ()) ~f:(fun assignment ->
        Builtin.export_single assignment ~env)
  in
  read_enviornment_variables ~env;
  setup_signal_handlers ~interactive:isatty;
  let verbose =
    match verbose with
    | None -> false
    | Some b -> b
  in
  let eval_args = Eval_args.create ~env ~stdin:eval_args_stdin ~stdout ~stderr ~verbose in
  active_job_group := Some (Env.job_group env);
  let%map exit_code =
    eval_lines ~interactive:isatty ~prog_input ~stdout ~stderr ~eval_args ()
  in
  (* shutdown exit_code *)
  exit_code
;;

let create_env ~working_directory =
  let (module Provider : Application_class.Provider with type t = Cluster_type.t) =
    Cluster_type.provider
  in
  Env.create (module Provider) ~working_directory
;;

let run ?sexp_mode ?filename ?verbose () =
  let stdin = force Reader.stdin in
  let stdout = force Writer.stdout in
  let stderr = force Writer.stderr in
  let filename =
    match filename with
    | None -> None
    | Some "" -> None
    | s -> s
  in
  let prog_stdin, eval_args_stdin, isatty =
    match filename with
    | None -> return stdin, None, Unix.isatty (Fd.stdin ())
    | Some name -> Reader.open_file name, Some stdin, return false
  in
  let%bind prog_stdin = prog_stdin in
  let%bind isatty = isatty in
  let prog_input =
    Eval.Prog_input.Stream
      ( prog_stdin
      , match sexp_mode with
        | None -> Not_sexp
        | Some b -> if b then Sexp else Not_sexp )
  in
  let%bind cwd = Unix.getcwd () in
  let env = create_env ~working_directory:cwd in
  run_with_io ?verbose ~prog_input ~env ~eval_args_stdin ~stdout ~stderr ~isatty ()
;;
