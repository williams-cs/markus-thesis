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
  let eval_run ast ~env =
    eval ast ~eval_args:(Eval_args.create ~env ~stdin:None ~stdout ~verbose:false)
  in
  let%bind isatty = Unix.isatty (Fd.stdin ()) in
  let env = Env.create ~working_directory:cwd in
  let read_enviornment_variables ~env =
    Array.iter (Unix.environment ()) ~f:(fun assignment ->
        Builtin.export_single assignment ~env)
  in
  read_enviornment_variables ~env;
  let rec repl ?state prior_input =
    if isatty
    then (
      let prompt = if Option.is_none state then "$ " else "> " in
      print_string prompt);
    let sexp_mode =
      match sexp_mode with
      | None -> false
      | Some b -> b
    in
    if sexp_mode
    then (
      let%bind res =
        Async.try_with (fun () ->
            let%bind sexp = Reader.read_sexp stdin in
            match sexp with
            | `Ok sexp ->
              let%bind () = Ast.t_of_sexp sexp |> eval_run ~env |> Deferred.ignore_m in
              return true
            | `Eof -> return false)
      in
      match res with
      | Ok cont -> if cont then repl "" else return ()
      | Error err ->
        fprintf
          stderr
          "Sexp parse error: %s\n"
          (err |> Error.of_exn |> Error.to_string_hum);
        return ())
    else (
      let%bind maybe_line = Reader.read_line stdin in
      match maybe_line with
      | `Eof -> return ()
      | `Ok line ->
        let line = line ^ "\n" in
        let input = prior_input ^ line in
        (match Ast.parse input with
        | Ok complete ->
          let%bind () = eval_run complete ~env |> Deferred.ignore_m in
          repl ""
        | Error _err ->
          let new_state = Ast.parse_partial ?state line in
          (match new_state with
          | Partial p -> repl ~state:(Partial p) input
          | Done (unconsumed, ast) ->
            let%bind () =
              if unconsumed.len > 0
              then (
                let msg = "Unconsumed input remaining!" in
                fprintf stderr "Parse error: %s\n" msg;
                return ())
              else eval_run ast ~env |> Deferred.ignore_m
            in
            repl ""
          | Fail (_unconsumed, _marks, msg) ->
            fprintf stderr "Parse error: %s\n" msg;
            repl "")))
  in
  let%map _ = repl "" in
  shutdown 0
;;
