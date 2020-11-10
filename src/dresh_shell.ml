open Core
open Async

let print_all readers writer to_close =
  let rec print_remaining_output readers =
    match readers with
    | [] -> return ()
    | head :: tail ->
      let%bind maybe_line = Reader.read_until head (`Char '\n') ~keep_delim:true in
      (match maybe_line with
      | `Eof -> print_remaining_output tail
      | `Ok line | `Eof_without_delim line ->
        Writer.write writer line;
        print_remaining_output (tail @ [ head ]))
  in
  let%bind () = print_remaining_output readers in
  if to_close then Writer.close writer else return ()
;;

let rec wait_until_exit process =
  let%bind wait_result = Process.wait process in
  match wait_result with
  | Error err ->
    (match err with
    | `Exit_non_zero v -> return v
    | `Signal _ -> wait_until_exit process)
  | Ok () -> return 0
;;

let eval_command prog args writer to_close =
  let builtin = Map.find Builtin.builtins prog in
  match builtin with
  | Some fn -> fn args
  | None ->
    let%bind process = Process.create ~prog ~args () in
    (match process with
    | Error err ->
      print_endline (Error.to_string_hum err);
      if to_close then Writer.close writer else return ()
    | Ok process ->
      let pid = Pid.to_int (Process.pid process) in
      let child_stdout = Process.stdout process in
      let child_stderr = Process.stderr process in
      let%bind () = print_all [ child_stdout; child_stderr ] writer to_close in
      let%map exit_code = wait_until_exit process in
      printf "Child process %i exited with status %i\n" pid exit_code)
;;

let deferred_iter l =
  let rec helper l acc =
    match l with
    | [] -> return acc
    | x :: xs ->
      let%bind y = x () in
      helper xs (y :: acc)
  in
  let%map res = helper l [] in
  List.rev res
;;

let rec eval ast writer to_close =
  let open Ast in
  match ast with
  | Noop -> return ()
  | Command (name, args) ->
    let%bind name = eval_token name in
    let%bind args =
      List.map ~f:(fun token () -> eval_token token) args |> deferred_iter
    in
    eval_command name args writer to_close
  | Series (ast1, ast2) ->
    let%bind _ = eval ast1 writer to_close in
    eval ast2 writer to_close

and eval_token_part =
  let open Ast in
  function
  | Subshell ss ->
    let ivar = Ivar.create () in
    let pipe =
      Pipe.create_writer (fun reader ->
          let%map queue = Pipe.read_all reader in
          let res = Queue.fold queue ~init:"" ~f:(fun accum s -> accum ^ s) in
          (* Remove trailing newline *)
          let clean_res =
            match String.suffix res 1 with
            | "\n" -> String.sub ~pos:0 ~len:(String.length res - 1) res
            | _ -> res
          in
          Ivar.fill ivar clean_res)
    in
    let%bind writer, _ = Writer.of_pipe (Info.of_string "eval pipe writer") pipe in
    let%bind () = eval ss writer true in
    Ivar.read ivar
  | Variable _v -> return ""
  | Literal s -> return s

and eval_token token_parts =
  let%map res =
    List.map ~f:(fun part () -> eval_token_part part) token_parts |> deferred_iter
  in
  String.concat res
;;

let run () =
  let stdin = force Reader.stdin in
  let stdout = force Writer.stdout in
  let rec repl () =
    let prompt = "$ " in
    print_string prompt;
    let%bind maybe_line = Reader.read_line stdin in
    match maybe_line with
    | `Eof -> return ()
    | `Ok line ->
      let maybe_ast = Ast.parse line in
      let%bind _ =
        match maybe_ast with
        | Error err ->
          printf "Parse error %s\n" (Error.to_string_hum err);
          return ()
        | Ok ast -> eval ast stdout false
      in
      repl ()
  in
  let%map _ = repl () in
  shutdown 0
;;
