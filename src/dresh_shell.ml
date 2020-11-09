open Core
open Async

let print_all readers =
  let rec print_remaining_output readers =
    match readers with
    | [] -> return ()
    | head :: tail ->
      let%bind maybe_line = Reader.read_line head in
      (match maybe_line with
      | `Eof -> print_remaining_output tail
      | `Ok line ->
        print_endline line;
        print_remaining_output (tail @ [ head ]))
  in
  print_remaining_output readers
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

let eval_command prog args =
  let builtin = Map.find Builtin.builtins prog in
  match builtin with
  | Some fn -> fn args
  | None ->
    let%bind process = Process.create ~prog ~args () in
    (match process with
    | Error err ->
      print_endline (Error.to_string_hum err);
      return ()
    | Ok process ->
      let pid = Pid.to_int (Process.pid process) in
      let child_stdout = Process.stdout process in
      let child_stderr = Process.stderr process in
      let%bind () = print_all [ child_stdout; child_stderr ] in
      let%map exit_code = wait_until_exit process in
      printf "Child process %i exited with status %i\n" pid exit_code)
;;

let rec eval ast =
  let open Ast in
  match ast with
  | Noop -> return ()
  | Command (name, args) -> eval_command name args
  | Series (ast1, ast2) ->
    let%bind _ = eval ast1 in
    eval ast2
;;

let run () =
  let stdin = force Reader.stdin in
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
        | Ok ast -> eval ast
      in
      repl ()
  in
  let%map _ = repl () in
  shutdown 0
;;
