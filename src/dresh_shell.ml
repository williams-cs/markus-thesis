open! Core
open! Async

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

let eval_command command =
  let parts =
    String.split ~on:' ' command
    |> List.filter ~f:(fun x -> not (String.is_empty x))
    |> List.map ~f:String.strip
  in
  match parts with
  | [] -> return ()
  | prog :: args ->
    let builtin = Map.find Builtin.builtins prog in
    (match builtin with
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
        printf "Child process %i exited with status %i\n" pid exit_code))
;;

let deferred_iter f l =
  let rec deferred_iter_helper l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: xs ->
      let%bind v = f x in
      deferred_iter_helper xs (v :: acc)
  in
  deferred_iter_helper l []
;;

let eval line =
  let commands = String.split ~on:';' line in
  deferred_iter eval_command commands
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
      let%bind _ = eval line in
      repl ()
  in
  let%map _ = repl () in
  shutdown 0
;;
