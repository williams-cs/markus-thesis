open Core
open Async
module Ast = Ast

let print_all readers ~writer =
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

let eval_command prog args ~writer ~verbose =
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
      let%bind () = print_all [ child_stdout; child_stderr ] ~writer in
      if verbose
      then (
        let%map exit_code = wait_until_exit process in
        printf "Child process %i exited with status %i\n" pid exit_code)
      else return ())
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

let rec eval ast ~writer ~verbose =
  let open Ast in
  match ast with
  | Command args ->
    let%bind args =
      List.map ~f:(fun token () -> eval_token token ~verbose) args |> deferred_iter
    in
    let args = List.concat args in
    (match args with
    | [] -> return ()
    | name :: args -> eval_command name args ~writer ~verbose)
  | Series (ast1, ast2) ->
    let%bind _ = eval ast1 ~writer ~verbose in
    eval ast2 ~writer ~verbose

and eval_token_part ~verbose =
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
    let%bind () = eval ss ~writer ~verbose in
    let%bind () = Writer.close writer in
    let%bind s = Ivar.read ivar in
    let insert_splits sl =
      let sl_mod = List.bind sl ~f:(fun x -> [ None; Some x ]) in
      match sl_mod with
      | _x :: xs -> xs
      | [] -> []
    in
    return (s |> parse_field_split |> insert_splits)
  | Variable _v -> return [ Some "" ]
  | Literal s -> return [ Some s ]

and eval_token token_parts ~verbose : string list Deferred.t =
  let%map res =
    List.map ~f:(fun part () -> eval_token_part part ~verbose) token_parts
    |> deferred_iter
  in
  let strings_with_splits = List.concat res in
  let z =
    List.fold strings_with_splits ~init:[] ~f:(fun accum x ->
        match accum with
        | [] ->
          (match x with
          | Some s -> [ s ]
          | None -> [])
        | y :: ys ->
          (match x with
          | Some s -> (y ^ s) :: ys
          | None -> "" :: accum))
    |> List.rev
  in
  z
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
        | Ok ast -> eval ast ~writer:stdout ~verbose:true
      in
      repl ()
  in
  let%map _ = repl () in
  shutdown 0
;;
