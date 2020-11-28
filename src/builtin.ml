open Core
open Async

let builtin_cd args =
  match args with
  | [] ->
    let home = Unix.getenv "HOME" in
    (match home with
    | None ->
      print_endline "cd: no home directory";
      return 1
    | Some dir ->
      let%bind () = Unix.chdir dir in
      return 0)
  | [ dir ] ->
    let%bind () = Unix.chdir dir in
    return 0
  | _ ->
    print_endline "cd: too many arguments";
    return 1
;;

let builtin_exit args =
  match args with
  | [] ->
    shutdown 0;
    return 0
  | [ x ] ->
    (match Or_error.try_with (fun () -> Int.of_string x) with
    | Error _ ->
      print_endline "exit: numeric argument required";
      return 1
    | Ok code ->
      shutdown code;
      return code)
  | _ ->
    print_endline "exit: too many arguments";
    return 1
;;

let builtins = Map.of_alist_exn (module String) [ "cd", builtin_cd; "exit", builtin_exit ]
