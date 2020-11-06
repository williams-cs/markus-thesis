open Core
open Async

let builtin_cd args =
  match args with
  | [] ->
    let home = Unix.getenv "HOME" in
    (match home with
    | None ->
      print_endline "cd: no home directory";
      return ()
    | Some dir -> Unix.chdir dir)
  | [ dir ] -> Unix.chdir dir
  | _ ->
    print_endline "cd: too many arguments";
    return ()
;;

let builtin_exit args =
  match args with
  | [] ->
    shutdown 0;
    return ()
  | [ x ] ->
    (match Or_error.try_with (fun () -> Int.of_string x) with
    | Error _ ->
      print_endline "exit: numeric argument required";
      return ()
    | Ok code ->
      shutdown code;
      return ())
  | _ ->
    print_endline "exit: too many arguments";
    return ()
;;

let builtins = Map.of_alist_exn (module String) [ "cd", builtin_cd; "exit", builtin_exit ]
