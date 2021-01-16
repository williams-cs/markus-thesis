open Core
open Async

let rec builtin_cd ~env ~args =
  match args with
  | [] ->
    let home = Unix.getenv "HOME" in
    (match home with
    | None ->
      print_endline "cd: no home directory";
      return 1
    | Some dir -> builtin_cd ~env ~args:[ dir ])
  | [ dir ] ->
    let%bind res =
      Async.try_with (fun () ->
          let%bind () = Unix.chdir (Env.cwd env) in
          let%bind () = Unix.chdir dir in
          let%bind cwd = Unix.getcwd () in
          Env.cd env ~dir:cwd;
          return 0)
    in
    (match res with
    | Ok code -> return code
    | Error exn ->
      print_endline (exn |> Error.of_exn |> Error.to_string_hum);
      return 1)
  | _ ->
    print_endline "cd: too many arguments";
    return 1
;;

let builtin_exit ~env:_ ~args =
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
