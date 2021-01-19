open Core
open Async

let rec builtin_cd ~env ~stdout ~stderr ~args =
  match args with
  | [] ->
    let home = Unix.getenv "HOME" in
    (match home with
    | None ->
      Writer.write_line stderr "cd: no home directory";
      return 1
    | Some dir -> builtin_cd ~env ~stdout ~stderr ~args:[ dir ])
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
      Writer.write_line stderr (exn |> Error.of_exn |> Error.to_string_hum);
      return 1)
  | _ ->
    Writer.write_line stderr "cd: too many arguments";
    return 1
;;

let builtin_exit ~env:_ ~stdout:_ ~stderr ~args =
  match args with
  | [] ->
    shutdown 0;
    return 0
  | [ x ] ->
    (match Or_error.try_with (fun () -> Int.of_string x) with
    | Error _ ->
      Writer.write_line stderr "exit: numeric argument required";
      return 1
    | Ok code ->
      shutdown code;
      return code)
  | _ ->
    Writer.write_line stderr "exit: too many arguments";
    return 1
;;

(* Returns: flags list * args list. Fails if flag is not in [valid_flags]. *)
let separate_flags args ~valid_flags =
  let rec helper args res_flags =
    match args with
    | [] -> Result.return (res_flags, [])
    | x :: xs ->
      if String.equal (String.slice x 0 1) "-"
      then (
        let arg_name = String.slice x 1 0 in
        if List.exists valid_flags ~f:(fun x -> String.equal x arg_name)
        then helper xs (arg_name :: res_flags)
        else Result.fail arg_name)
      else Result.return (res_flags, args)
  in
  helper args []
;;

let validate_identifier _id =
  (* TODO identifier validation *)
  true
;;

let builtin_export ~env ~stdout ~stderr ~args =
  let print_exports () =
    Env.exports_print env ~write_callback:(fun line -> Writer.write_line stdout line)
  in
  match args with
  | [] ->
    print_exports ();
    return 0
  | _ ->
    (match separate_flags args ~valid_flags:[ "p" ] with
    | Ok (flags, args) ->
      List.iter args ~f:(fun arg ->
          let parts = String.split arg ~on:'=' in
          match parts with
          | [] -> ()
          | [ key ] -> if validate_identifier key then Env.export_add env ~key
          | key :: values ->
            if validate_identifier key
            then (
              let data = String.concat ~sep:"=" values in
              Env.assign_set env ~key ~data |> ignore;
              Env.export_add env ~key));
      if List.exists flags ~f:(fun x -> String.equal x "p") then print_exports ();
      return 0
    | Error invalid_flag ->
      Writer.write_line stderr (sprintf "export: %s: invalid option" invalid_flag);
      return 1)
;;

let builtins =
  Map.of_alist_exn
    (module String)
    [ "cd", builtin_cd; "exit", builtin_exit; "export", builtin_export ]
;;
