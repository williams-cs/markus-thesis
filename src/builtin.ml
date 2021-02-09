open Core
open Async

type t =
  | Function of
      (env:Env.t
       -> stdout:Writer.t
       -> stderr:Writer.t
       -> args:string list
       -> int Deferred.t)
  | Source

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

let rec builtin_cd ~env ~stdout ~stderr ~args =
  match args with
  | [] ->
    let home = Env.assign_get env ~key:"HOME" in
    (match home with
    | "" ->
      fprintf stderr "cd: no home directory\n";
      return 1
    | dir -> builtin_cd ~env ~stdout ~stderr ~args:[ dir ])
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
      fprintf stderr "%s\n" (exn |> Error.of_exn |> Error.to_string_hum);
      return 1)
  | _ ->
    fprintf stderr "cd: too many arguments\n";
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
      fprintf stderr "exit: numeric argument required\n";
      return 1
    | Ok code ->
      shutdown code;
      return code)
  | _ ->
    fprintf stderr "exit: too many arguments\n";
    return 1
;;

let validate_identifier _id =
  (* TODO identifier validation *)
  true
;;

let export_single arg ~env =
  let parts = String.split arg ~on:'=' in
  match parts with
  | [] -> ()
  | [ key ] -> if validate_identifier key then Env.export_add env ~key
  | key :: values ->
    if validate_identifier key
    then (
      let data = String.concat ~sep:"=" values in
      Env.assign_set env ~key ~data |> ignore;
      Env.export_add env ~key)
;;

let has_flag flags name = List.mem flags name ~equal:String.equal

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
      List.iter args ~f:(fun arg -> export_single arg ~env);
      if has_flag flags "p" then print_exports ();
      return 0
    | Error invalid_flag ->
      fprintf stderr "export: %s: invalid option\n" invalid_flag;
      return 1)
;;

(** Builtin: cluster [-aps] NAME. 
Options:
  -s Set the currently modified cluster to that of the specified name (default cluster is named "default"). If it does
     not already exist, create the cluster.
  -a Add the specified ssh profile to the cluster
  -p (or no argument) Prints out the details of the cluster
If more than one option is specified, command is invalid.
*)
let builtin_cluster ~env ~stdout ~stderr ~args =
  match separate_flags args ~valid_flags:[ "a"; "p"; "s" ] with
  | Ok (flags, args) ->
    (match flags with
    | [] | [ "p" ] ->
      Env.cluster_print env args ~write_callback:(fun line ->
          Writer.write_line stdout line);
      return 0
    | [ "s" ] ->
      Env.cluster_set_active env (List.last args);
      return 0
    | [ "a" ] ->
      (match Env.cluster_add env args with
      | Ok () -> ()
      | Error l ->
        List.iter l ~f:(fun h -> fprintf stderr "cluster: error resolving host for %s" h));
      return 0
    | _ ->
      fprintf stderr "cluster: too many options\n";
      return 1)
  | Error invalid_flag ->
    fprintf stderr "cluster: %s: invalid option\n" invalid_flag;
    return 1
;;

let builtins =
  Map.of_alist_exn
    (module String)
    [ "cd", Function builtin_cd
    ; "exit", Function builtin_exit
    ; "export", Function builtin_export
    ; "cluster", Function builtin_cluster
    ; "source", Source
    ; ".", Source
    ]
;;
