open Core
open Async
open Util

exception ExitExn of int

type t =
  | Function of
      (env:Cluster_type.t Env.t
       -> stdout:Writer.t
       -> stderr:Writer.t
       -> args:string list
       -> int Deferred.t)
  | Source

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
  | [] -> raise (ExitExn 0)
  | [ x ] ->
    (match Or_error.try_with (fun () -> Int.of_string x) with
    | Error _ ->
      fprintf stderr "exit: numeric argument required\n";
      return 1
    | Ok code -> raise (ExitExn code))
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
  -t Set the type of the cluster (options: adhoc, mapreduce; default: adhoc)
  -c (no arg) Reset the cluster
If more than one option is specified, command is invalid.
*)
let builtin_cluster ~env ~stdout ~stderr ~args =
  match separate_flags args ~valid_flags:[ "a"; "c"; "p"; "s"; "t" ] with
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
      let active_cluster = Env.cluster_get_active env in
      (match Env.Cluster.add active_cluster args with
      | Ok () -> ()
      | Error l ->
        List.iter l ~f:(fun h ->
            fprintf stderr "cluster: error resolving host for %s\n" h));
      return 0
    | [ "t" ] ->
      (match args with
      | [ s ] ->
        let active_cluster = Env.cluster_get_active env in
        (match Cluster_type.maybe_t_of_string s with
        | Some cluster_type ->
          Env.Cluster.set_type active_cluster cluster_type;
          return 0
        | None ->
          fprintf stderr "cluster: cluster type not found\n";
          return 1)
      | _ ->
        fprintf stderr "cluster: incorrect number of arguments for cluster type\n";
        return 1)
    | [ "c" ] ->
      let active_cluster = Env.cluster_get_active env in
      Env.Cluster.clear_remotes active_cluster;
      return 0
    | _ ->
      fprintf stderr "cluster: too many options\n";
      return 1)
  | Error invalid_flag ->
    fprintf stderr "cluster: %s: invalid option\n" invalid_flag;
    return 1
;;

(*
-i: print counter with time (time,counter)
-t: print string with time (time,string)
-T: print string with time (string,time)
-p (or no arg): print string
-b: print counter with string (counter,string)
-c: clear counter
-s: start/reset timer
-n: set id/name
-z: print zero counter with zero time
*)
let internal_log ~env ~stdout:_ ~stderr ~args =
  match
    separate_flags args ~valid_flags:[ "c"; "i"; "p"; "t"; "T"; "b"; "s"; "n"; "z" ]
  with
  | Ok (flags, args) ->
    let log_time_key = Util.shard_internal "log_time" in
    let log_counter_key = Util.shard_internal "log_counter" in
    let log_id_key = Util.shard_internal "log_id" in
    let log_id = Env.assign_get env ~key:log_id_key in
    let log_id = if String.equal log_id "" then "_internal" else log_id in
    let log = Util.log ~id:log_id in
    let time_start () =
      let time_start_str =
        Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string
      in
      Env.assign_set env ~key:log_time_key ~data:time_start_str |> ignore
    in
    let time_elapsed () =
      let time_start_str = Env.assign_get env ~key:log_time_key in
      let time_start_str =
        if String.equal time_start_str ""
        then (
          time_start ();
          Env.assign_get env ~key:log_time_key)
        else time_start_str
      in
      let time_start_ns =
        Int63.of_string time_start_str |> Time_ns.of_int63_ns_since_epoch
      in
      Time_ns.diff (Time_ns.now ()) time_start_ns |> Time_ns.Span.to_sec
    in
    (match flags with
    | [] | [ "p" ] ->
      Log.printf log "%s" (String.concat ~sep:" " args);
      return 0
    | [ "s" ] ->
      time_start ();
      return 0
    | [ "t" ] ->
      let time_elapsed = time_elapsed () in
      Log.printf log "%f,%s" time_elapsed (String.concat ~sep:" " args);
      return 0
    | [ "T" ] ->
      let time_elapsed = time_elapsed () in
      Log.printf log "%s,%f" (String.concat ~sep:" " args) time_elapsed;
      return 0
    | [ "c" ] ->
      Env.assign_set env ~key:log_counter_key ~data:"0" |> ignore;
      return 0
    | [ "i" ] ->
      let time_elapsed = time_elapsed () in
      let counter_string = Env.assign_get env ~key:log_counter_key in
      let counter =
        if String.equal counter_string "" then 0 else Int.of_string counter_string
      in
      let counter = counter + 1 in
      Env.assign_set env ~key:log_counter_key ~data:(Int.to_string counter) |> ignore;
      Log.printf log "%f,%d" time_elapsed counter;
      return 0
    | [ "b" ] ->
      let counter_string = Env.assign_get env ~key:log_counter_key in
      let counter =
        if String.equal counter_string "" then 0 else Int.of_string counter_string
      in
      let counter = counter + 1 in
      Env.assign_set env ~key:log_counter_key ~data:(Int.to_string counter) |> ignore;
      Log.printf log "%d,%s" counter (String.concat ~sep:" " args);
      return 0
    | [ "z" ] ->
      Log.printf log "0.0,0";
      return 0
    | [ "n" ] ->
      Env.assign_set env ~key:log_id_key ~data:(String.concat ~sep:" " args) |> ignore;
      return 0
    | _ ->
      fprintf stderr "_log: too many options\n";
      return 1)
  | Error invalid_flag ->
    fprintf stderr "_log: %s: invalid option\n" invalid_flag;
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
    ; "_log", Function internal_log
    ]
;;
