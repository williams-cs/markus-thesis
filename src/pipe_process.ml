open Core
open Async

type env = Unix.env [@@deriving sexp]

type t =
  { pid : Pid.t
  ; stdin : Fd.t
  ; stdout : Fd.t
  ; stderr : Fd.t
  ; prog : string
  ; args : string list
  ; working_dir : string option
  ; env : env
  ; wait : Unix.Exit_or_signal.t Async.Deferred.t Lazy.t
  }
[@@deriving fields, sexp_of]

let pipe () =
  let read, write = Spawn.safe_pipe () in
  ( Fd.create Fd.Kind.Fifo read (Info.of_string "pipe_read")
  , Fd.create Fd.Kind.Fifo write (Info.of_string "pipe_write") )
;;

let closed_read_fd () =
  let read, write = Spawn.safe_pipe () in
  Core.Unix.close write;
  Fd.create Fd.Kind.Fifo read (Info.of_string "pipe_read")
;;

(* Code adapted from core_unix.ml *)
(* in_fd should be a reader*)
let create_process_internal ~working_dir ~prog ~argv ~env ~in_fd ~out_fd ~err_fd =
  let close = Core.Unix.close in
  let close_on_err = ref [] in
  try
    let pid =
      Spawn.spawn
        ?cwd:(Option.map working_dir ~f:(fun x -> Spawn.Working_dir.Path x))
        ~prog
        ~argv
        ~env:(Spawn.Env.of_list env)
        ~stdin:in_fd
        ~stdout:out_fd
        ~stderr:err_fd
        ()
      |> Pid.of_int
    in
    (* close in_fd;
    close out_fd;
    close err_fd; *)
    pid
  with
  | exn ->
    List.iter !close_on_err ~f:(fun x ->
        try close x with
        | _ -> ());
    raise exn
;;

(* Module copied from core_unix.ml *)
module Execvp_emulation : sig
  (* This is a reimplementation of execvp semantics with two main differences:
     - it does [spawn] instead of [execve] and returns its result on success
     - it checks file existence and access rights before trying to spawn.
       This optimization is valuable because a failed [spawn] is much more expensive than a
       failed [execve]. *)
  val run
    :  working_dir:string option
    -> spawn:(prog:string -> argv:string list -> 'a)
    -> prog:string
    -> args:string list
    -> ?prog_search_path:string list
    -> ?argv0:string
    -> unit
    -> 'a
end = struct
  let get_path prog_search_path =
    match prog_search_path with
    | Some [] -> invalid_arg "Core.Unix.create_process: empty prog_search_path"
    | Some dirs -> dirs
    | None ->
      Sys.getenv "PATH"
      |> Option.value_map ~f:(String.split ~on:':') ~default:[ "/bin"; "/usr/bin" ]
      |> List.map ~f:(function
             | "" -> "."
             | x -> x)
  ;;

  let candidate_paths ?prog_search_path prog =
    (* [assert] is to make bugs less subtle if we try to make this
       portable to non-POSIX in the future. *)
    (* assert (Filename.dir_sep = "/"); *)
    if String.contains prog '/'
    then [ prog ]
    else List.map (get_path prog_search_path) ~f:(fun h -> h ^/ prog)
  ;;

  type 'a spawn1_result =
    | Eaccess of exn
    | Enoent_or_similar of exn
    | Ok of 'a

  let run ~working_dir ~spawn ~prog ~args ?prog_search_path ?argv0 () =
    let open Unix in
    let argv = Option.value argv0 ~default:prog :: args in
    let spawn1 candidate =
      match
        (try
           Result.ok_exn
             (Core.Unix.access
                (if not (Filename.is_relative candidate)
                then candidate
                else (
                  match working_dir with
                  | Some working_dir -> working_dir ^/ candidate
                  | None -> candidate))
                [ `Exec ])
         with
        | Unix_error (code, _, args) ->
          raise (Unix_error (code, "Core.Unix.create_process", args)));
        spawn ~prog:candidate ~argv
      with
      | exception Unix_error (ENOEXEC, _, _) ->
        Ok
          ((* As crazy as it looks, this is what execvp does. It's even documented in the man
           page. *)
           spawn
             ~prog:"/bin/sh"
             ~argv:("/bin/sh" :: candidate :: args))
      | exception (Unix_error (EACCES, _, _) as exn) -> Eaccess exn
      | exception
          (Unix_error
             ( (* This list of nonfatal errors comes from glibc and openbsd implementations of
           execvpe, as collected in [execvpe_ml] function in ocaml (see
           otherlibs/unix/unix.ml in https://github.com/ocaml/ocaml/pull/1414). *)
               ( EISDIR | ELOOP | ENAMETOOLONG | ENODEV | ENOENT | ENOTDIR | ETIMEDOUT )
             , _
             , _ ) as exn) -> Enoent_or_similar exn
      | pid -> Ok pid
    in
    let rec go first_eaccess = function
      | [] -> assert false (* [candidate_paths] can't return an empty list *)
      | [ candidate ] ->
        (match spawn1 candidate with
        | Eaccess exn | Enoent_or_similar exn ->
          raise (Option.value first_eaccess ~default:exn)
        | Ok pid -> pid)
      | candidate :: (_ :: _ as candidates) ->
        (match spawn1 candidate with
        | Eaccess exn ->
          let first_eaccess = Some (Option.value first_eaccess ~default:exn) in
          go first_eaccess candidates
        | Enoent_or_similar _exn -> go first_eaccess candidates
        | Ok pid -> pid)
    in
    go None (candidate_paths ?prog_search_path prog)
  ;;
end

let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env ~in_fd () =
  let env_assignments = Core.Unix.Env.expand env in
  Execvp_emulation.run
    ~prog
    ~args
    ?argv0
    ?prog_search_path
    ~working_dir
    ~spawn:(fun ~prog ~argv ->
      create_process_internal ~working_dir ~prog ~argv ~env:env_assignments ~in_fd)
    ()
;;

let create
    ?argv0
    ?((* ?buf_len *)
    env = `Extend [])
    ?prog_search_path
    ?(* ?stdin:write_to_stdin *)
    working_dir
    ~prog
    ~args
    ~in_fd
    ~out_fd
    ~err_fd
    ()
  =
  let in_fd_core = Fd.file_descr_exn in_fd in
  let out_fd_core = Fd.file_descr_exn out_fd in
  let err_fd_core = Fd.file_descr_exn err_fd in
  match%map
    In_thread.syscall ~name:"create_process_env" (fun () ->
        create_process_env
          ~prog
          ~args
          ~env
          ?working_dir
          ?prog_search_path
          ?argv0
          ~in_fd:in_fd_core
          ~out_fd:out_fd_core
          ~err_fd:err_fd_core
          ())
  with
  | Error exn -> Or_error.of_exn exn
  | Ok pid ->
    (* let create_fd name file_descr =
      Fd.create
        Fifo
        file_descr
        (Info.create
           "child process"
           (* ~here:[%here] *)
           (name, `pid pid, `prog prog, `args args)
           [%sexp_of:
             string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]])
    in
    let stdin =
      let fd = create_fd "stdin" stdin in
      match write_to_stdin with
      | None -> Writer.create ?buf_len fd
      | Some _ ->
        Writer.create
          ?buf_len
          fd
          ~buffer_age_limit:`Unlimited
          ~raise_when_consumer_leaves:false
    in *)
    let t =
      { pid
      ; stdin = in_fd
      ; stdout = out_fd
      ; stderr = err_fd
      ; prog
      ; args
      ; working_dir
      ; env
      ; wait = lazy (Unix.waitpid_prompt pid)
      }
    in
    Ok t
;;

let wait t = force t.wait
