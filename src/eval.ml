open Core
open Async

module Eval_args = struct
  type t =
    { env : Cluster_type.t Env.t
    ; stdin : Fd.t
    ; stdout : Fd.t
    ; stderr : Fd.t
    ; verbose : bool
    }
  [@@deriving fields]

  let create ~env ~stdin ~stdout ~stderr ~verbose =
    { env; stdin; stdout; stderr; verbose }
  ;;

  let create_from_stdio ~env ~stdin ~stdout ~stderr ~verbose =
    let stdin_fd =
      match stdin with
      | None -> Pipe_process.closed_read_fd ()
      | Some s -> Reader.fd s
    in
    let stdout_fd = Writer.fd stdout in
    let stderr_fd = Writer.fd stderr in
    { env; stdin = stdin_fd; stdout = stdout_fd; stderr = stderr_fd; verbose }
  ;;

  let with_env { env = _; stdin; stdout; stderr; verbose } ~env =
    { env; stdin; stdout; stderr; verbose }
  ;;

  let stdin_reader t =
    let stdin = stdin t in
    Reader.create stdin
  ;;

  let stdout_writer t =
    let stdout = stdout t in
    Writer.create stdout
  ;;

  let stderr_writer t =
    let stderr = stderr t in
    Writer.create stderr
  ;;

  let to_string t =
    let fd_to_string fd =
      Fd.file_descr_exn fd |> Core.Unix.File_descr.to_int |> Int.to_string
    in
    let stdin = stdin t |> fd_to_string in
    let stdout = stdout t |> fd_to_string in
    let stderr = stderr t |> fd_to_string in
    let verbose = verbose t in
    (* TODO env string *)
    let env = "[env]" in
    sprintf
      "stdin: %s\nstdout: %s\nstderr: %s\nverbose: %b\nenv: %s"
      stdin
      stdout
      stderr
      verbose
      env
  ;;
end

module Prog_input = struct
  module Sexp_mode = struct
    type t =
      | Sexp
      | Not_sexp
  end

  type t =
    | Stream of Reader.t * Sexp_mode.t
    | Sexp of Sexp.t
end

let wait_until_exit process =
  let%bind wait_result = Pipe_process.wait process in
  match wait_result with
  | Error err ->
    (match err with
    | `Exit_non_zero v -> return v
    | `Signal _signal ->
      (* print_endline (Signal.to_string signal); *)
      (* if List.mem [ Signal.kill; Signal.segv; Signal.quit ] signal ~equal:Signal.equal *)
      (* then *)
      return 1)
    (* else wait_until_exit process) *)
  | Ok () -> return 0
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

let rec eval_command prog args ~eval_args =
  let job_group = eval_args |> Eval_args.env |> Env.job_group in
  if Job.Job_group.canceled job_group
  then return 1
  else (
    let builtin = Map.find Builtin.builtins prog in
    let update_dir () = Unix.chdir (eval_args |> Eval_args.env |> Env.cwd) in
    match builtin with
    | Some builtin ->
      (match builtin with
      | Function fn ->
        let env = Eval_args.env eval_args in
        let stdout = Eval_args.stdout_writer eval_args in
        let stderr = Eval_args.stderr_writer eval_args in
        fn ~env ~stdout ~stderr ~args
      | Source ->
        (match args with
        | [] ->
          fprintf
            (Eval_args.stderr_writer eval_args)
            "source: filename argument required\n";
          return 1
        | arg :: _ ->
          let%bind () = update_dir () in
          eval_source arg ~eval_args))
    | None ->
      let job = Job.create ~groups:[ job_group ] () in
      let%bind () = update_dir () in
      let in_fd = Eval_args.stdin eval_args in
      let out_fd = Eval_args.stdout eval_args in
      let err_fd = Eval_args.stderr eval_args in
      let%bind process = Pipe_process.create ~prog ~args ~in_fd ~out_fd ~err_fd () in
      (match process with
      | Error err ->
        Job.cancel_without_signal job;
        fprintf (Eval_args.stderr_writer eval_args) "%s\n" (Error.to_string_hum err);
        (* TODO debug *)
        (* let s = Eval_args.env eval_args |> Env.assign_get ~key:"id" in
        fprintf
          (force Writer.stderr)
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!eval: %s (%s)\n"
          prog
          s; *)
        return (-1)
      | Ok process ->
        (* Job.connect job ~process; *)
        let pid = Pid.to_int (Pipe_process.pid process) in
        let%bind exit_code = wait_until_exit process in
        let%bind () = Writer.flushed (Eval_args.stdout_writer eval_args) in
        let%bind () = if Job.canceled job then return () else return () in
        if Eval_args.verbose eval_args
        then
          fprintf
            (Eval_args.stderr_writer eval_args)
            "Child process %i exited with status %i\n"
            pid
            exit_code;
        Job.complete job;
        (* TODO debug *)
        (* let s = Eval_args.env eval_args |> Env.assign_get ~key:"id" in
        let ifd = Eval_args.stdin_reader eval_args |> Reader.fd |> Fd.to_int_exn in
        let ofd = Eval_args.stdout_writer eval_args |> Writer.fd |> Fd.to_int_exn in
        let efd = Eval_args.stderr_writer eval_args |> Writer.fd |> Fd.to_int_exn in
        fprintf
          (force Writer.stderr)
          "eval: %s (%d,%d,%d) (%d) (%s)\n"
          prog
          ifd
          ofd
          efd
          exit_code
          s; *)
        return exit_code))

and eval_source file ~eval_args =
  let%bind stdin = Reader.open_file file in
  let stdout = Eval_args.stdout eval_args in
  let stderr = Eval_args.stderr eval_args in
  let%bind res =
    eval_lines
      ~interactive:false
      ~prog_input:(Prog_input.Stream (stdin, Not_sexp))
      ~stdout
      ~stderr
      ~eval_args
      ()
  in
  let%bind () = Reader.close stdin in
  return res

and eval (ast : Ast.t) ~eval_args =
  let open Ast in
  match ast with
  | [] -> return 0
  | ((h, t), sep) :: ast ->
    (match sep with
    | Semicolon ->
      let%bind v = eval_and_or_list h t ~eval_args in
      (match ast with
      | [] -> return v
      | _ -> eval ast ~eval_args)
    | Ampersand ->
      eval_subshell [ (h, t), Semicolon ] ~eval_args
      |> Deferred.ignore_m
      |> Deferred.don't_wait_for;
      eval ast ~eval_args)

and eval_subshell ast ~eval_args =
  let env = Eval_args.env eval_args |> Env.copy in
  eval ast ~eval_args:(Eval_args.with_env eval_args ~env)

and eval_and_or_list h t ~eval_args =
  (* In shell, && and || have the same precendence *)
  let open Ast in
  let%bind b_num = eval_boolean_part h ~eval_args in
  let b = if b_num = 0 then true else false in
  match t with
  | [] -> return b_num
  | (and_or, x) :: ls ->
    (match and_or with
    | Or -> if b then return b_num else eval_and_or_list x ls ~eval_args
    | And -> if not b then return b_num else eval_and_or_list x ls ~eval_args)

and eval_boolean_part part ~eval_args =
  let maybe_bang, pipline = part in
  let has_bang = Option.is_some maybe_bang in
  let%map code = eval_pipeline pipline ~eval_args in
  match code with
  | 0 -> if has_bang then 1 else 0
  | x -> if has_bang then 0 else x

and eval_pipeline pipeline ~eval_args =
  match pipeline with
  | [] -> return 0
  | x :: xs ->
    (* Currently do not support pipe status code; returns the final command's code. *)
    (match xs with
    | [] -> eval_pipeline_part x ~eval_args
    | _ ->
      let pipe_read, pipe_write = Pipe_process.pipe () in
      let deferred_code =
        eval_pipeline_part x ~eval_args:{ eval_args with Eval_args.stdout = pipe_write }
      in
      let new_deferred_code =
        eval_pipeline xs ~eval_args:{ eval_args with Eval_args.stdin = pipe_read }
      in
      let%bind _code = deferred_code in
      let%bind () = Fd.close pipe_write in
      let%bind new_code = new_deferred_code in
      let%bind () = Fd.close pipe_read in
      return new_code)

and eval_pipeline_part ~eval_args =
  let open Ast in
  function
  | Subshell t -> eval_subshell t ~eval_args
  | If_clause (if_elif_blocks, maybe_else) ->
    eval_if_clause if_elif_blocks maybe_else ~eval_args
  | Simple_command part -> eval_simple_command part ~eval_args
  | Remote_command (t, cluster) -> eval_remote_command t cluster ~eval_args

and eval_assigment assignment ~eval_args =
  let name, token = assignment in
  let%bind res_list = eval_token token ~eval_args in
  let res = String.concat res_list in
  let env = Eval_args.env eval_args in
  let old = Env.assign_set env ~key:name ~data:res in
  return (name, old)

and unassign assignment_cache ~eval_args =
  let env = Eval_args.env eval_args in
  List.iter
    ~f:(fun (key, data) -> Env.assign_set env ~key ~data |> ignore)
    assignment_cache

and eval_simple_command (tokens, assignments, _io_redirects) ~eval_args =
  let%bind cache =
    List.map ~f:(fun assignment () -> eval_assigment assignment ~eval_args) assignments
    |> deferred_iter
  in
  let%bind args =
    List.map ~f:(fun token () -> eval_token token ~eval_args) tokens |> deferred_iter
  in
  let args = List.concat args in
  let res =
    match args with
    | [] -> return 0
    | name :: args -> eval_command name args ~eval_args
  in
  if List.length tokens > 0 then unassign cache ~eval_args;
  res

and eval_remote_command t cluster ~eval_args =
  let env = Eval_args.env eval_args in
  let job_group = env |> Env.job_group in
  if Job.Job_group.canceled job_group
  then return 1
  else (
    let program = t |> Ast.sexp_of_t in
    let stdout = Eval_args.stdout_writer eval_args in
    let remote_run_one
        (module Backend : Application_class.Backend)
        cluster_id
        setting
        remote_targets
      =
      let verbose = Eval_args.verbose eval_args in
      let stdin = Eval_args.stdin_reader eval_args in
      let stderr = Eval_args.stderr_writer eval_args in
      let env_image = Env.Image.of_env env |> Env.Image.to_public in
      let%bind result =
        Backend.remote_run
          ~cluster_id
          ~setting
          ~remote_targets
          ~program
          ~env_image
          ~stdin
          ~stdout
          ~stderr
          ~verbose
      in
      match result with
      | Ok () -> return 0
      | Error err ->
        fprintf (Eval_args.stderr_writer eval_args) "%s\n" (Error.to_string_hum err);
        return 1
    in
    let env = Eval_args.env eval_args in
    let remotes = Env.cluster_resolve env cluster in
    let%bind exit_code =
      match remotes with
      | Cluster cluster_target ->
        let (module Backend : Application_class.Backend) =
          Env.Cluster_target.backend cluster_target
        in
        let setting = Env.Cluster_target.setting cluster_target in
        let remotes = Env.Cluster_target.remotes cluster_target in
        let cluster_id = Env.Cluster_target.cluster_id cluster_target in
        remote_run_one
          (module Backend : Application_class.Backend)
          cluster_id
          setting
          remotes
      | Remote remote_target ->
        let (module Backend : Application_class.Backend) =
          Single_command_backend.create ()
        in
        let setting = "" in
        let remotes = [ remote_target ] in
        let cluster_id = "" in
        remote_run_one
          (module Backend : Application_class.Backend)
          cluster_id
          setting
          remotes
    in
    return exit_code)

and eval_if_clause if_elif_blocks maybe_else ~eval_args =
  match if_elif_blocks with
  | [] ->
    (match maybe_else with
    | None -> return 0
    | Some block -> eval block ~eval_args)
  | (if_cond, then_block) :: remaining ->
    let%bind return_code = eval if_cond ~eval_args in
    (match return_code with
    | 0 -> eval then_block ~eval_args
    | _ -> eval_if_clause remaining maybe_else ~eval_args)

and eval_token_part ~eval_args =
  let open Ast in
  function
  | Command_substitution ss ->
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
    let writer_fd = Writer.fd writer in
    let%bind () =
      eval_subshell ss ~eval_args:{ eval_args with stdout = writer_fd }
      |> Deferred.ignore_m
    in
    let%bind () = Writer.close writer in
    let%bind s = Ivar.read ivar in
    let insert_splits sl =
      let sl_mod = List.bind sl ~f:(fun x -> [ None; Some x ]) in
      match sl_mod with
      | _x :: xs -> xs
      | [] -> []
    in
    return (s |> parse_field_split |> insert_splits)
  | Variable v ->
    let env = Eval_args.env eval_args in
    let assignment = Env.assign_get env ~key:v in
    let res =
      match assignment with
      | "" -> []
      | s -> [ Some s ]
    in
    return res
  | Literal s -> return [ Some s ]

and eval_token token_parts ~eval_args : string list Deferred.t =
  let%map res =
    List.map ~f:(fun part () -> eval_token_part part ~eval_args) token_parts
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

and eval_lines ?interactive ~prog_input ~stdout ~stderr ~eval_args () =
  let stdout_writer = Writer.create stdout in
  let stderr_writer = Writer.create stderr in
  let rec repl ?state ?prior_input () =
    let interactive =
      match interactive with
      | None -> false
      | Some b -> b
    in
    let eval_run ast =
      let env = Eval_args.env eval_args in
      if interactive
      then (
        let job_group = Env.job_group env in
        Job.Job_group.reset job_group);
      let%map res = Async.try_with (fun () -> eval ast ~eval_args) in
      match res with
      | Ok exit_code ->
        let last_exit_code_var = "?" in
        Env.assign_set env ~key:last_exit_code_var ~data:(Int.to_string exit_code)
        |> ignore;
        ()
      | Error exn -> fprintf stderr_writer "%s\n" (Exn.to_string exn)
    in
    if interactive
    then (
      let prompt = if Option.is_none state then "$ " else "> " in
      Writer.write stdout_writer prompt);
    let eval_sexp sexp = Ast.t_of_sexp sexp |> eval_run |> Deferred.ignore_m in
    match prog_input with
    | Sexp sexp ->
      let%bind () = eval_sexp sexp in
      return 0
    | Prog_input.Stream (stdin, sexp_mode) ->
      (match sexp_mode with
      | Sexp ->
        let%bind res =
          Async.try_with (fun () ->
              let%bind sexp = Reader.read_sexp stdin in
              match sexp with
              | `Ok sexp ->
                let%bind () = eval_sexp sexp in
                return true
              | `Eof -> return false)
        in
        (match res with
        | Ok cont -> if cont then repl () else return 0
        | Error err ->
          fprintf
            stderr_writer
            "Sexp parse error: %s\n"
            (err |> Error.of_exn |> Error.to_string_hum);
          if interactive then repl () else return 1)
      | Not_sexp ->
        let%bind maybe_line = Reader.read_line stdin in
        (match maybe_line with
        | `Eof -> return 0
        | `Ok line ->
          (* let line = line ^ "\n" in *)
          let prior_input =
            match prior_input with
            | None -> ""
            | Some s -> s
          in
          let input = prior_input ^ line in
          (match Ast.parse input with
          | Ok complete ->
            let%bind () = eval_run complete |> Deferred.ignore_m in
            repl ()
          | Error _err ->
            let new_state = Ast.parse_partial ?state line in
            (match new_state with
            | Partial p -> repl ~state:(Partial p) ~prior_input:input ()
            | Done (unconsumed, ast) ->
              let%bind () =
                if unconsumed.len > 0
                then (
                  let msg = "Unconsumed input remaining!" in
                  fprintf stderr_writer "Parse error: %s\n" msg;
                  return ())
                else eval_run ast |> Deferred.ignore_m
              in
              if interactive then repl () else return 1
            | Fail (_unconsumed, _marks, msg) ->
              fprintf stderr_writer "Parse error: %s\n" msg;
              if interactive then repl () else return 1))))
  in
  repl ()
;;
