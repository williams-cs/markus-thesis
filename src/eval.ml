open Core
open Async
open Util

let remote_rpc = true

module Eval_args = struct
  module Stdin = struct
    type t =
      | Stdin_none
      | Stdin_reader of Reader.t Deferred.t
      | Stdin_pipe of (Writer.t -> unit Deferred.t)

    let create ~stdin =
      match stdin with
      | None -> Stdin_none
      | Some reader -> Stdin_reader (return reader)
    ;;
  end

  type t =
    { env : Env.t
    ; stdin : Stdin.t
    ; stdout : Writer.t Deferred.t
    ; stderr : Writer.t
    ; verbose : bool
    }
  [@@deriving fields]

  let create ~env ~stdin ~stdout ~stderr ~verbose =
    { env; stdin = Stdin.create ~stdin; stdout = return stdout; stderr; verbose }
  ;;

  let with_env { env = _; stdin; stdout; stderr; verbose } ~env =
    { env; stdin; stdout; stderr; verbose }
  ;;
end

let wait_until_exit process =
  let%bind wait_result = Process.wait process in
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
        let%bind stdout = Eval_args.stdout eval_args in
        let stderr = Eval_args.stderr eval_args in
        fn ~env ~stdout ~stderr ~args
      | Source ->
        (match args with
        | [] ->
          fprintf (Eval_args.stderr eval_args) "source: filename argument required\n";
          return 1
        | arg :: _ ->
          let%bind () = update_dir () in
          eval_source arg ~eval_args))
    | None ->
      let job = Job.create ~groups:[ job_group ] () in
      let%bind () = update_dir () in
      let%bind process = Process.create ~prog ~args ~stdin:"" () in
      (match process with
      | Error err ->
        Job.cancel_without_signal job;
        fprintf (Eval_args.stderr eval_args) "%s\n" (Error.to_string_hum err);
        let%bind () =
          (* drain input *)
          match Eval_args.stdin eval_args with
          | Stdin_none -> return ()
          | Stdin_reader reader ->
            let%bind reader = reader in
            Reader.drain reader
          | Stdin_pipe pipe ->
            let%bind writer, _ =
              Writer.of_pipe
                (Info.of_string "empty_writer")
                (Pipe.create_writer (fun reader -> Pipe.drain reader))
            in
            pipe writer
        in
        return (-1)
      | Ok process ->
        Job.connect job process;
        let pid = Pid.to_int (Process.pid process) in
        let child_stdin = Process.stdin process in
        let child_stdout = Process.stdout process in
        let child_stderr = Process.stderr process in
        let in_deferred =
          match Eval_args.stdin eval_args with
          | Stdin_none -> return ()
          | Stdin_reader reader -> glue ~reader ~writer:(return child_stdin)
          | Stdin_pipe pipe -> pipe child_stdin
        in
        let out_deferred =
          glue ~reader:(return child_stdout) ~writer:(Eval_args.stdout eval_args)
        in
        let err_deferred =
          glue ~reader:(return child_stderr) ~writer:(return (Eval_args.stderr eval_args))
        in
        let%bind exit_code = wait_until_exit process in
        let%bind () =
          if Job.canceled job
          then return ()
          else (
            let%bind () = in_deferred in
            let%bind () = out_deferred in
            let%bind () = err_deferred in
            return ())
        in
        if Eval_args.verbose eval_args
        then
          fprintf
            (Eval_args.stderr eval_args)
            "Child process %i exited with status %i\n"
            pid
            exit_code;
        Job.complete job;
        return exit_code))

and eval_source file ~eval_args =
  let%bind stdin = Reader.open_file file in
  let%bind stdout = Eval_args.stdout eval_args in
  let stderr = Eval_args.stderr eval_args in
  eval_lines ~interactive:false ~stdin ~stdout ~stderr ~eval_args ()

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
    let next_stdin = Ivar.create () in
    let deferred_code =
      eval_pipeline_part
        x
        ~eval_args:{ eval_args with Eval_args.stdout = Ivar.read next_stdin }
    in
    (* Currently do not support pipe status code; returns the final command's code. *)
    (match xs with
    | [] ->
      let%bind stdout = Eval_args.stdout eval_args in
      Ivar.fill next_stdin stdout;
      deferred_code
    | _ ->
      let new_deferred_code =
        eval_pipeline
          xs
          ~eval_args:
            { eval_args with
              Eval_args.stdin =
                Stdin_pipe
                  (fun stdin ->
                    Ivar.fill next_stdin stdin;
                    return ())
            }
      in
      let%bind _code = deferred_code in
      let%bind stdout = Ivar.read next_stdin in
      let%bind () = Writer.close stdout in
      new_deferred_code)

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
  let job_group = eval_args |> Eval_args.env |> Env.job_group in
  if Job.Job_group.canceled job_group
  then return 1
  else (
    let program =
      match t with
      | Remote_subshell sexp -> `Sexp (sexp |> Ast.sexp_of_t)
      | Remote_name name -> `Name name
    in
    let%bind stdout = Eval_args.stdout eval_args in
    (* let ivar = Ivar.create () in *)
    let remote_run_one host =
      let verbose = Eval_args.verbose eval_args in
      match remote_rpc with
      | true ->
        let stderr = Eval_args.stderr eval_args in
        let%map result =
          Remote_rpc.remote_run
            ~host
            ~program
            ~write_callback:(fun b len -> return (Writer.write_bytes stdout b ~len))
            ~close_callback:(fun () -> return ())
            ~stderr
            ~verbose
        in
        (match result with
        | Ok () -> ()
        | Error err -> fprintf (Eval_args.stderr eval_args) "%s" (Error.to_string_hum err))
      | false ->
        In_thread.run (fun () ->
            Remote_unsafe.remote_run
              ~host
              ~program
              ~verbose
              ~write_callback:(fun b len -> Writer.write_bytes stdout b ~len)
              ~close_callback:(fun () -> ()))
    in
    let env = Eval_args.env eval_args in
    let remotes = Env.cluster_resolve env cluster in
    let%bind () =
      Deferred.List.map ~how:`Parallel remotes ~f:(fun remote -> remote_run_one remote)
      |> Deferred.ignore_m
    in
    (* let%bind () = Ivar.read ivar in *)
    return 0)

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
    let%bind () =
      eval_subshell ss ~eval_args:{ eval_args with stdout = return writer }
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

and eval_lines ?sexp_mode ?interactive ~stdin ~stdout ~stderr ~eval_args () =
  let rec repl ?state ?prior_input () =
    let interactive =
      match interactive with
      | None -> false
      | Some b -> b
    in
    let eval_run ast =
      if interactive
      then (
        let env = Eval_args.env eval_args in
        let job_group = Env.job_group env in
        Job.Job_group.reset job_group;
        let%map res = Async.try_with (fun () -> eval ast ~eval_args) in
        match res with
        | Ok exit_code ->
          let last_exit_code_var = "?" in
          Env.assign_set env ~key:last_exit_code_var ~data:(Int.to_string exit_code)
          |> ignore;
          ()
        | Error exn -> fprintf stderr "%s" (Exn.to_string exn))
      else eval ast ~eval_args |> Deferred.ignore_m
    in
    if interactive
    then (
      let prompt = if Option.is_none state then "$ " else "> " in
      Writer.write stdout prompt);
    let sexp_mode =
      match sexp_mode with
      | None -> false
      | Some b -> b
    in
    if sexp_mode
    then (
      let%bind res =
        Async.try_with (fun () ->
            let%bind sexp = Reader.read_sexp stdin in
            match sexp with
            | `Ok sexp ->
              let%bind () = Ast.t_of_sexp sexp |> eval_run |> Deferred.ignore_m in
              return true
            | `Eof -> return false)
      in
      match res with
      | Ok cont -> if cont then repl () else return 0
      | Error err ->
        fprintf
          stderr
          "Sexp parse error: %s\n"
          (err |> Error.of_exn |> Error.to_string_hum);
        if interactive then repl () else return 1)
    else (
      let%bind maybe_line = Reader.read_line stdin in
      match maybe_line with
      | `Eof -> return 0
      | `Ok line ->
        let line = line ^ "\n" in
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
                fprintf stderr "Parse error: %s\n" msg;
                return ())
              else eval_run ast |> Deferred.ignore_m
            in
            if interactive then repl () else return 1
          | Fail (_unconsumed, _marks, msg) ->
            fprintf stderr "Parse error: %s\n" msg;
            if interactive then repl () else return 1)))
  in
  repl ()
;;
