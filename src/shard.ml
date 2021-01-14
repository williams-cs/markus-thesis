open Core
open Async
module Ast = Ast

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
    ; verbose : bool
    }
  [@@deriving fields]

  let create ~stdin ~stdout ~verbose =
    { env = Env.create (); stdin = Stdin.create ~stdin; stdout = return stdout; verbose }
  ;;
end

let glue ~reader ~writer =
  let%bind reader = reader in
  let%bind writer = writer in
  let rec print_until_done () =
    let%bind maybe_line = Reader.read_until reader (`Char '\n') ~keep_delim:true in
    match maybe_line with
    | `Eof -> return ()
    | `Ok line | `Eof_without_delim line ->
      Writer.write writer line;
      print_until_done ()
  in
  print_until_done ()
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

let eval_command prog args ~eval_args =
  let builtin = Map.find Builtin.builtins prog in
  match builtin with
  | Some fn -> fn args
  | None ->
    let%bind process = Process.create ~prog ~args ~stdin:"" () in
    (match process with
    | Error err ->
      print_endline (Error.to_string_hum err);
      return (-1)
    | Ok process ->
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
        glue ~reader:(return child_stderr) ~writer:(return (force Writer.stderr))
      in
      let%bind exit_code = wait_until_exit process in
      let%bind () = in_deferred in
      let%bind () = out_deferred in
      let%bind () = err_deferred in
      if Eval_args.verbose eval_args
      then
        fprintf
          (force Writer.stderr)
          "Child process %i exited with status %i\n"
          pid
          exit_code;
      return exit_code)
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

let rec eval (ast : Ast.t) ~eval_args =
  let open Ast in
  match ast with
  | [] -> return 0
  | ((h, t), sep) :: ast ->
    (match sep with
    | Semicolon ->
      let%bind () = eval_and_or_list h t ~eval_args |> Deferred.ignore_m in
      eval ast ~eval_args
    | Ampersand ->
      let%bind (), v =
        Deferred.both
          (eval_and_or_list h t ~eval_args |> Deferred.ignore_m)
          (eval ast ~eval_args)
      in
      return v)

and eval_and_or_list h t ~eval_args =
  (* In shell, && and || have the same precendence *)
  let open Ast in
  let%bind b = eval_boolean_part h ~eval_args in
  match t with
  | [] -> return b
  | (and_or, x) :: ls ->
    (match and_or with
    | Or -> if b then return true else eval_and_or_list x ls ~eval_args
    | And -> if not b then return false else eval_and_or_list x ls ~eval_args)

and eval_boolean_part part ~eval_args =
  let maybe_bang, pipline = part in
  let has_bang = Option.is_some maybe_bang in
  let%map code = eval_pipeline pipline ~eval_args in
  match code with
  | 0 -> not has_bang
  | _ -> has_bang

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
  | Subshell t -> eval t ~eval_args
  | Simple_command part -> eval_simple_command part ~eval_args
  (* TODO: remake string from AST instead of keeping string? *)
  | Remote_command (t, host) ->
    let program =
      match t with
      | Remote_subshell sexp -> `Sexp (sexp |> Ast.sexp_of_t)
      | Remote_name name -> `Name name
    in
    let%bind stdout = Eval_args.stdout eval_args in
    let ivar = Ivar.create () in
    Remote.remote_run
      ~host
      ~program
      ~write_callback:(fun b len -> Writer.write_bytes stdout b ~len)
      ~close_callback:(fun () -> Ivar.fill ivar ());
    let%bind () = Ivar.read ivar in
    return 0

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
      eval ss ~eval_args:{ eval_args with stdout = return writer } |> Deferred.ignore_m
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
    return [ Some (Env.assign_get env ~key:v) ]
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
;;

let run ?sexp_mode () =
  let stdin = force Reader.stdin in
  let stdout = force Writer.stdout in
  let stderr = force Writer.stderr in
  let eval_run ast =
    eval ast ~eval_args:(Eval_args.create ~stdin:None ~stdout ~verbose:false)
  in
  let%bind isatty = Unix.isatty (Fd.stdin ()) in
  let rec repl ?state prior_input =
    if isatty && Option.is_none state
    then (
      let prompt = "$ " in
      print_string prompt);
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
      | Ok cont -> if cont then repl "" else return ()
      | Error err ->
        fprintf
          stderr
          "Sexp parse error: %s\n"
          (err |> Error.of_exn |> Error.to_string_hum);
        return ())
    else (
      let%bind maybe_line = Reader.read_line stdin in
      match maybe_line with
      | `Eof -> return ()
      | `Ok line ->
        let line = line ^ "\n" in
        let input = prior_input ^ line in
        (match Ast.parse input with
        | Ok complete ->
          let%bind () = eval_run complete |> Deferred.ignore_m in
          repl ""
        | Error _err ->
          let new_state = Ast.parse_partial ?state line in
          (match new_state with
          | Partial p -> repl ~state:(Partial p) input
          | Done (unconsumed, ast) ->
            let%bind () =
              if unconsumed.len > 0
              then (
                let msg = "Unconsumed input remaining!" in
                fprintf stderr "Parse error: %s\n" msg;
                return ())
              else eval_run ast |> Deferred.ignore_m
            in
            repl ""
          | Fail (_unconsumed, _marks, msg) ->
            fprintf stderr "Parse error: %s\n" msg;
            repl "")))
  in
  let%map _ = repl "" in
  shutdown 0
;;
