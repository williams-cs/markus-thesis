open Async

module Eval_args : sig
  type t

  (* If no input, leave stdin as None. Otherwise, supply a Reader.t. *)
  val create_from_stdio
    :  env:Cluster_type.t Env.t
    -> stdin:Reader.t option
    -> stdout:Writer.t
    -> stderr:Writer.t
    -> verbose:bool
    -> t

  val create
    :  env:Cluster_type.t Env.t
    -> stdin:Fd.t
    -> stdout:Fd.t
    -> stderr:Fd.t
    -> verbose:bool
    -> t

  val to_string : t -> string
end

val eval : Ast.t -> eval_args:Eval_args.t -> int Deferred.t

module Prog_input : sig
  module Sexp_mode : sig
    type t =
      | Sexp
      | Not_sexp
  end

  type t =
    | Stream of Reader.t * Sexp_mode.t
    | Sexp of Sexp.t
end

(* Stdin/stdout/stderr refers the the evaluator itself, rather than the program being evaluated *)
val eval_lines
  :  ?interactive:bool
  -> prog_input:Prog_input.t
  -> stdout:Fd.t
  -> stderr:Fd.t
  -> eval_args:Eval_args.t
  -> unit
  -> int Deferred.t
