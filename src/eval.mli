open Async

module Eval_args : sig
  type t

  (* If no input, leave stdin as None. Otherwise, supply a Reader.t. *)
  val create
    :  env:Env.t
    -> stdin:Reader.t option
    -> stdout:Writer.t
    -> stderr:Writer.t
    -> verbose:bool
    -> t
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
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> eval_args:Eval_args.t
  -> unit
  -> int Deferred.t
