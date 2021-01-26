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

val remote_rpc : bool
val eval : Ast.t -> eval_args:Eval_args.t -> int Deferred.t

(* Stdin/stdout/stderr refers the the evaluator itself, rather than the program being evaluated *)
val eval_lines
  :  ?sexp_mode:bool
  -> ?interactive:bool
  -> stdin:Reader.t
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> eval_args:Eval_args.t
  -> unit
  -> int Deferred.t
