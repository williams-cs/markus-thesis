open Async

module Eval_args : sig
  type t

  (* If no input, leave stdin as None. Otherwise, supply a Reader.t. *)
  val create : env:Env.t -> stdin:Reader.t option -> stdout:Writer.t -> verbose:bool -> t
end

val eval : Ast.t -> eval_args:Eval_args.t -> int Deferred.t
