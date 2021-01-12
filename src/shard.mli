open Async
module Ast = Ast

module Eval_args : sig
  type t

  (* If no input, leave stdin as None. Otherwise, supply a Reader.t. *)
  val create : stdin:Reader.t option -> stdout:Writer.t -> verbose:bool -> t
end

val run : unit -> unit Deferred.t
val eval : Ast.t -> eval_args:Eval_args.t -> int Deferred.t
