open Async
module Ast = Ast
module Env = Env
module Eval = Eval
module Remote_rpc = Remote_rpc
module Remote_ssh = Remote_ssh
module Util = Util

val run_with_io
  :  ?verbose:bool
  -> prog_input:Eval.Prog_input.t
  -> eval_args_stdin:Reader.t option
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> isatty:bool
  -> unit
  -> int Deferred.t

val run : ?sexp_mode:bool -> ?filename:string -> ?verbose:bool -> unit -> int Deferred.t
