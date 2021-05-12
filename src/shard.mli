open Async
module Ast = Ast
module Beta_sampler = Beta_sampler
module Cluster_type = Cluster_type
module Env = Env
module Eval = Eval
module Remote_rpc = Remote_rpc
module Remote_ssh = Remote_ssh
module Util = Util

val create_env : working_directory:string -> Cluster_type.t Env.t

val run_with_io
  :  ?verbose:bool
  -> ?args:string list
  -> prog_input:Eval.Prog_input.t
  -> env:Cluster_type.t Env.t
  -> eval_args_stdin:Reader.t option
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> isatty:bool
  -> unit
  -> int Deferred.t

val run
  :  ?sexp_mode:bool
  -> ?filename:string
  -> ?args:string list
  -> ?verbose:bool
  -> unit
  -> int Deferred.t
