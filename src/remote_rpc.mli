open Async

val start_local_sender : verbose:bool -> unit Deferred.t
val start_local_receiver : verbose:bool -> unit Deferred.t

val start_remote_sender
  :  verbose:bool
  -> remote_port:int
  -> runner:
       (verbose:bool
        -> prog:Sexp.t
        -> env:Env.t
        -> eval_args_stdin:Reader.t option
        -> stdout:Writer.t
        -> stderr:Writer.t
        -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

val start_remote_receiver : verbose:bool -> unit Deferred.t

val remote_run
  :  remote_target:Env.Remote_target.t
  -> program:Sexp.t
  -> env:Env.t
  -> verbose:bool
  -> stdin:Reader.t
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> unit Deferred.Or_error.t
