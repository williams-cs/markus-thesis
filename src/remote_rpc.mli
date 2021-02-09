open Async

val start_local_sender : verbose:bool -> unit Deferred.t
val start_local_receiver : verbose:bool -> unit Deferred.t

val start_remote_sender
  :  verbose:bool
  -> remote_port:int
  -> runner:
       (verbose:bool
        -> prog:Sexp.t
        -> eval_args_stdin:Reader.t option
        -> stdout:Writer.t
        -> stderr:Writer.t
        -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

val start_remote_receiver : verbose:bool -> unit Deferred.t

val remote_run
  :  host:string
  -> port:int option
  -> program:Sexp.t
  -> verbose:bool
  -> stdin:Reader.t
  -> stdout:Writer.t
  -> stderr:Writer.t
  -> unit Deferred.Or_error.t
