open Async

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
