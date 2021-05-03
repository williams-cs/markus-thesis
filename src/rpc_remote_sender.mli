open Async

val max_concurrent_jobs : int

val start_remote_sender
  :  (module Application_class.Provider with type t = 'a)
  -> verbose:bool
  -> remote_port:int
  -> runner:
       (verbose:bool
        -> prog:Sexp.t
        -> env:'a Env.t
        -> eval_args_stdin:Reader.t option
        -> stdout:Writer.t
        -> stderr:Writer.t
        -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t
