open Core
open Async

val start_local_sender : verbose:bool -> unit Deferred.t
val start_local_receiver : verbose:bool -> unit Deferred.t

val start_remote_sender
  :  (module Application_class.Provider with type t = 'a)
  -> verbose:bool
  -> remote_port:int
  -> runner:
       (verbose:bool
        -> exit_handler:(int -> unit Deferred.t)
        -> prog:Sexp.t
        -> env:'a Env.t
        -> eval_args_stdin:Reader.t option
        -> stdout:Writer.t
        -> stderr:Writer.t
        -> int Deferred.t)
  -> unit Deferred.Or_error.t

val start_remote_receiver : verbose:bool -> unit Deferred.t

val setup_rpc_service
  :  host:string
  -> port:int option
  -> user:string option
  -> stderr:Writer.t
  -> verbose:bool
  -> job:Job.t option
  -> (Rpc_local_sender.t * Rpc_local_receiver.t, Error.t) result Deferred.t
