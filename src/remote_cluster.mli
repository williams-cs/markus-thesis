open Async

type t

val create : unit -> t

val init_targets
  :  t
  -> targets:Remote_target.t list
  -> stderr:Writer.t
  -> verbose:bool
  -> job:Job.t
  -> unit Deferred.Or_error.t

val run_task
  :  t
  -> target:[ `Any | `Specific of Remote_target.t ]
  -> program:Sexp.t
  -> env_image:Env_image.t
  -> send_lines:string list
  -> string Deferred.Or_error.t
