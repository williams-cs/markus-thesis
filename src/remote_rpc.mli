open Async

val start_server : port:int -> unit Deferred.t

val remote_run
  :  host:string
  -> program:[ `Sexp of Sexp.t | `Name of string ]
  -> write_callback:(bytes -> int -> unit Deferred.t)
  -> close_callback:(unit -> unit Deferred.t)
  -> stderr:Writer.t
  -> unit Deferred.Or_error.t
