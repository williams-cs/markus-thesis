open Core

val disconnect_active_sessions : unit -> unit

val remote_run
  :  host:string
  -> program:[ `Sexp of Sexp.t | `Name of string ]
  -> write_callback:(bytes -> int -> unit)
  -> close_callback:(unit -> unit)
  -> unit
