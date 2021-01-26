open Core

val disconnect_active_sessions : unit -> unit

(* Returns the path and hash of the resulting executable *)
val local_copy : host:string -> string * string

val remote_run
  :  host:string
  -> program:[ `Sexp of Sexp.t | `Name of string ]
  -> write_callback:(bytes -> int -> unit)
  -> close_callback:(unit -> unit)
  -> unit
