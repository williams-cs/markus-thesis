open Core

val remote_run
  :  host:string
  -> program:[ `Sexp of Sexp.t | `Name of string ]
  -> write_callback:(bytes -> int -> unit)
  -> close_callback:(unit -> unit)
  -> unit
