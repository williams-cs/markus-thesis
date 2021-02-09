open Core

(* Returns the path and hash of the resulting executable *)
val local_copy : host:string -> port:int option -> verbose:bool -> string * string

val remote_run_receiver
  :  host:string
  -> port:int option
  -> verbose:bool
  -> remote_port:int
  -> write_callback:(bytes -> int -> unit)
  -> close_callback:(unit -> unit)
  -> unit Or_error.t

val remote_run_sender
  :  host:string
  -> port:int option
  -> verbose:bool
  -> header:string
  -> port_callback:(int -> unit)
  -> read_callback:(bytes -> int -> int)
  -> unit Or_error.t
