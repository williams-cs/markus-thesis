val remote_run
  :  host:string
  -> program:string
  -> write_callback:(bytes -> int -> unit)
  -> close_callback:(unit -> unit)
  -> unit
