open Async

type t

val create : Rpc.Connection.t -> t
val start_local_sender : verbose:bool -> unit Deferred.t

val dispatch_open
  :  t
  -> host:string
  -> port:int option
  -> user:string option
  -> int Deferred.Or_error.t

val dispatch_header
  :  t
  -> program:Sexp.t
  -> env_image:Env_image.t
  -> string Deferred.Or_error.t

val dispatch_write : t -> id:string -> buf:bytes -> amt:int -> unit Deferred.Or_error.t
val dispatch_close_single : t -> id:string -> unit Deferred.Or_error.t
val dispatch_close : t -> unit Deferred.Or_error.t
val dispatch_keepalive : t -> unit Deferred.Or_error.t
