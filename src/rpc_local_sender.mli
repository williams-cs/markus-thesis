open Async

type t

val create : Rpc.Connection.t -> t
val start_local_sender : verbose:bool -> unit Deferred.t

val dispatch_open
  :  t
  -> host:string
  -> port:int option
  -> program:Sexp.t
  -> int Deferred.Or_error.t

val dispatch_write : t -> buf:bytes -> amt:int -> unit Deferred.Or_error.t
val dispatch_close : t -> unit Deferred.Or_error.t
