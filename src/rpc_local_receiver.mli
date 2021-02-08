open Core
open Async

module Response : sig
  type t =
    | Write_callback of bytes * int
    | Close_callback of unit Or_error.t
  [@@deriving bin_io]
end

type t

val create : Rpc.Connection.t -> t
val start_local_receiver : verbose:bool -> unit Deferred.t

val dispatch
  :  t
  -> host:string
  -> remote_port:int
  -> (Response.t Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Deferred.Or_error.t

val dispatch_close : t -> unit Deferred.Or_error.t
