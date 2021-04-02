open Core
open Async
open Rpc_common

module Response : sig
  type t =
    | Write_callback of Receiver_query.t
    | Close_callback of unit Or_error.t
  [@@deriving bin_io]
end

type t

val create : Rpc.Connection.t -> t
val start_local_receiver : verbose:bool -> unit Deferred.t

val dispatch
  :  t
  -> host:string
  -> port:int option
  -> remote_port:int
  -> (Response.t Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Deferred.Or_error.t

val dispatch'
  :  t
  -> host:string
  -> port:int option
  -> remote_port:int
  -> ((id:string -> Reader.t Deferred.t)
     * Rpc.Pipe_rpc.Metadata.t
     * unit Deferred.Or_error.t)
     Deferred.Or_error.t

val dispatch_close : t -> unit Deferred.Or_error.t
