open Async
open Rpc_common

val start_remote_receiver : verbose:bool -> unit Deferred.t

val dispatch
  :  Rpc.Connection.t
  -> (Sender_query.t Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Deferred.Or_error.t
