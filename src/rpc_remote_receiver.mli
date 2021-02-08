open Async

module Response : sig
  type t =
    | Header of Rpc_common.Header.t
    | Message of string
end

val start_remote_receiver : verbose:bool -> unit Deferred.t

val dispatch
  :  Rpc.Connection.t
  -> (Response.t Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Deferred.Or_error.t
