open Async

module Chain : sig
  type t

  val create : unit Deferred.t -> t
  val append : t -> unit Deferred.t -> t
  val wait : t -> unit Deferred.t
end

val interval : int
val initial_delay : int
val local_delay : int
val remote_delay : int
val enable_keepalive : bool
val schedule : (unit -> unit Deferred.Or_error.t) -> unit
