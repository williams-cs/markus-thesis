open Async

type t

module Job_group : sig
  type t_inner

  val create : unit -> t_inner
  val add : t_inner -> job:t -> unit
  val cancel : t_inner -> unit
  val canceled : t_inner -> bool
  val reset : t_inner -> unit

  type t = t_inner
end

val create : ?groups:Job_group.t list -> unit -> t
val cancel : t -> unit
val cancel_without_signal : t -> unit
val complete : t -> unit
val connect : ?process:Process.t -> t -> unit
val attach : process:Process.t -> t -> unit
val should_connect : t -> bool
val canceled : t -> bool
val all_jobs : Job_group.t
val wait_for_cancel : t -> unit Deferred.t
