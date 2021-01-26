open Async

type t

val create : unit -> t
val cancel : t -> unit
val cancel_without_signal : t -> unit
val complete : t -> unit
val connect : t -> Process.t -> unit
val should_connect : t -> bool
val cancelled : t -> bool
val cancel_all : unit -> unit
