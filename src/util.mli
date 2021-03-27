open Core
open Async

(* Make sure to call this for deterministic unit tests *)
val set_random_state : Random.State.t -> unit
val random_state : unit -> Random.State.t
val glue : reader:Reader.t Deferred.t -> writer:Writer.t Deferred.t -> unit Deferred.t
val glue' : reader:Reader.t -> writer:Writer.t -> unit Deferred.t
val simple_hash : string -> int

val verbose_println
  :  name:string
  -> verbose:bool
  -> stderr:Writer.t
  -> host:string
  -> port:int option
  -> string
  -> unit
