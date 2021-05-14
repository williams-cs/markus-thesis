open Core
open Async

val log : id:string -> Log.t

(* Takes args and valid flags, returns (flag list, arg list), or fails with
the invalid flag. *)
val separate_flags
  :  string list
  -> valid_flags:string list
  -> (string list * string list, string) Result.t

(* Make sure to call this for deterministic unit tests *)
val set_random_state : Random.State.t -> unit
val random_state : unit -> Random.State.t

(* Split off a separate random state, whose calls don't affect the main random
state. The split does affect the random state *)
val split_random : unit -> Random.State.t
val generate_uuid_list : string List.t -> string
val generate_uuid_hash_set : string Hash_set.t -> string
val glue : reader:Reader.t Deferred.t -> writer:Writer.t Deferred.t -> unit Deferred.t
val glue' : reader:Reader.t -> writer:Writer.t -> unit Deferred.t

val glue_transform
  :  reader:Reader.t
  -> writer:Writer.t
  -> transform:(string -> string)
  -> unit Deferred.t

val simple_hash : string -> int

val verbose_println
  :  name:string
  -> verbose:bool
  -> stderr:Writer.t
  -> host:string
  -> port:int option
  -> string
  -> unit

val shard_internal : string -> string
val do_throttle : unit -> bool Deferred.t
val get_packet_loss : unit -> int Deferred.t
