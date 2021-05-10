open Core

type t

(* Create an empty beta sampler. *)
val create : unit -> t

(* Add a key with the default a/b values of 1 and 1. If the key is
already present, this is a noop. *)
val add : t -> key:string -> unit

(* Increment the a (success) or b (failure) of a specific key. *)
val update : ?cap:int -> t -> key:string -> outcome:[ `Success | `Failure ] -> unit

(* Take a beta distribution sample of a specific key. If the key is not
present, returns None. *)
val sample : t -> key:string -> float option

(* Take a beta distribution sample of each key, optionally excluding some keys,
and choose the key with the highest result. 
The excluded keys are multiplied with the specified penalty, with the default
multiplier being zero.
If no keys are present, returns None. *)
val choose : ?excluding:string list -> ?penalty:float -> t -> (string * float) option
val get : t -> key:string -> (int * int) option
val keys : t -> string list

module Distribution : sig
  type t

  val create : a:int -> b:int -> t
  val sample : t -> float
  val sample_float : float -> float -> Random.State.t -> float
end
