open Core

type t

module Host_and_maybe_port : sig
  type t =
    { host : string
    ; port : int option
    }
  [@@deriving fields]

  val create : host:string -> port:int option -> t
  val to_string : t -> string
end

module Cluster : sig
  type t
end

val create : working_directory:string -> t
val copy : t -> t

(** Returns empty string if no assignment *)
val assign_get : t -> key:string -> string

(** Returns old assigned value *)
val assign_set : t -> key:string -> data:string -> string

(** Changes the working directory *)
val cd : t -> dir:string -> unit

(** Returns the working directory *)
val cwd : t -> string

(* Functions to support the builtin "export"*)
val export_add : t -> key:string -> unit
val exports : t -> string list
val exports_print : t -> write_callback:(string -> unit) -> unit

(* Functions to support the builtin "cluster"*)
val clusters : t -> (string, Cluster.t) Hashtbl.t
val cluster_get : t -> string -> Cluster.t option
val cluster_print : t -> string list -> write_callback:(string -> unit) -> unit
val cluster_set_active : t -> string option -> unit
val cluster_add : t -> string list -> (unit, string list) Result.t
val cluster_resolve : t -> string -> Host_and_maybe_port.t list
val job_group : t -> Job.Job_group.t
