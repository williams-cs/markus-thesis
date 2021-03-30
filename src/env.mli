open Core

type 'a t

module Cluster : sig
  type 'a t

  val add : 'a t -> string list -> (unit, string list) Result.t
  val set_type : 'a t -> 'a -> unit
  val get_type : 'a t -> 'a
  val clear_remotes : 'a t -> unit
end

module Image : sig
  type t_inner

  val of_env : 'a t -> t_inner

  val to_env
    :  (module Application_class.Provider with type t = 'a)
    -> working_directory:string
    -> t_inner
    -> 'a t

  val of_public : Env_image.t -> t_inner
  val to_public : t_inner -> Env_image.t

  type t = t_inner [@@deriving sexp, bin_io]
end

val create
  :  (module Application_class.Provider with type t = 'a)
  -> working_directory:string
  -> 'a t

val copy : 'a t -> 'a t

(** Returns empty string if no assignment *)
val assign_get : 'a t -> key:string -> string

(** Returns old assigned value *)
val assign_set : 'a t -> key:string -> data:string -> string

val assignments : 'a t -> string String.Map.t

(** Changes the working directory *)
val cd : 'a t -> dir:string -> unit

(** Returns the working directory *)
val cwd : 'a t -> string

(* Functions to support the builtin "export"*)
val export_add : 'a t -> key:string -> unit
val exports : 'a t -> string list
val exports_print : 'a t -> write_callback:(string -> unit) -> unit

(* Functions to support the builtin "cluster"*)
module Cluster_target : sig
  type t =
    { cluster_id : string
    ; backend : (module Application_class.Backend)
    ; setting : string
    ; remotes : Remote_target.t list
    }
  [@@deriving fields]

  val create
    :  (module Application_class.Backend)
    -> cluster_id:string
    -> setting:string
    -> remotes:Remote_target.t list
    -> t
end

module Cluster_resolution : sig
  type t =
    | Cluster of Cluster_target.t
    | Remote of Remote_target.t
end

val clusters : 'a t -> (string, 'a Cluster.t) Hashtbl.t
val cluster_get : 'a t -> string -> 'a Cluster.t option
val cluster_print : 'a t -> string list -> write_callback:(string -> unit) -> unit
val cluster_set_active : 'a t -> string option -> unit
val cluster_get_active : 'a t -> 'a Cluster.t
val cluster_resolve : 'a t -> string -> Cluster_resolution.t
val job_group : 'a t -> Job.Job_group.t
