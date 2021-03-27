open Async

module type Backend = sig
  val remote_run
    :  cluster_id:string
    -> remote_targets:Remote_target.t list
    -> setting:string
    -> program:Sexp.t
    -> env_image:Env_image.t
    -> verbose:bool
    -> stdin:Reader.t
    -> stdout:Writer.t
    -> stderr:Writer.t
    -> unit Deferred.Or_error.t
end

module type Provider = sig
  type t

  val default : t
  val maybe_t_of_string : string -> t option
  val string_of_t : t -> string
  val application_class_backend_of_t : t -> (module Backend)
end
