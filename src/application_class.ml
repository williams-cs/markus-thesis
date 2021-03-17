open Async

module type Backend = sig
  type 'a env_type

  val remote_run
    :  remote_target:Remote_target.t
    -> program:Sexp.t
    -> env:'a env_type
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
