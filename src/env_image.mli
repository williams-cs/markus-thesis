open Core

module Cluster_image : sig
  type t [@@deriving sexp, bin_io]
end

type t [@@deriving sexp, bin_io]

module Private : sig
  module Cluster_image : sig
    type t_private =
      { img_remotes : Remote_target.t list
      ; img_cluster_type : string
      }
    [@@deriving sexp, bin_io]

    val t_of_private : t_private -> Cluster_image.t
    val private_of_t : Cluster_image.t -> t_private
  end

  type t_private =
    { img_assignments : string String.Map.t
    ; img_exports : String.Set.t
    ; img_clusters : Cluster_image.t_private String.Map.t
    ; img_active_cluster : string
    }
  [@@deriving sexp, bin_io]

  val t_of_private : t_private -> t
  val private_of_t : t -> t_private
end
