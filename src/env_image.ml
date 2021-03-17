open Core

module Private = struct
  module Cluster_image = struct
    type t_private =
      { img_remotes : Remote_target.t list
      ; img_cluster_type : string
      }
    [@@deriving sexp, bin_io]

    let t_of_private = Fn.id
    let private_of_t = Fn.id
  end

  type t_private =
    { img_assignments : string String.Map.t
    ; img_exports : String.Set.t
    ; img_clusters : Cluster_image.t_private String.Map.t
    ; img_active_cluster : string
    }
  [@@deriving sexp, bin_io]

  let t_of_private = Fn.id
  let private_of_t = Fn.id
end

module Cluster_image = struct
  type t = Private.Cluster_image.t_private [@@deriving sexp, bin_io]
end

type t = Private.t_private [@@deriving sexp, bin_io]
