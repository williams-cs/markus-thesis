open Core

module Private = struct
  module Cluster_image = struct
    type t_private =
      { img_id : string
      ; img_remotes : Remote_target.t list
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

let add_assignment env_image ~key ~data =
  let priv = Private.private_of_t env_image in
  let { Private.img_assignments; _ } = priv in
  let new_assigments = String.Map.set img_assignments ~key ~data in
  let new_priv = { priv with img_assignments = new_assigments } in
  Private.t_of_private new_priv
;;
