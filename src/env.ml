open Core

module Host_and_maybe_port = struct
  open Ppx_compare_lib.Builtin

  module Stable = struct
    type t =
      { host : string
      ; port : int option
      }
    [@@deriving fields, sexp, bin_io, compare, hash]
  end

  include Stable
  include (Hashable.Make_binable (Stable) : Hashable.S_binable with type t := t)

  let create ~host ~port = { host; port }

  let to_string { host; port } =
    let port_part =
      match port with
      | None -> ""
      | Some i -> sprintf ":%d" i
    in
    sprintf "%s%s" host port_part
  ;;
end

module Cluster = struct
  type t = { remotes : Host_and_maybe_port.t Hash_set.t }

  let create () = { remotes = Hash_set.create (module Host_and_maybe_port) }
  let add_remote { remotes } remote = Hash_set.add remotes remote
  let get_remotes { remotes } = Hash_set.to_list remotes
end

type t =
  { assignments : (string, string) Hashtbl.t
  ; exports : string Hash_set.t
  ; clusters : (string, Cluster.t) Hashtbl.t
  ; active_cluster_ref : string ref
  ; working_directory_ref : string ref
  ; job_group : Job.Job_group.t
  }

let default_cluster = "default"

let resolve remote =
  let uri = Uri.of_string (sprintf "ssh://%s" remote) in
  match Uri.host uri with
  | None -> None
  | Some host ->
    let port = Uri.port uri in
    Some (Host_and_maybe_port.create ~host ~port)
;;

let create ~working_directory =
  let clusters = Hashtbl.create (module String) in
  Hashtbl.add clusters ~key:default_cluster ~data:(Cluster.create ()) |> ignore;
  { assignments = Hashtbl.create (module String)
  ; exports = Hash_set.create (module String)
  ; clusters
  ; active_cluster_ref = ref default_cluster
  ; working_directory_ref = ref working_directory
  ; job_group = Job.Job_group.create ()
  }
;;

let copy
    { assignments
    ; exports
    ; clusters
    ; active_cluster_ref
    ; working_directory_ref
    ; job_group = _
    }
  =
  { assignments = Hashtbl.copy assignments
  ; exports = Hash_set.copy exports
  ; clusters = Hashtbl.copy clusters
  ; active_cluster_ref = ref !active_cluster_ref
  ; working_directory_ref = ref !working_directory_ref (* Don't copy job group *)
  ; job_group = Job.Job_group.create ()
  }
;;

module Image = struct
  module Cluster_image = struct
    type t = { img_remotes : Host_and_maybe_port.t list } [@@deriving sexp, bin_io]

    let of_cluster { Cluster.remotes } = { img_remotes = Hash_set.to_list remotes }

    let to_cluster { img_remotes } =
      { Cluster.remotes = Hash_set.of_list (module Host_and_maybe_port) img_remotes }
    ;;
  end

  type t_inner =
    { img_assignments : string String.Map.t
    ; img_exports : String.Set.t
    ; img_clusters : Cluster_image.t String.Map.t
    ; img_active_cluster : string
    }
  [@@deriving sexp, bin_io]

  type t = t_inner [@@deriving sexp, bin_io]

  let of_env { assignments; exports; clusters; active_cluster_ref; _ } =
    { img_assignments = String.Map.of_hashtbl_exn assignments
    ; img_exports = String.Set.of_hash_set exports
    ; img_clusters =
        String.Map.of_hashtbl_exn clusters |> Map.map ~f:Cluster_image.of_cluster
    ; img_active_cluster = !active_cluster_ref
    }
  ;;

  let to_env
      ~working_directory
      { img_assignments; img_exports; img_clusters; img_active_cluster }
    =
    { assignments =
        Hashtbl.of_alist_exn (module String) (String.Map.to_alist img_assignments)
    ; exports = Hash_set.of_list (module String) (String.Set.to_list img_exports)
    ; clusters =
        Map.map ~f:Cluster_image.to_cluster img_clusters
        |> String.Map.to_alist
        |> Hashtbl.of_alist_exn (module String)
    ; active_cluster_ref = ref img_active_cluster
    ; working_directory_ref = ref working_directory
    ; job_group = Job.Job_group.create ()
    }
  ;;
end

let assign_get t ~key =
  let { assignments; _ } = t in
  Hashtbl.find assignments key |> Option.value ~default:""
;;

let assign_set t ~key ~data =
  let { assignments; _ } = t in
  let old_value = assign_get t ~key in
  Hashtbl.set assignments ~key ~data;
  old_value
;;

let assignments t =
  let { assignments; _ } = t in
  String.Map.of_hashtbl_exn assignments
;;

let export_add t ~key =
  let { exports; _ } = t in
  Hash_set.add exports key
;;

let exports t =
  let { exports; _ } = t in
  let export_list = Hash_set.to_list exports in
  List.sort export_list ~compare:String.compare
;;

let print_escape s =
  s
  |> String.to_list
  |> List.map ~f:(fun c ->
         let cs = String.of_char c in
         match c with
         | '\\' | '$' | '"' -> "\\" ^ cs
         | _ -> cs)
  |> String.concat
;;

let exports_print t ~write_callback =
  List.iter (exports t) ~f:(fun key ->
      let export_assign =
        match assign_get t ~key with
        | "" -> ""
        | value -> sprintf "=\"%s\"" (print_escape value)
      in
      let message = sprintf "export %s%s" key export_assign in
      write_callback message)
;;

let cd t ~dir =
  t.working_directory_ref := dir;
  ()
;;

let cwd t =
  let r = t.working_directory_ref in
  !r
;;

let clusters { clusters; _ } = clusters
let cluster_get { clusters; _ } name = Hashtbl.find clusters name

let cluster_print { clusters; _ } cluster_names ~write_callback =
  Hashtbl.iteri clusters ~f:(fun ~key ~data ->
      if List.is_empty cluster_names || List.mem cluster_names key ~equal:String.equal
      then (
        write_callback (sprintf "cluster -s \"%s\"" (print_escape key));
        List.iter (Cluster.get_remotes data) ~f:(fun remote ->
            write_callback
              (sprintf
                 "cluster -a \"%s\""
                 (print_escape (Host_and_maybe_port.to_string remote))))))
;;

let cluster_set_active t maybe_name =
  let name = Option.value ~default:default_cluster maybe_name in
  let { clusters; active_cluster_ref; _ } = t in
  Hashtbl.add clusters ~key:name ~data:(Cluster.create ()) |> ignore;
  active_cluster_ref := name
;;

let cluster_add t remotes =
  let { clusters; active_cluster_ref; _ } = t in
  let active_cluster = !active_cluster_ref in
  match Hashtbl.find clusters active_cluster with
  | Some cluster ->
    let failed =
      List.fold remotes ~init:[] ~f:(fun failed remote ->
          match resolve remote with
          | Some host_and_port ->
            Cluster.add_remote cluster host_and_port;
            failed
          | None -> remote :: failed)
    in
    (match failed with
    | [] -> Ok ()
    | _ -> Error failed)
  | None -> raise_s [%message "Active cluster should always exist"]
;;

let cluster_resolve t cluster_or_host =
  let { clusters; _ } = t in
  match Hashtbl.find clusters cluster_or_host with
  | Some cluster -> Cluster.get_remotes cluster
  | None ->
    (match resolve cluster_or_host with
    | Some host_and_port -> [ host_and_port ]
    | None -> raise_s [%message "Failed to resolve cluster or host: %s" cluster_or_host])
;;

let job_group t =
  let { job_group; _ } = t in
  job_group
;;
