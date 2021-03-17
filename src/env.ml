open Core

module Cluster = struct
  type 'a t =
    { remotes : Remote_target.t Hash_set.t
    ; provider : (module Application_class.Provider with type t = 'a)
    ; cluster_type : 'a ref
    ; application_class_backend : (module Application_class.Backend)
    }
  [@@deriving fields]

  let create (type a) (module Provider : Application_class.Provider with type t = a) =
    let cluster_type = Provider.default in
    { remotes = Hash_set.create (module Remote_target)
    ; provider = (module Provider)
    ; cluster_type = ref cluster_type
    ; application_class_backend = Provider.application_class_backend_of_t cluster_type
    }
  ;;

  let add_remote t remote = Hash_set.add (remotes t) remote

  let add t new_remotes =
    let failed =
      List.fold new_remotes ~init:[] ~f:(fun failed remote ->
          match Remote_target.resolve remote with
          | Some host_and_port ->
            add_remote t host_and_port;
            failed
          | None -> remote :: failed)
    in
    match failed with
    | [] -> Ok ()
    | _ -> Error failed
  ;;

  let get_remotes t = Hash_set.to_list (remotes t)
  let set_type t new_type = cluster_type t := new_type
  let get_type t = !(cluster_type t)
end

type 'a t =
  { assignments : (string, string) Hashtbl.t
  ; exports : string Hash_set.t
  ; clusters : (string, 'a Cluster.t) Hashtbl.t
  ; provider : (module Application_class.Provider with type t = 'a)
  ; active_cluster_ref : string ref
  ; working_directory_ref : string ref
  ; job_group : Job.Job_group.t
  }

let default_cluster = "default"

let create
    (type a)
    (module Provider : Application_class.Provider with type t = a)
    ~working_directory
  =
  let clusters = Hashtbl.create (module String) in
  Hashtbl.add clusters ~key:default_cluster ~data:(Cluster.create (module Provider))
  |> ignore;
  { assignments = Hashtbl.create (module String)
  ; exports = Hash_set.create (module String)
  ; clusters
  ; provider = (module Provider)
  ; active_cluster_ref = ref default_cluster
  ; working_directory_ref = ref working_directory
  ; job_group = Job.Job_group.create ()
  }
;;

let copy
    { assignments
    ; exports
    ; clusters
    ; provider
    ; active_cluster_ref
    ; working_directory_ref
    ; job_group = _
    }
  =
  { assignments = Hashtbl.copy assignments
  ; exports = Hash_set.copy exports
  ; clusters = Hashtbl.copy clusters
  ; provider
  ; active_cluster_ref = ref !active_cluster_ref
  ; working_directory_ref = ref !working_directory_ref (* Don't copy job group *)
  ; job_group = Job.Job_group.create ()
  }
;;

module Image = struct
  module Cluster_image = struct
    type t = Env_image.Private.Cluster_image.t_private [@@deriving sexp, bin_io]

    (* Potentially transfer backend info? *)
    let of_cluster
        (type a)
        ({ Cluster.remotes; cluster_type; provider; application_class_backend = _ } :
          a Cluster.t)
      =
      let (module Provider : Application_class.Provider with type t = a) = provider in
      let cluster_type = !cluster_type in
      { Env_image.Private.Cluster_image.img_remotes = Hash_set.to_list remotes
      ; img_cluster_type = Provider.string_of_t cluster_type
      }
    ;;

    let to_cluster
        (type a)
        (module Provider : Application_class.Provider with type t = a)
        { Env_image.Private.Cluster_image.img_remotes; img_cluster_type }
      =
      let maybe_cluster_type = Provider.maybe_t_of_string img_cluster_type in
      let cluster_type = Option.value_exn maybe_cluster_type in
      { Cluster.remotes = Hash_set.of_list (module Remote_target) img_remotes
      ; provider = (module Provider)
      ; cluster_type = ref cluster_type
      ; application_class_backend = Provider.application_class_backend_of_t cluster_type
      }
    ;;
  end

  type t = Env_image.Private.t_private [@@deriving sexp, bin_io]
  type t_inner = t

  let of_env { assignments; exports; clusters; active_cluster_ref; _ } =
    { Env_image.Private.img_assignments = String.Map.of_hashtbl_exn assignments
    ; img_exports = String.Set.of_hash_set exports
    ; img_clusters =
        String.Map.of_hashtbl_exn clusters
        |> Map.map ~f:(fun x -> Cluster_image.of_cluster x)
    ; img_active_cluster = !active_cluster_ref
    }
  ;;

  let to_env
      (type a)
      (module Provider : Application_class.Provider with type t = a)
      ~working_directory
      { Env_image.Private.img_assignments; img_exports; img_clusters; img_active_cluster }
    =
    { assignments =
        Hashtbl.of_alist_exn (module String) (String.Map.to_alist img_assignments)
    ; exports = Hash_set.of_list (module String) (String.Set.to_list img_exports)
    ; clusters =
        Map.map ~f:(fun x -> Cluster_image.to_cluster (module Provider) x) img_clusters
        |> String.Map.to_alist
        |> Hashtbl.of_alist_exn (module String)
    ; provider = (module Provider)
    ; active_cluster_ref = ref img_active_cluster
    ; working_directory_ref = ref working_directory
    ; job_group = Job.Job_group.create ()
    }
  ;;

  let of_public = Env_image.Private.private_of_t
  let to_public = Env_image.Private.t_of_private
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
                 (print_escape (Remote_target.to_string remote))))))
;;

let cluster_set_active (type a) t maybe_name =
  let name = Option.value ~default:default_cluster maybe_name in
  let { clusters; active_cluster_ref; provider; _ } = t in
  let (module Provider : Application_class.Provider with type t = a) = provider in
  Hashtbl.add clusters ~key:name ~data:(Cluster.create (module Provider)) |> ignore;
  active_cluster_ref := name
;;

let cluster_get_active t =
  let { active_cluster_ref; clusters; _ } = t in
  let active_cluster = !active_cluster_ref in
  match Hashtbl.find clusters active_cluster with
  | Some cluster -> cluster
  | None -> raise_s [%message "Active cluster should always exist"]
;;

module Cluster_target = struct
  type t =
    { backend : (module Application_class.Backend)
    ; setting : string
    ; remotes : Remote_target.t list
    }
  [@@deriving fields]

  let create (module Backend : Application_class.Backend) ~setting ~remotes =
    { backend = (module Backend); setting; remotes }
  ;;
end

module Cluster_resolution = struct
  type t =
    | Cluster of Cluster_target.t
    | Remote of Remote_target.t
end

let cluster_resolve t cluster_or_host =
  let { clusters; _ } = t in
  let target, setting = Remote_target.split_remote_target_setting cluster_or_host in
  match Hashtbl.find clusters target with
  | Some cluster ->
    let remotes = Cluster.get_remotes cluster in
    let target =
      Cluster_target.create (Cluster.application_class_backend cluster) ~setting ~remotes
    in
    Cluster_resolution.Cluster target
    (* |> List.map ~f:(fun remote -> Remote_target.with_setting remote ~setting)  *)
  | None ->
    (match Remote_target.resolve cluster_or_host with
    | Some host_and_port -> Cluster_resolution.Remote host_and_port
    | None -> raise_s [%message "Failed to resolve cluster or host: %s" cluster_or_host])
;;

let job_group t =
  let { job_group; _ } = t in
  job_group
;;
