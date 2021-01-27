open Core

module Cluster = struct
  type t = { remotes : string Hash_set.t }

  let create () = { remotes = Hash_set.create (module String) }
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
            write_callback (sprintf "cluster -a \"%s\"" (print_escape remote)))))
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
  | Some cluster -> List.iter remotes ~f:(fun remote -> Cluster.add_remote cluster remote)
  | None -> raise_s [%message "Active cluster should always exist"]
;;

let cluster_resolve t cluster_or_host =
  let { clusters; _ } = t in
  match Hashtbl.find clusters cluster_or_host with
  | Some cluster -> Cluster.get_remotes cluster
  | None -> [ cluster_or_host ]
;;

let job_group t =
  let { job_group; _ } = t in
  job_group
;;
