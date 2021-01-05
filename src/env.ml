open Core

type t = { assignments : (string, string) Hashtbl.t }

let create () = { assignments = Hashtbl.create (module String) }

let assign_get t ~key =
  let { assignments } = t in
  Hashtbl.find assignments key |> Option.value ~default:""
;;

let assign_set t ~key ~data =
  let { assignments } = t in
  let old_value = assign_get t ~key in
  Hashtbl.set assignments ~key ~data;
  old_value
;;
