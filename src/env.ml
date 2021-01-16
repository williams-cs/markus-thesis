open Core

type t =
  { assignments : (string, string) Hashtbl.t
  ; working_directory_ref : string ref
  }

let create ~working_directory =
  { assignments = Hashtbl.create (module String)
  ; working_directory_ref = ref working_directory
  }
;;

let copy { assignments; working_directory_ref } =
  { assignments = Hashtbl.copy assignments
  ; working_directory_ref = ref !working_directory_ref
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

let cd t ~dir =
  t.working_directory_ref := dir;
  ()
;;

let cwd t =
  let r = t.working_directory_ref in
  !r
;;
