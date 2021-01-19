open Core

type t =
  { assignments : (string, string) Hashtbl.t
  ; exports : string Hash_set.t
  ; working_directory_ref : string ref
  }

let create ~working_directory =
  { assignments = Hashtbl.create (module String)
  ; exports = Hash_set.create (module String)
  ; working_directory_ref = ref working_directory
  }
;;

let copy { assignments; exports; working_directory_ref } =
  { assignments = Hashtbl.copy assignments
  ; exports = Hash_set.copy exports
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

let export_add t ~key =
  let { exports; _ } = t in
  Hash_set.add exports key
;;

let exports t =
  let { exports; _ } = t in
  let export_list = Hash_set.to_list exports in
  List.sort export_list ~compare:String.compare
;;

let exports_print t ~write_callback =
  let escape s =
    s
    |> String.to_list
    |> List.map ~f:(fun c ->
           let cs = String.of_char c in
           match c with
           | '\\' | '$' | '"' -> "\\" ^ cs
           | _ -> cs)
    |> String.concat
  in
  List.iter (exports t) ~f:(fun key ->
      let export_assign =
        match assign_get t ~key with
        | "" -> ""
        | value -> sprintf "=\"%s\"" (escape value)
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
