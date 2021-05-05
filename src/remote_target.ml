open Core
open Ppx_compare_lib.Builtin

module Stable = struct
  type t =
    { host : string
    ; port : int option
    ; setting : string
    }
  [@@deriving fields, sexp, bin_io, compare, hash]
end

include Stable
include (Hashable.Make_binable (Stable) : Hashable.S_binable with type t := t)

let create ~host ~port ~setting = { host; port; setting }
let with_setting { host; port; setting = _ } ~setting = { host; port; setting }

let to_string { host; port; setting } =
  let setting_part =
    match setting with
    | "" -> ""
    | s -> sprintf "%s/" s
  in
  let port_part =
    match port with
    | None -> ""
    | Some i -> sprintf ":%d" i
  in
  sprintf "%s%s%s" setting_part host port_part
;;

let split_remote_target_setting remote =
  let remote_name_splitter = '/' in
  let parts = String.split ~on:remote_name_splitter remote in
  let non_final_parts =
    match List.length parts with
    | 1 -> []
    | _ -> List.slice parts 0 (List.length parts - 1)
  in
  let final_part = List.last parts |> Option.value ~default:"" in
  let setting =
    String.concat ~sep:(String.of_char remote_name_splitter) non_final_parts
  in
  final_part, setting
;;

let resolve remote =
  let target, setting = split_remote_target_setting remote in
  let uri = Uri.of_string (sprintf "ssh://%s" target) in
  match Uri.host uri with
  | None -> None
  | Some host ->
    let port = Uri.port uri in
    Some (create ~host ~port ~setting)
;;
