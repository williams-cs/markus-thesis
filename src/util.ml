open Core
open Async

let random_state_ref : Random.State.t option ref = ref None
let set_random_state state = random_state_ref := Some state

let random_state () =
  match !random_state_ref with
  | Some state -> state
  | None ->
    let state = Random.State.make_self_init () in
    random_state_ref := Some state;
    state
;;

let rec generate_uuid_list existing =
  let id = Uuid.create_random (random_state ()) |> Uuid.to_string in
  if List.exists existing ~f:(fun x -> String.equal x id)
  then generate_uuid_list existing
  else id
;;

let rec generate_uuid_hash_set existing =
  let id = Uuid.create_random (random_state ()) |> Uuid.to_string in
  if Hash_set.exists existing ~f:(fun x -> String.equal x id)
  then generate_uuid_hash_set existing
  else (
    Hash_set.add existing id;
    id)
;;

let glue_transform ~reader ~writer ~transform =
  let rpipe = Reader.pipe reader in
  let wpipe = Writer.pipe writer in
  let map_rpipe = Pipe.map rpipe ~f:transform in
  Pipe.transfer' map_rpipe wpipe ~f:Deferred.return
;;

let glue' ~reader ~writer = glue_transform ~reader ~writer ~transform:Fn.id

(* let rec print_until_done () = *)
(* printf "pud\n";
    let%bind maybe_line = Reader.read_until reader (`Char '\n') ~keep_delim:true in
    printf "pud2\n";
    match maybe_line with
    | `Eof -> return ()
    | `Ok line | `Eof_without_delim line ->
      Writer.write writer line;
      print_until_done ()
  in
  print_until_done () *)

let glue ~reader ~writer =
  let%bind reader = reader in
  let%bind writer = writer in
  glue_transform ~reader ~writer ~transform:Fn.id
;;

let simple_hash s =
  let sl = String.to_list s in
  let r1, r2 =
    List.fold sl ~init:(1, 0) ~f:(fun (s1, s2) c ->
        let sn = Char.to_int c in
        (s1 + sn) % 65521, (s2 + s1) % 65521)
  in
  Int.bit_or (Int.shift_left r2 16) r1
;;

let verbose_println ~name ~verbose ~stderr ~host ~port str =
  let port_string =
    match port with
    | None -> ""
    | Some i -> sprintf ":%d" i
  in
  if verbose then fprintf stderr "%s\n" (sprintf "[%s-%s%s] " name host port_string ^ str)
;;
