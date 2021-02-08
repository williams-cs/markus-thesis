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

let glue' ~reader ~writer =
  let rec print_until_done () =
    let%bind maybe_line = Reader.read_until reader (`Char '\n') ~keep_delim:true in
    match maybe_line with
    | `Eof -> return ()
    | `Ok line | `Eof_without_delim line ->
      Writer.write writer line;
      print_until_done ()
  in
  print_until_done ()
;;

let glue ~reader ~writer =
  let%bind reader = reader in
  let%bind writer = writer in
  glue' ~reader ~writer
;;

let verbose_println ~name ~verbose ~stderr ~host str =
  if verbose then fprintf stderr "%s\n" (sprintf "[%s-%s] " name host ^ str)
;;
