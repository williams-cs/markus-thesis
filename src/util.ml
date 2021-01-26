open Async

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
