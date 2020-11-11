open Core
open Async
open Dresh_shell

let run_test input =
  let stdout = force Writer.stdout in
  Ast.parse input
  |> Or_error.map ~f:(fun ast -> eval ast ~writer:stdout ~to_close:false ~verbose:false)
;;

let%expect_test "test echo" =
  match run_test "echo \"hello\"" with
  | Error err ->
    print_endline (Error.to_string_hum err);
    return ()
  | Ok res ->
    let%bind () = res in
    [%expect {|hello|}]
;;
