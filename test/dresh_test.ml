open Core
open Async
open Dresh_shell

let run_test_no_timeout input =
  let stdout = force Writer.stdout in
  match Ast.parse input with
  | Error err ->
    print_endline (Error.to_string_hum err);
    return ()
  | Ok ast -> eval ast ~writer:stdout ~verbose:false
;;

let run_test input =
  let timeout sec =
    let%bind () = Async_unix.after (Time.Span.of_sec sec) in
    print_endline "Test case exceeded timeout!";
    exit 1
  in
  Deferred.any [ timeout 1.0; run_test_no_timeout input ]
;;

let%expect_test "echo" =
  let%bind () = run_test "echo hello" in
  [%expect {|hello|}]
;;

let%expect_test "empty" =
  let%bind () = run_test "" in
  [%expect {||}]
;;

let%expect_test "quotes" =
  let%bind () = run_test "echo \"hello\"" in
  [%expect {|hello|}]
;;

let%expect_test "concat" =
  let%bind () = run_test "echo \"hello\"world\"!\"" in
  [%expect {|helloworld!|}]
;;

let%expect_test "subshell_simple" =
  let%bind () = run_test "echo $(echo \"abcdef\")" in
  [%expect {|abcdef|}]
;;

let%expect_test "subshell_complex" =
  let%bind () = run_test "echo \"ab$(echo \"cd\")e\"fg" in
  [%expect {|abcdefg|}]
;;

let%expect_test "subshell_name" =
  let%bind () = run_test "\"e\"$(echo \"ch\")o \"a$(echo \"b\")c\"d" in
  [%expect {|abcd|}]
;;

let%expect_test "escape" =
  let%bind () =
    run_test
      "\\ec\\ho \\\"\\\"\"\\$(echo fake\\ subshell)$(echo \"real subshell\"\\)\\\\)\""
  in
  [%expect {|""$(echo fake\ subshell)real subshell)\|}]
;;

let%expect_test "single_quote" =
  let%bind () = run_test "echo 'abcdefghi'" in
  [%expect {|abcdefghi|}]
;;

let%expect_test "single_quote_no_sub" =
  let%bind () = run_test "echo '\\\\$(echo hi)\\$\\'" in
  [%expect {|\\$(echo hi)\$\|}]
;;

let%expect_test "backtick" =
  let%bind () = run_test "echo `echo hello`" in
  [%expect {|hello|}]
;;

let%expect_test "backtick_nested_unescaped" =
  let%bind () = run_test "echo `echo `echo hello``" in
  [%expect {|echo hello|}]
;;

let%expect_test "backtick_nested_escaped" =
  let%bind () = run_test "echo `echo \\`echo hello\\``" in
  [%expect {|hello|}]
;;

let%expect_test "echo_subshell_space" =
  let%bind () = run_test "echo $(echo \"a  b\")" in
  [%expect {|a b|}]
;;

let%expect_test "or" =
  let%bind () = run_test "echo \"a\" || echo \"b\"" in
  [%expect {|a|}]
;;

let%expect_test "not_or" =
  let%bind () = run_test "! echo \"a\" || ! echo \"b\"" in
  [%expect {|
    a
    b|}]
;;

let%expect_test "and" =
  let%bind () = run_test "echo \"a\" && echo \"b\"" in
  [%expect {|
    a
    b|}]
;;

let%expect_test "parallel_execution" =
  let%bind () = run_test "echo $(sleep 0.02s; echo a) & echo $(sleep 0.01s; echo b)" in
  [%expect {|
    b 
    a|}]
;;

let%expect_test "serial_execution" =
  let%bind () = run_test "echo $(sleep 0.02s; echo a) ; echo $(sleep 0.01s; echo b)" in
  [%expect {|
    a 
    b|}]
;;
