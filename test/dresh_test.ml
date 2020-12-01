open Core
open Async
open Dresh_shell

let run_test_no_timeout input =
  let stdout = force Writer.stdout in
  match Ast.parse input with
  | Error err ->
    print_endline (Error.to_string_hum err);
    return ()
  | Ok ast ->
    eval ast ~eval_args:(Dresh_shell.Eval_args.create ~stdin:None ~stdout ~verbose:false)
    |> Deferred.ignore_m
;;

let run_test input =
  let timeout sec =
    let%bind () = Async_unix.after (Time.Span.of_sec sec) in
    print_endline "Test case exceeded timeout!";
    exit 1
  in
  Deferred.any [ timeout 5.0; run_test_no_timeout input ]
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

let%expect_test "parens" =
  let%bind () = run_test "(echo hi)" in
  [%expect {| hi |}]
;;

let%expect_test "parens_or" =
  let%bind () = run_test "(echo \"a\" || echo \"b\") || (echo \"c\" || echo \"d\")" in
  [%expect {| a |}]
;;

let%expect_test "parens_and" =
  let%bind () = run_test "(echo \"a\" && echo \"b\") && (echo \"c\" && echo \"d\")" in
  [%expect {|
    a
    b
    c
    d |}]
;;

let%expect_test "parens_inside_subshell" =
  let%bind () = run_test "echo $( (echo \"hello\") )" in
  [%expect {| hello |}]
;;

let%expect_test "parallel_execution" =
  let%bind () = run_test "echo $(sleep 0.2s; echo a) & echo $(sleep 0.1s; echo b)" in
  [%expect {|
    b 
    a|}]
;;

let%expect_test "serial_execution" =
  let%bind () = run_test "echo $(sleep 0.2s; echo a) ; echo $(sleep 0.1s; echo b)" in
  [%expect {|
    a 
    b|}]
;;

let%expect_test "pipeline" =
  let%bind () = run_test "echo \"abcdefghij\" | cat" in
  [%expect {|abcdefghij|}]
;;

let%expect_test "newlines" =
  let%bind () = run_test {|echo $(
echo hello
echo world
)|} in
  [%expect {| hello world |}]
;;

let%expect_test "error_empty_parens" =
  let%bind () = run_test "()" in
  [%expect {| : end_of_input |}]
;;

let%expect_test "empty_backticks" =
  let%bind () = run_test "``" in
  [%expect {||}]
;;

let%expect_test "empty_command_substitution" =
  let%bind () = run_test "$()" in
  [%expect {||}]
;;
