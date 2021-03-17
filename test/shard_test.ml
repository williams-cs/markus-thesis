open Core
open Async
open Shard

let () =
  Util.set_random_state (Random.State.make (Array.of_list [ 3; 1; 4; 1; 5; 9; 2; 6 ]))
;;

let test_cwd = "/tmp/shard/test"

let run_test_no_timeout input =
  let%bind () = Unix.mkdir ~p:() test_cwd in
  let stdout = force Writer.stdout in
  let stderr = force Writer.stderr in
  match Ast.parse input with
  | Error err ->
    print_endline (Error.to_string_hum err);
    return ()
  | Ok ast ->
    Eval.eval
      ast
      ~eval_args:
        (Eval.Eval_args.create
           ~env:(create_env ~working_directory:test_cwd)
           ~stdin:None
           ~stdout
           ~stderr
           ~verbose:false)
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

let%expect_test "subshell_multi" =
  let%bind () = run_test "echo $(echo \"first message\") $(echo \"second message\")" in
  [%expect {|first message second message|}]
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
  let%bind () = Clock.after (Time.Span.of_sec 0.3) in
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

let%expect_test "echo_pipe_to_echo" =
  let%bind () = run_test "echo $(sleep 0.1; echo test) | echo" in
  [%expect {||}]
;;

let%expect_test "variable_assign_command" =
  let%bind () = run_test "a=hi echo $a" in
  [%expect {|hi|}]
;;

let%expect_test "unassigned_variable" =
  let%bind () = run_test "echo $a" in
  [%expect {||}]
;;

let%expect_test "unassigned_variable_run" =
  let%bind () = run_test "$unassigned" in
  [%expect {||}]
;;

let%expect_test "variable_assign_noop" =
  let%bind () = run_test "test=test" in
  [%expect {||}]
;;

let%expect_test "variable_assign_local" =
  let%bind () = run_test "v=abcd; echo $v" in
  [%expect {|abcd|}]
;;

(* Note: Not consistent with bash shell due to numbers in variable names and command substitution in assignment *)
let%expect_test "variable_assign_multi" =
  let%bind () = run_test "v1=\"ha\" v2=$(echo $v1$v1) echo haha'$v2'\"$v2\"\\$v2" in
  [%expect {|haha$v2haha$v2|}]
;;

let%expect_test "special_character_eq" =
  let%bind () = run_test "echo f=ma" in
  [%expect {|f=ma|}]
;;

(* TODO: fix parser so that '@' can be used in normal contexts without escaping *)
(* let%expect_test "special_character_at" =
  let%bind () = run_test "echo seti@home" in
  [%expect {|seti@home|}]
;; *)

(* Filesystem tests. Based on a test_cwd of /tmp/shard/test *)

let%expect_test "pwd" =
  let%bind () = run_test "pwd" in
  (* Value same as test_cwd *)
  [%expect {|/tmp/shard/test|}]
;;

let%expect_test "cd" =
  let%bind () = run_test "(cd ..; pwd)" in
  [%expect {|/tmp/shard|}]
;;

let%expect_test "cd_independent" =
  let%bind () = run_test "(cd ..; pwd); (cd ../test; pwd) " in
  [%expect {|
  /tmp/shard
  /tmp/shard/test|}]
;;

(* Export tests *)

let%expect_test "export_empty" =
  let%bind () = run_test "export" in
  [%expect {||}]
;;

let%expect_test "export_assign" =
  let%bind () = run_test "export key=v; export" in
  [%expect {|export key="v"|}]
;;

let%expect_test "export_assign_escape" =
  let%bind () = run_test "export key=\\$\\\"\\\\; export" in
  [%expect {|export key="\$\"\\"|}]
;;

let%expect_test "export_no_assign" =
  let%bind () = run_test "export key; export" in
  [%expect {|export key|}]
;;

let%expect_test "export_existing_assign" =
  let%bind () = run_test "k0=v0; k1=v1; k2=v2; export -p k4 k3=v3 k2 k1=vn1" in
  [%expect {|
  export k1="vn1"
  export k2="v2"
  export k3="v3"
  export k4|}]
;;

let%expect_test "if_file_exists_false" =
  let%bind () =
    run_test
      "if [ -f non_existent_file ]; then echo \"File does exist\"; else echo \"File does \
       not exist\"; fi"
  in
  [%expect {|File does not exist|}]
;;

let%expect_test "if_variable_exists_true" =
  let%bind () =
    run_test
      "thevar=\"Hello\" ; if [ -n $thevar ]; then echo \"The variable exists\"; else \
       echo \"The variable does not exist\"; fi"
  in
  [%expect {|The variable exists|}]
;;

let%expect_test "if_without_else" =
  let%bind () =
    run_test
      "largenum=30 ; smallnum=5; if [ $largenum -gt $smallnum ]; then echo \"Greater \
       than\"; fi"
  in
  [%expect {|Greater than|}]
;;

let%expect_test "if_variable_equal_elif" =
  let%bind () =
    run_test
      "somevar=1337 ; if [ $somevar = 10 ]; then echo \"Branch 1\"; elif [ $somevar = \
       1337 ]; then echo \"Branch 2\"; else echo \"Branch 3\"; fi"
  in
  [%expect {|Branch 2|}]
;;

(* TODO redirection tests; potentially figure out how to mock file system *)
