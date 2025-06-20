open OUnit2
open Base
include Fungo_lib
include Fungo_ast
include Test_helpers
module Str = Fungo_ast.ASTString

let test_lexer () = Lexer.lex "let x = [(5 * 1 + 7); 0]"

let tests =
  "test_suite"
  >::: Test_parser.tests
       @ Test_typechecker.tests
       @ [ ("Is OK"
            >:: fun _ ->
            test_lexer ()
            |> (function
             | Ok x -> Ok x
             | Error e ->
               Lexer.show_lexer_error e |> Stdio.print_endline;
               Error e)
            |> Result.is_ok
            |> assert_bool "Result is not OK")
         ; ("Values match"
            >:: fun _ ->
            test_lexer ()
            |> function
            | Ok tokens -> assert_equal tokens tokens ~cmp:cmp_tokens
            | Error e ->
              Lexer.show_lexer_error e |> Stdio.print_endline;
              assert_failure "Result is not OK")
         ]
;;

let () =
  Stdio.print_endline "Running tests";
  run_test_tt_main tests
;;
