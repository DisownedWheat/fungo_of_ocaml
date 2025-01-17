open OUnit2

let test_lexer () = Fungo.Lexer.lex_raw "let x = [(5 * 1 + 7); 0]"
let cmp_tokens a b : bool = List.for_all2 Fungo.Token.cmp a b

let tests =
  "test_suite"
  >::: [
         ( "Is OK" >:: fun _ ->
           test_lexer ()
           |> (function
                | Ok x -> Ok x
                | Error e ->
                    Fungo.Lexer.show_lexer_error e |> print_endline;
                    Error e)
           |> Result.is_ok
           |> assert_bool "Result is not OK" );
         ( "Values match" >:: fun _ ->
           test_lexer () |> function
           | Ok tokens -> assert_equal tokens tokens ~cmp:cmp_tokens
           | Error e ->
               Fungo.Lexer.show_lexer_error e |> print_endline;
               assert_failure "Result is not OK" );
         ( "First Parser Test" >:: fun _ ->
           test_lexer () |> function
           | Ok tokens -> (
               Fungo.Parser.parse "test" tokens |> fun x ->
               match x with
               | Ok _ -> assert_bool "" true
               | Error e ->
                   print_endline "Something went very wrong";
                   Fungo.Parser.print_parser_error e;
                   assert_failure "Error parsing")
           | Error e ->
               Fungo.Lexer.show_lexer_error e |> print_endline;
               assert_failure "Failed to lex input in parser test" );
       ]

let () = run_test_tt_main tests
