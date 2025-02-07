open Core
module Lexer = Fungo_lib.Lexer
module Parser = Fungo_lib.Parser

let () =
  let input = "let x = 1" in
  Lexer.lex_raw input
  |> function
  | Error e -> Lexer.show_lexer_error e |> print_endline
  | Ok tokens ->
    Parser.parse "MainTesting" tokens
    |> (function
     | Error e -> Parser.show_parser_error e |> print_endline
     | Ok m -> Parser.Ast.ModuleDefinition.show m |> print_endline)
;;
