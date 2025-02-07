open Core
module Lexer = Fungo.Lexer
module Parser = Fungo.Parser

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
