open Core
module Lexer = Fungo_lib.Lexer
module Parser = Fungo_lib.Parser

let input =
  "\n\
   \t\t\tmodule X = \n\
   \t\t\t\ttype t\n\
   \t\t\t\tlet value x y = [x ; y] \n\
   \t\t\t\tlet make () = {test = true; test2 = false}\n\
   \t\t\tend\n\
   let func a b =\n\
   \tlet z = X.make () in\n\
   \tX.value z 1.5\n\n\
   let () =\n\
   \tfunc 1 2\n\
   \t\t\t"
;;

let () =
  Lexer.lex_raw input
  |> function
  | Error e -> Lexer.show_lexer_error e |> print_endline
  | Ok tokens ->
    Parser.parse "MainTesting" tokens
    |> (function
     | Error e -> Parser.show_parser_error e |> print_endline
     | Ok m -> Parser.Ast.ModuleDefinition.show m |> print_endline)
;;
