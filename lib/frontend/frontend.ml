open Base
module Ast = Fungo_ast

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )
let err f = Result.map_error ~f

type error =
  | ParserError of Parser.parser_error
  | LexerError of Lexer.lexer_error
[@@deriving show]

let compile file_name =
  let input = Stdio.In_channel.read_all file_name in
  Lexer.lex input
  |> err (fun x -> LexerError x)
  >>= (fun tokens -> Parser.parse file_name tokens |> err (fun x -> ParserError x))
  |> function
  | Ok ast -> Ast.ModuleDefinition.show ast |> Stdio.print_endline
  | Error e -> show_error e |> Stdio.print_endline
;;
