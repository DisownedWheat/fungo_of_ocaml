type parser_error

module Ast = Fungo_ast
module T = Token

val parse : string -> T.t list -> (Ast.ModuleDefinition.t, parser_error) result
val print_parser_error : parser_error -> unit
val show_parser_error : parser_error -> string
