type lexer_error [@@deriving show]

val lex : string -> (Token.t list, lexer_error) result
