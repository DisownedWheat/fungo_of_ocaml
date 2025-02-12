type lexer_error

val lex_raw : string -> (Token.t list, lexer_error) result
val lex : string -> (Token.t list, lexer_error) result
val show_lexer_error : lexer_error -> string
