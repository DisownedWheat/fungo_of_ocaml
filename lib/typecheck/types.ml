open Base
module Ast = Fungo_ast

type 'a module_def =
  { name : Ast.ASTString.t
  ; types : 'a list
  }
[@@deriving show]
