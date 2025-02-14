module Ast = Fungo_ast

type 'a module_def =
  { name : string
  ; types : 'a list
  }
