open OUnit2
open Fungo_lib
include Fungo_ast
include Test_helpers
module Str = Fungo_ast.ASTString

let first_parse_test =
  "First Parser Test"
  >:: fun _ ->
  let text = "let x = 1" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = IdentifierType.Identifier (Str.from_string "x", None)
             ; recursive = false
             ; args = []
             ; body = IntLiteral (Str.from_string "1")
             }
       ])
;;

let second_parse_test =
  "Nested Let Expression"
  >:: fun _ ->
  let text = "let x = let y = 1 in x" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = IdentifierType.Identifier (Str.from_string "x", None)
             ; recursive = false
             ; args = []
             ; body =
                 Expr.Expression
                   Expression.
                     { bindings =
                         [ LetBinding.
                             { name = IdentifierType.Identifier (Str.from_string "y", None)
                             ; recursive = false
                             ; args = []
                             ; body = Expr.IntLiteral (Str.from_string "1")
                             }
                         ]
                     ; value =
                         Expr.IdentifierExpr
                           (IdentifierType.Identifier (Str.from_string "x", None))
                     }
             }
       ])
;;

let check_operators =
  "Basic Operators"
  >:: fun _ ->
  let text = "let x = 1 + 5" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.FunctionCall
                   { name = str "+"; args = [ int "1"; int "5" ]; op = true }
             }
       ])
;;

let func_def =
  "Function Definition"
  >:: fun _ ->
  let text = "let my_func a b = a + b" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "my_func"
             ; recursive = false
             ; args = [ ident "a"; ident "b" ]
             ; body =
                 Expr.FunctionCall
                   { name = str "+"
                   ; op = true
                   ; args =
                       [ Expr.IdentifierExpr (ident "a")
                       ; Expr.IdentifierExpr (ident "b")
                       ]
                   }
             }
       ])
;;

let multiple_lets =
  "Multiple Lets"
  >:: fun _ ->
  let text = "\n\n\t\tlet x = 1\n\n\t\tlet y = 2\n\t\t" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.{ name = ident "x"; recursive = false; args = []; body = int "1" }
       ; TopLevel.LetBind
           LetBinding.{ name = ident "y"; recursive = false; args = []; body = int "2" }
       ])
;;

let tuples_and_exprs =
  "Tuples and Exprs"
  >:: fun _ ->
  let text = "\n\t\tlet x = ((1 + 3), 2, 4)\n\t\tlet (a, b) = x\n\t\t" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.TupleLiteral
                   [ Expr.FunctionCall
                       { name = str "+"; op = true; args = [ int "1"; int "3" ] }
                   ; int "2"
                   ; int "4"
                   ]
             }
       ; TopLevel.LetBind
           LetBinding.
             { name = IdentifierType.TupleDestructure ([ str "a"; str "b" ], None)
             ; recursive = false
             ; args = []
             ; body = Expr.IdentifierExpr (ident "x")
             }
       ])
;;

let array_literals =
  "Array Literals"
  >:: fun _ ->
  let text = "let x = [1;2;3;(4,5)]" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.ArrayLiteral
                   [ int "1"; int "2"; int "3"; Expr.TupleLiteral [ int "4"; int "5" ] ]
             }
       ])
;;

let record_literals =
  "RecordLiterals"
  >:: fun _ ->
  let text = "let x = ({field=\"Hello\"})" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.RecordLiteral
                   [ RecordField.
                       { name = str "field"; value = Expr.StringLiteral (str "Hello") }
                   ]
             }
       ])
;;

let accessors =
  "accessors"
  >:: fun _ ->
  let text = "let x = a.b.c.def" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.Accessor
                   { base = Expr.IdentifierExpr (ident "a")
                   ; fields = [ str "b"; str "c"; str "def" ]
                   }
             }
       ])
;;

let indexes =
  "indexes"
  >:: fun _ ->
  let text = "let x = (let z = 1 in a)[\"b\"]\n\t\tlet y = a[(let x = 1 in x)]\n\t\t" in
  parse
    text
    (compare_top_level
       [ TopLevel.LetBind
           LetBinding.
             { name = ident "x"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.Index
                   { left =
                       Expr.Expression
                         Expression.
                           { bindings =
                               [ LetBinding.
                                   { name = ident "z"
                                   ; args = []
                                   ; recursive = false
                                   ; body = int "1"
                                   }
                               ]
                           ; value = Expr.IdentifierExpr (ident "a")
                           }
                   ; right = Expr.StringLiteral (str "b")
                   }
             }
       ; TopLevel.LetBind
           LetBinding.
             { name = ident "y"
             ; recursive = false
             ; args = []
             ; body =
                 Expr.Index
                   { left = Expr.IdentifierExpr (ident "a")
                   ; right =
                       Expr.Expression
                         Expression.
                           { bindings =
                               [ LetBinding.
                                   { name = ident "x"
                                   ; recursive = false
                                   ; args = []
                                   ; body = int "1"
                                   }
                               ]
                           ; value = Expr.IdentifierExpr (ident "x")
                           }
                   }
             }
       ])
;;

let modules =
  "modules"
  >:: fun _ ->
  let text = "module X = \n\t\t\tlet x = 1\n\t\t\tend" in
  parse
    text
    (compare_top_level
       [ TopLevel.Module
           ModuleDefinition.
             { name = str "X"
             ; body =
                 [ TopLevel.LetBind
                     LetBinding.
                       { name = ident "x"; recursive = false; args = []; body = int "1" }
                 ]
             }
       ])
;;

let tests =
  [ first_parse_test
  ; second_parse_test
  ; check_operators
  ; func_def
  ; tuples_and_exprs
  ; multiple_lets
  ; array_literals
  ; record_literals
  ; accessors
  ; indexes
  ; modules
  ]
;;
