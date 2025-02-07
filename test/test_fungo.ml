open OUnit2
open Core
open Fungo
include Fungo_ast
module Str = Fungo_ast.ASTString

let str = Str.from_string
let name = Str.dummy
let ident s = IdentifierType.Identifier (str s, None)
let int s = Expr.IntLiteral (str s)

module type ShowableEqual = sig
  type t

  val show : t -> string
  val equal : t -> t -> bool
end

let test_lexer () = Fungo.Lexer.lex_raw "let x = [(5 * 1 + 7); 0]"
let cmp_tokens a b : bool = List.for_all2_exn ~f:Fungo.Token.cmp a b

let print_ast (type a) (module M : ShowableEqual with type t = a) (value : a) : string =
  M.show value
;;

let compare (type a) (module M : ShowableEqual with type t = a) (expected : a) (got : a) =
  M.equal expected got
  |> assert_bool
       ("\nExpected: "
        ^ print_ast (module M) expected
        ^ "\n Got: "
        ^ print_ast (module M) got)
;;

let lex text f =
  match Fungo.Lexer.lex_raw text with
  | Ok t -> f t
  | Error e ->
    Fungo.Lexer.show_lexer_error e |> print_endline;
    assert_failure "Failed to lex input in parser test"
;;

let parse text f =
  lex text (fun tokens ->
    match Fungo.Parser.parse "testing" tokens with
    | Error e ->
      print_endline text;
      Fungo.Parser.print_parser_error e;
      assert_failure "Error parsing"
    | Ok m -> f m)
;;

let compare_top_level ?(print = false) expected m =
  if print then print_ast (module ModuleDefinition) m |> print_endline;
  compare
    (module ModuleDefinition)
    ModuleDefinition.{ expected with name = ASTString.from_string "testing" }
    m
;;

let first_parse_test =
  "First Parser Test"
  >:: fun _ ->
  let text = "let x = 1" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name = Str.dummy
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = IdentifierType.Identifier (Str.from_string "x", None)
                   ; recursive = false
                   ; args = []
                   ; body =
                       Expression.
                         { bindings = []; value = IntLiteral (Str.from_string "1") }
                   }
             ]
         })
;;

let second_parse_test =
  "Nested Let Expression"
  >:: fun _ ->
  let text = "let x = let y = 1 in x" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name = Str.from_string "testing"
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = IdentifierType.Identifier (Str.from_string "x", None)
                   ; recursive = false
                   ; args = []
                   ; body =
                       Expression.
                         { bindings =
                             [ LetBinding.
                                 { name =
                                     IdentifierType.Identifier (Str.from_string "y", None)
                                 ; recursive = false
                                 ; args = []
                                 ; body =
                                     Expression.
                                       { bindings = []
                                       ; value = Expr.IntLiteral (Str.from_string "1")
                                       }
                                 }
                             ]
                         ; value =
                             Expr.IdentifierExpr
                               (IdentifierType.Identifier (Str.from_string "x", None))
                         }
                   }
             ]
         })
;;

let check_operators =
  "Basic Operators"
  >:: fun _ ->
  let text = "let x = 1 + 5" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name = Str.dummy
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = ident "x"
                   ; recursive = false
                   ; args = []
                   ; body =
                       Expression.
                         { bindings = []
                         ; value =
                             Expr.FunctionCall
                               { name = str "+"; args = [ int "1"; int "5" ]; op = true }
                         }
                   }
             ]
         })
;;

let func_def =
  "Function Definition"
  >:: fun _ ->
  let text = "let my_func a b = a + b" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = ident "my_func"
                   ; recursive = false
                   ; args = [ ident "a"; ident "b" ]
                   ; body =
                       Expression.
                         { bindings = []
                         ; value =
                             Expr.FunctionCall
                               { name = str "+"
                               ; op = true
                               ; args =
                                   [ Expr.IdentifierExpr (ident "a")
                                   ; Expr.IdentifierExpr (ident "b")
                                   ]
                               }
                         }
                   }
             ]
         })
;;

let multiple_lets =
  "Multiple Lets"
  >:: fun _ ->
  let text = "\n\n\t\tlet x = 1\n\n\t\tlet y = 2\n\t\t" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = ident "x"
                   ; recursive = false
                   ; args = []
                   ; body = Expression.{ bindings = []; value = int "1" }
                   }
             ; TopLevel.LetBind
                 LetBinding.
                   { name = ident "y"
                   ; recursive = false
                   ; args = []
                   ; body = Expression.{ bindings = []; value = int "2" }
                   }
             ]
         })
;;

let tuples_and_exprs =
  "Tuples and Exprs"
  >:: fun _ ->
  let text = "\n\t\tlet x = (1, 2)\n\t\tlet (a, b) = x\n\t\t" in
  parse
    text
    (compare_top_level
       ModuleDefinition.
         { name
         ; body =
             [ TopLevel.LetBind
                 LetBinding.
                   { name = ident "x"
                   ; recursive = false
                   ; args = []
                   ; body =
                       Expression.
                         { bindings = []; value = Expr.TupleLiteral [ int "1"; int "2" ] }
                   }
             ; TopLevel.LetBind
                 LetBinding.
                   { name = IdentifierType.TupleDestructure ([ str "a"; str "b" ], None)
                   ; recursive = false
                   ; args = []
                   ; body =
                       Expression.
                         { bindings = []; value = Expr.IdentifierExpr (ident "x") }
                   }
             ]
         })
;;

let tests =
  "test_suite"
  >::: first_parse_test
       :: second_parse_test
       :: check_operators
       :: func_def
       :: tuples_and_exprs
       :: multiple_lets
       :: [ ("Is OK"
             >:: fun _ ->
             test_lexer ()
             |> (function
              | Ok x -> Ok x
              | Error e ->
                Fungo.Lexer.show_lexer_error e |> print_endline;
                Error e)
             |> Result.is_ok
             |> assert_bool "Result is not OK")
          ; ("Values match"
             >:: fun _ ->
             test_lexer ()
             |> function
             | Ok tokens -> assert_equal tokens tokens ~cmp:cmp_tokens
             | Error e ->
               Fungo.Lexer.show_lexer_error e |> print_endline;
               assert_failure "Result is not OK")
          ]
;;

let () = run_test_tt_main tests
