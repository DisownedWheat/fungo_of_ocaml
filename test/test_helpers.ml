open OUnit2
open Core
open Fungo_lib
include Fungo_ast
module Str = Fungo_ast.ASTString
module Parser = Fungo_lib.Parser
module Lexer = Fungo_lib.Lexer

let str = Str.from_string
let name = Str.dummy
let ident s = IdentifierType.Identifier (str s, None)
let ident_expr s = Expr.IdentifierExpr (ident s)
let int s = Expr.IntLiteral (str s)

module type ShowableEqual = sig
  type t

  val show : t -> string
  val equal : t -> t -> bool
end

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

let lex ?(print = false) text f =
  match Fungo_lib.Lexer.lex_raw text with
  | Ok t ->
    if print then Token.print_tokens t;
    f t
  | Error e ->
    Lexer.show_lexer_error e |> print_endline;
    assert_failure "Failed to lex input in parser test"
;;

let parse ?(print = false) text f =
  lex ~print text (fun tokens ->
    match Parser.parse "testing" tokens with
    | Error e ->
      print_endline text;
      Parser.print_parser_error e;
      assert_failure "Error parsing"
    | Ok m -> f m)
;;

let compare_top_level ?(print = false) expected m =
  if print then print_ast (module ModuleDefinition) m |> print_endline;
  let expected = ModuleDefinition.{ name; body = expected } in
  compare
    (module ModuleDefinition)
    ModuleDefinition.{ expected with name = ASTString.from_string "testing" }
    m
;;

let cmp_tokens a b : bool = List.for_all2_exn ~f:Token.cmp a b
