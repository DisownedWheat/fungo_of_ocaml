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
  let expected = ModuleDefinition.{ name; body = expected } in
  compare
    (module ModuleDefinition)
    ModuleDefinition.{ expected with name = ASTString.from_string "testing" }
    m
;;

let cmp_tokens a b : bool = List.for_all2_exn ~f:Fungo.Token.cmp a b
