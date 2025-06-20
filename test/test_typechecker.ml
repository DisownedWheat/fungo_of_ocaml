open OUnit2
open Fungo_lib
include Fungo_ast
include Test_helpers
module Str = Fungo_ast.ASTString

let basic_alias =
  "basic_alias"
  >:: fun _ ->
  let text = "type a = int" in
  parse text (fun ast ->
    ModuleDefinition.show ast |> Stdio.print_endline;
    assert_string "")
;;

let tests = basic_alias :: []
