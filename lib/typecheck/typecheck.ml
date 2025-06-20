open Types
module Ast = Fungo_ast

type typecheck_error =
  | Invalid_top_level of Fungo_ast.TopLevel.t
  | None
[@@deriving show]

let check_module ast_mod =
  let Ast.ModuleDefinition.{ name; body } = ast_mod in
  let _ = { name; types = [] } in
  Stdio.print_endline "Type checking";
  match body with
  | [] -> Ok ast_mod
  | Ast.TopLevel.TypeDefinition (name, _) :: _ ->
    Stdio.print_endline name.value;
    Ok ast_mod
  | _ -> Error None
;;
