open Core
include Fungo_ast
module Str = ASTString
module T = Token
module Ast = Fungo_ast

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )

type parser_error =
  | UnexpectedToken of (Token.t list * string)
  | UnexpectedEOF
  | UnknownError of Token.t list
  | UnexpectedError
  | InvalidLetBinding of IdentifierType.t
[@@deriving show]

let print_parser_error e =
  match e with
  | UnexpectedError | UnexpectedEOF -> show_parser_error e |> print_endline
  | UnexpectedToken (l, msg) ->
    print_string "UnexpectedToken in ";
    print_endline msg;
    List.map l ~f:T.show |> List.iter ~f:print_endline;
    ()
  | UnknownError l ->
    print_endline "UnexpectedError";
    List.length l |> Int.to_string |> print_endline;
    List.map l ~f:T.show |> List.iter ~f:print_endline;
    ()
  | InvalidLetBinding x -> IdentifierType.show x |> print_endline
;;

let rec ident_type_match a b =
  match a, b with
  | IdentifierType.Identifier _, IdentifierType.Identifier _
  | IdentifierType.ArrayDestructure _, IdentifierType.ArrayDestructure _
  | IdentifierType.RecordDestructure _, IdentifierType.RecordDestructure _
  | IdentifierType.TupleDestructure _, IdentifierType.TupleDestructure _ -> true
  | Pointer x, Pointer y | Deref x, Deref y -> ident_type_match x y
  | _ -> false
;;

let rec parse_type_literal tokens =
  let rec parse_modules mods = function
    | T.Identifier name :: T.Operator (T.Dot, _) :: rest ->
      parse_modules (name :: mods) rest
    | T.Identifier name :: rest ->
      let mod_names = name :: mods |> List.rev |> List.map ~f:Str.from_token in
      Ok (mod_names, rest)
    | x ->
      Error (UnexpectedToken (x, "Unexpected token when parsing modules in type literal"))
  in
  match tokens with
  | T.Operator (T.LParen, _) :: T.Operator (T.RParen, _) :: rest ->
    Ok (TypeLiteral.Unit, rest)
  | T.Operator (T.LBracket, _) :: T.Operator (T.RBracket, _) :: rest ->
    parse_type_literal rest >>| fun (x, rest) -> TypeLiteral.Slice x, rest
  | T.Operator (T.Deref, _) :: rest ->
    parse_type_literal rest >>| fun (x, rest) -> TypeLiteral.PointerType x, rest
  | x ->
    parse_modules [] x
    >>= fun (mods, rest) ->
    (match mods with
     | [] -> Error (UnknownError tokens)
     | name :: modules -> Ok (TypeLiteral.Type { name; modules }, rest))
;;

let rec parse_record_definition tokens fields =
  let field tokens =
    match tokens with
    | T.Identifier i :: T.Operator (T.Colon, _) :: rest ->
      (match parse_type_literal rest with
       | Ok (t_, rest) ->
         Ok (RecordTypeField.{ name = Str.from_token i; type_ = t_ }, rest)
       | Error e -> Error e)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken (x, "Record Definition"))
  in
  match tokens with
  | T.Operator (T.RBrace, _) :: rest -> Ok (TypeDef.RecordDefinition fields, rest)
  | _ ->
    (match field tokens with
     | Ok (new_field, rest) ->
       (match rest with
        | T.Operator (T.Semicolon, _) :: rest ->
          parse_record_definition rest (new_field :: fields)
        | T.Operator (T.RBrace, _) :: _ ->
          parse_record_definition rest (new_field :: fields)
        | [] -> Error UnexpectedEOF
        | x ->
          Error (UnexpectedToken (x, "Record Definition Looking For Comma or RBrace")))
     | Error e -> Error e)
;;

let rec parse_tuple_definition tokens ts =
  match tokens with
  | T.Operator (T.RParen, _) :: rest ->
    Ok (TypeDef.TupleDefinition { length = List.length ts; types = ts }, rest)
  | x ->
    parse_type_literal x
    >>= fun (t_, rest) ->
    (match rest with
     | T.Operator (T.Comma, _) :: rest -> parse_tuple_definition rest (t_ :: ts)
     | T.Operator (T.RParen, _) :: _ -> parse_tuple_definition rest (t_ :: ts)
     | [] -> Error UnexpectedEOF
     | x -> Error (UnexpectedToken (x, "Tuple Definition")))
;;

let rec parse_variant_definition tokens ts =
  match tokens with
  | T.Operator (T.Pipe, _) :: T.Identifier t :: T.Of _ :: rest ->
    parse_type_definition rest
    >>= fun (node, rest) ->
    let name = Str.from_token t in
    let t = name, Some node in
    parse_variant_definition rest (t :: ts)
  | T.Operator (T.Pipe, _) :: T.Identifier t :: rest ->
    let name = Str.from_token t in
    parse_variant_definition rest ((name, None) :: ts)
  | _ -> Ok (TypeDef.Variant ts, tokens)

and parse_type_definition tokens =
  match tokens with
  | T.Operator (T.LBrace, _) :: rest -> parse_record_definition rest []
  | T.Operator (T.LParen, _) :: rest -> parse_tuple_definition rest []
  | T.Operator (T.Pipe, _) :: _ -> parse_variant_definition tokens []
  | x -> Error (UnknownError x)
;;

let parse_identifier tokens =
  let rec loop tokens =
    match tokens with
    | T.Operator (T.Pointer, _) :: rest ->
      loop rest >>= fun (x, rest) -> Ok (IdentifierType.Pointer x, rest)
    | T.Operator (T.Deref, _) :: rest ->
      loop rest >>= fun (x, rest) -> Ok (IdentifierType.Deref x, rest)
    | T.Identifier t :: rest ->
      Ok (IdentifierType.Identifier (Str.from_token t, None), rest)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken (x, "Base Identifier"))
  in
  match tokens with
  | T.Operator (T.LParen, _) :: T.Identifier t :: T.Operator (T.Colon, _) :: rest ->
    parse_type_literal rest
    >>| fun (node, rest) ->
    let str = Str.from_token t in
    let ident = IdentifierType.Identifier (str, Some node) in
    ident, rest
  | _ -> loop tokens
;;

let parse_destructure tokens typed delimiter end_token =
  let rec loop ?(delim_check = false) tokens fields =
    match tokens with
    | T.Operator (x, _) :: rest when T.equal_operator x end_token ->
      Ok (List.rev fields, rest)
    | T.Operator (d, t) :: rest when T.equal_operator d delimiter ->
      if delim_check
      then
        Error (UnexpectedToken (T.Operator (d, t) :: rest, "Double delim in destructure"))
      else loop ~delim_check:true rest fields
    | T.Identifier t :: rest -> loop ~delim_check:false rest (Str.from_token t :: fields)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken (x, "Destructure parsing"))
  in
  loop tokens []
  >>= fun (fields, rest) ->
  if typed
  then (
    match rest with
    | T.Operator (T.Colon, _) :: rest ->
      parse_type_literal rest
      >>= fun (t_, rest) ->
      (match rest with
       | T.Operator (T.RParen, _) :: tail -> Ok ((fields, Some t_), tail)
       | [] -> Error UnexpectedEOF
       | head ->
         Error
           (UnexpectedToken (head, "Destructure parsing RParen for typed destructure")))
    | [] -> Error UnexpectedEOF
    | T.Operator (T.RParen, _) :: rest -> Ok ((fields, None), rest)
    | x -> Error (UnexpectedToken (x, "Error finding RParen on destructure arg")))
  else Ok ((fields, None), rest)
;;

let rec parse_let_expression tokens =
  let rec parse_args tokens args =
    (match tokens with
     | T.Operator (T.LParen, _) :: T.Operator (T.LBrace, _) :: rest ->
       parse_destructure rest true T.Semicolon T.RBrace
       >>| fun (fields, tail) -> IdentifierType.RecordDestructure fields, tail
     | T.Operator (T.LBrace, _) :: rest ->
       parse_destructure rest false T.Semicolon T.RBrace
       >>| fun (fields, tail) -> IdentifierType.RecordDestructure fields, tail
     | T.Operator (T.LParen, _) :: T.Operator (T.LBracket, _) :: rest ->
       parse_destructure rest true T.Semicolon T.RBracket
       >>| fun (fields, tail) -> IdentifierType.ArrayDestructure fields, tail
     | T.Operator (T.LBracket, _) :: rest ->
       parse_destructure rest false T.Semicolon T.RBracket
       >>| fun (fields, tail) -> IdentifierType.ArrayDestructure fields, tail
     | T.Operator (T.LParen, _) :: T.Operator (T.LParen, _) :: rest ->
       parse_destructure rest true T.Comma T.RParen
       >>| fun (fields, tail) -> IdentifierType.TupleDestructure fields, tail
     | T.Operator (T.LParen, _) :: rest ->
       (match parse_destructure rest false T.Comma T.RParen with
        | Ok (fields, tail) -> Ok (IdentifierType.TupleDestructure fields, tail)
        | Error _ -> parse_identifier tokens)
     | _ :: _ -> parse_identifier tokens
     | [] -> Error UnexpectedEOF)
    >>= fun (node, rest) ->
    match rest with
    | T.Operator (T.Assign, _) :: tail -> Ok (List.rev (node :: args), tail)
    | _ -> parse_args rest (node :: args)
  in
  let recursive, tail =
    match tokens with
    | T.Rec _ :: tail -> true, tail
    | _ -> false, tokens
  in
  parse_args tail []
  >>= (fun (idents, tokens) ->
  match List.length idents, idents with
  | 1, head :: tail -> Ok (head, tail, tokens)
  | _, head :: tail
    when ident_type_match head (IdentifierType.Identifier (Str.dummy, None)) ->
    Ok (head, tail, tokens)
  | _, x :: _ -> Error (InvalidLetBinding x)
  | _, [] ->
    T.print_tokens tokens;
    Error UnexpectedError)
  >>= fun (name, args, tokens) ->
  parse_expression tokens []
  >>= fun (body, rest) -> Ok (LetBinding.{ name; recursive; args; body }, rest)

and parse_expression tokens bindings =
  match tokens with
  | T.Operator (T.LParen, _) :: tail ->
    parse_expression tail bindings
    >>= fun (node, rest) ->
    (match rest with
     | T.Operator (Token.RParen, _) :: tail -> Ok (node, tail)
     | _ :: _ ->
       parse_expr tokens
       >>= fun (expr, rest) -> Ok (Expression.{ bindings; value = expr }, rest)
     | [] -> Error UnexpectedEOF)
  | T.Let _ :: tail ->
    parse_let_expression tail
    >>= fun (binding, tokens) ->
    (match tokens with
     | T.In _ :: tail | T.Operator (T.Semicolon, _) :: tail ->
       parse_expression tail (binding :: bindings)
     | [] -> Error UnexpectedEOF
     | x -> Error (UnexpectedToken (x, "No In or ; in Let Expression")))
  | _ ->
    parse_expr tokens
    >>= fun (expr, rest) -> Ok (Expression.{ bindings; value = expr }, rest)

and parse_expr tokens =
  let expr = function
    | T.Operator (T.LParen, x) :: T.Operator (T.RParen, _) :: tail ->
      Ok (Expr.UnitExpr (Str.from_token x), tail)
    | T.IntLiteral x :: tail -> Ok (Expr.IntLiteral (Str.from_token x), tail)
    | T.FloatLiteral x :: tail -> Ok (Expr.FloatLiteral (Str.from_token x), tail)
    | T.StringLiteral x :: tail -> Ok (Expr.StringLiteral (Str.from_token x), tail)
    | T.Identifier x :: tail ->
      let name = Str.from_token x in
      let ident = name, None in
      Ok (Expr.IdentifierExpr (IdentifierType.Identifier ident), tail)
    | T.Operator (T.LBracket, _) :: tail ->
      parse_array tail >>| fun (exprs, tail) -> Expr.ArrayLiteral (List.rev exprs), tail
    | T.Operator (T.LBrace, _) :: tail -> parse_record_literal tail
    | T.Operator (T.LParen, _) :: tail ->
      parse_expr tail
      >>= fun (node, tokens) ->
      (match tokens with
       | T.Operator (T.RParen, _) :: tail -> Ok (node, tail)
       | x -> Error (UnexpectedToken (x, "Unexpected Token in parenthesised Expr")))
    | rest -> Error (UnknownError rest)
  in
  expr tokens
  >>= fun (node, tokens) ->
  match tokens with
  | T.Operator (T.LBracket, _) :: rest ->
    parse_expression rest []
    >>= fun (expression, rest) ->
    (match rest with
     | T.Operator (T.RBracket, _) :: rest ->
       Ok (Expr.Index { left = node; right = expression }, rest)
     | x -> Error (UnexpectedToken (x, "Index")))
  | T.Operator (T.Dot, _) :: _ ->
    parse_accessor tokens
    |> fun (fields, rest) -> Ok (Expr.Accessor { base = node; fields }, rest)
  | T.Operator (T.Comma, _) :: tail ->
    parse_expr tail
    >>| fun (right, tokens) ->
    ( (match right with
       | Expr.TupleLiteral exprs -> Expr.TupleLiteral (node :: exprs)
       | right -> Expr.TupleLiteral [ node; right ])
    , tokens )
  | T.Operator (Other _, value) :: tail ->
    parse_expr tail
    >>= fun (right, tokens) ->
    Ok
      ( Expr.FunctionCall
          { name = Str.from_token value; args = [ node; right ]; op = true }
      , tokens )
  | _ -> Ok (node, tokens)

and parse_accessor tokens =
  let rec loop fields = function
    | T.Operator (T.Dot, _) :: T.Identifier x :: rest ->
      loop (Str.from_token x :: fields) rest
    | rest -> List.rev fields, rest
  in
  loop [] tokens

and parse_record_literal tokens =
  let rec loop fields = function
    | T.Operator (T.RBrace, _) :: rest -> Ok (fields, rest)
    | T.Identifier name :: T.Operator (T.Assign, _) :: rest ->
      parse_expression rest []
      >>= fun (node, tokens) ->
      let new_field = RecordField.{ name = Str.from_token name; value = node } in
      loop (new_field :: fields) tokens
    | x -> Error (UnexpectedToken (x, "Record Literal"))
  in
  loop [] tokens
  >>| fun (fields, remaining) -> Expr.RecordLiteral (List.rev fields), remaining

and parse_array_or_tuple_literal end_token delim tokens exprs =
  let rec loop delim_check exprs tokens =
    match tokens with
    | T.Operator (x, _) :: tail when T.equal_operator x end_token -> Ok (exprs, tail)
    | (T.Operator (x, _) as c) :: tail when T.equal_operator x delim ->
      if delim_check
      then Error (UnexpectedToken (c :: tail, "Unexpected Comma"))
      else loop true exprs tail
    | [] -> Error UnexpectedEOF
    | _ -> parse_expr tokens >>= fun (expr, tail) -> loop delim_check (expr :: exprs) tail
  in
  loop false exprs tokens

and parse_array tokens = parse_array_or_tuple_literal T.RBracket T.Semicolon tokens []

let parse_import tokens =
  let rec parse_fungo_import tokens values =
    match tokens with
    | T.Identifier v :: T.Operator (T.Dot, _) :: rest ->
      parse_fungo_import rest (Str.from_token v :: values)
    | T.Identifier v :: rest -> Str.from_token v :: values, rest
    | _ -> values, tokens
  in
  match tokens with
  | T.StringLiteral t :: rest ->
    Ok (TopLevel.GoImport { module_ = Str.from_token t; alias = None }, rest)
  | T.Identifier t :: T.StringLiteral t2 :: rest ->
    Ok (GoImport { module_ = Str.from_token t2; alias = Some (Str.from_token t) }, rest)
  | T.Identifier _ :: _ ->
    parse_fungo_import tokens []
    |> fun (i, rest) -> Ok (TopLevel.FungoImport { modules = i }, rest)
  | [] -> Error UnexpectedEOF
  | x -> Error (UnexpectedToken (x, "Import"))
;;

let rec parse_top_level = function
  | T.Let _ :: rest ->
    parse_let_expression rest >>| fun (expr, rest) -> TopLevel.LetBind expr, rest
  | T.Module _ :: T.Identifier t :: T.Operator (T.Assign, _) :: rest ->
    parse_module rest []
    >>| fun (nodes, rest) ->
    TopLevel.Module { name = Str.from_token t; body = nodes }, rest
  | T.TypeKeyword _ :: T.Identifier t :: T.Operator (T.Assign, _) :: rest ->
    parse_type_definition rest
    >>| fun (node, rest) -> TopLevel.TypeDefinition (Str.from_token t, node), rest
  | T.Import _ :: rest -> parse_import rest
  | [] -> Error UnexpectedEOF
  | x -> Error (UnexpectedToken (x, "Top Level"))

and parse_module tokens nodes =
  match tokens with
  | T.End _ :: rest -> Ok (nodes, rest)
  | _ -> parse_top_level tokens >>= fun (node, rest) -> parse_module rest (node :: nodes)
;;

let parse name tokens =
  let rec loop tokens items =
    match tokens with
    | [] -> Ok ModuleDefinition.{ name = Str.from_string name; body = List.rev items }
    | _ ->
      parse_top_level tokens
      |> (function
       | Ok (node, rest) -> loop rest (node :: items)
       | Error e -> Error e)
  in
  loop tokens []
;;
