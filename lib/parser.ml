open Core
module Ast = Fungo_ast
module Str = Ast.ASTString
module T = Token

type parser_error =
  | UnexpectedToken of Token.t list
  | UnexpectedEOF
  | UnknownError of Token.t list
  | UnexpectedError
  | InvalidLetBinding of Ast.identifier_type
[@@deriving show]

let print_parser_error e =
  match e with
  | UnexpectedError | UnexpectedEOF -> show_parser_error e |> print_endline
  | UnexpectedToken l ->
      print_endline "UnexpectedToken";
      List.map l ~f:T.show |> List.iter ~f:print_endline;
      ()
  | UnknownError l ->
      print_endline "UnexpectedError";
      List.map l ~f:T.show |> List.iter ~f:print_endline;
      ()
  | InvalidLetBinding x -> Ast.show_identifier_type x |> print_endline

let rec ident_type_match a b =
  match (a, b) with
  | Ast.Identifier _, Ast.Identifier _
  | Ast.ArrayDestructure _, Ast.ArrayDestructure _
  | Ast.RecordDestructure _, Ast.RecordDestructure _
  | Ast.TupleDestructure _, Ast.TupleDestructure _ ->
      true
  | Pointer x, Pointer y | Deref x, Deref y -> ident_type_match x y
  | _ -> false

let rec parse_type_literal tokens =
  match tokens with
  | T.Operator (T.LParen, _) :: T.Operator (T.RParen, _) :: rest ->
      Ok (Ast.Unit, rest)
  | T.Operator (T.LBracket, _) :: T.Operator (T.RBracket, _) :: rest ->
      parse_type_literal rest
      |> Result.map ~f:(fun (x, rest) -> (Ast.Slice x, rest))
  | T.Operator (T.Deref, _) :: rest ->
      parse_type_literal rest
      |> Result.map ~f:(fun (x, rest) -> (Ast.PointerType x, rest))
  | [] -> Error UnexpectedEOF
  | x -> Error (UnexpectedToken x)

let rec parse_record_definition tokens fields =
  let field tokens =
    match tokens with
    | T.Identifier i :: T.Operator (T.Colon, _) :: rest -> (
        match parse_type_literal rest with
        | Ok (t_, rest) -> Ok ({ Ast.name = Str.from_token i; type_ = t_ }, rest)
        | Error e -> Error e)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken x)
  in

  match tokens with
  | T.Operator (T.RBrace, _) :: rest -> Ok (Ast.RecordDefinition fields, rest)
  | _ -> (
      match field tokens with
      | Ok (new_field, rest) -> (
          match rest with
          | T.Operator (T.Semicolon, _) :: rest ->
              parse_record_definition rest (new_field :: fields)
          | T.Operator (T.RBrace, _) :: _ ->
              parse_record_definition rest (new_field :: fields)
          | [] -> Error UnexpectedEOF
          | x -> Error (UnexpectedToken x))
      | Error e -> Error e)

let rec parse_tuple_definition tokens ts =
  match tokens with
  | T.Operator (T.RParen, _) :: rest ->
      Ok (Ast.TupleDefinition { length = List.length ts; types = ts }, rest)
  | x ->
      parse_type_literal x
      |> Result.bind ~f:(fun (t_, rest) ->
             match rest with
             | T.Operator (T.Comma, _) :: rest ->
                 parse_tuple_definition rest (t_ :: ts)
             | T.Operator (T.RParen, _) :: _ ->
                 parse_tuple_definition rest (t_ :: ts)
             | [] -> Error UnexpectedEOF
             | x -> Error (UnexpectedToken x))

let rec parse_variant_definition tokens ts =
  match tokens with
  | T.Operator (T.Pipe, _) :: T.Identifier t :: T.Of _ :: rest ->
      parse_type_definition rest
      |> Result.bind ~f:(fun (node, rest) ->
             let name = Str.from_token t in
             let t = (name, Some node) in
             parse_variant_definition rest (t :: ts))
  | T.Operator (T.Pipe, _) :: T.Identifier t :: rest ->
      let name = Str.from_token t in
      parse_variant_definition rest ((name, None) :: ts)
  | _ -> Ok (Ast.Variant ts, tokens)

and parse_type_definition tokens =
  match tokens with
  | T.Operator (T.LBrace, _) :: rest -> parse_record_definition rest []
  | T.Operator (T.LParen, _) :: rest -> parse_tuple_definition rest []
  | T.Operator (T.Pipe, _) :: _ -> parse_variant_definition tokens []
  | x -> Error (UnknownError x)

let parse_identifier tokens =
  let rec loop tokens =
    match tokens with
    | T.Operator (T.Pointer, _) :: rest ->
        loop rest |> Result.bind ~f:(fun (x, rest) -> Ok (Ast.Pointer x, rest))
    | T.Operator (T.Deref, _) :: rest ->
        loop rest |> Result.bind ~f:(fun (x, rest) -> Ok (Ast.Deref x, rest))
    | T.Identifier t :: rest ->
        Ok (Ast.Identifier (Str.from_token t, None), rest)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken x)
  in
  match tokens with
  | T.Operator (T.LParen, _)
    :: T.Identifier t
    :: T.Operator (T.Colon, _)
    :: rest ->
      parse_type_literal rest
      |> Result.map ~f:(fun (node, rest) ->
             let str = Str.from_token t in
             let ident = Ast.Identifier (str, Some node) in
             (ident, rest))
  | _ -> loop tokens

let parse_destructure tokens typed delimiter end_token =
  let rec loop tokens fields =
    match tokens with
    | T.Operator (x, _) :: rest when phys_equal x end_token -> Ok (fields, rest)
    | T.Identifier t :: T.Operator (d, _) :: rest when phys_equal d delimiter ->
        loop rest (Str.from_token t :: fields)
    | [] -> Error UnexpectedEOF
    | x -> Error (UnexpectedToken x)
  in
  loop tokens []
  |> Result.bind ~f:(fun (fields, rest) ->
         if typed then
           match rest with
           | T.Operator (T.Colon, _) :: rest ->
               parse_type_literal rest
               |> Result.bind ~f:(fun (t_, rest) ->
                      match rest with
                      | T.Operator (T.RParen, _) :: tail ->
                          Ok ((fields, Some t_), tail)
                      | [] -> Error UnexpectedEOF
                      | head -> Error (UnexpectedToken head))
           | [] -> Error UnexpectedEOF
           | T.Operator (T.RParen, _) :: rest -> Ok ((fields, None), rest)
           | x -> Error (UnexpectedToken x)
         else Ok ((fields, None), rest))

let rec parse_let_expression tokens =
  let rec parse_args tokens args =
    (match tokens with
    | T.Operator (T.LParen, _) :: T.Operator (T.LBrace, _) :: rest ->
        parse_destructure rest true T.Semicolon T.RBrace
        |> Result.map ~f:(fun (fields, tail) ->
               (Ast.RecordDestructure fields, tail))
    | T.Operator (T.LBrace, _) :: rest ->
        parse_destructure rest false T.Semicolon T.RBrace
        |> Result.map ~f:(fun (fields, tail) ->
               (Ast.RecordDestructure fields, tail))
    | T.Operator (T.LParen, _) :: T.Operator (T.LBracket, _) :: rest ->
        parse_destructure rest true T.Comma T.RBracket
        |> Result.map ~f:(fun (fields, tail) ->
               (Ast.ArrayDestructure fields, tail))
    | T.Operator (T.LBracket, _) :: rest ->
        parse_destructure rest false T.Comma T.RBracket
        |> Result.map ~f:(fun (fields, tail) ->
               (Ast.ArrayDestructure fields, tail))
    | T.Operator (T.LParen, _) :: T.Operator (T.LParen, _) :: rest ->
        parse_destructure rest true T.Comma T.RParen
        |> Result.map ~f:(fun (fields, tail) ->
               (Ast.TupleDestructure fields, tail))
    | T.Operator (T.LParen, _) :: rest -> (
        match parse_destructure rest false T.Comma T.RParen with
        | Ok (fields, tail) -> Ok (Ast.TupleDestructure fields, tail)
        | Error _ -> parse_identifier tokens)
    | _ :: _ -> parse_identifier tokens
    | [] -> Error UnexpectedEOF)
    |> Result.bind ~f:(fun (node, rest) ->
           match rest with
           | T.Operator (T.Assign, _) :: tail -> Ok (args, tail)
           | _ -> parse_args rest (node :: args))
  in
  let recursive, tail =
    match tokens with T.Rec _ :: tail -> (true, tail) | _ -> (false, tokens)
  in
  parse_args tail []
  |> Result.bind ~f:(fun (idents, tokens) ->
         match (List.length idents, idents) with
         | 1, head :: tail -> Ok (head, tail, tokens)
         | _, head :: tail
           when ident_type_match head (Ast.Identifier (Str.dummy, None)) ->
             Ok (head, tail, tokens)
         | _, x :: _ -> Error (InvalidLetBinding x)
         | _, [] ->
             T.print_tokens tokens;
             Int.to_string (List.length idents) |> print_endline;
             Error UnexpectedError)
  |> Result.bind ~f:(fun (name, args, tokens) ->
         parse_expression tokens []
         |> Result.bind ~f:(fun (body, rest) ->
                Ok ({ Ast.name; recursive; args; body }, rest)))

and parse_expression tokens bindings =
  match tokens with
  | T.Let _ :: tail ->
      parse_let_expression tail
      |> Result.bind ~f:(fun (binding, tokens) ->
             match tokens with
             | T.In _ :: tail | T.Operator (T.Semicolon, _) :: tail ->
                 parse_expression tail (binding :: bindings)
             | [] -> Error UnexpectedEOF
             | x -> Error (UnexpectedToken x))
  | _ ->
      parse_expr tokens
      |> Result.bind ~f:(fun (expr, rest) ->
             Ok ({ bindings; Ast.value = expr }, rest))

and parse_expr tokens = Ok (Ast.BoolLiteral true, List.tl_exn tokens)

let parse_import tokens =
  let rec parse_fungo_import tokens values =
    match tokens with
    | T.Identifier v :: T.Operator (T.Dot, _) :: rest ->
        parse_fungo_import rest (Str.from_token v :: values)
    | T.Identifier v :: rest -> (Str.from_token v :: values, rest)
    | _ -> (values, tokens)
  in

  match tokens with
  | T.StringLiteral t :: rest ->
      Ok (Ast.GoImport { module_ = Str.from_token t; alias = None }, rest)
  | T.Identifier t :: T.StringLiteral t2 :: rest ->
      Ok
        ( Ast.GoImport
            { module_ = Str.from_token t2; alias = Some (Str.from_token t) },
          rest )
  | T.Identifier _ :: _ ->
      parse_fungo_import tokens [] |> fun (i, rest) ->
      Ok (Ast.FungoImport { modules = i }, rest)
  | [] -> Error UnexpectedEOF
  | x -> Error (UnexpectedToken x)

let rec parse_top_level = function
  | T.Let x :: rest ->
      parse_expression (T.Let x :: rest) []
      |> Result.map ~f:(fun (expr, rest) -> (Ast.Expr expr, rest))
  | T.Module _ :: T.Identifier t :: T.Operator (T.Assign, _) :: rest ->
      parse_module rest []
      |> Result.map ~f:(fun (nodes, rest) ->
             (Ast.Module { Ast.name = Str.from_token t; body = nodes }, rest))
  | T.TypeKeyword _ :: T.Identifier t :: T.Operator (T.Assign, _) :: rest ->
      parse_type_definition rest
      |> Result.map ~f:(fun (node, rest) ->
             (Ast.TypeDefinition (Str.from_token t, node), rest))
  | T.Import _ :: rest -> parse_import rest
  | [] -> Error UnexpectedEOF
  | x -> Error (UnexpectedToken x)

and parse_module tokens nodes =
  match tokens with
  | T.End _ :: rest -> Ok (nodes, rest)
  | _ ->
      parse_top_level tokens
      |> Result.bind ~f:(fun (node, rest) -> parse_module rest (node :: nodes))

let parse name tokens =
  let rec loop tokens items =
    match tokens with
    | [] -> Ok (Ast.Module { name = Str.from_string name; body = items })
    | _ -> (
        parse_top_level tokens |> function
        | Ok (node, rest) -> loop rest (node :: items)
        | Error e -> Error e)
  in
  loop tokens []
