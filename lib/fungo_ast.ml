module ASTString = struct
  type t = { value : string; position : Token.position } [@@deriving show]

  let from_token token =
    { value = token.Token.value; position = token.position }

  let from_string value = { value; position = (0, 0) }
  let dummy = { value = ""; position = (0, 0) }
end

type type_literal =
  | Unit
  | Type of { name : ASTString.t; module_ : ASTString.t option }
  | PointerType of type_literal
  | Slice of type_literal
[@@deriving show]

and record_field = { name : ASTString.t; value : expression } [@@deriving show]

and record_type_field = { name : ASTString.t; type_ : type_literal }
[@@deriving show]

and type_def =
  | TypeLiteral of type_literal
  | Variant of (ASTString.t * type_def option) list
  | RecordDefinition of record_type_field list
  | TupleDefinition of { length : int; types : type_literal list }
[@@deriving show]

and identifier_type =
  | Identifier of (ASTString.t * type_literal option)
  | Deref of identifier_type
  | Pointer of identifier_type
  | ArrayDestructure of (ASTString.t list * type_literal option)
  | RecordDestructure of (ASTString.t list * type_literal option)
  | TupleDestructure of (ASTString.t list * type_literal option)
  | Bucket
[@@deriving show]

and let_binding = {
  name : identifier_type;
  recursive : bool;
  args : identifier_type list;
  body : expression;
}
[@@deriving show]

and expression = { bindings : let_binding list; value : expr } [@@deriving show]

and expr =
  | IdentifierExpr of identifier_type
  | BoolLiteral of bool
  | StringLiteral of ASTString.t
  | IntLiteral of ASTString.t
  | FloatLiteral of ASTString.t
  | IfExpr of {
      condition : expr;
      consequent : expression;
      alternative : expression option;
    }
  | RecordLiteral of record_field list
  | ArrayLiteral of expression list
  | TupleLiteral of expression list
  | FunctionCall of { name : ASTString.t; args : expression list; op : bool }
  | Accessor of { left : expression; right : expression }
  | Lambda of {
      args : identifier_type list;
      return_type : type_literal option;
      body : expression;
    }
  | ForInLoop of {
      condition_arg : identifier_type;
      condition_expr : expr;
      consequent : expression;
    }
  | WhileLoop of { condition : expr; consequent : expression }
  | UnitExpr
[@@deriving show]

and module_definition = { name : ASTString.t; body : top_level list }
[@@deriving show]

and top_level =
  | GoImport of { module_ : ASTString.t; alias : ASTString.t option }
  | FungoImport of { modules : ASTString.t list }
  | TypeDefinition of ASTString.t * type_def
  | Module of module_definition
  | Expr of expression
[@@deriving show]
