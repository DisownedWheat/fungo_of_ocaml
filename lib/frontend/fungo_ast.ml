module ASTString = struct
  type t =
    { value : string
    ; position : Token.position
    }
  [@@deriving show]

  let equal a b = String.equal a.value b.value

  let from_token (token : Token.value) =
    { value = token.value; position = token.position }
  ;;

  let from_string value = { value; position = 0, 0 }
  let dummy = { value = ""; position = 0, 0 }
end

module rec TypeLiteral : sig
  type t =
    | Unit
    | Type of
        { name : ASTString.t
        ; modules : ASTString.t list
        }
    | PointerType of t
    | Slice of t
  [@@deriving show, eq]
end = struct
  type t =
    | Unit
    | Type of
        { name : ASTString.t
        ; modules : ASTString.t list
        }
    | PointerType of t
    | Slice of t
  [@@deriving show, eq]
end

and RecordField : sig
  type t =
    { name : ASTString.t
    ; value : Expr.t
    }
  [@@deriving show, eq]
end = struct
  type t =
    { name : ASTString.t
    ; value : Expr.t
    }
  [@@deriving show, eq]
end

and RecordTypeField : sig
  type t =
    { name : ASTString.t
    ; type_ : TypeLiteral.t
    }
  [@@deriving show, eq]
end = struct
  type t =
    { name : ASTString.t
    ; type_ : TypeLiteral.t
    }
  [@@deriving show, eq]
end

and TypeDef : sig
  type t =
    | Abstract of (ASTString.t * ASTString.t list)
    | TypeLiteral of TypeLiteral.t
    | Variant of (ASTString.t * t option) list
    | RecordDefinition of RecordTypeField.t list
    | TupleDefinition of
        { length : int
        ; types : TypeLiteral.t list
        }
  [@@deriving show, eq]
end = struct
  type t =
    | Abstract of (ASTString.t * ASTString.t list)
    | TypeLiteral of TypeLiteral.t
    | Variant of (ASTString.t * t option) list
    | RecordDefinition of RecordTypeField.t list
    | TupleDefinition of
        { length : int
        ; types : TypeLiteral.t list
        }
  [@@deriving show, eq]
end

and IdentifierType : sig
  type t =
    | Identifier of (ASTString.t * TypeLiteral.t option)
    | Deref of t
    | Pointer of t
    | ArrayDestructure of (ASTString.t list * TypeLiteral.t option)
    | RecordDestructure of (ASTString.t list * TypeLiteral.t option)
    | TupleDestructure of (ASTString.t list * TypeLiteral.t option)
    | Bucket
  [@@deriving show, eq]
end = struct
  type t =
    | Identifier of (ASTString.t * TypeLiteral.t option)
    | Deref of t
    | Pointer of t
    | ArrayDestructure of (ASTString.t list * TypeLiteral.t option)
    | RecordDestructure of (ASTString.t list * TypeLiteral.t option)
    | TupleDestructure of (ASTString.t list * TypeLiteral.t option)
    | Bucket
  [@@deriving show, eq]
end

and LetBinding : sig
  type t =
    { name : IdentifierType.t
    ; recursive : bool
    ; args : IdentifierType.t list
    ; body : Expr.t
    }
  [@@deriving show, eq]
end = struct
  type t =
    { name : IdentifierType.t
    ; recursive : bool
    ; args : IdentifierType.t list
    ; body : Expr.t
    }
  [@@deriving show, eq]
end

and Expr : sig
  type t =
    | VoidExpr of (t * t)
    | Expression of
        { bindings : LetBinding.t list
        ; value : t
        }
    | IdentifierExpr of IdentifierType.t
    | BoolLiteral of bool
    | StringLiteral of ASTString.t
    | IntLiteral of ASTString.t
    | FloatLiteral of ASTString.t
    | IfExpr of
        { condition : t
        ; consequent : t
        ; alternative : t option
        }
    | RecordLiteral of RecordField.t list
    | ArrayLiteral of t list
    | TupleLiteral of t list
    | FunctionCall of
        { name : t
        ; args : t list
        ; op : bool
        }
    | Accessor of
        { base : t
        ; fields : ASTString.t list
        }
    | Index of
        { left : t
        ; right : t
        }
    | Lambda of
        { args : IdentifierType.t list
        ; return_type : TypeLiteral.t option
        ; body : t
        }
    | ForInLoop of
        { condition_arg : IdentifierType.t
        ; condition_expr : t
        ; consequent : t
        }
    | WhileLoop of
        { condition : t
        ; consequent : t
        }
    | UnitExpr of ASTString.t
  [@@deriving show, eq]

  val flatten_void : t -> t list
end = struct
  type t =
    | VoidExpr of (t * t)
    | Expression of
        { bindings : LetBinding.t list
        ; value : t
        }
    | IdentifierExpr of IdentifierType.t
    | BoolLiteral of bool
    | StringLiteral of ASTString.t
    | IntLiteral of ASTString.t
    | FloatLiteral of ASTString.t
    | IfExpr of
        { condition : t
        ; consequent : t
        ; alternative : t option
        }
    | RecordLiteral of RecordField.t list
    | ArrayLiteral of t list
    | TupleLiteral of t list
    | FunctionCall of
        { name : t
        ; args : t list
        ; op : bool
        }
    | Accessor of
        { base : t
        ; fields : ASTString.t list
        }
    | Index of
        { left : t
        ; right : t
        }
    | Lambda of
        { args : IdentifierType.t list
        ; return_type : TypeLiteral.t option
        ; body : t
        }
    | ForInLoop of
        { condition_arg : IdentifierType.t
        ; condition_expr : t
        ; consequent : t
        }
    | WhileLoop of
        { condition : t
        ; consequent : t
        }
    | UnitExpr of ASTString.t
  [@@deriving show, eq]

  let flatten_void e =
    let rec loop exprs = function
      | VoidExpr (left, right) -> left :: loop exprs right
      | e -> e :: exprs
    in
    loop [] e
  ;;
end

and TopLevel : sig
  type t =
    | GoImport of
        { module_ : ASTString.t
        ; alias : ASTString.t option
        }
    | FungoImport of { modules : ASTString.t list }
    | TypeDefinition of ASTString.t * TypeDef.t
    | Module of ModuleDefinition.t
    | LetBind of LetBinding.t
  [@@deriving show, eq]
end = struct
  type t =
    | GoImport of
        { module_ : ASTString.t
        ; alias : ASTString.t option
        }
    | FungoImport of { modules : ASTString.t list }
    | TypeDefinition of ASTString.t * TypeDef.t
    | Module of ModuleDefinition.t
    | LetBind of LetBinding.t
  [@@deriving show, eq]
end

and ModuleDefinition : sig
  type t =
    { name : ASTString.t
    ; body : TopLevel.t list
    }
  [@@deriving show, eq]
end = struct
  type t =
    { name : ASTString.t
    ; body : TopLevel.t list
    }
  [@@deriving show, eq]
end
