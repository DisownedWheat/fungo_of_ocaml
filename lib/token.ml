open Core

type position = int * int [@@deriving show]

type value = { line : int; column : int; position : position; value : string }
[@@deriving show]

type operator =
  | Dot
  | Comma
  | Semicolon
  | Colon
  | Deref
  | Pointer
  | Assign
  | Pipe
  | LBrace
  | RBrace
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Other of string
[@@deriving show]

type t =
  | Comment of value
  | IntLiteral of value
  | FloatLiteral of value
  | StringLiteral of value
  | CharLiteral of value
  | Let of value
  | Import of value
  | Of of value
  | Go of value
  | If of value
  | Then of value
  | Else of value
  | End of value
  | True of value
  | False of value
  | Match of value
  | FunctionMatch of value
  | With of value
  | When of value
  | TypeKeyword of value
  | Private of value
  | Mut of value
  | Module of value
  | Lambda of value
  | NameSpace of value
  | For of value
  | In of value
  | While of value
  | Do of value
  | Rec of value
  | Operator of (operator * value)
  | Identifier of value
  | ChannelAssign of value
[@@deriving show]

let cmp a b =
  match (a, b) with
  | IntLiteral x, IntLiteral y
  | FloatLiteral x, FloatLiteral y
  | StringLiteral x, StringLiteral y
  | CharLiteral x, CharLiteral y ->
      String.equal x.value y.value
  | Operator (a, _), Operator (b, _) -> phys_equal a b
  | _ -> phys_equal a b

let make ~line ~column ~position ~value ~(kind : value -> t) =
  kind { line; column; position; value }

let cmp_op t t2 =
  match t with Operator (op, _) -> phys_equal op t2 | _ -> false

let cmp_op_str t str =
  match t with
  | Operator (_, token) when String.equal token.value str -> true
  | _ -> false

let str value =
  make ~line:0 ~column:0 ~position:(0, 0) ~value ~kind:(fun s ->
      StringLiteral s)

let int value =
  make ~line:0 ~column:0 ~position:(0, 0) ~value ~kind:(fun s -> IntLiteral s)

let flt value =
  make ~line:0 ~column:0 ~position:(0, 0) ~value ~kind:(fun s -> FloatLiteral s)

let char value =
  make ~line:0 ~column:0 ~position:(0, 0) ~value ~kind:(fun s -> CharLiteral s)

let dummy kind = kind { line = 0; column = 0; position = (0, 0); value = "" }
let print_tokens l = List.iter ~f:print_endline (List.map ~f:show l)
