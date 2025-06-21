open Base
module Ast = Fungo_ast

module Go_types = struct
  type t =
    | Bool
    | String
    | Int
    | Int8
    | Int16
    | Int32
    | Int64
    | U_Int
    | U_Int8
    | U_Int16
    | U_Int32
    | U_Int64
    | Byte
    | Rune
    | Float32
    | Float64
    | Complex64
    | Complex128

  let get_type str =
    match str with
    | "bool" -> Some Bool
    | "string" -> Some String
    | "int" -> Some Int
    | "int8" -> Some Int8
    | "int16" -> Some Int16
    | "int32" -> Some Int32
    | "int64" -> Some Int64
    | "uint" -> Some U_Int
    | "uint8" -> Some U_Int8
    | "uint16" -> Some U_Int16
    | "uint32" -> Some U_Int32
    | "uint64" -> Some U_Int64
    | "byte" -> Some Byte
    | "rune" -> Some Rune
    | "float32" -> Some Float32
    | "float64" -> Some Float64
    | "complex64" -> Some Complex64
    | "complex128" -> Some Complex128
    | _ -> None
  ;;
end

module Module_type_map = struct
  type t = { keys : string list } [@@deriving show]

  let create () = { keys = [] }
end

type base_type =
  | Record
  | Module
[@@deriving show]

type t =
  | Go_type
  | Fg_type of base_type
[@@deriving show]

type 'a module_def =
  { name : Ast.ASTString.t
  ; types : Module_type_map.t
  }
[@@deriving show]
