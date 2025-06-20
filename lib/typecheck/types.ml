open Base
module Ast = Fungo_ast

type go_builtin =
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

type t =
  | Record
  | Module

type base_type =
  | Go_type
  | Fg_type of t

type 'a module_def =
  { name : Ast.ASTString.t
  ; types : 'a list
  }
[@@deriving show]
