open Base
open Utils

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id : ID

(*= struct

  type t = string

  let of_string x = x let to_string x = x let ( = ) = String.( = ) end *)

module Var_id : ID
module Ty_id : ID
module Property_id : ID
module Method_id : ID
module Fn_id : ID
module Gen_id : ID
module Mod_id : ID

type var_decorator = {const: bool}

val string_of_var_decorator : var_decorator -> string

type field_decorator = {static: bool; pub: bool; const: bool}

val string_of_field_decorator : field_decorator -> string

type un_op = UnOpNot | UnOpNeg

val un_op_to_string : un_op -> string

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpMod
  | BinOpLarger
  | BinOpLeq
  | BinOpSmaller
  | BinOpSeq
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNeq

val bin_op_to_string : bin_op -> string

type assign_op = MulAssign | DivAssign | ModAssign | PlusAssign | MinusAssign | Assign

val assign_op_to_string : assign_op -> string

type literal =
  | IntVal   of int
  | StrVal   of string
  | BoolVal  of bool
  | FloatVal of float
  | Null

val literal_to_string : literal -> string

type ty = plain_ty located

and plain_ty =
  | TyApply of Ty_id.t * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyGen   of Gen_id.t  (** ['a] *)

val ty_to_string : ty -> string

type generic_ty = plain_generic_ty located

and plain_generic_ty = GenTy of Gen_id.t | CstGenTy of Gen_id.t * ty

val generic_ty_to_string : generic_ty -> string
