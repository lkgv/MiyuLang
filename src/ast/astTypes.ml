open Base
open Utils

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
end

module Var_id : ID = String_id
module Ty_id : ID = String_id
module Property_id : ID = String_id
module Method_id : ID = String_id
module Fn_id : ID = String_id
module Gen_id : ID = String_id
module Mod_id : ID = String_id

type var_decorator = {const: bool}

let string_of_var_decorator {const} =
  let dec_lst = [] in
  let dec_lst = if const then "const" :: dec_lst else dec_lst in
  String.concat ~sep:", " dec_lst

type field_decorator = {static: bool; pub: bool; const: bool}

let string_of_field_decorator {static; pub; const} =
  let dec_lst = [] in
  let dec_lst = if const then "const" :: dec_lst else dec_lst in
  let dec_lst = if pub then "pub" :: dec_lst else dec_lst in
  let dec_lst = if static then "static" :: dec_lst else dec_lst in
  String.concat ~sep:", " dec_lst

type un_op = UnOpNot | UnOpNeg

let un_op_to_string = function UnOpNot -> "!" | UnOpNeg -> "-"

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

let bin_op_to_string = function
  | BinOpPlus    -> "+"
  | BinOpMinus   -> "-"
  | BinOpMult    -> "*"
  | BinOpDiv     -> "/"
  | BinOpMod     -> "%"
  | BinOpLarger  -> ">"
  | BinOpLeq     -> ">="
  | BinOpSmaller -> "<"
  | BinOpSeq     -> "<="
  | BinOpAnd     -> "&&"
  | BinOpOr      -> "||"
  | BinOpEq      -> "=="
  | BinOpNeq     -> "!="

type assign_op = MulAssign | DivAssign | ModAssign | PlusAssign | MinusAssign | Assign

let assign_op_to_string = function
  | MulAssign   -> "*="
  | DivAssign   -> "/="
  | ModAssign   -> "%="
  | PlusAssign  -> "+="
  | MinusAssign -> "-="
  | Assign      -> "="

type literal =
  | IntVal   of int
  | StrVal   of string
  | BoolVal  of bool
  | FloatVal of float
  | Null

let literal_to_string = function
  | IntVal x   -> Fmt.str "INT(%d)" x
  | StrVal x   -> Fmt.str "STRING(%s)" x
  | BoolVal x  -> Fmt.str "BOOL(%b)" x
  | FloatVal x -> Fmt.str "FLOAT(%f)" x
  | Null       -> "NULL"

type ty = plain_ty located

and plain_ty =
  | TyApply of Ty_id.t * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyGen   of Gen_id.t  (** ['a] *)

let rec ty_to_string ty =
  match ty.it with
  | TyApply (t, lst) ->
      if List.length lst = 0 then Ty_id.to_string t
      else
        Fmt.str "%s<%s>" (Ty_id.to_string t)
          (String.concat ~sep:", " (List.map ~f:ty_to_string lst))
  | TyGen t          -> Gen_id.to_string t

type generic_ty = plain_generic_ty located

and plain_generic_ty = GenTy of Gen_id.t | CstGenTy of Gen_id.t * ty

let generic_ty_to_string gen_ty =
  match gen_ty.it with
  | GenTy t           -> Gen_id.to_string t
  | CstGenTy (t, cst) -> Fmt.str "%s: %s" (Gen_id.to_string t) (ty_to_string cst)
