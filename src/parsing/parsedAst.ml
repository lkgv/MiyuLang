open Utils
open Ast.AstTypes

type program = Prog of module_clause option * expr list

and module_clause = plain_module_clause located

and plain_module_clause = Module of Mod_id.t

and expr = plain_expr located

and plain_expr =
  | Vars       of var list
  (* function name, list of generic type, list of param, return type (optional), expr*)
  | Fn         of Fn_id.t * generic_ty list * param list * ty option * plain_expr
  (* source, method name, list of param ty, param *)
  | MethodCall of expr * Method_id.t * ty list * call_param list
  | Retrive    of expr * Property_id.t * ty list
  | Index      of expr * expr
  (* function name, list of param ty, list of call param *)
  | Call       of Fn_id.t * ty list * call_param list
  (* class name, list of param ty, list of call param *)
  | New        of Ty_id.t * ty list * call_param list
  | Del        of Var_id.t
  | Literal    of literal
  | Array      of expr list
  (* op x*)
  | UnOp       of un_op * expr
  (* op x1 x2 *)
  | BinOp      of bin_op * expr * expr
  | AssignOp   of assign_op * expr * expr
  | Break      of expr
  | Continue   of expr
  | Return     of expr
  | Print      of expr
  | Block      of expr list
  | If         of if_item list
  | Loop       of plain_expr
  | While      of expr * plain_expr
  | For        of Var_id.t * expr * plain_expr
  | This
  (* identity name, list of generic type params *)
  | Id         of Var_id.t * ty list
  (* class name, father name, list of generic type, class body (field defns) *)
  | Class      of Ty_id.t * (Ty_id.t * ty list) option * generic_ty list * field_defn list
  (* trait name, father name, list of generic type, trait body (field defns) *)
  | Trait      of Ty_id.t * (Ty_id.t * ty list) list * generic_ty list * field_defn list
  (* class name, trait name, list of generic type, list of generic type param, impl body
     (field defns) *)
  | Impl       of Ty_id.t * Ty_id.t * generic_ty list * ty list * field_defn list
  | Import     of (Mod_id.t * string) list

and field_defn = plain_field_defn located

and plain_field_defn =
  | Properties of property list
  (* method name, field decorator, list of generic type, list of params, return type
     (optional), body *)
  | Method     of
      Method_id.t
      * field_decorator
      * generic_ty list
      * param list
      * ty option
      * plain_expr

and property = plain_property located

(* property name, field decorator, type (optional), init value (optional) *)
and plain_property =
  | Property of Property_id.t * field_decorator * ty option * expr option

and var = plain_var located

(* var name, var decorator, type (optional), init value (optional) *)
and plain_var = Var of Var_id.t * var_decorator * ty option * expr option

(* param name, annotations, type (optional), default value (optional) *)
and param = Param of Var_id.t * var_decorator * ty option * expr option

(* param name, input value *)
and call_param = CallParam of Var_id.t option * expr

and if_item = Cond of expr * plain_expr | Else of plain_expr
