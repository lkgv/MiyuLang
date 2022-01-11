open Utils
open Ast.AstTypes

type program = package list

and package =
  | Package    of Mod_id.t option * import_defn list * class_defn list * function_defn list * var list

and import_defn = plain_import_defn located

and plain_import_defn = Import of (Mod_id.t * string)

and class_defn = plain_class_defn located

and plain_class_defn =
  (* class name, father name, list of generic type, properties, methods *)
  | Class      of Ty_id.t * (Ty_id.t * ty list) option * generic_ty list * property_defn list * method_defn list
  (* trait name, father name, list of generic type, properties, methods *)
  | Trait      of Ty_id.t * (Ty_id.t * ty list) list * generic_ty list * property_defn list * method_defn list
  (* class name, trait name, list of generic type, list of generic type param, properties, methods *)
  | Impl       of Ty_id.t * Ty_id.t * generic_ty list * ty list * property_defn list * method_defn list

and function_defn = plain_function_defn located

and plain_function_defn =
  (* function name, list of generic type, list of param, return type (optional), expr*)
  | Fn         of Fn_id.t * generic_ty list * param list * ty option * block_expr

and expr = plain_expr located

and plain_expr =
  | Let        of var
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
  | If         of if_item list
  | Loop       of block_expr
  | While      of expr * block_expr
  | For        of Var_id.t * expr * block_expr
  | This
  (* identity name, list of generic type params *)
  | Id         of Var_id.t * ty list

and block_expr = plain_block_expr located

and plain_block_expr =
  | Block      of expr list

and property_defn = plain_property_defn located

and plain_property_defn =
  (* property name, field decorator, type (optional), init value (optional) *)
  | Property   of Property_id.t * field_decorator * ty option * expr option

and method_defn = plain_method_defn located

and plain_method_defn = 
  (* method name, field decorator, list of generic type, list of params, return type
     (optional), body *)
  | Method     of
      Method_id.t
      * field_decorator
      * generic_ty list
      * param list
      * ty option
      * block_expr

and var = plain_var located

(* var name, var decorator, type (optional), init value (optional) *)
and plain_var = Var of Var_id.t * var_decorator * ty option * expr option

(* param name, annotations, type (optional), default value (optional) *)
and param = Param of Var_id.t * var_decorator * ty option * expr option

(* param name, input value *)
and call_param = CallParam of Var_id.t option * expr

and if_item = Cond of expr * block_expr | Else of block_expr
