open Utils

type label = string

val label_to_string : label -> string
val string_to_label : string -> label

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

type var_decorator = {const: bool}
type field_decorator = {static: bool; pub: bool; const: bool}

type program = Prog of module_clause option * import_decl list * expr list

and module_clause = plain_module_clause located

and plain_module_clause = Module of label

and import_decl = plain_import_decl located

(* list of module_name, attribute *)
and plain_import_decl = Import of (label * label) list

and expr = plain_expr located

and plain_expr =
  (* class name, father name, list of generic type, class body (field defns) *)
  | Class      of label * label option * generic_ty list * field_defn list
  (* trait name, list of generic type, trait body (field defns) *)
  | Trait      of label * generic_ty list * field_defn list
  (* class name, trait name, list of generic type, list of param ty, impl body (field
     defns) *)
  | Impl       of label * label * generic_ty list * ty list * field_defn list
  | Vars       of var list
  (* function name, list of generic type, list of param, return type (optional), expr*)
  | Fn         of label * generic_ty list * param list * ty option * plain_expr
  (* source, method name, param *)
  | MethodCall of expr * label * call_param list
  | Retrive    of expr * label
  | Index      of expr * expr
  | Call       of label * call_param list
  (* class name, list of param ty, list of call param *)
  | New        of label * ty list * call_param list
  | Del        of label
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
  | For        of label * expr * plain_expr
  | This
  | Id         of label

and field_defn = plain_field_defn located

and plain_field_defn =
  | Properties of property list
  (* method name, field decorator, list of generic type, list of params, return type
     (optional), body *)
  | Method     of
      label * field_decorator * generic_ty list * param list * ty option * plain_expr

(* property name, field decorator, type (optional), init value (optional) *)
and property = Property of label * field_decorator * ty option * expr option

and var = plain_var located

(* var name, var decorator, type (optional), init value (optional) *)
and plain_var = Var of label * var_decorator * ty option * expr option

(* param name, annotations, type (optional), default value (optional) *)
and param = Param of label * var_decorator * ty option * expr option

(* param name, input value *)
and call_param = CallParam of label option * expr

and if_item = Cond of expr * plain_expr | Else of plain_expr

and ty = plain_ty located

and plain_ty = TyApply of label * ty list

and generic_ty = plain_generic_ty located

and plain_generic_ty = GenTy of label | CstGenTy of label * ty
