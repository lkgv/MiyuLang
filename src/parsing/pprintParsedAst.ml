open ParsedAst
open Utils
open Core

let indent_space = "   "

let rec pprint_expr ppf ~indent {it= expr; _} = pprint_plain_expr ppf ~indent expr

and pprint_plain_expr ppf ~indent expr =
  let print_expr x = Fmt.pf ppf "%sExpr: %s@." indent x in
  let new_indent = indent_space ^ indent in
  match expr with
  | Class (name, fname, gen_tys, fields) ->
      pprint_class_defn ppf ~indent (name, fname, gen_tys, fields)
  | Trait (name, gen_tys, fields) -> pprint_trait_defn ppf ~indent (name, gen_tys, fields)
  | Impl (cname, tname, gen_tys, gen_params, fields) ->
      pprint_impl_defn ppf ~indent (cname, tname, gen_tys, gen_params, fields)
  | Vars vs ->
      print_expr "Let" ;
      List.iter ~f:(pprint_var ppf ~indent:new_indent) vs
  | Fn (name, gen_tys, params, ty, body) ->
      pprint_fn_defn ppf ~indent (name, gen_tys, params, ty, body)
  | MethodCall (source, name, params) ->
      print_expr "MethodCall" ;
      Fmt.pf ppf "%sSource:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) source ;
      Fmt.pf ppf "%sMethod name: %s@." new_indent name ;
      Fmt.pf ppf "%sCall Params:@." new_indent ;
      List.iter ~f:(pprint_call_param ppf ~indent:(indent_space ^ new_indent)) params
  | Retrive (source, field) ->
      print_expr "Retrive" ;
      Fmt.pf ppf "%sSource:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) source ;
      Fmt.pf ppf "%sField: %s@." new_indent (label_to_string field)
  | Index (source, id) ->
      print_expr "Index" ;
      Fmt.pf ppf "%sSource:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) source ;
      Fmt.pf ppf "%sId:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) id
  | Call (fn, params) ->
      print_expr "FnCall" ;
      Fmt.pf ppf "%sFn: %s@." new_indent (label_to_string fn) ;
      Fmt.pf ppf "%sCall Params:@." new_indent ;
      List.iter ~f:(pprint_call_param ppf ~indent:(indent_space ^ new_indent)) params
  | New (name, gen_params, params) ->
      pprint_new_expr ppf ~indent (name, gen_params, params)
  | Del name -> print_expr (Fmt.str "Del:%s" (label_to_string name))
  | Literal value -> print_expr (Fmt.str "Literal:%s" (literal_to_string value))
  | Array xs ->
      print_expr "Array" ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) xs
  | UnOp (op, e) ->
      print_expr (Fmt.str "UnOp:%s" (un_op_to_string op)) ;
      pprint_expr ppf ~indent:new_indent e
  | BinOp (op, e1, e2) ->
      print_expr (Fmt.str "BinOp:%s" (bin_op_to_string op)) ;
      Fmt.pf ppf "%se1:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) e1 ;
      Fmt.pf ppf "%se2:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) e2
  | AssignOp (op, e1, e2) ->
      print_expr (Fmt.str "AssignOp:%s" (assign_op_to_string op)) ;
      Fmt.pf ppf "%se1:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) e1 ;
      Fmt.pf ppf "%se2:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) e2
  | Break e ->
      print_expr "Break" ;
      pprint_expr ppf ~indent:new_indent e
  | Continue e ->
      print_expr "Continue" ;
      pprint_expr ppf ~indent:new_indent e
  | Return e ->
      print_expr "Return" ;
      pprint_expr ppf ~indent:new_indent e
  | Print e ->
      print_expr "Print" ;
      pprint_expr ppf ~indent:new_indent e
  | Block es -> pprint_block_expr ppf ~indent es
  | If items -> pprint_if_expr ppf ~indent items
  | Loop e ->
      print_expr "Loop" ;
      pprint_plain_expr ppf ~indent:new_indent e
  | While (cond, expr) ->
      print_expr "While" ;
      Fmt.pf ppf "%sCondition:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) cond ;
      Fmt.pf ppf "%sExpression:@." new_indent ;
      pprint_plain_expr ppf ~indent:(indent_space ^ new_indent) expr
  | For (id, iter, block) ->
      print_expr "For" ;
      Fmt.pf ppf "%sId: %s@." new_indent id ;
      Fmt.pf ppf "%sIterator:@." new_indent ;
      pprint_expr ppf ~indent:(indent_space ^ new_indent) iter ;
      Fmt.pf ppf "%sExpression:@." new_indent ;
      pprint_plain_expr ppf ~indent:(indent_space ^ new_indent) block
  | This -> print_expr "This"
  | Id n -> print_expr (Fmt.str "ID(%s)" (label_to_string n))

and pprint_class_defn ppf ~indent (name, father, gen_tys, fields) =
  let new_indent = indent_space ^ indent in
  let prt_gen_tys lst =
    if List.length lst > 0 then Fmt.pf ppf "%sGeneric Types:@." new_indent ;
    List.iter ~f:(pprint_generic_ty ppf ~indent:(indent_space ^ new_indent)) lst in
  let prt_gen_params lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Type Params:@." new_indent ;
      List.iter ~f:(pprint_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  let prt_inherit_info = function
    | Some (n, lst) ->
        Fmt.pf ppf "%sinherits:%s@." new_indent (label_to_string n) ;
        prt_gen_params lst
    | None          -> () in
  Fmt.pf ppf "%sClass:%s@." indent (label_to_string name) ;
  prt_gen_tys gen_tys ;
  prt_inherit_info father ;
  List.iter ~f:(pprint_field_defn ppf ~indent:new_indent) fields

and pprint_trait_defn ppf ~indent (name, gen_tys, fields) =
  let new_indent = indent_space ^ indent in
  let prt_gen_tys lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Types:@." new_indent ;
      List.iter ~f:(pprint_generic_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  Fmt.pf ppf "%sTrait:%s@." indent (label_to_string name) ;
  prt_gen_tys gen_tys ;
  List.iter ~f:(pprint_field_defn ppf ~indent:new_indent) fields

and pprint_impl_defn ppf ~indent (cname, tname, gen_tys, gen_params, fields) =
  let new_indent = indent_space ^ indent in
  let prt_gen_tys lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Types:@." new_indent ;
      List.iter ~f:(pprint_generic_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  let prt_gen_params lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Type Params:@." new_indent ;
      List.iter ~f:(pprint_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  Fmt.pf ppf "%sImpl Class:%s Trait:%s@." indent (label_to_string cname)
    (label_to_string tname) ;
  prt_gen_tys gen_tys ;
  prt_gen_params gen_params ;
  List.iter ~f:(pprint_field_defn ppf ~indent:new_indent) fields

and pprint_field_defn ppf ~indent {it= field; _} =
  let new_indent = indent_space ^ indent in
  match field with
  | Properties xs ->
      Fmt.pf ppf "%sProperties:@." indent ;
      List.iter ~f:(pprint_property_defn ppf ~indent:new_indent) xs
  | Method (name, decr, gen_tys, params, ty, body) ->
      pprint_method_defn ppf ~indent (name, decr, gen_tys, params, ty, body)

and pprint_property_defn ppf ~indent (Property (name, decr, ty, value)) =
  let new_indent = indent_space ^ indent in
  let prt_ty = function Some t -> pprint_ty ppf ~indent:new_indent t | None -> () in
  let prt_v = function
    | Some v ->
        Fmt.pf ppf "%sDefault Value:@." new_indent ;
        pprint_expr ppf ~indent:new_indent v
    | None   -> () in
  Fmt.pf ppf "%sProperty: %s@." indent (label_to_string name) ;
  pprint_field_decorator ppf ~indent:new_indent decr ;
  prt_ty ty ;
  prt_v value

and pprint_field_decorator ppf ~indent {static; pub; const} =
  let to_s x s = if x then s else "" in
  Fmt.pf ppf "%sDecorator: %s%s%s@." indent (to_s static "static,") (to_s pub "pub,")
    (to_s const "const,")

and pprint_new_expr ppf ~indent (name, gen_params, params) =
  let new_indent = indent_space ^ indent in
  let prt_gen_params lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Type Params:@." new_indent ;
      List.iter ~f:(pprint_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  Fmt.pf ppf "%sNew: %s@." indent (label_to_string name) ;
  prt_gen_params gen_params ;
  Fmt.pf ppf "%sCall Params:@." new_indent ;
  List.iter ~f:(pprint_call_param ppf ~indent:(indent_space ^ new_indent)) params

and pprint_ty ppf ~indent ty =
  let rec ty_to_string ({it= TyApply (tname, tparams); _} : ty) : string =
    let tps = List.map ~f:ty_to_string tparams in
    let tp_str = String.concat ~sep:", " tps in
    let suf = match tp_str with "" -> "" | _ as s -> Fmt.str "<%s>" s in
    label_to_string tname ^ suf in
  Fmt.pf ppf "%sType:%s@." indent (ty_to_string ty)

and pprint_method_defn ppf ~indent (name, decr, gen_tys, params, ret_ty, body) =
  let new_indent = indent_space ^ indent in
  let prt_ret_ty = function
    | Some ty ->
        Fmt.pf ppf "%sReturn type:@." new_indent ;
        pprint_ty ppf ~indent:(indent_space ^ new_indent) ty
    | None    -> () in
  let prt_gen_tys lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Types:@." new_indent ;
      List.iter ~f:(pprint_generic_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  Fmt.pf ppf "%sMethod: %s@." indent (label_to_string name) ;
  prt_ret_ty ret_ty ;
  prt_gen_tys gen_tys ;
  pprint_field_decorator ppf ~indent:new_indent decr ;
  Fmt.pf ppf "%sParams:@." new_indent ;
  List.iter ~f:(pprint_param ppf ~indent:(indent_space ^ new_indent)) params ;
  Fmt.pf ppf "%sBody:@." new_indent ;
  pprint_plain_expr ppf ~indent:(indent_space ^ new_indent) body

and pprint_block_expr ppf ~indent es =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%sBlock:@." indent ;
  List.iter ~f:(pprint_expr ppf ~indent:new_indent) es

and pprint_if_expr ppf ~indent items =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%sIf:@." indent ;
  List.iter ~f:(pprint_if_item ppf ~indent:new_indent) items

and pprint_if_item ppf ~indent item =
  let new_indent = indent_space ^ indent in
  match item with
  | Cond (cond, action) ->
      Fmt.pf ppf "%sCond:@." indent ;
      pprint_expr ppf ~indent:new_indent cond ;
      Fmt.pf ppf "%sThen:@." indent ;
      pprint_plain_expr ppf ~indent:new_indent action
  | Else action         ->
      Fmt.pf ppf "%sElse:@." indent ;
      pprint_plain_expr ppf ~indent:new_indent action

and pprint_param ppf ~indent (Param (name, decr, ty, value)) =
  let new_indent = indent_space ^ indent in
  let prt_ty = function Some t -> pprint_ty ppf ~indent:new_indent t | None -> () in
  let prt_v = function
    | Some v ->
        Fmt.pf ppf "%sDefault Value:@." new_indent ;
        pprint_expr ppf ~indent:(indent_space ^ new_indent) v
    | None   -> () in
  Fmt.pf ppf "%sParam: %s@." indent (label_to_string name) ;
  pprint_var_decorator ppf ~indent:new_indent decr ;
  prt_ty ty ;
  prt_v value

and pprint_var ppf ~indent {it= var; _} = pprint_plain_var ppf ~indent var

and pprint_plain_var ppf ~indent (Var (name, decr, ty, value)) =
  let new_indent = indent_space ^ indent in
  let prt_ty = function Some t -> pprint_ty ppf ~indent:new_indent t | None -> () in
  let prt_v = function
    | Some v ->
        Fmt.pf ppf "%sDefault Value:@." new_indent ;
        pprint_expr ppf ~indent:(indent_space ^ new_indent) v
    | None   -> () in
  Fmt.pf ppf "%sVar: %s@." indent (label_to_string name) ;
  pprint_var_decorator ppf ~indent:new_indent decr ;
  prt_ty ty ;
  prt_v value

and pprint_fn_defn ppf ~indent (name, gen_tys, params, ret_ty, body) =
  let new_indent = indent_space ^ indent in
  let prt_ret_ty = function
    | Some ty ->
        Fmt.pf ppf "%sReturn type:@." new_indent ;
        pprint_ty ppf ~indent:(indent_space ^ new_indent) ty
    | None    -> () in
  let prt_gen_tys lst =
    if List.length lst > 0 then (
      Fmt.pf ppf "%sGeneric Types:@." new_indent ;
      List.iter ~f:(pprint_generic_ty ppf ~indent:(indent_space ^ new_indent)) lst ) in
  Fmt.pf ppf "%sFn: %s@." indent (label_to_string name) ;
  prt_ret_ty ret_ty ;
  prt_gen_tys gen_tys ;
  Fmt.pf ppf "%sParams:@." new_indent ;
  List.iter ~f:(pprint_param ppf ~indent:(indent_space ^ new_indent)) params ;
  Fmt.pf ppf "%sBody:@." new_indent ;
  pprint_plain_expr ppf ~indent:(indent_space ^ new_indent) body

and pprint_call_param ppf ~indent (CallParam (pname, value)) =
  let new_indent = indent_space ^ indent in
  let n = match pname with Some l -> Fmt.str ": %s" (label_to_string l) | None -> "" in
  Fmt.pf ppf "%sCallParam%s@." indent n ;
  pprint_expr ppf ~indent:new_indent value

and pprint_var_decorator ppf ~indent {const} =
  let to_s x s = if x then s else "" in
  Fmt.pf ppf "%sDecorator: %s@." indent (to_s const "const,")

and pprint_generic_ty ppf ~indent {it= gen_ty; _} =
  pprint_plain_generic_ty ppf ~indent gen_ty

and pprint_plain_generic_ty ppf ~indent gen_ty =
  let new_indent = indent_space ^ indent in
  match gen_ty with
  | GenTy t           -> Fmt.pf ppf "%sGeneric type: %s@." indent (label_to_string t)
  | CstGenTy (t, cst) ->
      Fmt.pf ppf "%sGeneric type with constraint: %s@." indent (label_to_string t) ;
      Fmt.pf ppf "%sConstraint:@." new_indent ;
      pprint_ty ppf ~indent:(indent_space ^ new_indent) cst

and pprint_module_clause ppf ~indent {it= Module mname; _} =
  Fmt.pf ppf "%sModule %s@." indent (label_to_string mname)

and pprint_import_decl ppf ~indent {it= Import paths; _} =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%sImport:@." indent ;
  paths
  (* |> List.map ~f:(List.map ~f:label_to_string) |> List.map ~f:(String.concat ~sep:" .
     ") *)
  |> List.iter ~f:(fun (p, x) -> Fmt.pf ppf "%s%s . %s@." new_indent p x)

and pprint_program ppf (Prog (module_clause, import_decls, body)) =
  let indent = "└──" in
  let prt_module_clause = function
    | Some m -> pprint_module_clause ppf ~indent m
    | None   -> () in
  Fmt.pf ppf "Program@." ;
  prt_module_clause module_clause ;
  List.iter ~f:(pprint_import_decl ppf ~indent) import_decls ;
  List.iter ~f:(pprint_expr ppf ~indent) body
