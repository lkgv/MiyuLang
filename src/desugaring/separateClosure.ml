open Core
open Utils
open Utils.Env
open Utils.Comparison
open Ast.AstTypes
open Parsing.ParsedAst

module VarKey : COMPARE with type t = Var_id.t = struct
  t = Var_id.t

  let compare x y =
    let x_ = Var_id.to_string x in
    let y_ = Var_id.to_string y in
    if x_ < y_ then LT else if x_ = y_ then EQ else GT
end

module AstValue : VALUE with type t = expr = struct
  t = expr
end

module AstEnv : ENV
  with type key_t = VarKey.t
  with type value_t = AstValue.t
  = MakeEnv (Var_id) (AstValue)


let separate_closure prog =
  prog
  |> fun (import_defns, class_defns, function_defns, var_defns) ->
  List.map
    ~f:(separate_class_closure class_defns)
    class_defns
    (import_defns, class_defns, function_defns, var_defns)
  |> separate_in_functions

let desugar_program prog =
  desugar_generics_program prog
  |> fun (Typed_ast.Prog (class_defns, function_defns, main_expr)) ->
  List.map ~f:(desugar_class_defn class_defns function_defns) class_defns
  |> fun desugared_class_defns ->
  List.map ~f:(desugar_function_defn class_defns function_defns) function_defns
  |> fun desugared_function_defns ->
  desugar_block_expr class_defns function_defns [] main_expr
  |> fun desugared_main_expr ->
  let desugared_program =
    Desugared_ast.Prog
      (desugared_class_defns, desugared_function_defns, desugared_main_expr) in
  remove_var_shadowing_program desugared_program
