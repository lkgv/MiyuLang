open Ast.AstTypes
open Parsing
open DesugaredAst

let desugar_program (ParsedAst.Prog (package_clause, package_items)) =
  package_items
  (* |> remove_variable_shadowing *)
  |> PartitionItems.partition_items
  |> SeparateClosure.separate_local_closure
  |> fun (import_defns, class_defns, function_defns, var_defns) ->
  let {it= p; _} = package_clause in
  let pname = match p with ParsedAst.Module n -> n | None -> None in
  Package (pname, import_defns, class_defns, function_defns, var_defns)
