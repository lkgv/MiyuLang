open Ast.AstTypes
open DesugaredAst

let desugar_program (ParsedAst.Prog (package_clause, package_items)) =
  package_items
  |> partition_items
  |> separate_local_functions
  |> remove_variable_shadowing
  |> fun (import_defns, class_defns, function_defns, var_defns) ->
    let package_clause = {it=p; _ in
    let pname = match p with
      | ParsedAst.Module n -> n
      | None -> None in
    Package (pname, import_defns, class_defns, function_defns, var_defns)
