open Core
open ParsedAst
let partition_items items =
  let import_defns = List.filter ~f:() items in
  let class_defns = List.filter ~f:() items in
  let function_defns = List.filter ~f:() items in
  let var_defns = List.filter ~f:() items in
  (import_defns, class_defns, function_defns, var_defns)