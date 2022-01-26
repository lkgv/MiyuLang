open Core
open Utils
open Parsing.ParsedAst

let partition_items items =
  let rec accu_items items =
    match items with
    | ({it= x; _} as item) :: xs -> (
        let import_defns, class_defns, function_defns, var_defns = accu_items xs in
        match x with
        | Import _                   -> ( item :: import_defns
                                        , class_defns
                                        , function_defns
                                        , var_defns )
        | Class _ | Trait _ | Impl _ ->
            (import_defns, item :: class_defns, function_defns, var_defns)
        | Fn _                       -> ( import_defns
                                        , class_defns
                                        , item :: function_defns
                                        , var_defns )
        | Vars _                     -> ( import_defns
                                        , class_defns
                                        , function_defns
                                        , item :: var_defns )
        | _                          -> ( import_defns
                                        , class_defns
                                        , function_defns
                                        , var_defns ) )
    | []                         -> ([], [], [], []) in
  accu_items items
