exception ContextError of string

type ('key, 'value) ctx = ('key, 'value) Hashtbl.t
type ('key, 'value) env = ('key, 'value) ctx list

val create_env : unit -> ('key, 'value) env
val create_ctx : ('key, 'value) env -> ('key, 'value) env
val destroy_ctx : ('key, 'value) env -> ('key, 'value) env
val add : ('key, 'value) env -> 'key -> 'value -> ('key, 'value) env
val find_opt : ('key, 'value) env -> 'key -> 'value option
val find : ('key, 'value) env -> 'key -> 'value
