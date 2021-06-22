exception ContextError of string

type ('key, 'value) ctx = ('key, 'value) Hashtbl.t
type ('key, 'value) env = ('key, 'value) ctx list

let rec create_env () = create_ctx []

and create_ctx env =
  let ctx = Hashtbl.create 44 in
  ctx :: env

and destroy_ctx env =
  match env with
  | x :: xs -> Hashtbl.reset x ; xs
  | []      -> raise (ContextError "you are destroying context in an empty environment!")

and add env k v =
  match env with
  | x :: _ -> Hashtbl.add x k v ; env
  | []     -> raise (ContextError "you are adding element to an empty environment!")

and find_opt env k =
  match env with
  | x :: xs -> (
    match Hashtbl.find_opt x k with Some v -> Some v | None -> find_opt xs k )
  | []      -> None

and find env k =
  match env with
  | x :: xs -> ( match Hashtbl.find_opt x k with Some v -> v | None -> find xs k )
  | []      -> raise (ContextError "key not found in environment")
