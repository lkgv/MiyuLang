open Rbtree

module type VALUE = sig
  type t
end

module type ENV = sig
  type key_t
  type value_t
  type pair = {k: key_t; mutable v: value_t option}

  module Pair : COMPARE with type t = pair
  module Map : SET with type elt = Pair.t

  val empty : Map.t
  val is_empty : Map.t -> bool
  val find : Map.t -> key_t -> value_t
  val add : Map.t -> key_t -> value_t -> Map.t
  val del : Map.t -> key_t -> Map.t
  val cover : Map.t -> key_t -> value_t -> Map.t
  val set : Map.t -> key_t -> value_t -> unit
  val to_list : Map.t -> pair list
  val has_key : Map.t -> key_t -> bool
end

module MakeEnv (Key : COMPARE) (Value : VALUE) :
  ENV with type key_t = Key.t with type value_t = Value.t = struct
  type key_t = Key.t
  type value_t = Value.t
  type pair = {k: key_t; mutable v: value_t option}

  module Pair : COMPARE with type t = pair = struct
    type t = pair

    let compare {k= k1; _} {k= k2; _} = Key.compare k1 k2
  end

  module Map : SET with type elt = Pair.t = MakeSet (Pair)

  let empty = Map.empty
  let is_empty map = Map.is_empty map

  let find map k =
    let {v; _} = Map.find {k; v= None} map in
    match v with Some value -> value | None -> raise Not_found

  let has_key map k =
    try
      ignore (find map k) ;
      true
    with Not_found -> false

  let add map k v = Map.add {k; v= Some v} map
  let del map k = Map.del (Map.find {k; v= None} map) map
  let cover map k v = Map.add {k; v= Some v} (Map.del {k; v= None} map)
  let set map k v = (Map.find {k; v= None} map).v <- Some v
  let to_list map = Map.to_list map
end
