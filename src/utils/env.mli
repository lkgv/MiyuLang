open Comparison
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
  ENV with type key_t = Key.t with type value_t = Value.t 