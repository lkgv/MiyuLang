type comparison = LT | EQ | GT

module type COMPARE = sig
  type t

  val compare : t -> t -> comparison
end

module type SET = sig
  type t
  type elt

  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val del : elt -> t -> t
  val mem : elt -> t -> bool
  val min : t -> elt
  val find : elt -> t -> elt
  val compare : t -> t -> comparison
  val to_list : t -> elt list
end

module MakeSet (CompArg : COMPARE) : SET with type elt = CompArg.t
