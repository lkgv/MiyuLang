type comparison = LT | EQ | GT

module type COMPARE = sig
  type t

  val compare : t -> t -> comparison
end