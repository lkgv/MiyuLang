open Comparison

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

module MakeSet (CompArg : COMPARE) : SET with type elt = CompArg.t = struct
  module Comp = CompArg

  type elt = Comp.t
  type color = R | B | BB
  type t = E | EE | T of color * t * elt * t

  let empty = E
  let is_empty = function E | EE -> true | _ -> false

  let rec min = function
    | E | EE -> raise Not_found
    | T (_, E, x, _) | T (_, EE, x, _) -> x
    | T (_, left, _, _) -> min left

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
     |B, T (R, a, x, T (R, b, y, c)), z, d
     |B, a, x, T (R, T (R, b, y, c), z, d)
     |B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | BB, a, x, T (R, T (R, b, y, c), z, d) | BB, T (R, a, x, T (R, b, y, c)), z, d ->
        T (B, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)

  let add x s =
    let rec ins = function
      | E | EE             -> T (R, E, x, E)
      | T (color, a, y, b) -> (
        match Comp.compare x y with
        | LT -> balance (color, ins a, y, b)
        | EQ -> T (color, a, y, b)
        | GT -> balance (color, a, y, ins b) ) in
    let blacken = function
      | T (R, T (R, a, x, b), y, c) -> T (B, T (R, a, x, b), y, c)
      | T (R, a, x, T (R, b, y, c)) -> T (B, a, x, T (R, b, y, c))
      | tree                        -> tree in
    s |> ins |> blacken

  let rotate = function
    | R, T (BB, a, x, b), y, T (B, c, z, d) ->
        balance (B, T (R, T (B, a, x, b), y, c), z, d)
    | R, EE, y, T (B, c, z, d) -> balance (B, T (R, E, y, c), z, d)
    | R, T (B, a, x, b), y, T (BB, c, z, d) ->
        balance (B, a, x, T (R, b, y, T (B, c, z, d)))
    | R, T (B, a, x, b), y, EE -> balance (B, a, x, T (R, b, y, E))
    | B, T (BB, a, x, b), y, T (B, c, z, d) ->
        balance (BB, T (R, T (B, a, x, b), y, c), z, d)
    | B, EE, y, T (B, c, z, d) -> balance (BB, T (R, E, y, c), z, d)
    | B, T (B, a, x, b), y, T (BB, c, z, d) ->
        balance (BB, a, x, T (R, b, y, T (B, c, z, d)))
    | B, T (B, a, x, b), y, EE -> balance (BB, a, x, T (R, b, y, E))
    | B, T (BB, a, w, b), x, T (R, T (B, c, y, d), z, e) ->
        T (B, balance (B, T (R, T (B, a, w, b), x, c), y, d), z, e)
    | B, EE, x, T (R, T (B, c, y, d), z, e) ->
        T (B, balance (B, T (R, E, x, c), y, d), z, e)
    | B, T (R, a, w, T (B, b, x, c)), y, T (BB, d, z, e) ->
        T (B, a, w, balance (B, b, x, T (R, c, y, T (B, d, z, e))))
    | B, T (R, a, w, T (B, b, x, c)), y, EE ->
        T (B, a, w, balance (B, b, x, T (R, c, y, E)))
    | color, a, x, b -> T (color, a, x, b)

  let rec min_del = function
    | T (R, E, x, E)              -> (x, E)
    | T (B, E, x, E)              -> (x, EE)
    | T (B, E, x, T (R, E, y, E)) -> (x, T (B, E, y, E))
    | T (c, a, x, b)              ->
        let x_, a_ = min_del a in
        (x_, rotate (c, a_, x, b))
    | _                           -> raise (Invalid_argument "min_del")

  let del x s =
    let rec delete t =
      match t with
      | E                           -> E
      | EE                          -> EE
      | T (R, E, y, E)              -> ( match Comp.compare x y with
                                         | EQ -> E
                                         | _  -> T (R, E, y, E) )
      | T (B, E, y, E)              -> ( match Comp.compare x y with
                                         | EQ -> EE
                                         | _  -> T (B, E, y, E) )
      | T (B, T (R, E, y, E), z, E) -> (
        match Comp.compare x z with
        | LT -> T (B, delete (T (R, E, y, E)), z, E)
        | EQ -> T (B, E, y, E)
        | GT -> T (B, T (R, E, y, E), z, E) )
      | T (c, a, y, b)              -> (
        match Comp.compare x y with
        | LT -> rotate (c, delete a, y, b)
        | EQ ->
            let y_, b_ = min_del b in
            rotate (c, a, y_, b_)
        | GT -> rotate (c, a, y, delete b) ) in
    let redden = function
      | T (B, T (B, a, x, b), y, T (B, c, z, d)) ->
          T (R, T (B, a, x, b), y, T (B, c, z, d))
      | t -> t in
    s |> redden |> delete

  let rec find x = function
    | E | EE                -> raise Not_found
    | T (_, left, y, right) -> (
      match Comp.compare x y with LT -> find x left | GT -> find x right | EQ -> y )

  let mem x s =
    try
      ignore (find x s) ;
      true
    with Not_found -> false

  let to_list s =
    let rec trav l = function
    | E | EE                -> l
    | T (_, left, x, right) -> trav (x :: trav l right) left in
    trav [] s

  let rec compare_lists l1 l2 =
    match (l1, l2) with
    | [], []             -> EQ
    | [], _ :: _         -> LT
    | _ :: _, []         -> GT
    | x1 :: t1, x2 :: t2 -> (
      match Comp.compare x1 x2 with EQ -> compare_lists t1 t2 | (LT | GT) as cmp -> cmp )

  let compare s1 s2 = compare_lists (to_list s1) (to_list s2)
end

module Int : COMPARE with type t = int = struct
  type t = int

  let compare x y = if x < y then LT else if x == y then EQ else GT
end

module IntSet : SET with type elt = Int.t = MakeSet (Int)

let () =
  Printf.printf "Hello!\n" ;
  let k = ref IntSet.empty in
  let plist t =
    let lst = IntSet.to_list t in
    List.iter (fun x -> Printf.printf "%d " x) lst ;
    Printf.printf "\n" in
  for i = 10 downto 1 do
    Printf.printf "[Insert %d]\n\t" i ;
    k := IntSet.add i !k ;
    plist !k ;
    Printf.printf "after: %d.\n"
      (let x = IntSet.min !k in
       x)
  done ;
  plist !k ;
  for i = 1 to 10 do
    Printf.printf "[Delete %d]\n\t" i ;
    k := IntSet.del i !k ;
    plist !k ;
    match IntSet.is_empty !k with
    | true  -> Printf.printf "[Del %d] after: empty\n" i
    | false ->
        Printf.printf "[Del %d] after: %d\n" i
          (let x = IntSet.min !k in
           x)
  done
