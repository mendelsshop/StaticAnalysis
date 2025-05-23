module type Set = sig
  type t

  val eq : t -> t -> bool
  val to_string : t -> string
end

module type PartiallyOrderdSet = sig
  include Set

  val leq : t -> t -> bool
end

module type WidenNarrow = sig
  type t

  val widen : t -> t -> t
  val narrow : t -> t -> t
end

module type JoinSemiLattice = sig
  include PartiallyOrderdSet

  val bottom : t
  val join : t -> t -> t
end

module type WidenNarrowJoinSemiLattice = sig
  type t

  include JoinSemiLattice with type t := t
  include WidenNarrow with type t := t
end

module type MeetSemiLattice = sig
  include PartiallyOrderdSet

  val top : t
  val meet : t -> t -> t
end

module type WidenNarrowMeetSemiLattice = sig
  type t

  include MeetSemiLattice with type t := t
  include WidenNarrow with type t := t
end

module type Lattice = sig
  type t

  include JoinSemiLattice with type t := t
  include MeetSemiLattice with type t := t
end

module type WidenNarrowLattice = sig
  type t

  include Lattice with type t := t
  include WidenNarrow with type t := t
end

module Map = struct
  module MapSet (M : MapExt.SExt) (L : Set) = struct
    include M

    type t = L.t M.t

    let eq = M.equal L.eq
    let to_string = M.to_string L.to_string
  end

  module MapPartiallyOrderdSet (M : MapExt.SExt) (L : PartiallyOrderdSet) =
  struct
    include MapSet (M) (L)

    let leq = M.equal L.leq
  end

  module MapJoinSemiLattice (M : MapExt.SExt) (L : JoinSemiLattice) = struct
    let point_wise f =
      M.merge (fun _ x y ->
          let x = Option.value ~default:L.bottom x in
          let y = Option.value ~default:L.bottom y in
          f x y |> Option.some)

    include MapPartiallyOrderdSet (M) (L)

    let bottom = M.empty
    let join = point_wise L.join
  end

  module MapWidenNarrowJoinSemiLattice
      (M : MapExt.SExt)
      (L : WidenNarrowJoinSemiLattice) =
  struct
    include MapJoinSemiLattice (M) (L)

    let widen = point_wise L.widen
    let narrow = point_wise L.narrow
  end

  (*doesn't have top*)

  module MapLattice (M : MapExt.SExt) (L : Lattice) = struct
    include MapJoinSemiLattice (M) (L)

    let meet = point_wise L.meet
  end

  module MapWidenNarrowLattice (M : MapExt.SExt) (L : WidenNarrowLattice) =
  struct
    include MapWidenNarrowJoinSemiLattice (M) (L)
    include MapLattice (M) (L)
  end
end

module Product = struct
  module ProductSet (L : Set) (L1 : Set) = struct
    type t = L.t * L1.t

    let pair_wise f1 f2 (l1, l2) (r1, r2) = (f1 l1 r1, f2 l2 r2)
    let eq (l1, l2) (r1, r2) = L.eq l1 r1 && L1.eq l2 r2
    let to_string (l, r) = L.to_string l ^ " x " ^ L1.to_string r
  end

  module ProductPartiallyOrderdSet
      (L : PartiallyOrderdSet)
      (L1 : PartiallyOrderdSet) =
  struct
    include ProductSet (L) (L1)

    let leq (l1, l2) (r1, r2) = L.leq l1 r1 && L1.leq l2 r2
  end

  module ProductJoinSemiLattice (L : JoinSemiLattice) (L1 : JoinSemiLattice) =
  struct
    include ProductPartiallyOrderdSet (L) (L1)

    let bottom = (L.bottom, L1.bottom)
    let join = pair_wise L.join L1.join
  end

  module ProductMeetSemiLattice (L : MeetSemiLattice) (L1 : MeetSemiLattice) =
  struct
    include ProductPartiallyOrderdSet (L) (L1)

    let bottom = (L.top, L1.top)
    let join = pair_wise L.meet L1.meet
  end

  module ProductWidenNarrowJoinSemiLattice
      (L : WidenNarrowJoinSemiLattice)
      (L1 : WidenNarrowJoinSemiLattice) =
  struct
    include ProductJoinSemiLattice (L) (L1)

    let widen = pair_wise L.widen L1.widen
    let narrow = pair_wise L.widen L1.widen
  end

  module ProductWidenNarrowMeetSemiLattice
      (L : WidenNarrowMeetSemiLattice)
      (L1 : WidenNarrowMeetSemiLattice) =
  struct
    include ProductMeetSemiLattice (L) (L1)

    let widen = pair_wise L.widen L1.widen
    let narrow = pair_wise L.widen L1.widen
  end

  module ProductLattice (L : Lattice) (L1 : Lattice) = struct
    include ProductMeetSemiLattice (L) (L1)
    include ProductJoinSemiLattice (L) (L1)
  end

  module WidenNarrowProductLattice
      (L : WidenNarrowLattice)
      (L1 : WidenNarrowLattice) =
  struct
    include ProductWidenNarrowMeetSemiLattice (L) (L1)
    include ProductWidenNarrowJoinSemiLattice (L) (L1)
  end
end

module Number = struct
  type t = Integer of int | PInfinity | NInfinity

  let leq l r =
    match (l, r) with
    | NInfinity, _ | _, PInfinity -> true
    | Integer l, Integer r -> l <= r
    | _ -> false

  let eq l r =
    match (l, r) with
    | NInfinity, NInfinity | PInfinity, PInfinity -> true
    | Integer l, Integer r -> l = r
    | _ -> false

  let compare x y =
    match (x, y) with
    | PInfinity, PInfinity | NInfinity, NInfinity -> 0
    | PInfinity, _ -> -1
    | _, NInfinity -> -1
    | NInfinity, _ -> 1
    | _, PInfinity -> 1
    | Integer x, Integer y -> compare x y

  let min l r =
    match (l, r) with
    | NInfinity, _ | _, NInfinity -> NInfinity
    | l, PInfinity | PInfinity, l -> l
    | Integer l, Integer r -> Integer (min l r)

  let max l r =
    match (l, r) with
    | NInfinity, l | l, NInfinity -> l
    | _, PInfinity | PInfinity, _ -> PInfinity
    | Integer l, Integer r -> Integer (max l r)

  let to_string = function
    | Integer i -> string_of_int i
    | NInfinity -> "NInfinity"
    | PInfinity -> "PInfinity"
end

module Interval = struct
  type t = Interval of Number.t * Number.t | Bottom

  let bottom = Bottom
  let top = Interval (NInfinity, PInfinity)

  let leq l r =
    match (l, r) with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Interval (l1, l2), Interval (r1, r2) ->
        Number.leq l1 r1 && Number.leq l2 r2

  let eq l r =
    match (l, r) with
    | Bottom, Bottom -> true
    | Interval (l1, l2), Interval (r1, r2) -> Number.eq l1 r1 && Number.eq l2 r2
    | _ -> false

  let join l r =
    match (l, r) with
    | i, Bottom | Bottom, i -> i
    | Interval (l1, l2), Interval (r1, r2) ->
        Interval (Number.min l1 r1, Number.max l2 r2)

  let meet l r =
    match (l, r) with
    | _, Bottom | Bottom, _ -> Bottom
    | Interval (l1, l2), Interval (r1, r2) ->
        let max = Number.max l1 r1 in
        let min = Number.min l2 r2 in
        if Number.leq max min then Interval (max, min) else Bottom

  let widen l r =
    match (l, r) with
    | Bottom, t | t, Bottom -> t
    | Interval (l1, l2), Interval (r1, r2) ->
        let l3 = if Number.compare l1 r1 <= 0 then l1 else NInfinity in
        let r3 = if Number.compare l2 r2 >= 0 then l2 else PInfinity in
        Interval (l3, r3)

  let narrow l r =
    match (l, r) with
    | Bottom, t | t, Bottom -> t
    (* | Bottom, _ | _, Bottom -> Bottom *)
    | Interval (l1, l2), Interval (r1, r2) ->
        let l3 = if l1 = NInfinity then r1 else l1 in
        let r3 = if l2 = PInfinity then r2 else l2 in
        Interval (l3, r3)

  let to_string = function
    | Bottom -> "bot"
    | Interval (x, y) ->
        "(" ^ Number.to_string x ^ ", " ^ Number.to_string y ^ ")"
end

module Boolean = struct
  type t = Boolean of bool | Bottom | Top

  let bottom = Bottom
  let top = Top
  let eq = ( = )

  let leq l r =
    match (l, r) with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Top, _ -> false
    | _, Top -> true
    | Boolean l, Boolean r -> l = r

  let join l r =
    match (l, r) with
    | i, Bottom | Bottom, i -> i
    | Top, _ | _, Top -> Top
    | Boolean l, Boolean r -> if l = r then Boolean l else Top

  let meet l r =
    match (l, r) with
    | _, Bottom | Bottom, _ -> Bottom
    | Top, i | i, Top -> i
    | Boolean l, Boolean r -> if l = r then Boolean l else Bottom

  let widen = join
  let narrow = meet

  let to_string = function
    | Boolean b -> string_of_bool b
    | Bottom -> "bot"
    | Top -> "top"
end

module RelationalBoolean = struct
  module IntervalMap = Map.MapWidenNarrowLattice (Cfg.VariableMap) (Interval)

  module TrueFalse =
    Product.ProductWidenNarrowJoinSemiLattice (IntervalMap) (IntervalMap)

  module Boolean =
    Product.ProductWidenNarrowJoinSemiLattice (Boolean) (TrueFalse)

  include Boolean
end
