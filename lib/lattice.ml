module type T = sig
  type t

  val bottom : t
  val top : t
  val leq : t -> t -> bool
  val eq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
end

(*TODO: map lattic doesn't have top, need to make lattice hierarchy*)
module MapLattice (M : Map.S) (L : T) = struct
  type t = L.t M.t

  let point_wise f =
    M.merge (fun _ x y ->
        f (Option.value ~default:L.bottom y) (Option.value ~default:L.bottom x)
        |> Option.some)

  let bottom = M.empty
  let join = point_wise L.join
  let meet = point_wise L.meet
  let leq = M.equal L.leq
  let eq = M.equal L.eq
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
    | Integer l, Integer r -> l <= r
    | _ -> false

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
end
