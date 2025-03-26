module VariableMap = Map.Make (Cfg.Identifier)

module MapLattice (M : Map.S) (L : Lattice.T) = struct
  type t = L.t M.t

  let bottom = M.empty
  let join _x _y = failwith ""
end

module Fixpoint (L : Lattice.T) = struct
  type state = L.t VariableMap.t

  let run _g _e = failwith ""
end
