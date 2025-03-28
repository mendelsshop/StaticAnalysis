open Cfg
module VariableMap = Map.Make (Identifier)

module MapLattice (M : Map.S) (L : Lattice.T) = struct
  type t = L.t M.t

  let bottom = M.empty
  let join _x _y = failwith ""
end

module NodeSet = Set.Make (Node)

module Fixpoint (L : Lattice.T) = struct
  type node_state = L.t VariableMap.t
  type state = node_state NodeMap.t

  (*TODO: is using set for worklist fine, as that means no duplicates*)
  let run graph f =
    let state =
      graph.nodes |> List.map (fun n -> (n, L.bottom)) |> NodeMap.of_list
    in
    let rec fix worklist state =
      match NodeSet.elements worklist with
      | [] -> state
      | node :: worklist ->
          let preds = NodeMap.find node graph.predecesseors in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeMap.find |> Fun.flip) state)
            |> List.fold_left L.join L.bottom
          in
          let y = f node curent_state in
          let worklist' = worklist |> NodeSet.of_list in
          if y = curent_state then fix worklist' state
          else
            let succesors = NodeMap.find node graph.successors in
            fix
              (*TODO: find might fail*)
              (NodeSet.union
                 (List.map (fun (_, s) -> s) succesors |> NodeSet.of_list)
                 worklist')
              (NodeMap.add node curent_state state)
    in
    fix (graph.nodes |> NodeSet.of_list) state
end
