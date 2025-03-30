open Cfg
module VariableMap = Map.Make (Identifier)
module NodeSet = Set.Make (Node)

module Make (L : Lattice.T) = struct
  (*TODO: 
     we should have to explictly use a map lattice here it should be up the functor caller to dso 
    but, map lattice doesn't have top, need to make lattice hierarchy
  *)
  module State = Lattice.MapLattice (VariableMap) (L)

  type state = State.t NodeMap.t

  (*TODO: is using set for worklist fine, as that means no duplicates*)
  let run graph f =
    let state =
      graph.nodes |> List.map (fun n -> (n, State.bottom)) |> NodeMap.of_list
    in
    let rec fix worklist (state : state) =
      match NodeSet.elements worklist with
      | [] -> state
      | node :: worklist ->
          let preds = NodeMap.find node graph.predecesseors in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeMap.find |> Fun.flip) state)
            |> List.fold_left State.join State.bottom
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
