open Cfg
module VariableMap = Map.Make (Identifier)
module NodeSet = Set.Make (Node)

module Make
    (L : Lattice.JoinSemiLattice)
    (U : sig
      val update : node -> L.t -> L.t -> L.t
    end) =
struct
  type state = L.t NodeReferenceMap.t

  (*TODO: is using set for worklist fine, as that means no duplicates*)
  let run graph f =
    let state =
      graph.nodes
      |> List.map (fun n -> (Node n.id, L.bottom))
      |> NodeReferenceMap.of_list
    in
    let rec fix worklist (state : state) =
      match NodeReferenceSet.elements worklist with
      | [] -> state
      | node :: worklist ->
          let preds = NodeReferenceMap.find node graph.predecesseors in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeReferenceMap.find |> Fun.flip) state)
            |> List.fold_left L.join L.bottom
          in
          let y = f node curent_state in
          let worklist' = worklist |> NodeReferenceSet.of_list in
          if y = curent_state then fix worklist' state
          else
            let succesors = NodeReferenceMap.find node graph.successors in
            fix
              (*TODO: find might fail*)
              (NodeReferenceSet.union
                 (List.map (fun (_, s) -> s) succesors
                 |> NodeReferenceSet.of_list)
                 worklist')
              (NodeReferenceMap.add node
                 (U.update
                    ((List.find (fun _ -> true) graph.nodes)
                       node y curent_state)
                    state))
    in
    fix (graph.nodes |> NodeReferenceSet.of_list) state
end

module Default (L : Lattice.JoinSemiLattice) =
  Make
    (L)
    (struct
      let update _ _ n = n
    end)

module Widen (L : Lattice.WidenNarrowJoinSemiLattice) =
  Make
    (L)
    (struct
      let update _ old news = L.widen old news
    end)
