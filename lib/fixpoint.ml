open Cfg
module VariableMap = Map.Make (Identifier)
module NodeSet = Set.Make (Node)

module Make
    (L : Lattice.JoinSemiLattice)
    (U : sig
      val update : node -> L.t -> L.t -> L.t
    end) =
struct
  type state = L.t NodeMap.t

  (*TODO: is using set for worklist fine, as that means no duplicates*)
  let run graph f =
    let state =
      graph.nodes |> List.map (fun n -> (n, L.bottom)) |> NodeMap.of_list
    in
    let rec fix worklist (state : state) =
      match NodeSet.elements worklist with
      | [] -> state
      | node :: worklist ->
          let preds =
            NodeReferenceMap.find (Node node.id) graph.predecesseors
          in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeMap.find |> Fun.flip) state)
            |> List.fold_left L.join L.bottom
          in
          let y = f node curent_state in
          let worklist' = worklist |> NodeSet.of_list in
          if y = curent_state then fix worklist' state
          else
            let succesors =
              NodeReferenceMap.find (Node node.id) graph.successors
            in
            fix
              (*TODO: find might fail*)
              (NodeSet.union
                 (List.map (fun (_, s) -> s) succesors |> NodeSet.of_list)
                 worklist')
              (NodeMap.add node (U.update node y curent_state) state)
    in
    fix (graph.nodes |> NodeSet.of_list) state
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
