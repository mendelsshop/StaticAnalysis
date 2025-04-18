open Cfg
module VariableMap = MapExt.MakeExt (Identifier)
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
      | (Node node as node_ref) :: worklist ->
          let node =
            List.find (fun { id; command = _ } -> id = node) graph.nodes
          in
          let preds =
            NodeReferenceMap.find_opt node_ref graph.predecesseors
            |> Option.value ~default:[]
          in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeReferenceMap.find |> Fun.flip) state)
            |> List.fold_left L.join L.bottom
          in
          let y = f node curent_state in
          let worklist' = worklist |> NodeReferenceSet.of_list in
          if y = curent_state then fix worklist' state
          else
            let succesors =
              NodeReferenceMap.find_opt node_ref graph.successors
              |> Option.value ~default:[]
            in
            fix
              (*TODO: find might fail*)
              (NodeReferenceSet.union
                 (List.map (fun (_, s) -> s) succesors
                 |> NodeReferenceSet.of_list)
                 worklist')
              (NodeReferenceMap.add node_ref
                 (U.update node curent_state y)
                 state)
    in
    fix
      (graph.nodes |> List.map (fun n -> Node n.id) |> NodeReferenceSet.of_list)
      state
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
