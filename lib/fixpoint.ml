open Cfg
module VariableMap = MapExt.MakeExt (Identifier)
module NodeSet = Set.Make (Node)

module Make
    (L : Lattice.JoinSemiLattice)
    (U : sig
      type t

      val update : t -> node -> L.t -> L.t -> L.t * t
      val init : t
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
    let rec fix worklist (state : state) update_state =
      match NodeReferenceSet.elements worklist with
      | [] -> state
      | (Node node as node_ref) :: worklist ->
          let node =
            List.find
              (fun { id; command = _; loop_head = _ } -> id = node)
              graph.nodes
          in
          let preds =
            NodeReferenceMap.find_opt node_ref graph.predecesseors
            |> Option.value ~default:[]
          in
          let old_state =
            NodeReferenceMap.find_opt node_ref state
            |> Option.value ~default:L.bottom
          in
          let curent_state =
            preds |> List.map snd
            |> List.map ((NodeReferenceMap.find |> Fun.flip) state)
            |> List.fold_left L.join L.bottom
          in
          let y = f node curent_state in
          let worklist' = worklist |> NodeReferenceSet.of_list in
          let new_state, update_state' =
            U.update update_state node old_state y
          in
          let state' = NodeReferenceMap.add node_ref new_state state in
          string_of_int node.id ^ ": " ^ L.to_string old_state |> print_endline;
          string_of_int node.id ^ ": " ^ L.to_string new_state |> print_endline;

          print_endline (Cfg.Node.node_to_string node);
          if L.leq new_state old_state then fix worklist' state update_state'
          else
            let succesors =
              NodeReferenceMap.find_opt node_ref graph.successors
              |> Option.value ~default:[]
            in
            fix
              (*TODO: find might fail*)
              (NodeReferenceSet.union worklist'
                 (List.map (fun (_, s) -> s) succesors
                 |> NodeReferenceSet.of_list))
              state' update_state'
    in
    fix
      (graph.nodes |> List.map (fun n -> Node n.id) |> NodeReferenceSet.of_list)
      state U.init
end

module Default (L : Lattice.JoinSemiLattice) =
  Make
    (L)
    (struct
      type t = unit

      let init = ()
      let update () _ _ n = (n, ())
    end)

module Widen
    (L : Lattice.WidenNarrowJoinSemiLattice)
    (W : sig
      val delay : int
    end) =
  Make
    (L)
    (struct
      type t = int NodeReferenceMap.t

      let init = NodeReferenceMap.empty

      let update s n old news =
        if n.loop_head then
          let delayed =
            NodeReferenceMap.find_opt (Node n.id) s |> Option.value ~default:0
          in
          let s' = NodeReferenceMap.add (Node n.id) (delayed + 1) s in
          ((if delayed > W.delay then L.widen old news else news), s')
        else (news, s)
    end)
