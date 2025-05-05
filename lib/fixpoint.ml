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

  let state graph =
    graph.nodes
    |> List.map (fun n -> (Node n.id, L.bottom))
    |> NodeReferenceMap.of_list

  (*TODO: is using set for worklist fine, as that means no duplicates*)
  let run' graph f state =
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
          "old " ^ string_of_int node.id ^ ": " ^ L.to_string old_state
          |> print_endline;
          "current " ^ string_of_int node.id ^ ": " ^ L.to_string curent_state
          |> print_endline;
          "y " ^ string_of_int node.id ^ ": " ^ L.to_string y |> print_endline;
          "new " ^ string_of_int node.id ^ ": " ^ L.to_string new_state
          |> print_endline;
          print_endline (Cfg.Node.node_to_string node);
          if L.eq new_state old_state then fix worklist' state update_state'
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

  let run g f = run' g f (state g)
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

(* widening, but transfer funtion can return multiple states for different nodes. *)
(* maybe the lattice should be L * Lattice.Map.MapWidenNarrowJoinSemiLattice (NodeReferenceMap)(L) and we do comparison based L, and other operations on everything *)
(*   the idea is that L is catch all output of this state and the map is specific to any succesor/predescor state *)
module Widen'
    (L : Lattice.WidenNarrowJoinSemiLattice)
    (W : sig
      val delay : int
    end) =
struct
  module M = Lattice.Map.MapWidenNarrowJoinSemiLattice (NodeReferenceMap) (L)
  module L' = Lattice.Product.ProductWidenNarrowJoinSemiLattice (L) (M)

  module F =
    Make
      (L')
      (struct
        type t = int NodeReferenceMap.t

        let init = NodeReferenceMap.empty

        let update s n old news =
          if n.loop_head then
            let delayed =
              NodeReferenceMap.find_opt (Node n.id) s |> Option.value ~default:0
            in
            let s' = NodeReferenceMap.add (Node n.id) (delayed + 1) s in
            ((if delayed > W.delay then L'.widen old news else news), s')
          else (news, s)
      end)

  (* this is for foward analysis so we give the transfer function a list of successor *)
  let run' g f s =
    F.run' g
      (fun n s ->
        let inState =
          NodeReferenceMap.find_opt (Node n.id) (snd s)
          |> Option.value ~default:L.bottom
        in
        let successors =
          NodeReferenceMap.find_opt (Node n.id) g.successors
          |> Option.value ~default:[]
        in
        f n inState successors)
      s

  let run g f = run' g f (F.state g)
end

module Narrow'
    (L : Lattice.WidenNarrowJoinSemiLattice)
    (W : sig
      val delay : int
    end) =
struct
  module M = Lattice.Map.MapWidenNarrowJoinSemiLattice (NodeReferenceMap) (L)
  module L' = Lattice.Product.ProductWidenNarrowJoinSemiLattice (L) (M)

  module F =
    Make
      (L')
      (struct
        type t = int NodeReferenceMap.t

        let init = NodeReferenceMap.empty

        let update s n old news =
          if n.loop_head then
            let delayed =
              NodeReferenceMap.find_opt (Node n.id) s |> Option.value ~default:0
            in
            let s' = NodeReferenceMap.add (Node n.id) (delayed + 1) s in
            ((if delayed > W.delay then L'.narrow old news else news), s')
          else (news, s)
      end)

  (* this is for foward analysis so we give the transfer function a list of successor *)
  let run' g f s =
    F.run' g
      (fun n s ->
        let inState =
          NodeReferenceMap.find_opt (Node n.id) (snd s)
          |> Option.value ~default:L.bottom
        in
        let successors =
          NodeReferenceMap.find_opt (Node n.id) g.successors
          |> Option.value ~default:[]
        in
        f n inState successors)
      s

  let run g f = run' g f (F.state g)
end
