type identifier = Identifier of string

module Identifier = struct
  type t = identifier

  let compare (Identifier i) (Identifier j) = String.compare i j
end

(*TODO: maybe you gadts for expr*)
type basic_bool_expr = Boolean of bool | Identifier of identifier
type basic_int_expr = Integer of int | Identifier of identifier
type int_unary_operator = Negate | AbsoluteValue
type bool_unary_operator = Not
type node_reference = Node of int
type int_binary_operator = Add | Subtract | Multiply | Divide | Modulo
type bool_binary_operator = And | Or

type comparison =
  | LessThen
  | GreaterThan
  | LessThenOrEqual
  | GreaterThanOrEqual
  | Equal
  | NotEqual

type bool_expr =
  | Basic of basic_bool_expr
  | UnaryOperator of {
      operator : bool_unary_operator;
      operand : basic_bool_expr;
    }
  | BinaryOperator of {
      left : basic_bool_expr;
      operator : bool_binary_operator;
      right : basic_bool_expr;
    }
  | Compare of {
      left : basic_int_expr;
      operator : comparison;
      right : basic_int_expr;
    }

type int_expr =
  | Basic of basic_int_expr
  | UnaryOperator of { operator : int_unary_operator; operand : basic_int_expr }
  | BinaryOperator of {
      left : basic_int_expr;
      operator : int_binary_operator;
      right : basic_int_expr;
    }

type command =
  | AssignInt of { target : identifier; value : int_expr }
  | AssignBool of { target : identifier; value : bool_expr }
  | Cond of bool_expr

let int_expr_to_string = function
  | Integer n -> string_of_int n
  | Identifier (Identifier i) -> i

let bool_expr_to_string = function
  | Boolean b -> string_of_bool b
  | Identifier (Identifier i) -> i

let bool_expr_to_string = function
  | Compare { left; operator; right } ->
      int_expr_to_string left ^ " "
      ^ (match operator with
        | Equal -> "="
        | NotEqual -> "<>"
        | LessThen -> "<"
        | GreaterThan -> ">"
        | LessThenOrEqual -> "<="
        | GreaterThanOrEqual -> ">=")
      ^ " " ^ int_expr_to_string right
  | Basic b -> bool_expr_to_string b
  | UnaryOperator { operator; operand } ->
      (match operator with Not -> "not ") ^ bool_expr_to_string operand
  | BinaryOperator { left; operator; right } ->
      bool_expr_to_string left ^ " "
      ^ (match operator with And -> " and " | Or -> " or ")
      ^ " " ^ bool_expr_to_string right

let int_expr_to_string = function
  | Basic b -> int_expr_to_string b
  | UnaryOperator { operator; operand } ->
      (match operator with Negate -> "-" | AbsoluteValue -> "+")
      ^ int_expr_to_string operand
  | BinaryOperator { left; operator; right } ->
      int_expr_to_string left ^ " "
      ^ (match operator with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "mod")
      ^ " " ^ int_expr_to_string right

let command_to_string = function
  | AssignInt { target = Identifier i; value } ->
      i ^ " = " ^ int_expr_to_string value
  | Cond b -> bool_expr_to_string b ^ "?"
  | AssignBool { target = Identifier i; value } ->
      i ^ " = " ^ bool_expr_to_string value

type edge = Directed | True | False
type node = { id : int; command : command }

module Node = struct
  type t = node

  let compare { id = id1; _ } { id = id2; _ } = Int.compare id1 id2

  let node_to_string { id; command } =
    string_of_int id ^ ": " ^ command_to_string command
end

module NodeReference = struct
  type t = node_reference

  let compare (Node id1) (Node id2) = Int.compare id1 id2
end

module NodeMap = Map.Make (Node)
module NodeReferenceMap = Map.Make (NodeReference)
module NodeReferenceSet = Set.Make (NodeReference)

type graph = {
  nodes : node list;
  successors : (edge * node_reference) list NodeReferenceMap.t;
  (*backwards edge means its only possible to go backwards if its possible to go fowards*)
  predecesseors : (edge * node_reference) list NodeReferenceMap.t;
}

let add_successor inNode successor { nodes; successors; predecesseors } =
  {
    nodes;
    successors = NodeReferenceMap.add (Node inNode) successor successors;
    predecesseors;
  }

let add_predecesseor inNode predecesseor { nodes; successors; predecesseors } =
  {
    nodes;
    predecesseors =
      NodeReferenceMap.add (Node inNode) predecesseor predecesseors;
    successors;
  }

let add_node n { nodes; successors; predecesseors } =
  { nodes = n :: nodes; successors; predecesseors }

let edges_to_string (Node n, (e : (edge * node_reference) list)) =
  List.map (fun (_, Node n') -> string_of_int n ^ " --> " ^ string_of_int n') e
  |> String.concat "\n"

let cfg_to_string { nodes; successors; predecesseors } =
  (nodes |> List.map Node.node_to_string |> String.concat "\n")
  ^ "\n"
  ^ (successors |> NodeReferenceMap.to_list |> List.map edges_to_string
   |> String.concat "\n")
  ^ "\n"
  ^ (predecesseors |> NodeReferenceMap.to_list |> List.map edges_to_string
   |> String.concat "\n")
