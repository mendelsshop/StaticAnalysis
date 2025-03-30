type identifier = Identifier of string

module Identifier = struct
  type t = identifier

  let compare (Identifier i) (Identifier j) = String.compare i j
end

type basic_expr = Integer of int | Boolean of bool | Identifier of identifier
type unary_operator = Not | Negate | AbsoluteValue
type node_reference = Node of int

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  | LessThen
  | GreaterThan
  | LessThenOrEqual
  | GreaterThanOrEqual
  | Equal
  | NotEqual

type expr =
  | Basic of basic_expr
  | UnaryOperator of { operator : unary_operator; operand : basic_expr }
  | BinaryOperator of {
      left : basic_expr;
      operator : binary_operator;
      right : basic_expr;
    }

type command = Assign of { target : identifier; value : expr } | Cond of expr
type edge = Directed | True | False
type node = { id : int; command : command }

module Node = struct
  type t = node

  let compare { id = id1; _ } { id = id2; _ } = Int.compare id1 id2
end

module NodeMap = Map.Make (Node)

type graph = {
  nodes : node list;
  successors : (edge * node) list NodeMap.t;
  predecesseors : (edge * node) list NodeMap.t;
}
