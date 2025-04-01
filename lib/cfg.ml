type identifier = Identifier of string

module Identifier = struct
  type t = identifier

  let compare (Identifier i) (Identifier j) = String.compare i j
end

(*TODO: maybe you gadts for expr*)
type basic_bool_expr = Boolean of bool | Identifier of identifier
type basic_int_expr = Integer of int | Identifier of identifier
type int_unary_operator = Negate | AbsoluteValue
type bool_unary_operator = No
type node_reference = Node of int
type int_binary_operator = Add | Subtract | Multiply | Divide
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
