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
  | _ -> failwith ""

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
