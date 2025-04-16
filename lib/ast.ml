(*type _ identifier = string*)
(*type math_operator = Add | Sub | Mul | Div | Mod*)
(*type _ comparer =*)
(*  | Greater : int comparer*)
(*  | Less : int comparer*)
(*  | LessOrEqual : int comparer*)
(*  | GreaterOrEqual : int comparer*)
(*  | Equal : 'a comparer*)
(*  | NotEqual : 'a comparer*)
(*type _ expression =*)
(*  | Variable : 'a identifier -> 'a expression*)
(*  | Number : int -> int expression*)
(*  | Boolean : bool -> bool expression*)
(*  | Math : int expression * math_operator * int expression -> int expression*)
(*  | Compare : 'a expression * 'a comparer * 'a expression -> 'a expression*)
(*  | And : bool expression * bool expression -> bool expression*)
(*  | Or : bool expression * bool expression -> bool expression*)
(*  | Not : bool expression -> bool expression*)
(*type statement =*)
(*  | If : bool expression * statement * statement -> statement*)
(*  | While : bool expression * statement -> statement*)
(*  | Assign : 'a identifier * 'a expression -> statement*)
(*  | Done : statement*)
(*  | Sequence : statement * statement -> statement*)
type ty = TNumber | TBoolean
type identifier = string * ty
type math_operator = Add | Sub | Mul | Div | Mod
type unary_operator = Abs | Neg

type comparer =
  | Greater
  | Less
  | LessOrEqual
  | GreaterOrEqual
  | Equal
  | NotEqual

type expression =
  | Variable of string
  | Number of int
  | Boolean of bool
  | Math of expression * math_operator * expression
  | UnaryMath of unary_operator * expression
  | Compare of expression * comparer * expression
  | And of expression * expression
  | Or of expression * expression
  | Not of expression

type statement =
  | If of expression * statement * statement
  | While of expression * statement
  | Assign of identifier * expression
  | Sequence of statement * statement
