open Ast

type value = VNumber of int | VBoolean of bool

let vboolean v = VBoolean v

module Env = Hashtbl.Make (String)

let to_boolean = function VBoolean v -> Some v | _ -> None
let to_integer = function VNumber v -> Some v | _ -> None
let ( let* ) = Option.bind
let ( let+ ) o f = Option.map f o

let operation (op : comparer) =
  let compare_int op l r =
    let* l' = to_integer l in
    let+ r' = to_integer r in
    VBoolean (op l' r')
  in

  let compare_all op l r = Some (VBoolean (op l r)) in
  match op with
  | Greater -> compare_int ( > )
  | Less -> compare_int ( < )
  | LessOrEqual -> compare_int ( <= )
  | GreaterOrEqual -> compare_int ( >= )
  | Equal -> compare_all ( = )
  | NotEqual -> compare_all ( <> )

let math (op : math_operator) =
  match op with
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )

let rec eval_expression (e : expression) env =
  match e with
  | Variable v -> Env.find_opt env v
  | Number v -> Some (VNumber v)
  | Boolean v -> Some (VBoolean v)
  | Math (l, o, r) ->
      let* l' = eval_expression l env in
      let* l' = to_integer l' in
      let* r' = eval_expression r env in
      let+ r' = to_integer r' in
      VNumber ((math o) l' r')
  | Compare (l, o, r) ->
      let* l' = eval_expression l env in
      let* r' = eval_expression r env in
      operation o l' r'
  | And (l, r) ->
      let* l' = eval_expression l env in
      let* l'' = to_boolean l' in
      if not l'' then Some (VBoolean false)
      else
        let* r' = eval_expression r env in
        let+ r'' = to_boolean r' in
        VBoolean r''
  | Or (l, r) ->
      let* l' = eval_expression l env in
      let* l'' = to_boolean l' in
      if l'' then Some (VBoolean true)
      else
        let* r' = eval_expression r env in
        let+ r'' = to_boolean r' in
        VBoolean r''
  | Not v ->
      let* v' = eval_expression v env in
      let+ b = to_boolean v' in
      VBoolean (not b)

let rec eval (s : statement) env =
  match s with
  | If (c, cons, alt) ->
      let* c' = eval_expression c env in
      let* c'' = to_boolean c' in
      eval (if c'' then cons else alt) env
  | While (c, cons) ->
      let* c' = eval_expression c env in
      let* c'' = to_boolean c' in
      if c'' then
        let* _ = eval cons env in
        eval s env
      else Option.some ()
  | Assign (i, v) ->
      let+ v' = eval_expression v env in
      Env.add env i v'
  | Done _ -> Some ()
  | Sequence (s1, s2) ->
      let* _ = eval s1 env in
      eval s2 env
