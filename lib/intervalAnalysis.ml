open Fixpoint
open Cfg
open Lattice
module IntervalMap = Map.MapWidenNarrowLattice (VariableMap) (Interval)
module BooleanMap = Map.MapWidenNarrowLattice (VariableMap) (Boolean)
module D = Product.ProductWidenNarrowJoinSemiLattice (IntervalMap) (BooleanMap)
open Widen (D)

let eval_simple_int_expr (e : Cfg.basic_int_expr) s =
  match e with
  | Integer n -> Interval.Interval (Integer n, Integer n)
  | Identifier i ->
      VariableMap.find_opt i (fst s) |> Option.value ~default:Interval.bottom

let eval_simple_bool_expr (e : Cfg.basic_bool_expr) s =
  match e with
  | Boolean b -> Boolean.Boolean b
  | Identifier i ->
      VariableMap.find_opt i (snd s) |> Option.value ~default:Boolean.bottom

let abs = function
  | Interval.Bottom -> Interval.Bottom
  | Interval (x, y) ->
      let abs = function
        | Number.NInfinity -> (true, Number.PInfinity)
        | Number.Integer n when n < 0 -> (true, Number.Integer (-n))
        | n -> (false, n)
      in
      let x_neg, x' = abs x in
      let y_neg, y' = abs y in
      (*if both negative then when you flip signs [-11, -5] to [11, 5] so you have to swicth order to [5, 11]*)
      if x_neg && y_neg then Interval (y', x')
        (*if we cross sign bondries like [-7. 5] then we automatically have to start at 0 and go to at least the y, or the -x if its bigger
        if not we would get something like: [5, 7] or [7, 5] in which case we would be making our abstraction invalid*)
      else if x_neg then Interval (Number.Integer 0, Number.max x' y')
      (*otherwise we have to positive integer so nothing changed*)
        else Interval (x', y')

let negate = function
  | Interval.Bottom -> Interval.Bottom
  | Interval (x, y) ->
      let negate = function
        | Number.NInfinity -> Number.PInfinity
        | Number.Integer n -> Number.Integer (-n)
        | Number.PInfinity -> Number.NInfinity
      in
      let x' = negate x in
      let y' = negate y in
      (*TODO: maybe just y' x'*)
      Interval (Number.min x' y', Number.max x' y')

let do_binary_int_op f x y =
  match (x, y) with
  | _, Interval.Bottom | Interval.Bottom, _ -> Interval.Bottom
  (*we do not implicitly wrap call to f in interval because it might fail (become bottom)*)
  | Interval.Interval (x1, x2), Interval.Interval (y1, y2) -> f (x1, x2) (y1, y2)

let add =
  let add x y =
    match (x, y) with
    | _, Number.NInfinity | Number.NInfinity, _ -> Number.NInfinity
    | _, Number.PInfinity | Number.PInfinity, _ -> Number.PInfinity
    | Number.Integer x, Number.Integer y -> Number.Integer (x + y)
  in
  do_binary_int_op (fun (x1, x2) (y1, y2) ->
      Interval.Interval (add x1 y1, add x2 y2))

let sub =
  let sub x y =
    match (x, y) with
    | _, Number.NInfinity | Number.NInfinity, _ -> Number.NInfinity
    | _, Number.PInfinity | Number.PInfinity, _ -> Number.PInfinity
    | Number.Integer x, Number.Integer y -> Number.Integer (x - y)
  in
  do_binary_int_op (fun (x1, x2) (y1, y2) ->
      Interval.Interval (sub x1 y1, sub x2 y2))

let is_pos = function
  | Number.PInfinity -> true
  | Number.NInfinity -> false
  | Integer n -> n >= 0

let mul =
  let max = List.fold_left Number.max Number.NInfinity in
  let min = List.fold_left Number.min Number.PInfinity in
  let mul x y =
    match (x, y) with
    | e, Number.NInfinity | Number.NInfinity, e ->
        if is_pos e then Number.NInfinity else Number.PInfinity
    | e, Number.PInfinity | Number.PInfinity, e ->
        if is_pos e then Number.PInfinity else Number.NInfinity
    | Number.Integer x, Number.Integer y -> Number.Integer (x * y)
  in
  do_binary_int_op (fun (x1, x2) (y1, y2) ->
      let x1y1 = mul x1 y1 in
      let x1y2 = mul x1 y2 in
      let x2y1 = mul x2 y1 in
      let x2y2 = mul x2 y2 in
      Interval.Interval
        (min [ x1y1; x1y2; x2y1; x2y2 ], max [ x1y1; x1y2; x2y1; x2y2 ]))

let rec div x y =
  let max = List.fold_left Number.max Number.NInfinity in
  let min = List.fold_left Number.min Number.PInfinity in
  let div' x y =
    match (x, y) with
    | e, Number.NInfinity | Number.NInfinity, e ->
        if is_pos e then Number.NInfinity else Number.PInfinity
    | e, Number.PInfinity | Number.PInfinity, e ->
        if is_pos e then Number.PInfinity else Number.NInfinity
    | Number.Integer x, Number.Integer y -> Number.Integer (x / y)
  in
  do_binary_int_op
    (fun (x1, x2) (y1, y2) ->
      let x1divy1 = div' x1 y1 in
      let x1divy2 = div' x1 y2 in
      let x2divy1 = div' x2 y1 in
      let x2divy2 = div' x2 y2 in
      if Number.leq (Integer 1) y1 then
        Interval.Interval (min [ x1divy1; x1divy2 ], max [ x2divy1; x2divy2 ])
      else if Number.leq y2 (Integer (-1)) then
        Interval.Interval (min [ x1divy1; x1divy2 ], max [ x2divy1; x2divy2 ])
      else
        Interval.join
          (div x
             (Interval.meet y (Interval.Interval (Integer 1, Number.PInfinity))))
          (div x
             (Interval.meet y
                (Interval.Interval (Number.NInfinity, Integer (-1))))))
    x y

let eval_int_expr (e : Cfg.int_expr) s =
  match e with
  | Cfg.Basic e -> eval_simple_int_expr e s
  | Cfg.UnaryOperator { operator; operand } -> (
      let operand' = eval_simple_int_expr operand s in
      match operator with
      | Negate -> negate operand'
      | AbsoluteValue -> abs operand')
  | Cfg.BinaryOperator { left; operator; right } -> (
      let left' = eval_simple_int_expr left s in
      let right' = eval_simple_int_expr right s in
      match operator with
      | Add -> add left' right'
      | Subtract -> sub left' right'
      | Multiply -> mul left' right'
      | Divide -> div left' right'
      | Modulo -> failwith "mod")

let eval_bool_expr (e : Cfg.bool_expr) s =
  match e with
  | Cfg.Basic e -> eval_simple_bool_expr e s
  | Cfg.UnaryOperator { operator = Not; operand } -> (
      let operand' = eval_simple_bool_expr operand s in
      match operand' with Boolean b -> Boolean (not b) | _ -> operand')
  | Cfg.BinaryOperator { left; operator; right } -> (
      let _left' = eval_simple_bool_expr left in
      let _right' = eval_simple_bool_expr right in
      match operator with And -> failwith "and" | Or -> failwith "or")
  | Cfg.Compare { left; operator; right } -> (
      let _left' = eval_simple_int_expr left in
      let _right' = eval_simple_int_expr right in
      match operator with
      | LessThen -> failwith "<"
      | GreaterThan -> failwith ">"
      | LessThenOrEqual -> failwith "<="
      | GreaterThanOrEqual -> failwith ">="
      | Equal -> failwith "=="
      | NotEqual -> failwith "!=")

let transfer (n : node) s =
  match n.command with
  | Cfg.AssignInt { target; value } ->
      (VariableMap.add target (eval_int_expr value s) (fst s), snd s)
  | Cfg.AssignBool { target; value } ->
      (fst s, VariableMap.add target (eval_bool_expr value s) (snd s))
  | Cfg.Cond _ -> failwith "cond"

let run = (Fun.flip run) transfer
