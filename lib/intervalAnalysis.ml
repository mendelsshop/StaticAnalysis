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
  | Identifier i -> VariableMap.find i (fst s)

let eval_simple_bool_expr (e : Cfg.basic_bool_expr) s =
  match e with
  | Boolean b -> Boolean.Boolean b
  | Identifier i -> VariableMap.find i (snd s)

let eval_int_expr (e : Cfg.int_expr) s =
  match e with
  | Cfg.Basic e -> eval_simple_int_expr e s
  | Cfg.UnaryOperator _ -> failwith ""
  | Cfg.BinaryOperator _ -> failwith ""

let eval_bool_expr (e : Cfg.bool_expr) s =
  match e with
  | Cfg.Basic e -> eval_simple_bool_expr e s
  | Cfg.UnaryOperator _ -> failwith ""
  | Cfg.BinaryOperator _ -> failwith ""
  | Cfg.Compare _ -> failwith ""

let transfer (n : node) s =
  match n.command with
  | Cfg.AssignInt { target; value } ->
      (VariableMap.add target (eval_int_expr value s) (fst s), snd s)
  | Cfg.AssignBool { target; value } ->
      (fst s, VariableMap.add target (eval_bool_expr value s) (snd s))
  | Cfg.Cond _ -> failwith ""

let run = (Fun.flip run) transfer
