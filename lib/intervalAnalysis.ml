open Fixpoint
open Cfg
open Lattice
module IntervalMap = MapLattice (VariableMap) (Interval)
open Make (IntervalMap)

let eval_expr (e : Cfg.basic_expr) s =
  match e with
  | Integer n -> Interval.Interval (Integer n, Integer n)
  | Boolean _ -> failwith ""
  | Identifier i -> VariableMap.find i s

let eval_expr (e : Cfg.expr) s =
  match e with
  | Cfg.Basic e -> eval_expr e s
  | Cfg.UnaryOperator _ -> failwith ""
  | Cfg.BinaryOperator _ -> failwith ""

let transfer (n : node) s =
  match n.command with
  | Cfg.Assign { target; value } -> VariableMap.add target (eval_expr value s) s
  | Cfg.Cond _ -> failwith ""

let run = (Fun.flip run) transfer
