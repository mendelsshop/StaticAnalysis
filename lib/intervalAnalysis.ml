open Fixpoint
open Cfg
open Lattice

module IntervalMap =
  Map.MapWidenNarrowJoinMeetSemiLattice (VariableMap) (Interval)

module BooleanMap =
  Map.MapWidenNarrowJoinMeetSemiLattice (VariableMap) (ComplexBoolean)

module D =
  Product.ProductWidenNarrowJoinMeetSemiLattice (IntervalMap) (BooleanMap)

module F =
  Widen'
    (D)
    (struct
      let delay = 70
    end)

module F' =
  Narrow'
    (D)
    (struct
      let delay = 70
    end)

(* open F *)
(* TODO: what is top for complex boolean *)

let eval_simple_int_expr (e : Cfg.basic_int_expr) s =
  match e with
  | Integer n -> Interval.Interval (Integer n, Integer n)
  | Identifier i ->
      VariableMap.find_opt i (fst s) |> Option.value ~default:Interval.bottom

let eval_simple_bool_expr (e : Cfg.basic_bool_expr) s =
  match e with
  | Boolean b -> (Boolean.Boolean b, (VariableMap.empty, VariableMap.empty))
  | Identifier i ->
      VariableMap.find_opt i (snd s)
      |> Option.value ~default:ComplexBoolean.bottom

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

let cmp is_true is_false x y t f =
  match (x, y) with
  | Interval.Bottom, _ | _, Interval.Bottom -> ComplexBoolean.bottom
  | Interval (x1, x2), Interval (y1, y2) ->
      if is_true (x1, x2) (y1, y2) then (Boolean.Boolean true, (t, f))
      else if is_false (x1, x2) (y1, y2) then (Boolean.Boolean false, (t, f))
      else (Boolean.Top, (t, f))

let filter_ident = function Cfg.Identifier i -> Some i | _ -> None

let bottomize = function
  | Interval.Interval (a, b) when Number.compare a b > 0 -> Interval.Bottom
  | i -> i

let bind_outside _a _b _c _d =
  bottomize
  @@ Interval.Interval
       ( (if _c <= _a && _d >= _a then _c else _a),
         if _c <= _b && _d >= _b then _d else _b )

let eval_bool_expr (e : Cfg.bool_expr) s =
  match e with
  | Cfg.Basic e -> eval_simple_bool_expr e s
  | Cfg.UnaryOperator { operator = Not; operand } -> (
      let operand' = eval_simple_bool_expr operand s in
      match operand' with
      | Boolean b, (t, f) -> (Boolean (not b), (f, t))
      (* even if the boolean value is unknown we can still flip the filtering values *)
      | Top, (t, f) -> (Boolean.Top, (f, t))
      | Bottom, _ -> ComplexBoolean.bottom)
  | Cfg.BinaryOperator { left; operator; right } -> (
      let left' = eval_simple_bool_expr left s in
      let right' = eval_simple_bool_expr right s in
      match operator with
      | And when fst left' = Boolean.Bottom || fst right' == Boolean.Bottom ->
          ComplexBoolean.bottom
      (* meet is correct b/c we already eliminated bottom *)
      | And -> ComplexBoolean.meet left' right'
      | Or -> ComplexBoolean.join left' right')
  (* | Cfg.Compare { left = Identifier i; operator = LessThen; right = Integer n } *)
  (*   -> ( *)
  (*     let left_v = eval_simple_int_expr (Identifier i) s in *)
  (*     let n = Number.Integer n in *)
  (*     match left_v with *)
  (*     | Interval (a, b) when Number.compare a n < 0 -> *)
  (*         ( Boolean.Top, *)
  (*           ( VariableMap.singleton i (Interval.Interval (a, Number.min b n)), *)
  (*             VariableMap.empty ) ) *)
  (*     | _ -> ComplexBoolean.bottom) *)
  (* | Cfg.Compare *)
  (*     { left = Identifier i; operator = LessThen; right = Identifier i' } -> ( *)
  (*     let left_v = eval_simple_int_expr (Identifier i) s in *)
  (*     let right_v = eval_simple_int_expr (Identifier i') s in *)
  (*     match (left_v, right_v) with *)
  (*     | Interval (a, b), Interval (c, d) when Number.compare a d < 0 -> *)
  (*         ( Boolean.Top, *)
  (*           ( VariableMap.of_list *)
  (*               [ *)
  (*                 (i, Interval.Interval (a, Number.min b d)); *)
  (*                 (i', Interval.Interval (Number.max a c, d)); *)
  (*               ], *)
  (*             VariableMap.empty ) ) *)
  (*     | _ -> ComplexBoolean.bottom) *)
  | Cfg.Compare { left; operator; right } -> (
      let uncurry f (x, y) = f x y in
      let try_add o =
        Option.map (uncurry VariableMap.add) o |> Option.value ~default:Fun.id
      in
      let filter f = function Some x when f x -> Some x | _ -> None in
      let left' = eval_simple_int_expr left s in
      let right' = eval_simple_int_expr right s in
      let const4 v _ _ _ _ = v in
      let pair_rev y x = (x, y) in
      let second_is f (_, y) = f y in
      let not_bottom = function Interval.Bottom -> false | _ -> true in
      let t_and_f ?(t = const4 false) ?(t_l = const4 Interval.Bottom)
          ?(t_r = const4 Interval.Bottom) ?(f = const4 false)
          ?(f_l = const4 Interval.Bottom) ?(f_r = const4 Interval.Bottom) () =
        let left_ident = filter_ident left in
        let right_ident = filter_ident right in
        let map a b c d l r =
          VariableMap.empty
          |> try_add
               (left_ident
               |> Option.map (pair_rev (l a b c d))
               |> filter (second_is not_bottom)
               )
          |> try_add
               (right_ident
               |> Option.map (pair_rev (r a b c d))
               |> filter (second_is not_bottom))
        in
        let true_map a b c d = map a b c d t_l t_r in
        let false_map a b c d = map a b c d f_l f_r in

        match (left', right') with
        | Interval (a, b), Interval (c, d) when t a b c d && f a b c d ->
            (true_map a b c d, false_map a b c d)
        | Interval (a, b), Interval (c, d) when t a b c d ->
            (true_map a b c d, VariableMap.empty)
        | Interval (a, b), Interval (c, d) when f a b c d ->
            (VariableMap.empty, false_map a b c d)
        | _ -> (VariableMap.empty, VariableMap.empty)
      in

      match operator with
      (* for false branch it should just be the true branch of the inverse operation 
         tested with > *)
      | LessThen ->
          uncurry
            (cmp
               (fun (_x1, x2) (y1, _y2) -> Number.compare x2 y1 < 0)
               (fun (x1, _x2) (_y1, y2) -> Number.compare y2 x1 <= 0)
               left' right')
            (t_and_f
             (*  on the number line we have: ...[a, b]...[c, d]... *)
             (* or really [a ... {c ... b ] ... d} *)
               ~t:(fun a _b _c d -> Number.compare a d < 0)
                 (*  [a ... [c ... b ] ... d] *)
                 (* [a, min(b d)] *)
               ~t_l:(fun a b _c d ->
                 Interval.Interval (a, Number.min b (Number.sub1 d)))
                 (*  [a ... [c ... b ] ... d] *)
                 (* [max(a , c) , d)*)
                 (* i.e.:  when this is true our rhs is greater than our begin of our lhs *)
               ~t_r:(fun a _b c d ->
                 Interval.Interval (Number.max (Number.add1 a) c, d))
                 (* on the number line we have: ...[c, d]...[a, b]... *)
                 (* or really [c ... {a ... d ] ... b} *)
               ~f:(fun _a b c _d -> Number.compare c b <= 0)
                 (*  really [c ... [a ... d ] ... b] *)
               ~f_l:(fun a b c _d -> Interval.Interval (Number.max a c, b))
               ~f_r:(fun _a b c d -> Interval.Interval (c, Number.min d b))
               ())
      | GreaterThan ->
          uncurry
            (cmp
               (fun (x1, _x2) (_y1, y2) -> Number.compare y2 x1 < 0)
               (fun (_x1, x2) (y1, _y2) -> Number.compare x2 y1 <= 0)
               left' right')
            (t_and_f
             (* on the number line we have: ...[c, d]...[a, b]... *)
             (* or really [c ... {a ... d ] ... b} *)
               ~t:(fun _a b c _d -> Number.compare c b < 0)
                 (*  really [c ... [a ... d ] ... b] *)
               ~t_l:(fun a b c _d ->
                 Interval.Interval (Number.max a (Number.add1 c), b))
               ~t_r:(fun _a b c d ->
                 Interval.Interval (c, Number.min d (Number.sub1 b)))
                 (* [a ... {c ... b ] ... d} *)
               ~f:(fun a _b _c d -> Number.compare a d <= 0)
               ~f_l:(fun a b _c d -> Interval.Interval (a, Number.min b d))
               ~f_r:(fun a _b c d -> Interval.Interval (Number.max a c, d))
               ())
      | LessThenOrEqual ->
          uncurry
            (cmp
               (fun (_x1, x2) (y1, _y2) -> Number.compare x2 y1 <= 0)
               (fun (x1, _x2) (_y1, y2) -> Number.compare y2 x1 < 0)
               left' right')
            (t_and_f
               ~t:(fun a _b _c d -> Number.compare a d <= 0)
               ~t_l:(fun a b _c d -> Interval.Interval (a, Number.min b d))
               ~t_r:(fun a _b c d -> Interval.Interval (Number.max a c, d))
                 (* on the number line we have: ...[c, d]...[a, b]... *)
                 (* or really [c ... {a ... d ] ... b} *)
               ~f:(fun _a b c _d -> Number.compare c b < 0)
                 (*  really [c ... [a ... d ] ... b] *)
               ~f_l:(fun a b c _d ->
                 Interval.Interval (Number.max a (Number.add1 c), b))
               ~f_r:(fun _a b c d ->
                 Interval.Interval (c, Number.min d (Number.sub1 b)))
               ())
      | GreaterThanOrEqual ->
          uncurry
            (cmp
               (fun (x1, _x2) (_y1, y2) -> Number.compare y2 x1 <= 0)
               (fun (_x1, x2) (y1, _y2) -> Number.compare x2 y1 < 0)
               left' right')
            (t_and_f
             (* on the number line we have: ...[c, d]...[a, b]... *)
             (* or really [c ... {a ... d ] ... b} *)
               ~t:(fun _a b c _d -> Number.compare c b <= 0)
                 (*  really [c ... [a ... d ] ... b] *)
               ~t_l:(fun a b c _d -> Interval.Interval (Number.max a c, b))
               ~t_r:(fun _a b c d -> Interval.Interval (c, Number.min d b))
                 (*  on the number line we have: ...[a, b]...[c, d]... *)
                 (* or really [a ... {c ... b ] ... d} *)
               ~f:(fun a _b _c d -> Number.compare a d < 0)
                 (*  [a ... [c ... b ] ... d] *)
                 (* [a, min(b d)] *)
               ~f_l:(fun a b _c d ->
                 Interval.Interval (a, Number.min b (Number.sub1 d)))
                 (*  [a ... [c ... b ] ... d] *)
                 (* [max(a , c) , d)*)
                 (* i.e.:  when this is true our rhs is greater than our begin of our lhs *)
               ~f_r:(fun a _b c d ->
                 Interval.Interval (Number.max (Number.add1 a) c, d))
               ())
      | Equal ->
          let meet = Interval.meet left' right' in
          uncurry
            (cmp
               (* probably never 100% true unless two ranges are equal? *)
               (fun (x1, x2) (y1, y2) -> x1 = y1 && x2 = y2)
               (fun (_x1, _x2) (_y1, _y2) -> meet = Bottom)
               left' right')
            (t_and_f
             (* on the number line we have: ...[c, d]...[a, b]... *)
             (* or really [c ... {a ... d ] ... b} *)
             (* or the number line we have: ...[a, b]...[c, d]... *)
             (* or really [a ... {c ... b ] ... d} *)
               ~t:(fun _a _b _c _d -> true)
               ~t_l:(fun _a _b _c _d -> meet)
               ~t_r:(fun _a _b _c _d -> meet)
               ~f:(fun _a _b _c _d -> true)
               ~f_l:bind_outside
               ~f_r:(fun _a _b _c _d -> bind_outside _c _d _a _b)
               ())
      | NotEqual ->
          let meet = Interval.meet left' right' in
          uncurry
            (cmp
               (fun (_x1, _x2) (_y1, _y2) -> meet = Bottom)
               (* probably never 100% true unless two ranges are equal? *)
               (fun (x1, x2) (y1, y2) -> x1 = y1 && x2 = y2)
               left' right')
            (t_and_f
             (* if true *)
             (* few cases: *)
             (* [a, b] ... [c, d] or [c, d] ... [a, b] - not really interestring (doesn't filter any values out) *)
             (* [a ... {c ... b] ... d} or [c ... {a ... d] ... b} *)
             (* filters to [a, c] and [b, d] or [c, a] and [d, b] *)
             (* the last case is: [a ... {c ... d} ... b] or [c ... {a ... b} ... d] *)
             (* filters to [a, c] or [d, b] and for rhs technically bottom or [c, a] or [b, d] and for lhs technically bottom *)
             (* also maybe should not include some end points i.e. add 1/sub 1 in some cases *)
               ~t:(fun _a _b _c _d -> true)
               ~t_l:bind_outside
               ~t_r:(fun _a _b _c _d -> bind_outside _c _d _a _b)
                 (* ~t_r:(const4 right') *)
                 (* theoritcally this means (we can use the same idea we are using for) either left < right or left > right *)
                 (* if false *)
                 (* on the number line we have: ...[c, d]...[a, b]... *)
                 (* or really [c ... {a ... d ] ... b} *)
                 (* or the number line we have: ...[a, b]...[c, d]... *)
                 (* or really [a ... {c ... b ] ... d} *)
               ~f:(fun _a _b _c _d -> true)
               ~f_l:(fun _a _b _c _d -> meet)
               ~f_r:(fun _a _b _c _d -> meet)
               ()))

(* TODO: maybe only kill the variable if the update actually changes the value *)
let filter variable map =
  let filter_string_map =
    VariableMap.filter (fun id -> Fun.const (id <> variable))
  in
  let filter_boolean (v, (t, f)) =
    (v, (filter_string_map t, filter_string_map f))
  in
  BooleanMap.map filter_boolean map

let update_env s int_s =
  let old_int_s = fst s in
  let new_int_s =
    (* updates the env prefering the new state *)
    (* TODO: better way to merge the two maps *)
    VariableMap.merge
      (fun _ a b -> Option.map Option.some b |> Option.value ~default:a)
      old_int_s int_s
  in
  (new_int_s, snd s)

let transfer (n : node) s successors =
  match n.command with
  | Cfg.AssignInt { target; value } ->
      let boolean_state = snd s in
      (* update any boolean t/f that this invalidates *)
      let boolean_state = filter target boolean_state in
      let result =
        (VariableMap.add target (eval_int_expr value s) (fst s), boolean_state)
      in
      ( result,
        successors
        |> List.map (fun (_, node) -> (node, result))
        |> NodeReferenceMap.of_list )
  | Cfg.AssignBool { target; value } ->
      let result =
        (fst s, VariableMap.add target (eval_bool_expr value s) (snd s))
      in
      ( result,
        successors
        |> List.map (fun (_, node) -> (node, result))
        |> NodeReferenceMap.of_list )
  | Cfg.Cond c ->
      let c' = eval_bool_expr c s in
      print_endline (ComplexBoolean.to_string c');
      ( s,
        successors
        |> List.map (fun (edge, node) ->
               ( node,
                 match (edge, c') with
                 | True _, ((Boolean.Boolean true | Boolean.Top), (t, _)) ->
                     update_env s t
                 | False _, ((Boolean.Boolean false | Boolean.Top), (_, _f)) ->
                     update_env s _f
                 | _, ((Boolean.Bottom | Boolean.Boolean _), _) -> D.bottom
                 (* is this last case needed *)
                 | _ -> s ))
        |> NodeReferenceMap.of_list )

let run g =
  let s = F.F.state g in
  let s' = F.run' g transfer s in
  F'.run' g transfer s'
