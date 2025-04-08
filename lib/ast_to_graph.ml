let gensym r =
  let current = !r in
  incr r;
  "temp" ^ string_of_int current

let rec append list x k =
  match list with [] -> k x | i :: is -> i :: append is x k

let rec expr_to_simple_bool_expr (expr : Ast.expression) g :
    Cfg.basic_bool_expr * (Cfg.command list -> Cfg.command list) =
  let apply_op r l op :
      Cfg.basic_bool_expr * (Cfg.command list -> Cfg.command list) =
    let l', stmts = expr_to_simple_bool_expr l g in
    let r', stmts' = expr_to_simple_bool_expr r g in
    let temp = gensym g in
    let command =
      Cfg.AssignBool
        {
          target = Identifier temp;
          value = Cfg.BinaryOperator { operator = op; left = l'; right = r' };
        }
    in
    (Cfg.Identifier (Identifier temp), fun s -> command :: s |> stmts' |> stmts)
  in
  match expr with
  | Ast.Variable (v, _ty) -> (Cfg.Identifier (Identifier v), Fun.id)
  | Ast.Number _ -> failwith "not reachable"
  | Ast.Boolean b -> (Cfg.Boolean b, Fun.id)
  | Ast.Math (_, _, _) -> failwith "not reachable"
  | Ast.UnaryMath (_, _) -> failwith "not reachable"
  | Ast.Compare (l, o, r) ->
      let l', stmts = expr_to_simple_int_expr l g in
      let r', stmts' = expr_to_simple_int_expr r g in
      let temp = gensym g in
      let command =
        Cfg.AssignBool
          {
            target = Identifier temp;
            value =
              Cfg.Compare
                {
                  operator =
                    (match o with
                    | Ast.Greater -> Cfg.GreaterThan
                    | Ast.Less -> Cfg.LessThen
                    | Ast.LessOrEqual -> Cfg.LessThenOrEqual
                    | Ast.GreaterOrEqual -> Cfg.GreaterThanOrEqual
                    | Ast.Equal -> Cfg.Equal
                    | Ast.NotEqual -> Cfg.NotEqual);
                  left = l';
                  right = r';
                };
          }
      in
      ( Cfg.Identifier (Identifier temp),
        fun s -> command :: s |> stmts' |> stmts )
  | Ast.And (l, r) -> apply_op r l Cfg.And
  | Ast.Or (l, r) -> apply_op r l Cfg.Or
  | Ast.Not b ->
      let b', stmts = expr_to_simple_bool_expr b g in
      let temp = gensym g in
      let command =
        Cfg.AssignBool
          {
            target = Identifier temp;
            value = Cfg.UnaryOperator { operator = Cfg.Not; operand = b' };
          }
      in
      (Cfg.Identifier (Identifier temp), fun s -> command :: s |> stmts)

and expr_to_simple_int_expr (expr : Ast.expression) g :
    Cfg.basic_int_expr * (Cfg.command list -> Cfg.command list) =
  match expr with
  | Ast.Number n -> (Cfg.Integer n, Fun.id)
  | Ast.Variable (v, _ty) -> (Cfg.Identifier (Identifier v), Fun.id)
  | Ast.Boolean _ -> failwith "not reachable"
  | Ast.UnaryMath (o, v) ->
      let v', stmts = expr_to_simple_int_expr v g in
      let temp = gensym g in
      let command =
        Cfg.AssignInt
          {
            target = Identifier temp;
            value =
              Cfg.UnaryOperator
                {
                  operator =
                    (match o with
                    | Ast.Neg -> Cfg.Negate
                    | Ast.Abs -> Cfg.AbsoluteValue);
                  operand = v';
                };
          }
      in
      (Cfg.Identifier (Identifier temp), fun s -> command :: s |> stmts)
  | Ast.Math (l, o, r) ->
      let l', stmts = expr_to_simple_int_expr l g in
      let r', stmts' = expr_to_simple_int_expr r g in
      let temp = gensym g in
      let command =
        Cfg.AssignInt
          {
            target = Identifier temp;
            value =
              Cfg.BinaryOperator
                {
                  operator =
                    (match o with
                    | Ast.Add -> Cfg.Add
                    | Ast.Sub -> Cfg.Subtract
                    | Ast.Mul -> Cfg.Multiply
                    | Ast.Div -> Cfg.Divide
                    | Ast.Mod -> Cfg.Modulo);
                  left = l';
                  right = r';
                };
          }
      in
      ( Cfg.Identifier (Identifier temp),
        fun s -> command :: s |> stmts' |> stmts )
  | Ast.Compare (_, _, _) -> failwith "not reachable"
  | Ast.And (_, _) -> failwith "not reachable"
  | Ast.Or (_, _) -> failwith "not reachable"
  | Ast.Not _ -> failwith "not reachable"
