let gensym r =
  let current = !r in
  incr r;
  "temp" ^ string_of_int current

(*TODO: add succecor/predeccesor
  it won't always be i+1, because that migth be from another branch of if/while
  also maybe there is none (last statement/last statement in while that is last statement)
  maybe pass it as part of continuation

  what i came up with 
  you pass to the continuation the node index backwards it is up to the caller of the continuation to add the edges
*)
let id' _ x = x

let rec expr_to_simple_bool_expr (expr : Ast.expression) g i :
    Cfg.basic_bool_expr * (int -> Cfg.graph -> Cfg.graph) * int =
  let apply_op r l op i :
      Cfg.basic_bool_expr * (int -> Cfg.graph -> Cfg.graph) * int =
    let l', stmts, i' = expr_to_simple_bool_expr l g i in
    let r', stmts', i'' = expr_to_simple_bool_expr r g i' in
    let temp = gensym g in
    let var_name : Cfg.node =
      {
        id = i'' + 1;
        command =
          Cfg.AssignBool
            {
              target = Identifier temp;
              value =
                Cfg.BinaryOperator { operator = op; left = l'; right = r' };
            };
      }
    in
    ( Cfg.Identifier (Identifier temp),
      (fun _ g -> Cfg.add_node var_name g |> stmts' (i'' + 1) |> stmts (i' + 1)),
      i'' + 1 )
  in
  match expr with
  | Ast.Variable (v, _ty) -> (Cfg.Identifier (Identifier v), id', i)
  | Ast.Number _ -> failwith "not reachable"
  | Ast.Boolean b -> (Cfg.Boolean b, id', i)
  | Ast.Math (_, _, _) -> failwith "not reachable"
  | Ast.UnaryMath (_, _) -> failwith "not reachable"
  | Ast.Compare (l, o, r) ->
      let l', stmts, i' = expr_to_simple_int_expr l g i in
      let r', stmts', i'' = expr_to_simple_int_expr r g i' in
      let temp = gensym g in
      let command : Cfg.node =
        {
          id = i'' + 1;
          command =
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
              };
        }
      in
      ( Cfg.Identifier (Identifier temp),
        (fun _ s ->
          Cfg.add_node command s |> stmts' (i'' + 1) |> stmts (i' + 1)),
        i'' + 1 )
  | Ast.And (l, r) -> apply_op r l Cfg.And i
  | Ast.Or (l, r) -> apply_op r l Cfg.Or i
  | Ast.Not b ->
      let b', stmts, i' = expr_to_simple_bool_expr b g i in
      let temp = gensym g in
      let command : Cfg.node =
        {
          id = i' + 1;
          command =
            Cfg.AssignBool
              {
                target = Identifier temp;
                value = Cfg.UnaryOperator { operator = Cfg.Not; operand = b' };
              };
        }
      in
      ( Cfg.Identifier (Identifier temp),
        (fun _ s -> Cfg.add_node command s |> stmts (i' + 1)),
        i' + 1 )

and expr_to_simple_int_expr (expr : Ast.expression) g i :
    Cfg.basic_int_expr * (int -> Cfg.graph -> Cfg.graph) * int =
  match expr with
  | Ast.Number n -> (Cfg.Integer n, id', i)
  | Ast.Variable (v, _ty) -> (Cfg.Identifier (Identifier v), id', i)
  | Ast.Boolean _ -> failwith "not reachable"
  | Ast.UnaryMath (o, v) ->
      let v', stmts, i' = expr_to_simple_int_expr v g i in
      let temp = gensym g in
      let command : Cfg.node =
        {
          id = i' + 1;
          command =
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
              };
        }
      in
      ( Cfg.Identifier (Identifier temp),
        (fun _ s -> Cfg.add_node command s |> stmts (i' + 1)),
        i' + 1 )
  | Ast.Math (l, o, r) ->
      let l', stmts, i' = expr_to_simple_int_expr l g i in
      let r', stmts', i'' = expr_to_simple_int_expr r g i' in
      let temp = gensym g in
      let command : Cfg.node =
        {
          id = i'' + 1;
          command =
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
              };
        }
      in
      ( Cfg.Identifier (Identifier temp),
        (fun _ s ->
          Cfg.add_node command s |> stmts' (i'' + 1) |> stmts (i' + 1)),
        i'' + 1 )
  | Ast.Compare (_, _, _) -> failwith "not reachable"
  | Ast.And (_, _) -> failwith "not reachable"
  | Ast.Or (_, _) -> failwith "not reachable"
  | Ast.Not _ -> failwith "not reachable"

let rec stmt_to_cfg (s : Ast.statement) g i =
  match s with
  | Ast.If (c, t, a) ->
      let c', stmts, i' = expr_to_simple_bool_expr c g i in
      let t', i'' = stmt_to_cfg t g (i' + 1) in
      let a', i''' = stmt_to_cfg a g i'' in
      let command : Cfg.node = { id = i' + 1; command = Cfg.Cond (Basic c') } in
      ( (fun i s -> stmts (i' + 1) (Cfg.add_node command (s |> a' i |> t' i))),
        i''' )
  | Ast.While (c, t) ->
      let c', stmts, i' = expr_to_simple_bool_expr c g i in
      let t', i'' = stmt_to_cfg t g (i' + 1) in
      let command : Cfg.node = { id = i' + 1; command = Cfg.Cond (Basic c') } in
      ((fun _ s -> stmts (i' + 1) (Cfg.add_node command (t' (i'' + 1) s))), i'')
  | Ast.Assign ((ident, Integer), v) ->
      let v', stmts, i' = expr_to_simple_int_expr v g i in
      let command : Cfg.node =
        {
          id = i' + 1;
          command =
            Cfg.AssignInt { target = Identifier ident; value = Cfg.Basic v' };
        }
      in
      ((fun _ s -> stmts (i' + 1) (Cfg.add_node command s)), i' + 1)
  | Ast.Assign ((ident, Boolean), v) ->
      let v', stmts, i' = expr_to_simple_bool_expr v g i in
      let command : Cfg.node =
        {
          id = i' + 1;
          command =
            Cfg.AssignBool { target = Identifier ident; value = Cfg.Basic v' };
        }
      in
      ((fun _ s -> stmts (i' + 1) (Cfg.add_node command s)), i' + 1)
  | Ast.Sequence (s, s') ->
      let stmts, i' = stmt_to_cfg s g i in
      let stmts', i'' = stmt_to_cfg s' g i' in
      ((fun _ s -> s |> stmts' (i'' + 1) |> stmts (i' + 1)), i'')
