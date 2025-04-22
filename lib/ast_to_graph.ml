let gensym r =
  let current = !r in
  incr r;
  "temp" ^ string_of_int current

let id' _ _ x = x

let add_predecesseor_opt src dest f g =
  src |> Option.map f
  |> Option.fold ~none:g ~some:(fun src -> Cfg.add_predecesseor src dest g)

let add_successor_opt src dest f g =
  dest |> Option.map f
  |> Option.fold ~none:g ~some:(Fun.flip (Cfg.add_successor src) g)

let rec expr_to_simple_bool_expr (expr : Ast.expression) g i :
    Cfg.basic_bool_expr
    * (bool -> int option -> Cfg.graph -> Cfg.graph)
    * int
    * bool =
  let apply_op r l op i :
      Cfg.basic_bool_expr
      * (bool -> int option -> Cfg.graph -> Cfg.graph)
      * int
      * bool =
    let l', l_stmts, i', l_already_simple = expr_to_simple_bool_expr l g i in
    let r', r_stmts, i'', r_already_simple = expr_to_simple_bool_expr r g i' in
    let id = i'' + 1 in
    let temp = gensym g in
    ( Cfg.Identifier (Identifier temp),
      (fun loop_head i g ->
        let command : Cfg.node =
          {
            id;
            command =
              Cfg.AssignBool
                {
                  target = Identifier temp;
                  value =
                    Cfg.BinaryOperator { operator = op; left = l'; right = r' };
                };
            loop_head = loop_head && l_already_simple && r_already_simple;
          }
        in

        g |> Cfg.add_node command
        |> r_stmts (loop_head && l_already_simple) (Some id)
        |> l_stmts loop_head (Some (i' + 1))
        |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
        |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
      id,
      false )
  in
  match expr with
  | Ast.Variable v -> (Cfg.Identifier (Identifier v), id', i, true)
  | Ast.Number _ -> failwith "not reachable"
  | Ast.Boolean b -> (Cfg.Boolean b, id', i, true)
  | Ast.Math (_, _, _) -> failwith "not reachable"
  | Ast.UnaryMath (_, _) -> failwith "not reachable"
  | Ast.Compare (l, o, r) ->
      let l', l_stmts, i', l_already_simple = expr_to_simple_int_expr l g i in
      let r', r_stmts, i'', r_already_simple = expr_to_simple_int_expr r g i' in
      let id = i'' + 1 in
      let temp = gensym g in
      ( Cfg.Identifier (Identifier temp),
        (fun loop_head i g ->
          let command : Cfg.node =
            {
              id;
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
              loop_head = loop_head && l_already_simple && r_already_simple;
            }
          in

          g |> Cfg.add_node command
          |> r_stmts (loop_head && l_already_simple) (Some id)
          |> l_stmts loop_head (Some (i' + 1))
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id,
        false )
  | Ast.And (l, r) -> apply_op r l Cfg.And i
  | Ast.Or (l, r) -> apply_op r l Cfg.Or i
  | Ast.Not b ->
      let b', b_stmts, i', b_already_simple = expr_to_simple_bool_expr b g i in
      let id = i' + 1 in
      let temp = gensym g in
      ( Cfg.Identifier (Identifier temp),
        (fun loop_head i g ->
          let command : Cfg.node =
            {
              id;
              command =
                Cfg.AssignBool
                  {
                    target = Identifier temp;
                    value =
                      Cfg.UnaryOperator { operator = Cfg.Not; operand = b' };
                  };
              loop_head = loop_head && b_already_simple;
            }
          in
          g |> Cfg.add_node command
          |> b_stmts loop_head (Some id)
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id,
        false )

and expr_to_simple_int_expr (expr : Ast.expression) g i :
    Cfg.basic_int_expr
    * (bool -> int option -> Cfg.graph -> Cfg.graph)
    * int
    * bool =
  match expr with
  | Ast.Number n -> (Cfg.Integer n, id', i, true)
  | Ast.Variable v -> (Cfg.Identifier (Identifier v), id', i, true)
  | Ast.Boolean _ -> failwith "not reachable"
  | Ast.UnaryMath (o, v) ->
      let v', v_stmts, i', v_already_simple = expr_to_simple_int_expr v g i in
      let temp = gensym g in
      let id = i' + 1 in
      ( Cfg.Identifier (Identifier temp),
        (fun loop_head i g ->
          let command : Cfg.node =
            {
              id;
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
              loop_head = loop_head && v_already_simple;
            }
          in

          g |> Cfg.add_node command
          |> v_stmts loop_head (Some id)
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id,
        false )
  | Ast.Math (l, o, r) ->
      let l', l_stmts, i', l_already_simple = expr_to_simple_int_expr l g i in
      let r', r_stmts, i'', r_already_simple = expr_to_simple_int_expr r g i' in
      let temp = gensym g in
      let id = i'' + 1 in
      ( Cfg.Identifier (Identifier temp),
        (fun loop_head i g ->
          let command : Cfg.node =
            {
              id;
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
              loop_head = loop_head && l_already_simple && r_already_simple;
            }
          in
          g |> Cfg.add_node command
          |> r_stmts (loop_head && l_already_simple) (Some id)
          |> l_stmts loop_head (Some (i' + 1))
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id,
        false )
  | Ast.Compare (_, _, _) -> failwith "not reachable"
  | Ast.And (_, _) -> failwith "not reachable"
  | Ast.Or (_, _) -> failwith "not reachable"
  | Ast.Not _ -> failwith "not reachable"

let rec stmt_to_cfg (s : Ast.statement) g i =
  match s with
  | Ast.If (c, t, a) ->
      let c', c_stmts, i', _ = expr_to_simple_bool_expr c g i in
      let cond_id = i' + 1 in
      let t_stmts, i'' = stmt_to_cfg t g cond_id in
      let a_stmts, i''' = stmt_to_cfg a g i'' in
      let command : Cfg.node =
        { id = cond_id; command = Cfg.Cond (Basic c'); loop_head = false }
      in
      ( (fun i g ->
          c_stmts false (Some cond_id)
            (Cfg.add_node command
               (g |> a_stmts i |> t_stmts i
               |> Cfg.add_predecesseor (i'' + 1) (Cfg.False, Node cond_id)
               |> Cfg.add_predecesseor (cond_id + 1) (Cfg.True, Node cond_id)
               |> Cfg.add_successor cond_id (Cfg.False, Node (i'' + 1))
               |> Cfg.add_successor cond_id (Cfg.True, Node (cond_id + 1))))),
        i''' )
  | Ast.While (c, t) ->
      let c', c_stmts, i', c_already_simple = expr_to_simple_bool_expr c g i in
      let cond_id = i' + 1 in
      let t_stmts, i'' = stmt_to_cfg t g cond_id in
      let command : Cfg.node =
        {
          id = cond_id;
          command = Cfg.Cond (Basic c');
          loop_head = c_already_simple;
        }
      in
      let start_id = i in
      ( (fun i g ->
          c_stmts true (Some cond_id)
            (Cfg.add_node command
               (g
               |> t_stmts (Some (start_id + 1))
               |> add_predecesseor_opt i (Cfg.False, Node cond_id) (fun i -> i)
               |> Cfg.add_predecesseor (cond_id + 1) (Cfg.True, Node cond_id)
               |> add_successor_opt cond_id i (fun i -> (Cfg.False, Node i))
               |> Cfg.add_successor cond_id (Cfg.True, Node (cond_id + 1))))),
        i'' )
  | Ast.Assign ((ident, TNumber), v) ->
      let v', v_stmts, i', _ = expr_to_simple_int_expr v g i in
      let id = i' + 1 in
      let command : Cfg.node =
        {
          id;
          command =
            Cfg.AssignInt { target = Identifier ident; value = Cfg.Basic v' };
          loop_head = false;
        }
      in
      ( (fun i g ->
          g |> Cfg.add_node command |> v_stmts false (Some id)
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id )
  | Ast.Assign ((ident, TBoolean), v) ->
      let v', v_stmts, i', _ = expr_to_simple_bool_expr v g i in
      let id = i' + 1 in
      let command : Cfg.node =
        {
          id;
          command =
            Cfg.AssignBool { target = Identifier ident; value = Cfg.Basic v' };
          loop_head = false;
        }
      in
      ( (fun i g ->
          g |> Cfg.add_node command |> v_stmts false (Some id)
          |> add_predecesseor_opt i (Cfg.Directed, Node id) (fun i -> i)
          |> add_successor_opt id i (fun i -> (Cfg.Directed, Node i))),
        id )
  | Ast.Sequence (s, s') ->
      let stmts, i' = stmt_to_cfg s g i in
      let stmts', i'' = stmt_to_cfg s' g i' in
      ((fun i s -> s |> stmts' i |> stmts (Some (i' + 1))), i'')
