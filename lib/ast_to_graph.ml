let gensym r =
  let current = !r in
  incr r;
  "temp" ^ string_of_int current

let rec append list x k =
  match list with [] -> k x | i :: is -> i :: append is x k

let rec expr_to_simple_bool_expr (expr : Ast.expression) g =
  let apply_op r l op : 'a list * Cfg.basic_bool_expr =
    let stmts, l' = expr_to_simple_bool_expr l g in
    let stmts', r' = expr_to_simple_bool_expr r g in
    let temp = gensym g in

    ( stmts @ stmts'
      @ [
          Cfg.AssignBool
            {
              target = Identifier temp;
              value =
                Cfg.BinaryOperator { operator = op; left = l'; right = r' };
            };
        ],
      Cfg.Identifier (Identifier temp) )
  in
  match expr with
  | Ast.Variable (v, _ty) -> ([], Cfg.Identifier (Identifier v))
  | Ast.Number _ -> failwith "not reachable"
  | Ast.Boolean b -> ([], Cfg.Boolean b)
  | Ast.Math (_, _, _) -> failwith "not reachable"
  | Ast.Compare (_l, _o, _r) ->
      (*let stmts, l' = expr_to_simple_int_expr l g in*)
      (*let stmts', r' = expr_to_simple_int_expr r g in*)
      (*let temp = gensym g in*)
      failwith ""
      (*( stmts @ stmts'*)
      (*  @ [*)
      (*      Cfg.AssignBool*)
      (*        {*)
      (*          target = Identifier temp;*)
      (*          value =*)
      (*            Cfg.Compare*)
      (*              {*)
      (*                operator =*)
      (*                  (match o with*)
      (*                  | Ast.Greater -> Cfg.GreaterThan*)
      (*                  | Ast.Less -> Cfg.LessThen*)
      (*                  | Ast.LessOrEqual -> Cfg.LessThenOrEqual*)
      (*                  | Ast.GreaterOrEqual -> Cfg.GreaterThanOrEqual*)
      (*                  | Ast.Equal -> Cfg.Equal*)
      (*                  | Ast.NotEqual -> Cfg.NotEqual);*)
      (*                left = l';*)
      (*                right = r';*)
      (*              };*)
      (*        };*)
      (*    ],*)
      (*  Cfg.Identifier (Identifier temp) )*)
  | Ast.And (l, r) -> apply_op r l Cfg.And
  | Ast.Or (l, r) -> apply_op r l Cfg.Or
  | Ast.Not b ->
      let stmts, b' = expr_to_simple_bool_expr b g in
      let temp = gensym g in
      ( stmts
        @ [
            Cfg.AssignBool
              {
                target = Identifier temp;
                value = Cfg.UnaryOperator { operator = Cfg.Not; operand = b' };
              };
          ],
        Cfg.Identifier (Identifier temp) )

and expr_to_simple_int_expr (expr : Ast.expression) g :
    Cfg.basic_int_expr * (Cfg.command list -> Cfg.command list) =
  match expr with
  | Ast.Number n -> (Cfg.Integer n, Fun.id)
  | Ast.Variable (v, _ty) -> (Cfg.Identifier (Identifier v), Fun.id)
  | Ast.Boolean _ -> failwith "not reachable"
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
        fun s -> stmts (stmts' (List.cons command s)) )
  | Ast.Compare (_, _, _) -> failwith "not reachable"
  | Ast.And (_, _) -> failwith "not reachable"
  | Ast.Or (_, _) -> failwith "not reachable"
  | Ast.Not _ -> failwith "not reachable"
