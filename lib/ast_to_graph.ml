let gensym r =
  let current = !r in
  incr r;
  "temp" ^ string_of_int current

let rec expr_to_simple_bool_expr (expr : Ast.expression) g :
    'a list * Cfg.basic_bool_expr =
  match expr with
  | Ast.Variable (v, _ty) -> ([], Cfg.Identifier (Identifier v))
  | Ast.Number _ -> failwith "not reachable"
  | Ast.Boolean b -> ([], Cfg.Boolean b)
  | Ast.Math (_, _, _) -> failwith "not reachable"
  | Ast.Compare (_, _, _) -> failwith ""
  | Ast.And (_, _) -> failwith ""
  | Ast.Or (_, _) -> failwith ""
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

and expr_to_simple_int_expr (expr : Ast.expression) g =
  match expr with
  | Ast.Variable (v, _ty) -> ([], Cfg.Identifier (Identifier v))
  | Ast.Number n -> ([], Cfg.Integer n)
  | Ast.Boolean _ -> failwith "not reachable"
  | Ast.Math (l, o, r) ->
      let stmts, l' = expr_to_simple_int_expr l g in
      let stmts', r' = expr_to_simple_int_expr r g in
      let temp = gensym g in
      ( stmts @ stmts'
        @ [
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
          ],
        Cfg.Identifier (Identifier temp) )
  | Ast.Compare (_, _, _) -> failwith "not reachable"
  | Ast.And (_, _) -> failwith "not reachable"
  | Ast.Or (_, _) -> failwith "not reachable"
  | Ast.Not _ -> failwith "not reachable"
