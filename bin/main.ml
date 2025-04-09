let () = print_endline "Hello, World!"

open StaticAnalysis

let stmt1 =
  Ast.Assign
    ( ("i", Ast.Integer),
      Ast.Math
        ( Ast.Math (Ast.Number 1, Ast.Add, Ast.Number 2),
          Ast.Sub,
          Ast.Math
            ( Ast.Number 3,
              Ast.Add,
              Ast.Math (Ast.Number 4, Ast.Add, Ast.Number 5) ) ) )

let stmt2 =
  Ast.Assign
    ( ("j", Ast.Integer),
      Ast.Math
        ( Ast.Number 6,
          Ast.Sub,
          Ast.Math
            ( Ast.Number 7,
              Ast.Add,
              Ast.Math (Ast.Number 8, Ast.Add, Ast.Number 9) ) ) )

let cond = Ast.Not (Ast.Boolean true)

let k =
  [] |> (Ast_to_graph.stmt_to_cfg (Ast.If (cond, stmt1, stmt2)) (ref 0) 0 |> fst)

let () =
  print_endline (k |> List.map Cfg.command_to_string |> String.concat "\n")
