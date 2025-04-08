let () = print_endline "Hello, World!"

open StaticAnalysis

let k =
  []
  |> (Ast_to_graph.expr_to_simple_int_expr
        (Ast.Math
           ( Ast.Math (Ast.Number 1, Ast.Add, Ast.Number 2),
             Ast.Sub,
             Ast.Math (Ast.Number 3, Ast.Add, Ast.Math (Ast.Number 4, Ast.Add, Ast.Number 5)) ))
        (ref 0)
     |> snd)

let () = print_endline (k |> List.map Cfg.command_to_string |> String.concat "\n")
