let () = print_endline "Hello, World!"

open StaticAnalysis

(*let stmt1 =*)
(*  Ast.Assign*)
(*    ( ("i", Ast.Integer),*)
(*      Ast.Math*)
(*        ( Ast.Math (Ast.Number 1, Ast.Add, Ast.Number 2),*)
(*          Ast.Sub,*)
(*          Ast.Math*)
(*            ( Ast.Number 3,*)
(*              Ast.Add,*)
(*              Ast.Math (Ast.Number 4, Ast.Add, Ast.Number 5) ) ) )*)
let stmt2 =
  Ast.Assign
    ( ("j", Ast.Integer),
      Ast.Math
        ( Ast.Number 6,
          Ast.Sub,
          Ast.Math
            ( Ast.Math (Ast.Number 8, Ast.Add, Ast.Number 10),
              Ast.Add,
              Ast.Math (Ast.Number 8, Ast.Add, Ast.Number 9) ) ) )

let stmt3 = Ast.Assign (("j", Ast.Integer), Ast.Number 10)
(*let cond1 = Ast.Not (Ast.Boolean true)*)

let stmt1 =
  Ast.Assign
    ( ("k", Ast.Boolean),
      Ast.Or (Ast.Not (Ast.Boolean true), Ast.Not (Ast.Boolean true)) )
(*let cond2 = Ast.Not (Ast.Boolean false)*)

let k, _ =
  Ast_to_graph.stmt_to_cfg
    (Ast.Sequence (stmt1, Ast.If (Variable ("k", Ast.Boolean), stmt3, stmt2)))
    (ref 0) (-1)
(*Ast_to_graph.stmt_to_cfg stmt2 (ref 0) (-1)*)
(*(Ast_to_graph.stmt_to_cfg (Ast.Sequence ( stmt2, stmt3)) (ref 0) (-1) )*)

let cfg =
  (*-1 mean no successor (realy should be option (None)*)
  k (-1)
    {
      successors = Cfg.NodeReferenceMap.empty;
      predecesseors = Cfg.NodeReferenceMap.empty;
      nodes = [];
    }

let () = print_endline (Cfg.cfg_to_string cfg)
