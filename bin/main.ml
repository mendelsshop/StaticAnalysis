open StaticAnalysis

let read_to_string file = open_in file |> In_channel.input_all

(*(*let stmt1 =*)*)
(*(*  Ast.Assign*)*)
(*(*    ( ("i", Ast.Integer),*)*)
(*(*      Ast.Math*)*)
(*(*        ( Ast.Math (Ast.Number 1, Ast.Add, Ast.Number 2),*)*)
(*(*          Ast.Sub,*)*)
(*(*          Ast.Math*)*)
(*(*            ( Ast.Number 3,*)*)
(*(*              Ast.Add,*)*)
(*(*              Ast.Math (Ast.Number 4, Ast.Add, Ast.Number 5) ) ) )*)*)
(*let stmt2 =*)
(*  Ast.Assign*)
(*    ( ("j", Ast.Integer),*)
(*      Ast.Math*)
(*        ( Ast.Number 6,*)
(*          Ast.Sub,*)
(*          Ast.Math*)
(*            ( Ast.Math (Ast.Number 8, Ast.Add, Ast.Number 10),*)
(*              Ast.Add,*)
(*              Ast.Math (Ast.Number 8, Ast.Add, Ast.Number 9) ) ) )*)
(*let stmt3 = Ast.Assign (("j", Ast.Integer), Ast.Number 10)*)
(*let cond1 = Ast.Not (Ast.Boolean true)*)
(*let stmt1 =*)
(*  Ast.Assign*)
(*    ( ("k", Ast.Boolean),*)
(*      Ast.Or (Ast.Not (Ast.Boolean true), Ast.Not (Ast.Boolean true)) )*)
(*let cond2 = Ast.Not (Ast.Boolean false)*)
let _ =
  let args = Array.to_list Sys.argv in
  let file =
    match List.nth_opt args 1 with
    | Some file -> file
    | None ->
        print_endline "usage strings [file]";
        exit 1
  in
  let input = read_to_string file in
  let parsed = Parser.run Parser.parser input in
  Result.map
    (fun parsed ->
      let k, _ = Ast_to_graph.stmt_to_cfg parsed (ref 0) (-1) in

      let cfg =
        k None
          {
            successors = Cfg.NodeReferenceMap.empty;
            predecesseors = Cfg.NodeReferenceMap.empty;
            nodes = [];
          }
      in
      Cfg.cfg_to_string cfg)
    parsed
  |> Result.fold ~ok:Fun.id ~error:Fun.id
  |> print_endline
