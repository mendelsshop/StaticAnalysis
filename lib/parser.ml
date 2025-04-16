open AMPCL
open Ast

module Unit = struct
  include Unit

  let show _ = ""
end

module Parser = AMPCL.Parser.Char.String.Show.Make (Unit)
open Parser

let keywords = [ "if"; "else"; "while"; "let"; "boolean"; "number" ]

let comment =
  let non_newline = sat (fun c -> c <> '\n') in
  let comment_contents = many non_newline <$> implode in
  string "--" << comment_contents

let whitespace =
  let white_space = sat (function ' ' | '\n' | '\t' -> true | _ -> false) in
  many white_space <$> implode

let garbage = many (whitespace <|> comment) <$> String.concat "\n"
let ( & ) f g x = f (g x)
let skip_garbage f = garbage >>= fun _ -> f
let ( ~~ ) = skip_garbage

let identifier =
  let idents = many alphanum in
  let flatten (f, rest) = f :: rest in
  seq lower idents <$> flatten <$> implode
  |> check (not & Fun.flip List.mem keywords)

let number = many1 digit <$> ((fun n -> Number n) & int_of_string & implode)

let boolean =
  let trueP = string "true" <$> fun _ -> Boolean true in
  let falseP = string "false" <$> fun _ -> Boolean false in
  trueP <|> falseP

let parens = between (char '(') ~~(char ')')
let ops l = List.map (fun (p, o) -> p <$> Fun.const o) l

let unary_math expr =
  let op = choice (ops [ (char '-', Neg); (char '+', Abs) ]) in
  return (fun op expr -> UnaryMath (op, expr)) <*> ~~op <*> expr

let not expr =
  let op = char '!' in
  ~~op << (expr <$> fun expr -> Not expr)

(*left associative parer
  bexpr is the base parser of higher precedence
  op is a list operator parsers
*)
let left bexpr op constructor =
  makeRecParser (fun expr ->
      return constructor <*> bexpr <*> ~~(choice op) <*> expr <|> bexpr)

let math bexpr op = left bexpr op (fun l o r -> Math (l, o, r))
let compare bexpr op = left bexpr op (fun l o r -> Compare (l, o, r))

let expr =
  makeRecParser (fun expr ->
      let simple_exprs =
        ~~(choice
             [
               number;
               boolean;
               (identifier <$> fun i -> Variable i);
               parens expr;
             ])
      in
      let unary_exprs =
        makeRecParser (fun expr ->
            choice [ unary_math expr; not expr; simple_exprs ])
      in
      let prec1 =
        math unary_exprs
          (ops [ (char '*', Mul); (char '/', Div); (char '%', Mod) ])
      in
      let prec2 = math prec1 (ops [ (char '+', Add); (char '-', Sub) ]) in
      let prec3 =
        compare prec2
          (ops
             [
               (string "<=", LessOrEqual);
               (string ">=", GreaterOrEqual);
               (string "<", Less);
               (string ">", Greater);
             ])
      in
      let prec4 =
        compare prec3 (ops [ (string "==", Equal); (string "!=", NotEqual) ])
      in
      let prec5 = left prec4 [ string "&&" ] (fun l _ r -> And (l, r)) in
      let prec5 = left prec5 [ string "||" ] (fun l _ r -> Or (l, r)) in
      prec5)
