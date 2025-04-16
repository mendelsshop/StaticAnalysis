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
