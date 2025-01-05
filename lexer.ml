(*
  Lexer for lambda-calculus.

  DO NOT MODIFY THIS FILE
*)

open Utils


type token = LambdaTok | EqTok | DotTok | LetTok | InTok | Literal of string | LParen | RParen


let rec eat_id sofar cs =
  match cs with
  | [] -> sofar, []
  | '.' :: _ -> sofar, cs
  | ')' :: _ -> sofar, cs
  | '(' :: _ -> sofar, cs
  | '\\' :: _ -> sofar, cs
  | ' ' :: cs' -> sofar, cs'
  | '\n' :: cs' -> sofar, cs'
  | '\r' :: cs' -> sofar, cs'
  | c :: cs' -> eat_id (sofar ^ (char_to_string c)) cs'


let tokenize_id = function
  | "let" -> LetTok
  | "in" -> InTok
  | s -> Literal s


let rec tokenize = function
  | [] -> []
  | '\\' :: cs -> LambdaTok :: tokenize cs
  | '.' :: cs -> DotTok :: tokenize cs
  | '(' :: cs -> LParen :: tokenize cs
  | ')' :: cs -> RParen :: tokenize cs
  | '=' :: cs -> EqTok :: tokenize cs
  | ' ' :: cs -> tokenize cs
  | '\n' :: cs -> tokenize cs
  | '\r' :: cs -> tokenize cs
  | cs -> let s, cs' = eat_id "" cs in
	  (tokenize_id s) :: tokenize cs'
