(*
  Parser for lambda-calculus.
*)

open Utils
open Lexer

(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term

(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

exception SyntaxError of string

let rec parse_term = function
    | [] -> raise (SyntaxError "Tokens expected.\n")
    | (Literal id) :: toks -> Variable id, toks
    | LParen :: LambdaTok :: (Literal id) :: DotTok :: toks -> let (t, toks') = parse_term toks in (
                                                                match toks' with
                                                                | [] -> raise (SyntaxError "RParen expected.\n")
                                                                | RParen :: toks'' -> Abstraction (id, t), toks''
                                                                | _ -> raise (SyntaxError "RParen expected.\n")
    )
    | LParen :: toks -> let (t1, toks') = parse_term toks in (
                        match toks' with
                        | [] -> raise (SyntaxError "RParen or Term expected.\n")
                        | RParen :: toks'' -> t1, toks''
                        | _ -> let (t2, toks'') = parse_term toks' in (
                                match toks'' with
                                | [] -> raise (SyntaxError "RParen expected.\n")
                                | RParen :: toks''' -> Application(t1, t2), toks'''
                                | _ -> raise (SyntaxError "RParen expected.\n")
                        )
    )
    | LetTok :: (Literal id) :: EqTok :: toks -> let (t1, toks') = parse_term toks in (
                                                    match toks' with
                                                    | [] -> raise (SyntaxError "InTok expected.\n")
                                                    | InTok :: toks'' -> let (t2, toks''') = parse_term toks'' in (
                                                        Application (Abstraction (id, t2), t1), toks'''
                                                    )
                                                    | _ -> raise (SyntaxError "InTok expected.\n")
    )
    | _ -> raise (SyntaxError "Unexpeced token.\n")


let parse lexp = 
    let (te, tol) = string_to_list lexp |> tokenize |> parse_term
    in
    match tol with
    | [] -> te
    | _ -> raise (SyntaxError "Unexpected input.\n")


let rec format_term = function
    | Variable s -> s
    | Abstraction (s, t) -> "(\\" ^ s ^ "." ^ format_term t ^ ")"
    | Application (t1, t2) -> "(" ^ format_term t1 ^ " " ^ format_term t2 ^ ")"


let rec format_term_conv = function
	| Variable s 			-> s
	| Abstraction ( s, Application( t1, t2 ) ) 	-> "\\" ^ s ^ "." ^ "(" ^ (format_term_conv t1) ^ " " ^ (format_term_conv t2) ^ ")"
	| Abstraction ( s, t1 ) 	-> "\\" ^ s ^ "." ^ (format_term_conv t1)
	| Application ( t1, t2 ) 	-> (format_term_conv t1) ^ " " ^ (format_term_conv t2)
