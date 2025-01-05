(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer


let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None -> 
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' -> 
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let s1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"


let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
"

let s3 = "
((\\id1.(t1 id1)) (\\id2.(t1 t2)))
"

let () =
  printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s1;
  ignore (evaluate ~verbose:true reduce_cbn (parse s1));
  printf "\n\nEvaluating:\n%s\nin cbv semantics:\n\n" s2;
  ignore (evaluate ~verbose:true reduce_cbv (parse s2));

  printf "\n\n Testing on:\n%s\nReduce cbv\n\n" s3;
  ignore (evaluate ~verbose:true reduce_cbv (parse s3));
  printf "\n\n Testing on:\n%s\nReduce cbn\n\n" s3;
  ignore (evaluate ~verbose:true reduce_cbn (parse s3));
  
  (*Format Term Conv Tests*)
  
  printf "%s\n" (format_term_conv(parse("(\\t.(\\f.t))" )));
  printf "%s\n" (format_term_conv(parse("(t1 (t2 t3))")));
  printf "%s\n" (format_term_conv(parse("((t1 t2) t3)")));
  printf "%s\n" (format_term_conv(parse(s1)));
  printf "%s\n" (format_term_conv(parse("(\\b.(\\c. ((b (not c)) c)))")));
