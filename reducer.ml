(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
	if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
	then raise (OutOfVariablesError)
	else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)



(*
  ADD FUNCTIONS BELOW
*)

let rec fv term = 
	match term with
	|Variable v -> StringSet.add v StringSet.empty
	|Abstraction (v,t) -> StringSet.diff (fv t) fv(v)
	|Application (t1,t2) -> StringSet.union (fv t1) (fv t2)



