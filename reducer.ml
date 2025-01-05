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
	|Abstraction (v,t) -> StringSet.diff (fv t) (StringSet.singleton v)
	|Application (t1,t2) -> StringSet.union (fv t1) (fv t2)


let rec substitute v term1 term2 =
  match term2 with
  | Variable y -> if v = y then term1 else term2 
  | Abstraction (y, t1) ->
      if v = y then term2 
      else if
	   not (StringSet.mem y (fv term1)) then Abstraction (y, substitute v term1 t1)
      else
        let z = fresh_var (StringSet.union (fv t1) (StringSet.union (fv term1) (StringSet.singleton v))) in
        let new_t1 = substitute y (Variable z) t1 in
        Abstraction (z, substitute v term1 new_t1)
  | Application (t1, t2) -> Application (substitute v term1 t1, substitute v term1 t2) 


let rec reduce_cbv term =
  match term with
  | Variable _ -> None 

  | Abstraction _ -> None 

  | Application (Abstraction (x, t1), v2) -> 
      (match reduce_cbv v2 with
       | None -> Some (substitute x v2 t1)
       | Some reduced_v2 -> Some (Application (Abstraction (x, t1), reduced_v2))) 

  | Application (t1, t2) ->
      (match reduce_cbv t1 with
       | Some reduced_t1 -> Some (Application (reduced_t1, t2))
       | None -> 
           (match reduce_cbv t2 with
            | Some reduced_t2 -> Some (Application (t1, reduced_t2))
            | None -> None))

  | _ -> None


let rec reduce_cbn term =
  match term with
  | Variable _ -> None 

  | Abstraction _ -> None

  | Application (Abstraction (x, t1), t2) ->
      Some (substitute x t2 t1)

  | Application (t1, t2) ->
      begin match reduce_cbn t1 with
      | Some t1' -> Some (Application (t1', t2))
      | None -> None 
      end

  | _ -> None 
