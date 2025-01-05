(*
  Various utilities that replace Core functions

  DO NOT MODIFY THIS FILE
*)

[@@@ocaml.warning "-8"];;

let printf = Printf.printf

let char_to_string = String.make 1

(* convert string to list of chars *)
let string_to_list s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] ((String.length s) - 1)

(* convert list of chars to string *)
let string_from_list l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n + 1) xs
    | [] -> s
  in f 0 l

(* create a list with range of numbers *)
let range i j =
  let rec f acc k = if k = i - 1 then acc else f (k :: acc) (k - 1)
  in f [] (j - 1)


(* set of strings *)
module StringSet = Set.Make(String);;

(* set of strings from list *)
let string_set_from_list = List.fold_left (fun acc e -> StringSet.add e acc) StringSet.empty

(* print a set of strings line by line *)
let print_string_set = StringSet.iter print_endline
