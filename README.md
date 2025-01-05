# LP:exe-3


The solution should compile with OCaml 4.02, and does not require Core.

Your files should compile with:

$ ocamlc -o tests utils.ml lexer.ml parser.ml reducer.ml tests.ml

And then run like this:

$ ./tests

After you compile, you can also use utop to interactively test your implementations like this:

$ utop -I .
utop # #load_rec "tests.cmo";;
utop # open Parser;;
utop # open Reducer;;
utop # open Tests;;
utop # evaluate ~verbose:true reduce_cbv (parse "((\\x. x) y)");;