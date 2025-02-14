open Fungo_lib

let input =
  "\n\
   \t\t\tmodule X = \n\
   \t\t\t\ttype t\n\
   \t\t\t\tlet value x y = [x ; y] \n\
   \t\t\t\tlet make () = {test = true; test2 = false}\n\
   \t\t\tend\n\
   let func a b =\n\
   \tlet z = X.make () in\n\
   \tX.value z 1.5\n\n\
   let () =\n\
   \tfunc 1 2\n\
   \t\t\t"
;;

let () = Frontend.compile input
