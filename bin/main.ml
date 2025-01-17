open Core
(* open Fungo.Lexer *)

let test_func () =
  let rec loop l = match l with [] -> 0 | x :: rest -> x + loop rest in
  let x = 5 in
  loop [ x; 2; 3; 4; 5 ]

let () =
  let _ = test_func () in
  print_endline (Char.is_print '>' |> Bool.to_string)
