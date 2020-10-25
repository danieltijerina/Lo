open Ast

let upper_prog elem = 
  match elem with
  | Clase c -> print_endline c.name;
  | Func f -> print_endline f.name;;

let rec back_main tree =
  match tree with 
  | Program [] -> print_string "end"
  | Program (i :: j) ->
      upper_prog i;
      back_main (Program j)