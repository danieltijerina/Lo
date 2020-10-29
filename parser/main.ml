open Backend
open Ast
open VarTabl
open Format

let print_type t = 
  match t with
  | IntTy -> "int"
  | FloatTy -> "float"
  | CharTy -> "char"
  | StringTy -> "string"
  | BoolTy -> "bool"
  | VoidTy -> "void"
  | ClassTy -> "class"

let test_result tbl = 
  let mainf = Hashtbl.find tbl "Figura" in
    match mainf with
    | ClaseT c -> printf "%d\n" (Hashtbl.length (Hashtbl.find c.funcs "test").variables);
    | _ -> assert false;; 

(*
let test_result tbl = 
  let mainf = Hashtbl.find tbl "main" in
    match mainf with
    | FuncT f -> print_endline (print_type f.tipo);
    | _ -> assert false;;
*)

let _ =
  let in_channel = open_in "ejemploSenc.lo" in
  try
    let lexbuf = Lexing.from_channel in_channel in
    while true do
      let result = Parser.init Lexer.token lexbuf in
        test_result (semantic_start result);
        (* print_string result; print_newline(); flush stdout *)
    done
  with Lexer.Eof ->
    exit 0