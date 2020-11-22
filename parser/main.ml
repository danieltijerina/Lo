open Backend
open Ast
open VarTabl
open Format
open Printf

let print_type t = 
  match t with
  | IntTy -> "int"
  | FloatTy -> "float"
  | CharTy -> "char"
  | StringTy -> "string"
  | BoolTy -> "bool"
  | VoidTy -> "void"
  | ClassTy -> "class"
  | _ -> "some constant"

let test_result tbl = 
  let mainf = Hashtbl.find tbl "Figura" in
    match mainf with
    | ClaseT c -> printf "%s\n" (print_type (Hashtbl.find c.vars "test1").tipo);
    | _ -> assert false;;

(*
let test_result tbl = 
  let mainf = Hashtbl.find tbl "main" in
    match mainf with
    | FuncT f -> print_endline (print_type f.tipo);
    | _ -> assert false;;
*)



let _ =
  let filename = (match String.split_on_char '.' Sys.argv.(1) with | x :: y :: [] -> x | _ -> "out") in 
    let in_channel = open_in Sys.argv.(1) in
    try
      let lexbuf = Lexing.from_channel in_channel in
      while true do
        let oc = open_out (String.concat "." [filename; "clo"]) in
          let parse_tree = Parser.init Lexer.token lexbuf in
            semantic_start parse_tree oc;
          (* print_string result; print_newline(); flush stdout *)
      done
  with Lexer.Eof ->
    exit 0