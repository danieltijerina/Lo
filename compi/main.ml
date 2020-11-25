open Backend
open Ast
open VarTabl
open Format
open Printf

let _ =
  let filename = (match String.split_on_char '.' Sys.argv.(1) with | x :: y :: [] -> x | _ -> "out") in 
    let line_counter = ref 0 in
      let in_channel = open_in Sys.argv.(1) in
      try
        let lexbuf = Lexing.from_channel in_channel in
        while true do
          let oc = open_out (String.concat "." [filename; "clo"]) in
            let parse_tree = Parser.init Lexer.token lexbuf in
              semantic_start parse_tree oc;
        done
      with 
        | Lexer.Eof -> exit 0
        | _ -> print_endline "Syntax error on file" ; exit 0;