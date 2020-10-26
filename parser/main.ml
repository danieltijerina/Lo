open Backend
open Ast

let _ =
  let in_channel = open_in "ejemploSenc.lo" in
  try
    let lexbuf = Lexing.from_channel in_channel in
    while true do
      let result = Parser.init Lexer.token lexbuf in
        back_main result;
        (* print_string result; print_newline(); flush stdout *)
    done
  with Lexer.Eof ->
    exit 0