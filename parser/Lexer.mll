{
open Parser
exception Eof
}
rule token = parse
  [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['0'-'9']+ '.' ['0'-'9']+ as fxm {FLOAT(float_of_string fxm)}
  | '"' ['a'-'z' 'A'-'Z' '0'-'9' ' ']+ '"' { STRING } (* maybe we should include special characters *)
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | '='            { EQ }
  | '>'            { GT }
  | '<'            { LT }
  | ">="           { GE }
  | "<="           { LE }
  | "=="           { EE }
  | "!="           { NE }
  | "&&"           { AND }
  | "||"           { OR }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | ':'            { COLON }
  | '.'            { POINT }
  | "func"         { FUNC } 
  | "class"        { CLASS } 
  | "constructor"  { CONSTRUCTOR } 
  | "main"         { MAIN }
  | "print"        { PRINT }
  | "var"          { VAR }
  | "if"           { IF }
  | "else"         { ELSE }
  | "float"        { FLOATTYPE }
  | "int"          { INTTYPE }
  | "char"         { CHARTYPE }
  | "string"       { STRINGTYPE }
  | "bool"         { BOOLTYPE }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "for"          { FOR }
  | "while"        { WHILE }
  | "return"       { RETURN }
  | "in"           { IN }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*     { ID }
  | eof            { raise Eof }