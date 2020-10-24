%{
open Printf
open Lexing

type variable = {
  name : string;
  tipo : string;
}

type funcion = {
  name : string;
  tipo : string;
  variables: (string, variable) Hashtbl.t;
}

type clase = {
  name : string;
  tipo : string;
  funcs : (string, funcion) Hashtbl.t;
  vars : (string, variable) Hashtbl.t;
}

type high_level = 
  | Clase of clase
  | Func of funcion

type op = Plus | Minus | Times | Div | LT | GT | LE | GE | NE | EE | And | Or;;
type ttype = Int | Float | Char | String | Bool

let var_table = Hashtbl.create 123
%}
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token INTTYPE FLOATTYPE CHARTYPE STRINGTYPE BOOLTYPE
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token EOL EQ GT GE LT LE NE EE AND OR
%token PRINT VAR IF ELSE
%token MAIN FOR WHILE RETURN IN
%token FUNC CLASS CONSTRUCTOR
%token COMMA COLON SEMICOLON POINT
%token <string> ID
%left PLUS MINUS
%left TIMES DIV 
%start init
%type <string> init
%%
init:
  b main {"adecuado"}
  | main {"adecuado"}
;
b:
  clase {}
  | funcion {}
  | b clase {}
  | b funcion {}
;

main:
  FUNC MAIN LPAREN RPAREN COLON INTTYPE bloque {}
;

clase:
  CLASS ID class_bloque {print_string "class "; print_endline $2}
;
class_bloque:
  LBRACE cb RBRACE {}
;
cb:
  funcion {}
  | constructor {}
  | decvar {}
  | funcion cb {}
  | constructor cb {}
  | decvar cb {}
  | /* empty */ {}
;

funcion:
  FUNC ID LPAREN f1 RPAREN f2 bloque {print_string "func w params "; print_endline $2}
  | FUNC ID LPAREN RPAREN f2 bloque {print_string "func w/o params "; print_endline $2}
;
f1:
  tipo ID COMMA f1 {print_string $1; print_string " "; print_endline $2}
  | tipo ID {print_string $1; print_string " "; print_endline $2}
;
f2:
  COLON tipo {}
  | /* empty */ {}
;
constructor:
  CONSTRUCTOR ID LPAREN f1 RPAREN bloque {print_string "cons w params "; print_endline $2}
  | CONSTRUCTOR ID LPAREN RPAREN bloque {print_string "cons w/o params "; print_endline $2}
;

tipo:
  INTTYPE {"int"}
  | FLOATTYPE {"float"}
  | CHARTYPE {"char"}
  | STRINGTYPE {"string"}
  | BOOLTYPE {"bool"}
;
bloque:
  LBRACE b1 RBRACE {}
  | LBRACE RBRACE {}
;
b1:
  estatuto b1 {}
  | estatuto  {}
;
estatuto:
  asignacion {}
  | condicion {}
  | escritura {}
  | decvar {}
  | forloop {}
  | whileloop {}
  | expresion SEMICOLON {}
  | return {}
;
asignacion:
  asignavar EQ expresion SEMICOLON {}
;
asignavar:
  ID LBRACK exp RBRACK LBRACK exp RBRACK {}
  | ID LBRACK exp RBRACK {}
  | ID av1 {}
  | ID {}
;
av1:
  POINT ID av2 {}
;
av2:
  POINT ID av2 {}
  | /* empty */ {}
;
escritura:
  PRINT LPAREN e1 RPAREN SEMICOLON {}
;
e1:
  expresion COMMA e1 {}
  | expresion {}
;
expresion:
  e_exp {}
  | e_exp OR expresion {}
;
condicion:
  IF LPAREN expresion RPAREN bloque c1 {}
;
c1:
  ELSE bloque {}
  | /* empty */ {}
;
e_exp:
  c_exp {}
  | c_exp AND e_exp {}
;
c_exp:
  exp {}
  | exp GT exp {}
  | exp LT exp {}
  | exp GE exp {}
  | exp LE exp {}
  | exp EE exp {}
  | exp NE exp {}
;
exp:
  termino {}
  | termino PLUS exp {}
  | termino MINUS exp {}
;
termino:
  factor {}
  | factor TIMES termino {}
  | factor DIV termino {}
;
factor:
  varcte {}
  | variable {}
  | LPAREN exp RPAREN {}
;
varcte:
  INT {}
  | FLOAT {}
  | STRING {}
  | TRUE {}
  | FALSE {}
;
variable:
  ID LPAREN var1 RPAREN {}
  | ID LBRACK exp RBRACK {}
  | ID POINT variable {}
  | ID {}
;
var1:
  expresion var2 {}
  | ID var2 {}
  | /* empty */ {}
;
var2:
  COMMA expresion var2 {}
  | COMMA ID var2 {}
  | /* empty */ {}
;

decvar:
  tipo ID decvar1 decvar2 SEMICOLON {}
  | ID ID decvar1 decvar2 SEMICOLON {}
  | ID ID LPAREN var1 RPAREN SEMICOLON {}
;

decvar1:
  LBRACK INT RBRACK          {}
  | EQ exp                   {}
  | /*  empty */             {}
;

decvar2:
  COMMA ID decvar1 decvar2   {}
  | /* empty */              {}
;

forloop:
  FOR LPAREN asignavar SEMICOLON expresion SEMICOLON asignavar EQ expresion RPAREN bloque {}
;

whileloop:
  WHILE LPAREN expresion RPAREN bloque {}
;

return:
  RETURN expresion SEMICOLON  {}
;