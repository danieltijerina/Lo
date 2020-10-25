%{
open Printf
open Lexing
open Ast
open Util
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
%type <upper_prog> init
%%
init:
  b init {
    let Program initial = $2 in Program ($1 :: initial)}
  | main {Program ($1 :: [])}
;
b:
  clase { $1 }
  | funcion { Func {name="main"; tipo="int"; variables=Hashtbl.create 0} }
;

main:
  FUNC MAIN LPAREN RPAREN COLON INTTYPE bloque { Func {name="main"; tipo="int"; variables=Hashtbl.create 0} }
;

clase:
  CLASS ID LBRACE class_bloque RBRACE {  
    let clase = Clase {name=$2; tipo="int"; funcs=Hashtbl.create 123; vars=Hashtbl.create 0} in 
      addClaseHashtable clase (ClaseBloque $4);
      clase }
;

class_bloque:
  | funcion class_bloque {$1 :: $2}
  | constructor class_bloque {Func {name="main"; tipo="int"; variables=Hashtbl.create 0} :: $2}
  | decvar class_bloque {Var {name="main"; tipo="int"} :: $2}
  | /* empty */ {[]}
;

funcion:
  FUNC ID LPAREN f1 RPAREN f2 blo = bloque { 
      let funct = Func {name=$2; tipo="int"; variables=Hashtbl.create 0} in
      processBloque funct (Blo blo);
      funct
    }
  | FUNC ID LPAREN RPAREN f2 bloque {Func {name=$2; tipo="int"; variables=Hashtbl.create 0}}
;
f1:
  tipo ID COMMA f1 { print_string " "; print_endline $2}
  | tipo ID { print_string " "; print_endline $2}
;
f2:
  COLON tipo { $2 }
  | /* empty */ { VoidTy }
;
constructor:
  CONSTRUCTOR ID LPAREN f1 RPAREN bloque {print_string "cons w params "; print_endline $2}
  | CONSTRUCTOR ID LPAREN RPAREN bloque {print_string "cons w/o params "; print_endline $2}
;

tipo:
  INTTYPE { IntTy }
  | FLOATTYPE { FloatTy }
  | CHARTYPE { CharTy }
  | STRINGTYPE { StringTy }
  | BOOLTYPE { BoolTy }
;
bloque:
  LBRACE b1 RBRACE {$2}
;
b1:
  estatuto b1 {$1 :: $2}
  | {[]}
;
estatuto:
  asignacion { Assigna }
  | condicion { CondIf }
  | escritura { Escritura }
  | decvar { Declaracion }
  | forloop { ForLoop }
  | whileloop { WhileLoop } 
  | expresion SEMICOLON { Expresion }
  | return { Return }
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