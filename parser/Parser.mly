%token <int> INT
%token <float> FLOAT
%token INTTYPE FLOATTYPE CHARTYPE STRINGTYPE BOOLTYPE
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token EOL EQ GT GE LT LE NE EE AND OR
%token PRINT VAR IF ELSE
%token MAIN FOR WHILE RETURN IN
%token FUNC CLASS CONSTRUCTOR
%token COMMA COLON SEMICOLON POINT
%token ID STRING
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
  clase {[]}
  | funcion {[]}
  | b clase {[]}
  | b funcion {[]}
;

main:
  FUNC MAIN LPAREN RPAREN COLON INTTYPE bloque {[]}
;

clase:
  CLASS ID class_bloque {[]}
;
class_bloque:
  LBRACE cb RBRACE {[]}
;
cb:
  funcion {[]}
  | constructor {[]}
  | decvar {[]}
  | funcion cb {[]}
  | constructor cb {[]}
  | decvar cb {[]}
  | /* empty */ {[]}
;

funcion:
  FUNC ID LPAREN f1 RPAREN f2 bloque {[]}
  | FUNC ID LPAREN RPAREN f2 bloque {[]}
;
f1:
  tipo ID COMMA f3 {[]}
  | tipo ID {[]}
;
f2:
  COLON tipo {[]}
  | /* empty */ {[]}
;
f3:
  tipo ID COMMA f3 {[]}
  | tipo ID {[]}
;
constructor:
  CONSTRUCTOR ID LPAREN f1 RPAREN bloque {[]}
  | CONSTRUCTOR ID LPAREN RPAREN bloque {[]}
;

tipo:
  INTTYPE {[]}
  | FLOATTYPE {[]}
  | CHARTYPE {[]}
  | STRINGTYPE {[]}
  | BOOLTYPE {[]}
;
bloque:
  LBRACE b1 RBRACE {[]}
  | LBRACE RBRACE {[]}
;
b1:
  estatuto b1 {[]}
  | estatuto  {[]}
;
estatuto:
  asignacion {[]}
  | condicion {[]}
  | escritura {[]}
  | decvar {[]}
  | forloop {[]}
  | whileloop {[]}
  | expresion SEMICOLON {[]}
  | return {[]}
;
asignacion:
  asignavar EQ expresion SEMICOLON {[]}
;
asignavar:
  ID LBRACK exp RBRACK LBRACK exp RBRACK {[]}
  | ID LBRACK exp RBRACK {[]}
  | ID av1 {[]}
  | ID {[]}
;
av1:
  POINT ID av2 {[]}
;
av2:
  POINT ID av2 {[]}
  | /* empty */ {[]}
;
escritura:
  PRINT LPAREN e1 RPAREN SEMICOLON {[]}
;
e1:
  expresion COMMA e1 {[]}
  | expresion {[]}
;
expresion:
  e_exp {[]}
  | e_exp OR expresion {[]}
;
condicion:
  IF LPAREN expresion RPAREN bloque c1 {[]}
;
c1:
  ELSE bloque {[]}
  | /* empty */ {[]}
;
e_exp:
  c_exp {[]}
  | c_exp AND e_exp {[]}
;
c_exp:
  exp {[]}
  | exp GT exp {[]}
  | exp LT exp {[]}
  | exp GE exp {[]}
  | exp LE exp {[]}
  | exp EE exp {[]}
  | exp NE exp {[]}
;
exp:
  termino {[]}
  | termino PLUS exp {[]}
  | termino MINUS exp {[]}
;
termino:
  factor {[]}
  | factor TIMES termino {[]}
  | factor DIV termino {[]}
;
factor:
  varcte {[]}
  | variable {[]}
  | LPAREN exp RPAREN {[]}
;
varcte:
  INT {[]}
  | FLOAT {[]}
  | STRING {[]}
  | TRUE {[]}
  | FALSE {[]}
;
variable:
  ID LPAREN var1 RPAREN {[]}
  | ID LBRACK exp RBRACK {[]}
  | ID POINT variable {[]}
  | ID {[]}
;
var1:
  expresion var2 {[]}
  | ID var2 {[]}
  | /* empty */ {[]}
;
var2:
  COMMA expresion var2 {[]}
  | COMMA ID var2 {[]}
  | /* empty */ {[]}
;

decvar:
  tipo ID decvar1 decvar2 SEMICOLON {[]}
  | ID ID decvar1 decvar2 SEMICOLON {[]}
  | ID ID LPAREN var1 RPAREN SEMICOLON {[]}
;

decvar1:
  LBRACK INT RBRACK          {[]}
  | EQ exp                   {[]}
  | /*  empty */             {[]}
;

decvar2:
  COMMA ID decvar1 decvar2   {[]}
  | /* empty */              {[]}
;

forloop:
  FOR LPAREN asignavar SEMICOLON expresion SEMICOLON asignavar EQ expresion RPAREN bloque {[]}
;

whileloop:
  WHILE LPAREN expresion RPAREN bloque {[]}
;

return:
  RETURN expresion SEMICOLON  {[]}
;