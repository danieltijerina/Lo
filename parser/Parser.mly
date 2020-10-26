%{
open Printf
open Lexing
open Ast
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
  c = clase { c }
  | f = funcion { Func f }
;

main:
  FUNC MAIN LPAREN RPAREN COLON INTTYPE blo = bloque { Func {name="main"; tipo=IntTy; bloque=blo; params=[]} }
;

clase:
  CLASS id=ID LBRACE cb = class_bloque RBRACE { Clase {name=id; bloque=cb} }
;

class_bloque:
  | f = funcion cb = class_bloque { (Fun f) :: cb}
  | c = constructor cb = class_bloque { (Fun c) :: cb}
  | d = decvar class_bloque { (CVar d) :: $2}
  | /* empty */ {[]}
;

funcion:
  FUNC ID LPAREN par=funcParamsRec RPAREN t = funcType b = bloque { {name=$2; tipo=t; bloque=b; params=par} }
  | FUNC ID LPAREN RPAREN t = funcType b = bloque { {name=$2; tipo=t; bloque=b; params=[]} }
;

funcType:
  COLON tipo { $2 }
  | /* empty */ { VoidTy }
;

funcParamsRec:
  t=tipo id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK COMMA r=funcParamsRec { {param_id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; tipo=t} :: r}
  | t=tipo id=ID LBRACK d1=INT RBRACK COMMA r=funcParamsRec { {param_id=VDVarArray {name=id; dim=d1;}; tipo=t} :: r }
  | t=tipo id=ID COMMA r=funcParamsRec { {param_id=VDVarID {name=id}; tipo=t} :: r}
  | t=tipo id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK { {param_id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; tipo=t} :: []}
  | t=tipo id=ID LBRACK d1=INT RBRACK { {param_id=VDVarArray {name=id; dim=d1;}; tipo=t}:: [] }
  | t=tipo id=ID { {param_id=VDVarID {name=id}; tipo=t} :: []}

constructor:
  CONSTRUCTOR id=ID LPAREN param=funcParamsRec RPAREN blo=bloque { {name=id; tipo=VoidTy; bloque=blo; params=param} }
  | CONSTRUCTOR id=ID LPAREN RPAREN blo=bloque { {name=id; tipo=VoidTy; bloque=blo; params=[]} }
;

tipo:
  INTTYPE { IntTy }
  | FLOATTYPE { FloatTy }
  | CHARTYPE { CharTy }
  | STRINGTYPE { StringTy }
  | BOOLTYPE { BoolTy }
;
bloque:
  LBRACE blo = b1 RBRACE { blo }
;

b1:
  est = estatuto blo = b1 {est :: blo}
  | {[]}
;
estatuto:
  a = asignacion { a }
  | c = condicion { c }
  | e = escritura { e }
  | d = decvar { EVar d }
  | f = forloop { f }
  | w = whileloop { w } 
  | e = expresion SEMICOLON { Expresion e }
  | r = return { Return r }
;
asignacion:
  var=asignavar EQ exp=expresion SEMICOLON { Asigna {izq=var; der=exp; } }
;
asignavar:
  id=ID LBRACK e1=exp RBRACK LBRACK e2=exp RBRACK { Var2Array {name=id; expresion1=e1; expresion2=e2}}
  | id=ID LBRACK e=exp RBRACK { VarArray {name= id; expresion=e;}}
  | id=ID inner=av1 { VarPoint {name=id; inner=inner } }
  | id=ID { VarID {name=id} }
;
av1: POINT v=asignavar { v }
;

escritura:
  PRINT LPAREN e1=e1 RPAREN SEMICOLON { Escritura e1 }
;
e1:
  e=expresion COMMA rest=e1 {e :: rest}
  | e=expresion {e :: []}
;
expresion:
  e=e_exp { Exp e }
  | e1=e_exp OR e2=expresion { OrExp {left=e1; right=e2} }
;
condicion:
  IF LPAREN e=expresion RPAREN b=bloque el=c1 { CondIf {cond= e; true_block=b; false_block=el } }
;
c1:
  ELSE b=bloque {b}
  | /* empty */ {[]}
;
e_exp:
  e=c_exp {AExp e}
  | e1=c_exp AND e2=e_exp {AndExp {left=e1; right=e2}}
;
c_exp:
  e=exp { OExp e }
  | e1=exp GT e2=exp { GreaterT {left=e1; right=e2} }
  | e1=exp LT e2=exp { LessT {left=e1; right=e2} }
  | e1=exp GE e2=exp { GreaterE {left=e1; right=e2} }
  | e1=exp LE e2=exp { LessE {left=e1; right=e2} }
  | e1=exp EE e2=exp { Equal {left=e1; right=e2} }
  | e1=exp NE e2=exp { NotEqual {left=e1; right=e2} }
;
exp:
  t=termino { Termino t }
  | t=termino PLUS e=exp { Plus {left=t; right=e} }
  | t=termino MINUS e=exp { Mnius {left=t; right=e} }
;
termino:
  f=factor { Factor f }
  | f=factor TIMES t=termino { Times {left=f; right=t} }
  | f=factor DIV t=termino { Div {left=f; right=t} }
;
factor:
  c = varcte {Const c}
  | v=variable {FVarId v}
  | LPAREN e=exp RPAREN {FExp e}
;
varcte:
  i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
;
variable:
  id=ID LPAREN p=var1 RPAREN { VarFuncCall { func=id; params=p } }
  | id=ID LBRACK e=exp RBRACK { VarArray {name=id; expresion=e;} }
  | id=ID POINT i=variable { VarPoint {name=id; inner=i} }
  | id=ID { VarID {name=id} }
; 
var1:
  e=expresion r=var2 {e :: r}
  | /* empty */ {[]}
;
var2:
  COMMA e=expresion r=var2 {e :: r}
  | /* empty */ {[]}
;

decvar:
  t=tipo v=decvarRec SEMICOLON { {vars=v; tipo=t; id_class=""}  }
  | classId=ID v=decvarRec SEMICOLON { {vars=v; tipo=ClassTy; id_class=classId} }
  | classId=ID id=ID LPAREN par=var1 RPAREN SEMICOLON { {vars= ({id=VDVarID {name=id}; right=VDConst {params=par }})::[]; tipo=ClassTy; id_class=classId}}

decvarRec:
  id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK e=decvarExp COMMA r=decvarRec { {id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; right=e} :: r}
  | id=ID LBRACK d1=INT RBRACK e=decvarExp COMMA r=decvarRec { { id=VDVarArray {name=id; dim=d1;}; right=e} :: r }
  | id=ID e=decvarExp COMMA r=decvarRec { {id=VDVarID {name=id}; right=e} :: r}
  | id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK e=decvarExp { {id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; right=e} :: []}
  | id=ID LBRACK d1=INT RBRACK e=decvarExp { { id=VDVarArray {name=id; dim=d1;}; right=e} :: [] }
  | id=ID e=decvarExp { {id=VDVarID {name=id}; right=e} :: []}

decvarExp:
  EQ e=expresion { VDExp e }
  |  { None }

forloop:
  FOR LPAREN init=asignavar SEMICOLON cond=expresion SEMICOLON as1=asignavar EQ as2=expresion RPAREN b=bloque {
    ForLoop {init=init; cond=cond; post=({izq=as1; der=as2}); bloque=b }
  }
;

whileloop:
  WHILE LPAREN e=expresion RPAREN b=bloque { WhileLoop {cond=e; bloque=b} }
;

return:
  RETURN e=expresion SEMICOLON  { e }
;