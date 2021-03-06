%{
open Printf
open Lexing
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> CHAR
%token INTTYPE FLOATTYPE CHARTYPE STRINGTYPE BOOLTYPE
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token EOL EQ GT GE LT LE NE EE AND OR
%token PRINT VAR IF ELSE READ
%token MAIN FOR WHILE RETURN
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
  FUNC MAIN LPAREN RPAREN COLON INTTYPE blo = bloque { Func {fname="main"; tipo=IntTy; fbloque=blo; params=[]; dim1=1; dim2=1} }
;

clase:
  CLASS id=ID cparent=clasePadre LBRACE cb = class_bloque RBRACE { Clase {name=id; bloque=cb; parent=cparent; } }
;

clasePadre:
  COLON id=ID { Parent id }
  | /*  */    { NoParent }


class_bloque:
  | f = funcion cb = class_bloque { (Fun f) :: cb}
  | c = constructor cb = class_bloque { (Fun c) :: cb}
  | d = decvar class_bloque { (CVar d) :: $2}
  | /* empty */ {[]}
;

funcion:
  FUNC ID LPAREN par=funcParamsRec RPAREN t = funcType d1=funcDim d2=funcDim b = bloque { {fname=$2; tipo=t; fbloque=b; params=par; dim1=d1; dim2=d2} }
  | FUNC ID LPAREN RPAREN t = funcType d1=funcDim d2=funcDim b = bloque { {fname=$2; tipo=t; fbloque=b; params=[]; dim1=d1; dim2=d2} }
  | FUNC ID LPAREN par=funcParamsRec RPAREN t = funcType d1=funcDim b = bloque { {fname=$2; tipo=t; fbloque=b; params=par; dim1=d1; dim2=1} }
  | FUNC ID LPAREN RPAREN t = funcType d1=funcDim b = bloque { {fname=$2; tipo=t; fbloque=b; params=[]; dim1=d1; dim2=1} }
  | FUNC ID LPAREN par=funcParamsRec RPAREN t = funcType b = bloque { {fname=$2; tipo=t; fbloque=b; params=par; dim1=1; dim2=1} }
  | FUNC ID LPAREN RPAREN t = funcType b = bloque { {fname=$2; tipo=t; fbloque=b; params=[]; dim1=1; dim2=1} }
  | FUNC ID LPAREN par=funcParamsRec RPAREN b = bloque { {fname=$2; tipo=VoidTy; fbloque=b; params=par; dim1=0; dim2=0} }
  | FUNC ID LPAREN RPAREN b = bloque { {fname=$2; tipo=VoidTy; fbloque=b; params=[]; dim1=0; dim2=0} }
;

funcType:
  COLON tipo { $2 }
;

funcDim:
  LBRACK i=INT RBRACK { i }

funcParamsRec:
  t=tipo id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK COMMA r=funcParamsRec { {param_id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; ptipo=t} :: r}
  | t=tipo id=ID LBRACK d1=INT RBRACK COMMA r=funcParamsRec { {param_id=VDVarArray {name=id; dim=d1;}; ptipo=t} :: r }
  | t=tipo id=ID COMMA r=funcParamsRec { {param_id=VDVarID {name=id}; ptipo=t} :: r}
  | t=tipo id=ID LBRACK d1=INT RBRACK LBRACK d2=INT RBRACK { {param_id=VDVar2Array {name=id; dim1=d1; dim2=d2;}; ptipo=t} :: []}
  | t=tipo id=ID LBRACK d1=INT RBRACK { {param_id=VDVarArray {name=id; dim=d1;}; ptipo=t}:: [] }
  | t=tipo id=ID { {param_id=VDVarID {name=id}; ptipo=t} :: []}

constructor:
  CONSTRUCTOR id=ID LPAREN param=funcParamsRec RPAREN blo=bloque { {fname=id; tipo=VoidTy; fbloque=blo; params=param; dim1=0; dim2=0} }
  | CONSTRUCTOR id=ID LPAREN RPAREN blo=bloque { {fname=id; tipo=VoidTy; fbloque=blo; params=[]; dim1=1; dim2=1} }
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
  | l = lectura { l }
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

lectura:
  READ LPAREN id=asignavar RPAREN SEMICOLON { Lectura id }
;

e1:
  e=expresion COMMA rest=e1 {e :: rest}
  | e=expresion {e :: []}
;
expresion:
  e=e_exp { Exp e }
  | LPAREN e=expresion RPAREN {e}
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
  | LPAREN e=e_exp RPAREN {e}
  | e1=expresion AND e2=expresion {AndExp {left=e1; right=e2}}
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
  | e=exp PLUS t=termino  { Plus {left=e; right=t} }
  | e=exp MINUS t=termino { Mnius {left=e; right=t} }
;
termino:
  f=factor { Factor f }
  | t=termino TIMES f=factor { Times {left=t; right=f} }
  | t=termino DIV f=factor   { Div {left=t; right=f} }
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
  | c = CHAR { Char c.[1] }
;
variable:
  id=ID LPAREN p=var1 RPAREN { VarFuncCall { func=id; params=p } }
  | id=ID LBRACK e1=exp RBRACK LBRACK e2=exp RBRACK { Var2Array {name=id; expresion1=e1; expresion2=e2}}
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