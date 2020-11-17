type const = 
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Char of char

type type_def = 
  | IntTy
  | FloatTy
  | CharTy
  | StringTy
  | BoolTy
  | IntCte
  | FloatCte
  | CharCte
  | StringCte
  | BoolCte
  | IntTmp
  | FloatTmp
  | CharTmp
  | StringTmp
  | BoolTmp
  | VoidTy
  | ClassTy
  | JTag
  | IntPtr
  | FloatPtr
  | CharPtr
  | StringPtr
  | BoolPtr

type quad = {
  operator: string;
  operand_left: int;
  operand_right: int;
  stored: int;
}

type count_tbl = {
  count: int;
  base: int;
}

type cte_table = {
  integer: (int, int) Hashtbl.t;
  floating: (float, int) Hashtbl.t;
  strings: (string, int) Hashtbl.t;
  characters: (char, int) Hashtbl.t;
  booleans: (bool, int) Hashtbl.t;
}

type varDeclID =
  | VDVarID of {name: string}
  | VDVarArray of {name: string; dim:int}
  | VDVar2Array of {name: string; dim1: int; dim2: int}

type exp = 
  | OrExp of { left: aExp; right: exp}
  | Exp of aExp
and aExp =
  | AndExp of { left: oExp; right: aExp}
  | AExp of oExp
and oExp =
  | GreaterT of { left: tExp; right: tExp } 
  | LessT of { left: tExp; right: tExp } 
  | GreaterE of { left: tExp; right: tExp } 
  | LessE of { left: tExp; right: tExp } 
  | Equal of { left: tExp; right: tExp } 
  | NotEqual of { left: tExp; right: tExp } 
  | OExp of tExp
and tExp = 
  | Plus of {left: term; right: tExp}
  | Mnius of {left: term; right: tExp}
  | Termino of term
and term = 
  | Times of {left: fact; right: term}
  | Div of {left: fact; right: term}
  | Factor of fact
and fact = 
  | Const of const
  | FVarId of var_id
  | FExp of tExp
and var_id = 
  | VarID of {name: string}
  | VarFuncCall of {func: string; params: exp list }
  | VarArray of {name: string; expresion: tExp}
  | Var2Array of {name: string; expresion1: tExp; expresion2: tExp}
  | VarPoint of {name: string; inner: var_id}

type varDeclExp = 
  | VDExp of exp
  | None
  | VDConst of {
    params: exp list
  }

type varDecl = 
  {
    id: varDeclID;
    right: varDeclExp;
  }

type variableDef = {
  vars : varDecl list;
  tipo : type_def;
  id_class: string;
}

type estatuto = 
  | Asigna of asignacion
  | CondIf of condIfDef
  | Escritura of exp list
  | EVar of variableDef
  | ForLoop of forLoopDef
  | WhileLoop of whileLoopDef
  | Return of exp
  | Expresion of exp
and asignacion = {
  izq: var_id;
  der: exp;
}
and forLoopDef = {
  init: var_id;
  cond: exp;
  post: asignacion;
  bloque: estatuto list;
}
and condIfDef = {
  cond: exp;
  true_block: estatuto list;
  false_block: estatuto list;
}
and whileLoopDef = {
  cond: exp;
  bloque: estatuto list;
}

type functionParams = {
  param_id: varDeclID;
  ptipo: type_def;
}

type funcionDef = {
  fname : string;
  tipo : type_def;
  params: functionParams list;
  fbloque : estatuto list;
}

type clase_bloque =
  | Fun of funcionDef
  | CVar of variableDef

type claseDef = {
  name : string;
  bloque : clase_bloque list
}

type high_level = 
  | Func of funcionDef
  | Clase of claseDef

type bloque_expresiones = Blo of estatuto list

type upper_prog = Program of high_level list