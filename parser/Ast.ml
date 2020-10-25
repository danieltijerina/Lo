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
  (* dep : (string, var_y_func) Hashtbl.t; *)
}

type high_level = 
  | Func of funcion
  | Clase of clase
  | Var of variable

type bloque_clase = ClaseBloque of high_level list

type type_def = 
  | IntTy
  | FloatTy
  | CharTy
  | StringTy
  | BoolTy
  | VoidTy

type const = 
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Char of char

type expresion = 
  | OrExp

type estatuto = 
  | Assigna
  | CondIf
  | Escritura
  | Declaracion
  | ForLoop
  | WhileLoop
  | Return
  | Expresion

type bloque_expresiones = Blo of estatuto list

type upper_prog = Program of high_level list