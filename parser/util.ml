open Ast

let var_tbl : (string, high_level) Hashtbl.t = Hashtbl.create 0;;

let add_var parent key value = 
  match parent with 
  | Func funcion -> Hashtbl.add funcion.variables key value
  | Clase clase -> Hashtbl.add clase.vars key value;;

let add_func parent key value =
  match parent with
  | Clase clase -> Hashtbl.add clase.funcs key value
  | _ -> assert false;;

let addClaseHash clase innerBloque = 
  match innerBloque with
  | Func f -> add_func clase f.name f
  | Var v -> add_var clase v.name v

let rec addClaseHashtable clase bloque =
  match clase, bloque with
    | Clase c, ClaseBloque [] -> []
    | Clase c, ClaseBloque (i :: j) -> 
      addClaseHash clase i;
      addClaseHashtable clase (ClaseBloque j);
    | _, _ -> assert false;;

let processBloqueInd upper bloque = []

let rec processBloque upper bloque =
  match upper, bloque with
    | Func f, Blo [] -> []
    | Func f, Blo (x :: y) -> 
      processBloqueInd upper x;
      processBloque upper (Blo y);
    | _, _ -> assert false;
