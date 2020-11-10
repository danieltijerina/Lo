open Ast

type variable = {
  name : string;
  tipo : type_def;
  id_class: string;
  address : int;
}

type funcion_tbl = {
  name : string;
  ftipo : type_def;
  variables: (string, variable) Hashtbl.t;
  params: variable list;
}

type clase_tbl = {
  name : string;
  funcs : (string, funcion_tbl) Hashtbl.t;
  vars : (string, variable) Hashtbl.t;
}

type high_level = 
  | ClaseT of clase_tbl
  | FuncT of funcion_tbl

let update_count tbl key = 
  Hashtbl.replace tbl key {count=(Hashtbl.find tbl key).count + 1; base=(Hashtbl.find tbl key).base}

let add_element tbl key value = 
  Hashtbl.add tbl key value;
  value;;

let rec getVariablesFromParamsRec params = 
  match params with
  | [] -> []
  | (f::fs) -> getVariablesFromParams f :: (getVariablesFromParamsRec fs)
and getVariablesFromParams param =
  match param.param_id with
  | VDVarID vid -> {name=vid.name; tipo=param.tipo; id_class=""; address=0;};
  | VDVarArray vid -> {name=vid.name; tipo=param.tipo; id_class=""; address=0;};
  | VDVar2Array vid -> {name=vid.name; tipo=param.tipo; id_class=""; address=0;};;

let add_high_level_element tbl value =
  match value with
  | Func f -> add_element tbl f.fname (FuncT {name=f.fname; ftipo=f.tipo; variables=Hashtbl.create 0; params=(getVariablesFromParamsRec f.params);})
  | Clase c -> add_element tbl c.name (ClaseT {name=c.name; funcs=Hashtbl.create 0; vars=Hashtbl.create 0})

let get_element x =
  match x with
  | ClaseT clase -> clase.name
  | FuncT funcion -> funcion.name;;

let add_var parent key value = 
  match parent with 
  | FuncT funcion -> Hashtbl.add funcion.variables key value
  | ClaseT clase -> Hashtbl.add clase.vars key value;;

let find_var parent key =
  match parent with
  | FuncT funcion -> Hashtbl.find funcion.variables key
  | ClaseT clase -> Hashtbl.find clase.vars key;;

let add_func parent key value =
  match parent with
  | ClaseT clase -> Hashtbl.add clase.funcs key value
  | _ -> assert false;;

let find_func parent key =
  match parent with
  | ClaseT clase -> Hashtbl.find clase.funcs key
  | _ -> assert false;;



(* 
add_element has_tab "figura" (Clase {name="figura"; tipo="int"; funcs=Hashtbl.create 123; vars=Hashtbl.create 123; dep=Hashtbl.create 123;});;
(*let x = Hashtbl.find has_tab "main" in 
  print_endline (get_element x);;*)

add_var (Hashtbl.find has_tab "figura") "i" {name="i"; tipo="float";};;
let var_name = find_var (Hashtbl.find has_tab "figura") "i";;
print_string var_name.name; print_string " "; print_endline var_name.tipo;;

add_func (Hashtbl.find has_tab "figura") "getColor" {name="getColor"; tipo="string"; variables=Hashtbl.create 123;};;
print_endline (find_func (Hashtbl.find has_tab "figura") "getColor").tipo;;
*)