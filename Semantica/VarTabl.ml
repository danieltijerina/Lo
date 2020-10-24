
type variable = {
  name : string;
  tipo : string;
}

type funcion = {
  name : string;
  tipo : string;
  variables: (string, variable) Hashtbl.t;
}

type var_y_func = 
  | Variable of variable
  | Function of funcion

type clase = {
  name : string;
  tipo : string;
  funcs : (string, funcion) Hashtbl.t;
  vars : (string, variable) Hashtbl.t;
  dep : (string, var_y_func) Hashtbl.t;
}

type high_level = 
  | Clase of clase
  | Func of funcion


let add_element tbl x y = 
  Hashtbl.add tbl x y;;

let has_tab = Hashtbl.create 1234;;

let get_element x =
  match x with
  | Clase clase -> clase.name
  | Func funcion -> funcion.name;;

let add_var parent key value = 
  match parent with 
  | Func funcion -> Hashtbl.add funcion.variables key value
  | Clase clase -> Hashtbl.add clase.vars key value;;

let find_var parent key =
  match parent with
  | Func funcion -> Hashtbl.find funcion.variables key
  | Clase clase -> Hashtbl.find clase.vars key;;

let add_func parent key value =
  match parent with
  | Clase clase -> Hashtbl.add clase.funcs key value
  | _ -> assert false;;

let find_func parent key =
  match parent with
  | Clase clase -> Hashtbl.find clase.funcs key
  | _ -> assert false;;

add_element has_tab "figura" (Clase {name="figura"; tipo="int"; funcs=Hashtbl.create 123; vars=Hashtbl.create 123; dep=Hashtbl.create 123;});;
(*let x = Hashtbl.find has_tab "main" in 
  print_endline (get_element x);;*)

add_var (Hashtbl.find has_tab "figura") "i" {name="i"; tipo="float";};;
let var_name = find_var (Hashtbl.find has_tab "figura") "i";;
print_string var_name.name; print_string " "; print_endline var_name.tipo;;

add_func (Hashtbl.find has_tab "figura") "getColor" {name="getColor"; tipo="string"; variables=Hashtbl.create 123;};;
print_endline (find_func (Hashtbl.find has_tab "figura") "getColor").tipo;;

(* 
type var_tbl = {
  name : string;
  tipo: string;
  scope: (string, var_tbl) Hashtbl.t;
}

let add_element tbl x y = 
  Hashtbl.add tbl x y;;

let has_tab = Hashtbl.create 1234;;

(* Agregando la funcion de main *)
add_element has_tab "main" ({name="nombre"; tipo="int"; scope=Hashtbl.create 0;});;
(* Agregando la funcion de foo *)
add_element has_tab "foo" ({name="nombre"; tipo="int"; scope=Hashtbl.create 0;});;

(* Agregando la variable de size_of_square in foo *)
let y = Hashtbl.find has_tab "foo" in 
  add_element y.scope "size_of_square" ({name="size_of_square"; tipo="bool"; scope=Hashtbl.create 0;});;

(* Checando la variable de size_of_square *)
let x = Hashtbl.find has_tab "foo" in 
  let y = Hashtbl.find x.scope "size_of_square" in
    print_string y.name;;

    *)