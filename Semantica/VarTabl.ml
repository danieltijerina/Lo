
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

add_element has_tab "main" (Clase {name="nombre"; tipo="int"; dep=Hashtbl.create 0;});;
let x = Hashtbl.find has_tab "main" in 
  print_string (get_element x);;

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