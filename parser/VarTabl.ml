open Ast

type variable = {
  name : string;
  tipo : type_def;
  dimension1 : int;
  dimension2 : int;
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


let initialize_count tbl =
  Hashtbl.add tbl JTag {count=0; base=0};
  Hashtbl.add tbl IntTy {count=0; base=1000};
  Hashtbl.add tbl FloatTy {count=0; base=2000};
  Hashtbl.add tbl StringTy {count=0; base=3000};
  Hashtbl.add tbl CharTy {count=0; base=4000};
  Hashtbl.add tbl BoolTy {count=0; base=5000};
  Hashtbl.add tbl IntCte {count=0; base=10000};
  Hashtbl.add tbl FloatCte {count=0; base=11000};
  Hashtbl.add tbl StringCte {count=0; base=12000};
  Hashtbl.add tbl CharCte {count=0; base=13000};
  Hashtbl.add tbl BoolCte {count=0; base=14000};
  Hashtbl.add tbl IntTmp {count=0; base=20000};
  Hashtbl.add tbl FloatTmp {count=0; base=21000};
  Hashtbl.add tbl StringTmp {count=0; base=22000};
  Hashtbl.add tbl CharTmp {count=0; base=23000};
  Hashtbl.add tbl BoolTmp {count=0; base=24000};
  Hashtbl.add tbl ClassTy {count=0; base=30000};
  Hashtbl.add tbl IntPtr {count=0; base=40000};
  Hashtbl.add tbl FloatPtr {count=0; base=41000};
  Hashtbl.add tbl StringPtr {count=0; base=42000};
  Hashtbl.add tbl CharPtr {count=0; base=43000};
  Hashtbl.add tbl BoolPtr {count=0; base=44000};;

let initialize_count_class tbl = 
  Hashtbl.add tbl IntTy {count=0; base=1000};
  Hashtbl.add tbl FloatTy {count=0; base=2000};
  Hashtbl.add tbl StringTy {count=0; base=3000};
  Hashtbl.add tbl CharTy {count=0; base=4000};
  Hashtbl.add tbl BoolTy {count=0; base=5000};;

let update_count tbl key = 
  Hashtbl.replace tbl key {count=(Hashtbl.find tbl key).count + 1; base=(Hashtbl.find tbl key).base};;

let update_n_count tbl key n =
  Hashtbl.replace tbl key {count=(Hashtbl.find tbl key).count + n; base=(Hashtbl.find tbl key).base}

let add_element tbl key value = 
  try 
  Hashtbl.find tbl key;
  failwith "name already exists";
  with Not_found -> 
  (Hashtbl.add tbl key value;
  value);;

let rec getVariablesFromParamsRec params fun_var_count vars_tbl= 
  match params with
  | [] -> []
  | (f::fs) -> getVariablesFromParams f fun_var_count vars_tbl :: (getVariablesFromParamsRec fs fun_var_count vars_tbl)
and getVariablesFromParams param fun_var_count vars_tbl=
  let new_addr = Hashtbl.find fun_var_count param.ptipo in 
    let addr_int = new_addr.base + new_addr.count in 
      update_count fun_var_count param.ptipo;
      let variable_ = begin match param.param_id with
      | VDVarID vid -> ({name=vid.name; tipo=param.ptipo; dimension1=1; dimension2=1; id_class=""; address=addr_int;};)
      | VDVarArray vid -> ({name=vid.name; tipo=param.ptipo; dimension1=vid.dim; dimension2=1; id_class=""; address=addr_int;};)
      | VDVar2Array vid -> ({name=vid.name; tipo=param.ptipo; dimension1=vid.dim1; dimension2=vid.dim2; id_class=""; address=addr_int;};)
      end in 
      Hashtbl.add vars_tbl variable_.name variable_;
      variable_;;


let rec addParams elem =
  match elem with
  | FuncT func -> addParamsToVartblRec func.params func.variables
  | _ -> failwith "Cannot add params to something that is not a function"
and addParamsToVartblRec params table =
  match params with
  | [] -> []
  | (p::ps) -> addParamsToVartbl p table :: (addParamsToVartblRec ps table)
and addParamsToVartbl param table =
  Hashtbl.add table param.name param;;

let add_high_level_element tbl value mem class_mem =
  match value with
  | Func f -> let fun_var_count = Hashtbl.create 0 in 
                initialize_count fun_var_count;
                let vars_tbl = Hashtbl.create 0 in 
                let new_element = add_element tbl f.fname (FuncT {name=f.fname; ftipo=f.tipo; variables=vars_tbl; params=(getVariablesFromParamsRec f.params fun_var_count vars_tbl);}) in
                Hashtbl.add mem f.fname fun_var_count;
                new_element
  (* | Func f -> add_element tbl f.fname (FuncT {name=f.fname; ftipo=f.tipo; variables=Hashtbl.create 0; params=(getVariablesFromParamsRec f.params);}) *)
  | Clase c -> let class_var_count = Hashtbl.create 0 in
                initialize_count_class class_var_count;
                let new_class = add_element tbl c.name (ClaseT {name=c.name; funcs=Hashtbl.create 0; vars=Hashtbl.create 0}) in
                Hashtbl.add class_mem c.name class_var_count;
                new_class;;

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