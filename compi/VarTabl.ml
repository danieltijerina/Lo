(*
  VarTabl
  Modulo encargado de inicializar tablas de variables asi como hacer operaciones sobre ellas.
*)

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
  classInit: string;
  fdim1: int;
  fdim2: int;
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
  Hashtbl.add tbl BoolTy {count=0; base=5000};
  Hashtbl.add tbl IntCte {count=0; base=10000};
  Hashtbl.add tbl FloatCte {count=0; base=11000};
  Hashtbl.add tbl StringCte {count=0; base=12000};
  Hashtbl.add tbl CharCte {count=0; base=13000};
  Hashtbl.add tbl BoolCte {count=0; base=14000};;

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
      let variable_ = begin match param.param_id with
      | VDVarID vid -> (update_count fun_var_count param.ptipo; {name=vid.name; tipo=param.ptipo; dimension1=1; dimension2=1; id_class=""; address=addr_int;};)
      | VDVarArray vid -> (update_n_count fun_var_count param.ptipo vid.dim; {name=vid.name; tipo=param.ptipo; dimension1=vid.dim; dimension2=1; id_class=""; address=addr_int;};)
      | VDVar2Array vid -> (update_n_count fun_var_count param.ptipo (vid.dim1 * vid.dim2); {name=vid.name; tipo=param.ptipo; dimension1=vid.dim1; dimension2=vid.dim2; id_class=""; address=addr_int;};)
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

let copy_class_var_count to_count from_count_name class_mem = 
  let from_count = Hashtbl.find class_mem from_count_name in 
    Hashtbl.replace to_count IntTy {count=(Hashtbl.find from_count IntTy).count; base=(Hashtbl.find from_count IntTy).base};
    Hashtbl.replace to_count FloatTy {count=(Hashtbl.find from_count FloatTy).count; base=(Hashtbl.find from_count FloatTy).base};
    Hashtbl.replace to_count StringTy {count=(Hashtbl.find from_count StringTy).count; base=(Hashtbl.find from_count StringTy).base};
    Hashtbl.replace to_count CharTy {count=(Hashtbl.find from_count CharTy).count; base=(Hashtbl.find from_count CharTy).base};
    Hashtbl.replace to_count BoolTy {count=(Hashtbl.find from_count BoolTy).count; base=(Hashtbl.find from_count BoolTy).base};;

let rec copy_class_var_tbl_rec to_var_tbl from_var_tbl = 
  Hashtbl.iter (copy_class_var_tbl to_var_tbl) from_var_tbl;
and copy_class_var_tbl to_var_tbl k content =
  Hashtbl.add to_var_tbl k content;;

let rec copy_class_func_tbl_rec to_func_tbl from_func_tbl = 
  Hashtbl.iter (copy_class_func_tbl to_func_tbl) from_func_tbl;
and copy_class_func_tbl to_func_tbl k content = 
  Hashtbl.add to_func_tbl k content;;

let checkClassParent clase main_tbl var_count var_tbl func_tbl class_mem = 
  match clase.parent with 
  | NoParent -> ();
  | Parent p -> try 
                let cParent = Hashtbl.find main_tbl p in 
                match cParent with 
                | FuncT f -> failwith "Parent is a function";
                | ClaseT c -> copy_class_var_count var_count c.name class_mem;
                              copy_class_var_tbl_rec var_tbl c.vars;
                              copy_class_func_tbl_rec func_tbl c.funcs;
                              ();
                with Not_found -> failwith "Clase padre no existe";;

let add_high_level_element tbl value mem class_mem =
  match value with
  | Func f -> let fun_var_count = Hashtbl.create 0 in 
                initialize_count fun_var_count;
                let vars_tbl = Hashtbl.create 0 in 
                let new_element = add_element tbl f.fname (FuncT {name=f.fname; ftipo=f.tipo; variables=vars_tbl; params=(getVariablesFromParamsRec f.params fun_var_count vars_tbl); classInit=""; fdim1=f.dim1; fdim2=f.dim2}) in
                Hashtbl.add mem f.fname fun_var_count;
                new_element
  | Clase c -> let class_var_count = Hashtbl.create 0 in
                initialize_count_class class_var_count;
                let vars_tbl = Hashtbl.create 0 in 
                let funcs_tbl = Hashtbl.create 0 in 
                checkClassParent c tbl class_var_count vars_tbl funcs_tbl class_mem;
                let new_class = add_element tbl c.name (ClaseT {name=c.name; funcs=funcs_tbl; vars=vars_tbl}) in
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