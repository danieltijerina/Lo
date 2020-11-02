open Ast
open VarTabl
open ExpValidator
open Util

let rec process_VarId var = 
  match var with 
  | VarID vid -> print_string vid.name;
  | VarFuncCall vfun -> ()
  | VarArray varray -> print_string varray.name; print_string "[1]"; 
  | Var2Array varray2 -> print_string varray2.name; print_string "[][]";
  | VarPoint vpoin -> print_string vpoin.name; print_string "."; process_VarId vpoin.inner;;

let process_func_elem elem =
  match elem with 
  | Asigna a -> print_string "Asignando "; process_VarId a.izq; print_endline "";
  | CondIf cif -> ()
  | Escritura e -> ()
  | EVar evar -> ()
  | ForLoop floop -> ()
  | WhileLoop wloop -> ()
  | Return r -> ()
  | Expresion ex -> ();;

let rec process_func_bloque bloque = 
  match bloque with 
  | [] -> ()
  | (f :: fs) -> process_func_elem f; process_func_bloque fs;;

let process_function func =
  print_string "Function start:"; print_endline func.fname; process_func_bloque func.fbloque;;

let process_clase_elem elem = 
  match elem with
  | Fun f -> process_function f;
  | CVar v -> ();;

let rec process_clase_bloque bloque =
  match bloque with 
  | [] -> ()
  | (f :: fs) -> process_clase_elem f; process_clase_bloque fs;;

let upper_prog elem = 
  match elem with
  | Clase c -> print_string "Class start:"; print_endline c.name; process_clase_bloque c.bloque; print_endline "Class ends";
  | Func f -> process_function f;;

let process_expression exp tbls = 
  process_or_expression exp tbls

(* Procesar semantica del parse tree*)
let rec back_main tree =
  match tree with 
  | Program [] -> print_endline "end of file"
  | Program (i :: j) ->
      upper_prog i;
      back_main (Program j);;

(* Semantics *)

(* Match the variable declaration to add it to the table*)
let add_vars_to_tbl t var class_id tbl =
  match var.id with
  | VDVarID v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id}
  | VDVarArray v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id} (* Need to add array part *)
  | VDVar2Array v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id};; (* Need to add array part *)

let process_asignacion left right tbls = 
  assert_equalCS (changeTypeToCS (variableLookup left tbls)) (process_expression right tbls); ();;

(* Iterate through all the variables in a variable declaration *)
let rec add_vars_to_tbl_rec t vars class_id tbl = 
  match vars with
  | [] -> ();
  | (f::fs) -> 
      add_vars_to_tbl t f class_id tbl;
      add_vars_to_tbl_rec t fs class_id tbl;;

let process_print exp tbl =
  match (process_expression exp tbl) with
  | CsChar -> ()
  | CsString -> ()
  | _ -> failwith "Non-printable type"

let rec process_print_rec exps tbl =
  match exps with
  | [] -> ()
  | (e::es) -> process_print e tbl; process_print_rec es tbl

let process_condition exp tbl =
  match (process_expression exp tbl) with
  | CsBool -> ()
  | _ -> failwith "Condition expression is not bool"

let getFunctionTbl name tbl = 
  let ftbl = (Hashtbl.find tbl name) in
    match ftbl with
    | FuncT ftbl -> ftbl.variables
    | ClaseT ctbl -> failwith "Function not found"

let getFunctionTblInClass name classTbl = 
  match classTbl with 
    | ClaseT ctbl -> (Hashtbl.find ctbl.funcs name).variables
    | FuncT ftbl -> failwith "Classtbl is a function"

let getClaseTbl high_level =
  match high_level with
  | ClaseT ct -> ct
  | FuncT ft -> failwith "Should be a classtable"

(* Match estatuto to add variable to table *)
let rec add_func_elems_to_tbl elem tbls ft=
  match elem with 
  | Asigna a -> process_asignacion a.izq a.der tbls; 
  | CondIf cif -> process_condition cif.cond tbls; process_block cif.true_block tbls ft; process_block cif.false_block tbls ft;
  | Escritura e -> process_print_rec e tbls;
  | EVar evar -> add_vars_to_tbl_rec evar.tipo evar.vars evar.id_class tbls.function_tbl;
  | ForLoop floop -> ()
  | WhileLoop wloop -> process_condition wloop.cond tbls; process_block wloop.bloque tbls ft;
  | Return r -> assert_equalCS (changeTypeToCS ft) (process_expression r tbls); ();
  | Expresion ex -> assert_equalCS CsVoid (process_expression ex tbls); ();
(* Iterate through the function elements to add variables to the tbl *)
and add_func_elems_to_tbl_rec bloque tbls ft=
  match bloque with
  | [] -> ()
  | (f::fs) -> 
      add_func_elems_to_tbl f tbls ft;
      add_func_elems_to_tbl_rec fs tbls ft
(* Process blocks for conditions and loops *)
and process_block bloque tbl ft=
  add_func_elems_to_tbl_rec bloque tbl ft

(* Match the element of a class to a function and add the function elements to tbl *)
let add_inner_fucs_of_class elem class_tbl tbl =
  match elem, class_tbl with
  | Fun f, ClaseT ct -> add_func_elems_to_tbl_rec f.fbloque { function_tbl=(getFunctionTblInClass f.fname class_tbl); class_tbl=ClassTbl (getClaseTbl class_tbl); global_tbl=tbl} f.tipo; ();
  | CVar cv, ClaseT ct -> ();
  | _, _ -> assert false;;

(* Iterate through the class to check on functions *)
let rec add_inner_fucs_of_class_rec bloque class_tbl tbl = 
  match bloque with
  | [] -> ();
  | (f::fs) -> 
      add_inner_fucs_of_class f class_tbl tbl;
      add_inner_fucs_of_class_rec fs class_tbl tbl;;

(* Check elem to match class of function, then add all variables in functions to table*)
let add_inner_func_to_tbl elem tbl = 
  match elem with
  | Func f -> add_func_elems_to_tbl_rec f.fbloque { function_tbl=(getFunctionTbl f.fname tbl); class_tbl=Nil; global_tbl=tbl} f.tipo;
  | Clase c -> add_inner_fucs_of_class_rec c.bloque (Hashtbl.find tbl c.name) tbl;;

(* Processing a single element of each class *)
let add_class_att_to_table_inner elem class_tbl = 
  match elem, class_tbl with
  | Fun f, ClaseT ctbl -> add_func class_tbl f.fname {name=f.fname; tipo=f.tipo; variables=Hashtbl.create 0}; [];
  | CVar cv, ClaseT ctbl -> add_vars_to_tbl_rec cv.tipo cv.vars cv.id_class ctbl.vars; [];; (*If type is class, validate class type *)

(* Iterating through all the class variables and funcs *)
let rec add_class_att_to_table bloque class_tbl = 
  match bloque with 
  | [] -> class_tbl;
  | (f::fs) -> 
      add_class_att_to_table_inner f class_tbl;
      add_class_att_to_table fs class_tbl;;

(* Processes single element of high order *)
let add_upper_prog_to_table elem table = 
  match elem with
  | Func f -> add_high_level_element table elem
  | Clase c -> let class_tbl = add_high_level_element table elem in add_class_att_to_table c.bloque class_tbl;;

(* Iterating through all the elemnts in tree *)
let rec semantic_main tree table = 
  match tree with 
  | Program [] -> table
  | Program (i :: j) ->
      add_upper_prog_to_table i table; (* Adds high level funcs and classes to main table, including vars and funcs of class *)
      semantic_main (Program j) table; (* Iterate *)
      add_inner_func_to_tbl i table; (* Process functions of classes or funcs *)
      table;;

(* Main Semantic start *)
let semantic_start tree = 
  let main_table = Hashtbl.create 1234 in
    semantic_main tree main_table;;