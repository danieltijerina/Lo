open Ast
open VarTabl
open ExpValidator
open Util
open Printer
open Printf

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

let process_expression exp tbls var_count cte_tbl oc = 
  process_or_expression exp tbls var_count cte_tbl oc

(* Procesar semantica del parse tree*)
let rec back_main tree =
  match tree with 
  | Program [] -> print_endline "end of file"
  | Program (i :: j) ->
      upper_prog i;
      back_main (Program j);;

(* Semantics *)

(* Match the variable declaration to add it to the table*)
let add_vars_to_tbl t var class_id tbl var_count cte_tbl =
  match var.id with
  | VDVarID v -> update_count var_count t; add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address= let tmp = Hashtbl.find var_count t in tmp.base + tmp.count - 1};
  | VDVarArray v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address=0} (* Need to add array part *)
  | VDVar2Array v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address=0};; (* Need to add array part *)

let process_asignacion left right tbls var_count cte_tbl oc = 
  assert_equalCS (changeTypeToCS (variableLookup left tbls)) (process_expression right tbls var_count cte_tbl oc); ();;

(* Iterate through all the variables in a variable declaration *)
let rec add_vars_to_tbl_rec t vars class_id tbl var_count cte_tbl = 
  match vars with
  | [] -> ();
  | (f::fs) -> 
      add_vars_to_tbl t f class_id tbl var_count cte_tbl;
      add_vars_to_tbl_rec t fs class_id tbl var_count cte_tbl;;

let process_print exp tbl var_count cte_tbl oc=
  match (process_expression exp tbl var_count cte_tbl oc) with
  | CsChar -> ()
  | CsString -> ()
  | _ -> failwith "Non-printable type"

let rec process_print_rec exps tbl var_count cte_tbl oc=
  match exps with
  | [] -> ()
  | (e::es) -> process_print e tbl var_count cte_tbl oc; process_print_rec es tbl var_count cte_tbl oc

let process_condition exp tbl var_count cte_tbl oc=
  match (process_expression exp tbl var_count cte_tbl oc) with
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
let rec add_func_elems_to_tbl elem tbls var_count cte_tbl ft oc=
  match elem with 
  | Asigna a -> process_asignacion a.izq a.der tbls var_count cte_tbl oc; 
  | CondIf cif -> process_condition cif.cond tbls var_count cte_tbl oc; process_block cif.true_block tbls var_count cte_tbl ft oc; process_block cif.false_block tbls var_count cte_tbl ft oc;
  | Escritura e -> process_print_rec e tbls var_count cte_tbl oc;
  | EVar evar -> add_vars_to_tbl_rec evar.tipo evar.vars evar.id_class tbls.function_tbl var_count cte_tbl;
  | ForLoop floop -> process_for_loop floop tbls var_count cte_tbl ft oc;
  | WhileLoop wloop -> process_condition wloop.cond tbls var_count cte_tbl oc; process_block wloop.bloque tbls var_count cte_tbl ft oc;
  | Return r -> assert_equalCS (changeTypeToCS ft) (process_expression r tbls var_count cte_tbl oc); ();
  | Expresion ex -> assert_equalCS CsVoid (process_expression ex tbls var_count cte_tbl oc); ();
(* Iterate through the function elements to add variables to the tbl *)
and add_func_elems_to_tbl_rec bloque tbls var_count cte_tbl ft oc=
  match bloque with
  | [] -> ()
  | (f::fs) -> 
      add_func_elems_to_tbl f tbls var_count cte_tbl ft oc;
      add_func_elems_to_tbl_rec fs tbls var_count cte_tbl ft oc
(* Process blocks for conditions and loops *)
and process_block bloque tbl var_count cte_tbl ft oc=
  add_func_elems_to_tbl_rec bloque tbl var_count cte_tbl ft oc
and process_for_loop floop tbl var_count cte_tbl ft oc= 
  variableLookup floop.init tbl;
  assert_equalCS CsBool (process_expression floop.cond tbl var_count cte_tbl oc);
  add_func_elems_to_tbl (Asigna floop.post) tbl var_count cte_tbl ft oc;
  process_block floop.bloque tbl var_count cte_tbl ft oc;;


(* Match the element of a class to a function and add the function elements to tbl *)
let add_inner_fucs_of_class elem class_tbl tbl var_count cte_tbl oc=
  match elem, class_tbl with
  | Fun f, ClaseT ct -> add_func_elems_to_tbl_rec f.fbloque { function_tbl=(getFunctionTblInClass f.fname class_tbl); class_tbl=ClassTbl (getClaseTbl class_tbl); global_tbl=tbl} var_count cte_tbl f.tipo oc; ();
  | CVar cv, ClaseT ct -> ();
  | _, _ -> assert false;;

(* Iterate through the class to check on functions *)
let rec add_inner_fucs_of_class_rec bloque class_tbl tbl var_count cte_tbl oc = 
  match bloque with
  | [] -> ();
  | (f::fs) -> 
      add_inner_fucs_of_class f class_tbl tbl var_count cte_tbl oc;
      add_inner_fucs_of_class_rec fs class_tbl tbl var_count cte_tbl oc;;

(* Check elem to match class of function, then add all variables in functions to table*)
let add_inner_func_to_tbl elem tbl var_count cte_tbl oc = 
  match elem with
  | Func f -> add_func_elems_to_tbl_rec f.fbloque { function_tbl=(getFunctionTbl f.fname tbl); class_tbl=Nil; global_tbl=tbl} var_count cte_tbl f.tipo oc;
  | Clase c -> add_inner_fucs_of_class_rec c.bloque (Hashtbl.find tbl c.name) tbl var_count cte_tbl oc;;

(* Processing a single element of each class *)
let add_class_att_to_table_inner elem class_tbl var_count cte_tbl = 
  match elem, class_tbl with
  | Fun f, ClaseT ctbl -> add_func class_tbl f.fname {name=f.fname; tipo=f.tipo; variables=Hashtbl.create 0}; [];
  | CVar cv, ClaseT ctbl -> add_vars_to_tbl_rec cv.tipo cv.vars cv.id_class ctbl.vars var_count cte_tbl; [];; (*If type is class, validate class type *)

(* Iterating through all the class variables and funcs *)
let rec add_class_att_to_table bloque class_tbl var_count cte_tbl = 
  match bloque with 
  | [] -> class_tbl;
  | (f::fs) -> 
      add_class_att_to_table_inner f class_tbl var_count cte_tbl;
      add_class_att_to_table fs class_tbl var_count cte_tbl;;

(* Processes single element of high order *)
let add_upper_prog_to_table elem table var_count cte_tbl = 
  match elem with
  | Func f -> add_high_level_element table elem
  | Clase c -> let class_tbl = add_high_level_element table elem in add_class_att_to_table c.bloque class_tbl var_count cte_tbl;;

(* Iterating through all the elemnts in tree *)
let rec semantic_main tree table var_count cte_tbl oc = 
  match tree with 
  | Program [] -> table
  | Program (i :: j) ->
      add_upper_prog_to_table i table var_count cte_tbl; (* Adds high level funcs and classes to main table, including vars and funcs of class *)
      semantic_main (Program j) table var_count cte_tbl oc; (* Iterate *)
      add_inner_func_to_tbl i table var_count cte_tbl oc; (* Process functions of classes or funcs *)
      table;;

let initialize_count tbl =
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
  Hashtbl.add tbl IntTmp {count=0; base=20000};
  Hashtbl.add tbl FloatTmp {count=0; base=21000};
  Hashtbl.add tbl StringTmp {count=0; base=22000};
  Hashtbl.add tbl CharTmp {count=0; base=23000};
  Hashtbl.add tbl BoolTmp {count=0; base=24000};;

(* Main Semantic start *)
let semantic_start tree oc = 
  let main_table = Hashtbl.create 1234 in
    let var_count = Hashtbl.create 1234 in
      let cte_table = {integer=Hashtbl.create 123; floating=Hashtbl.create 123; strings=Hashtbl.create 123; characters=Hashtbl.create 123; booleans=Hashtbl.create 123} in
        initialize_count var_count;
        semantic_main tree main_table var_count cte_table oc;;
    