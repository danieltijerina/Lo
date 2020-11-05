open Ast
open CuboSemantico
open Util
open VarTabl
open Printf

let assert_equalCS right left = 
  match right, left with
  | CsInt, CsInt -> true
  | CsFloat, CsFloat -> true
  | CsChar, CsChar -> true
  | CsString, CsString -> true
  | CsBool, CsBool -> true
  | CsVoid, CsVoid -> true
  | CsClass, CsClass -> true
  | _, _ -> failwith "Incorrect data type"

let changeTypeToCS t = 
  match t with
  | IntTy -> CsInt
  | FloatTy -> CsFloat
  | CharTy -> CsChar
  | StringTy -> CsString
  | BoolTy -> CsBool
  | VoidTy -> CsVoid
  | ClassTy -> CsClass
  | _ -> failwith "Constant data type is invalid"

let find_or_add_int_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.integer e
  with Not_found -> 
    Hashtbl.add cte_tbl.integer e (let tmp = Hashtbl.find var_count IntCte in tmp.base + tmp.count); Hashtbl.find cte_tbl.integer e

let find_or_add_float_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.floating e
  with Not_found -> 
    Hashtbl.add cte_tbl.floating e (let tmp = Hashtbl.find var_count FloatCte in tmp.base + tmp.count); Hashtbl.find cte_tbl.floating e

let find_or_add_bool_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.booleans e
  with Not_found -> 
    Hashtbl.add cte_tbl.booleans e (let tmp = Hashtbl.find var_count BoolCte in tmp.base + tmp.count); Hashtbl.find cte_tbl.booleans e

let find_or_add_string_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.strings e
  with Not_found -> 
    Hashtbl.add cte_tbl.strings e (let tmp = Hashtbl.find var_count StringCte in tmp.base + tmp.count); Hashtbl.find cte_tbl.strings e

let find_or_add_char_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.characters e
  with Not_found -> 
    Hashtbl.add cte_tbl.characters e (let tmp = Hashtbl.find var_count CharCte in tmp.base + tmp.count); Hashtbl.find cte_tbl.characters e

let rec process_var_expression exp tbls oc =
  fprintf oc "%d " (vLookup exp tbls);
  changeTypeToCS (variableLookup exp tbls)

and process_const_expression exp var_count cte_tbl oc = 
  match exp with 
  | Int e -> fprintf oc "%d " (find_or_add_int_cte var_count cte_tbl e); update_count var_count IntCte; CsInt
  | Float e -> fprintf oc "%d " (find_or_add_float_cte var_count cte_tbl e); update_count var_count FloatCte; CsFloat
  | Bool e -> fprintf oc "%d " (find_or_add_bool_cte var_count cte_tbl e); update_count var_count BoolCte; CsBool
  | String e -> fprintf oc "%d " (find_or_add_string_cte var_count cte_tbl e); update_count var_count StringCte; CsString
  | Char e -> fprintf oc "%d " (find_or_add_char_cte var_count cte_tbl e); update_count var_count CharCte; CsChar

and process_factor_expression exp tbls var_count cte_tbl oc =
  match exp with
  | Const e -> process_const_expression e var_count cte_tbl oc
  | FExp e -> process_pm_expression e tbls var_count cte_tbl oc
  | FVarId e -> process_var_expression e tbls oc

and process_term_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Times e -> fprintf oc "%s " "*"; times_type_check (process_factor_expression e.left tbls var_count cte_tbl oc) (process_term_expression e.right tbls var_count cte_tbl oc)
  | Div e -> fprintf oc "%s " "/"; div_type_check (process_factor_expression e.left tbls var_count cte_tbl oc) (process_term_expression e.right tbls var_count cte_tbl oc)
  | Factor e -> process_factor_expression e tbls var_count cte_tbl oc

and process_pm_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Plus e -> fprintf oc "%s " "+"; sum_type_check (process_term_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | Mnius e -> fprintf oc "%s " "-"; sub_type_check (process_term_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | Termino e -> process_term_expression e tbls var_count cte_tbl oc

and process_logic_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | GreaterT e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | LessT e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | GreaterE e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | LessE e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | Equal e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | NotEqual e -> relational_type_check (process_pm_expression e.left tbls var_count cte_tbl oc) (process_pm_expression e.right tbls var_count cte_tbl oc)
  | OExp e -> process_pm_expression e tbls var_count cte_tbl oc

and process_and_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | AndExp e -> fprintf oc "%s\n" "andExp"; logical_type_check (process_logic_expression e.left tbls var_count cte_tbl oc) (process_and_expression e.right tbls var_count cte_tbl oc)
  | AExp e -> process_logic_expression e tbls var_count cte_tbl oc

and process_or_expression exp tbls var_count cte_tbl oc =
  match exp with
  | OrExp e -> fprintf oc "%s\n" "orExp"; logical_type_check (process_and_expression e.left tbls var_count cte_tbl oc) (process_or_expression e.right tbls var_count cte_tbl oc);
  | Exp e -> process_and_expression e tbls var_count cte_tbl oc