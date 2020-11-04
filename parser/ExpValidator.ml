open Ast
open CuboSemantico
open Util
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

let rec process_var_expression exp tbls oc =
  fprintf oc "%d " (vLookup exp tbls);
  changeTypeToCS (variableLookup exp tbls)

and process_const_expression exp oc = 
  match exp with 
  | Int e -> fprintf oc "%d " e; CsInt
  | Float e -> CsFloat
  | Bool e -> CsBool
  | String e -> CsString
  | Char e -> CsChar

and process_factor_expression exp tbls oc =
  match exp with
  | Const e -> process_const_expression e oc
  | FExp e -> process_pm_expression e tbls oc
  | FVarId e -> process_var_expression e tbls oc

and process_term_expression exp tbls oc = 
  match exp with
  | Times e -> fprintf oc "%s " "*"; times_type_check (process_factor_expression e.left tbls oc) (process_term_expression e.right tbls oc)
  | Div e -> fprintf oc "%s " "/"; div_type_check (process_factor_expression e.left tbls oc) (process_term_expression e.right tbls oc)
  | Factor e -> process_factor_expression e tbls oc

and process_pm_expression exp tbls oc = 
  match exp with
  | Plus e -> fprintf oc "%s " "+"; sum_type_check (process_term_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | Mnius e -> fprintf oc "%s " "-"; sub_type_check (process_term_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | Termino e -> process_term_expression e tbls oc

and process_logic_expression exp tbls oc = 
  match exp with
  | GreaterT e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | LessT e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | GreaterE e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | LessE e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | Equal e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | NotEqual e -> relational_type_check (process_pm_expression e.left tbls oc) (process_pm_expression e.right tbls oc)
  | OExp e -> process_pm_expression e tbls oc

and process_and_expression exp tbls oc = 
  match exp with
  | AndExp e -> fprintf oc "%s\n" "andExp"; logical_type_check (process_logic_expression e.left tbls oc) (process_and_expression e.right tbls oc)
  | AExp e -> process_logic_expression e tbls oc

and process_or_expression exp tbls oc =
  match exp with
  | OrExp e -> fprintf oc "%s\n" "orExp"; logical_type_check (process_and_expression e.left tbls oc) (process_or_expression e.right tbls oc);
  | Exp e -> process_and_expression e tbls oc