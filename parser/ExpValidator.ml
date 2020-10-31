open Ast
open CuboSemantico
open Util

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

(* This function still needs to lookup data type in Vartbl *)
let rec process_var_expression exp tbls =
  changeTypeToCS (variableLookup exp tbls)
  (*
  match exp with
  | VarID e -> print_endline e.name; CsVoid;
  | VarFuncCall e -> print_endline e.func; CsVoid;
  | VarArray e -> print_endline e.name; CsVoid;
  | Var2Array e -> print_endline e.name; CsVoid;
  | VarPoint e -> print_endline e.name; CsVoid;
  *) 

and process_const_expression exp = 
  match exp with 
  | Int e -> CsInt
  | Float e -> CsFloat
  | Bool e -> CsBool
  | String e -> CsString
  | Char e -> CsChar

and process_factor_expression exp tbls =
  match exp with
  | Const e -> process_const_expression e
  | FExp e -> process_pm_expression e tbls
  | FVarId e -> process_var_expression e tbls

and process_term_expression exp tbls = 
  match exp with
  | Times e -> times_type_check (process_factor_expression e.left tbls) (process_term_expression e.right tbls)
  | Div e -> div_type_check (process_factor_expression e.left tbls) (process_term_expression e.right tbls)
  | Factor e -> process_factor_expression e tbls

and process_pm_expression exp tbls = 
  match exp with
  | Plus e -> sum_type_check (process_term_expression e.left tbls) (process_pm_expression e.right tbls)
  | Mnius e -> sub_type_check (process_term_expression e.left tbls) (process_pm_expression e.right tbls)
  | Termino e -> process_term_expression e tbls

and process_logic_expression exp tbls = 
  match exp with
  | GreaterT e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | LessT e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | GreaterE e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | LessE e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | Equal e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | NotEqual e -> relational_type_check (process_pm_expression e.left tbls) (process_pm_expression e.right tbls)
  | OExp e -> process_pm_expression e tbls

and process_and_expression exp tbls = 
  match exp with
  | AndExp e -> logical_type_check (process_logic_expression e.left tbls) (process_and_expression e.right tbls)
  | AExp e -> process_logic_expression e tbls

and process_or_expression exp tbls =
  match exp with
  | OrExp e -> logical_type_check (process_and_expression e.left tbls) (process_or_expression e.right tbls)
  | Exp e -> process_and_expression e tbls