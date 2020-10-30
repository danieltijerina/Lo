open Ast
open CuboSemantico

(* This function still needs to lookup data type in Vartbl *)
let rec process_var_expression exp =
  match exp with
  | VarID e -> print_endline e.name
  | VarFuncCall e -> print_endline e.func
  | VarArray e -> print_endline e.name
  | Var2Array e -> print_endline e.name
  | VarPoint e -> print_endline e.name

and process_const_expression exp = 
  match exp with 
  | Int e -> CsInt
  | Float e -> CsFloat
  | Bool e -> CsBool
  | String e -> CsString
  | Char e -> CsChar

and process_factor_expression exp =
  match exp with
  | Const e -> process_const_expression e
  | FExp e -> process_pm_expression e
  (* | FVarId e -> process_var_expression e;; *)

and process_term_expression exp = 
  match exp with
  | Times e -> times_type_check (process_factor_expression e.left) (process_term_expression e.right)
  | Div e -> div_type_check (process_factor_expression e.left) (process_term_expression e.right)
  | Factor e -> process_factor_expression e

and process_pm_expression exp = 
  match exp with
  | Plus e -> sum_type_check (process_term_expression e.left) (process_pm_expression e.right)
  | Mnius e -> sub_type_check (process_term_expression e.left) (process_pm_expression e.right)
  | Termino e -> process_term_expression e

and process_logic_expression exp = 
  match exp with
  | GreaterT e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | LessT e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | GreaterE e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | LessE e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | Equal e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | NotEqual e -> relational_type_check (process_pm_expression e.left) (process_pm_expression e.right)
  | OExp e -> process_pm_expression e

and process_and_expression exp = 
  match exp with
  | AndExp e -> logical_type_check (process_logic_expression e.left) (process_and_expression e.right)
  | AExp e -> process_logic_expression e

and process_or_expression exp =
  match exp with
  | OrExp e -> logical_type_check (process_and_expression e.left) (process_or_expression e.right)
  | Exp e -> process_and_expression e