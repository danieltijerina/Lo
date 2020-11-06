open Ast
open CuboSemantico
open Util
open VarTabl
open Printf

let assert_equal right left = 
  match right, left with
  | IntTy, IntTy -> true
  | FloatTy, FloatTy -> true
  | CharTy, CharTy -> true
  | StringTy, StringTy -> true
  | BoolTy, BoolTy -> true
  | VoidTy, VoidTy -> true
  | ClassTy, ClassTy -> true
  | _, _ -> failwith "Incorrect data type"

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

let get_temp_type ty = 
  match ty with
  | IntTy -> IntTmp
  | FloatTy -> FloatTmp
  | CharTy -> CharTmp
  | StringTy -> StringTmp
  | BoolTy -> BoolTmp

let get_next_temporal var_count ty =
  let tmpty = get_temp_type ty in 
    let tmp = Hashtbl.find var_count tmpty in 
      update_count var_count tmpty;
      tmp.base + tmp.count

let rec process_var_expression exp tbls oc =
  (variableLookup exp tbls)

and process_const_expression exp var_count cte_tbl oc = 
  match exp with 
  | Int e -> let addr = (find_or_add_int_cte var_count cte_tbl e) in 
                update_count var_count IntCte; 
                { rtipo=IntTy; address=addr; }
  | Float e -> let addr = (find_or_add_float_cte var_count cte_tbl e) in 
                  update_count var_count FloatCte; 
                  { rtipo=FloatTy; address=addr;}
  | Bool e -> let addr = (find_or_add_bool_cte var_count cte_tbl e) in 
                update_count var_count BoolCte; 
                {rtipo=BoolTy; address=addr; }
  | String e -> let addr = (find_or_add_string_cte var_count cte_tbl e) in 
                  update_count var_count StringCte; 
                  {rtipo=StringTy; address=addr;}
  | Char e -> let addr = (find_or_add_char_cte var_count cte_tbl e) in 
                  update_count var_count CharCte; 
                  {rtipo=CharTy; address=addr;}

and process_factor_expression exp tbls var_count cte_tbl oc =
  match exp with
  | Const e -> (process_const_expression e var_count cte_tbl oc)
  | FExp e -> process_pm_expression e tbls var_count cte_tbl oc
  | FVarId e -> process_var_expression e tbls oc

and process_term_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Times e -> let left_ = (process_factor_expression e.left tbls var_count cte_tbl oc) in
                let right_ = (process_term_expression e.right tbls var_count cte_tbl oc) in
                  let t = times_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "*" left_.address right_.address nxt_temp; 
                      {rtipo=t; address=nxt_temp};
  | Div e -> let left_ = (process_factor_expression e.left tbls var_count cte_tbl oc) in 
              let right_ = (process_term_expression e.right tbls var_count cte_tbl oc) in
                let t = div_type_check left_.rtipo right_.rtipo in 
                  let nxt_temp = get_next_temporal var_count t in
                    fprintf oc "%s %d %d %d\n" "/" left_.address right_.address nxt_temp;
                    {rtipo=t; address=nxt_temp};
  | Factor e -> process_factor_expression e tbls var_count cte_tbl oc;

and process_pm_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Plus e -> let left_ = (process_term_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = sum_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "+" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp};   
  | Mnius e -> let left_ = (process_term_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = sub_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "-" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp};
  | Termino e -> process_term_expression e tbls var_count cte_tbl oc

and process_logic_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | GreaterT e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in
                      let t = relational_type_check left_.rtipo right_.rtipo in
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" ">" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp};
  | LessT e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "<" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp};
  | GreaterE e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                      let t = relational_type_check left_.rtipo right_.rtipo in 
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" ">=" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp};
  | LessE e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "<=" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp};
  | Equal e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "==" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp};
  | NotEqual e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                      let t = relational_type_check left_.rtipo right_.rtipo in
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" "!=" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp};
  | OExp e -> process_pm_expression e tbls var_count cte_tbl oc

and process_and_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | AndExp e -> let left_ = (process_logic_expression e.left tbls var_count cte_tbl oc) in 
                  let right_ = (process_and_expression e.right tbls var_count cte_tbl oc) in 
                    let t = logical_type_check left_.rtipo right_.rtipo in
                      let nxt_temp = get_next_temporal var_count t in
                        fprintf oc "%s %d %d %d\n" "&&" left_.address right_.address nxt_temp;
                        {rtipo=t; address=nxt_temp};
  | AExp e -> process_logic_expression e tbls var_count cte_tbl oc

and process_or_expression exp tbls var_count cte_tbl oc =
  match exp with
  | OrExp e -> let left_ = (process_and_expression e.left tbls var_count cte_tbl oc) in 
                  let right_ = (process_or_expression e.right tbls var_count cte_tbl oc) in 
                    let t = logical_type_check left_.rtipo right_.rtipo in
                      let nxt_temp = get_next_temporal var_count t in
                        fprintf oc "%s %d %d %d\n" "||" left_.address right_.address nxt_temp;
                        {rtipo=t; address=nxt_temp};
  | Exp e -> process_and_expression e tbls var_count cte_tbl oc