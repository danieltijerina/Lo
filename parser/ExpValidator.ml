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
    Hashtbl.add cte_tbl.integer e (let tmp = Hashtbl.find var_count IntCte in tmp.base + Hashtbl.length cte_tbl.integer); update_count var_count IntCte; Hashtbl.find cte_tbl.integer e

let find_or_add_float_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.floating e
  with Not_found -> 
    Hashtbl.add cte_tbl.floating e (let tmp = Hashtbl.find var_count FloatCte in tmp.base + Hashtbl.length cte_tbl.floating); update_count var_count FloatCte; Hashtbl.find cte_tbl.floating e

let find_or_add_bool_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.booleans e
  with Not_found -> 
    Hashtbl.add cte_tbl.booleans e (let tmp = Hashtbl.find var_count BoolCte in tmp.base + Hashtbl.length cte_tbl.booleans); update_count var_count BoolCte; Hashtbl.find cte_tbl.booleans e

let find_or_add_string_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.strings e
  with Not_found -> 
    Hashtbl.add cte_tbl.strings e (let tmp = Hashtbl.find var_count StringCte in tmp.base + Hashtbl.length cte_tbl.strings); update_count var_count StringCte; Hashtbl.find cte_tbl.strings e

let find_or_add_char_cte var_count cte_tbl e = 
  try 
    Hashtbl.find cte_tbl.characters e
  with Not_found -> 
    Hashtbl.add cte_tbl.characters e (let tmp = Hashtbl.find var_count CharCte in tmp.base + Hashtbl.length cte_tbl.characters); update_count var_count CharCte; Hashtbl.find cte_tbl.characters e

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

let rec process_var_expression exp tbls var_count cte_tbl oc =
  (variableLookup exp tbls var_count cte_tbl oc)

and process_const_expression exp var_count cte_tbl oc = 
  match exp with 
  | Int e -> let addr = (find_or_add_int_cte var_count cte_tbl e) in 
                { rtipo=IntTy; address=addr; }
  | Float e -> let addr = (find_or_add_float_cte var_count cte_tbl e) in 
                  { rtipo=FloatTy; address=addr;}
  | Bool e -> let addr = (find_or_add_bool_cte var_count cte_tbl e) in 
                {rtipo=BoolTy; address=addr; }
  | String e -> let addr = (find_or_add_string_cte var_count cte_tbl e) in 
                  {rtipo=StringTy; address=addr;}
  | Char e -> let addr = (find_or_add_char_cte var_count cte_tbl e) in 
                  {rtipo=CharTy; address=addr;}

and process_factor_expression exp tbls var_count cte_tbl oc =
  match exp with
  | Const e -> (process_const_expression e var_count cte_tbl oc)
  | FExp e -> process_pm_expression e tbls var_count cte_tbl oc
  | FVarId e -> process_var_expression e tbls var_count cte_tbl oc

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

and checkFuncParamsRec fparams vfparams current_tbls var_count const_tbl oc count= 
  match fparams, vfparams with 
  | [], [] -> ()
  | [], _ -> failwith "Different parameter size"
  | _, [] -> failwith "Different parameter size"
  | (f:: fs), (x :: xs) -> checkFuncParams f x current_tbls var_count const_tbl oc count; 
                          checkFuncParamsRec fs xs current_tbls var_count const_tbl oc (count+1);
and checkFuncParams fparam vfparam current_tbls var_count const_tbl oc count= 
  let ex = (process_or_expression vfparam current_tbls var_count const_tbl oc) in
    assert_equal ex.rtipo fparam.tipo;
    fprintf oc "%s %d %d %d\n" "param" ex.address fparam.address (-1);

and variableLookupVarID var current_tbls= 
  match current_tbls.function_tbl with 
  | FuncTbl f -> (try (Hashtbl.find f.variables var)
                    with e -> 
                      match current_tbls.class_tbl with
                      | ClassTbl ct -> (try (Hashtbl.find ct.vars var) with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
                      | Nil -> raise e;)
  | FNil -> (match current_tbls.class_tbl with
            | ClassTbl ct -> (try (Hashtbl.find ct.vars var) with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
            | Nil -> failwith "No variable found";)

and functionLookup f current_tbls var_count const_tbl oc= 
  match f with 
  | VarFuncCall vf -> ( match current_tbls.class_tbl with 
                        | ClassTbl ct -> (try let fres = (Hashtbl.find ct.funcs vf.func) in 
                        fprintf oc "era %s\n" vf.func;
                        checkFuncParamsRec fres.params vf.params current_tbls var_count const_tbl oc 1;
                        fprintf oc "%s %s\n" "goSub" vf.func; (* Check initial address *) 
                        {rtipo=fres.ftipo; address=0} with Not_found -> (
                                try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in match tbl  with 
                                                      | FuncT funct -> fprintf oc "era %s\n" vf.func;
                                                      checkFuncParamsRec funct.params vf.params current_tbls var_count const_tbl oc 1;
                                                      fprintf oc "%s %s\n" "goSub" vf.func; (* Check initial address *)
                                                      {rtipo=funct.ftipo; address=0}; 
                                                      | _ -> failwith "No function found"
                                with Not_found -> failwith "No function found"
                              ))
                        | Nil -> (try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in match tbl  with 
                                                        | FuncT funct -> fprintf oc "era %s\n" vf.func;
                                                        checkFuncParamsRec funct.params vf.params current_tbls var_count const_tbl oc 1;
                                                        fprintf oc "%s %s\n" "goSub" vf.func ; (* Check initial address *) 
                                                        {rtipo=funct.ftipo; address=0}; 
                                                        | _ -> failwith "No function found"
                                  with Not_found -> failwith "No function found") )

and variableInClassLookup var_id class_tbl var_count const_tbl oc global_tbl=
  match var_id with
  | VarID v -> (try let res = (Hashtbl.find class_tbl.vars v.name) in {rtipo=res.tipo; address=res.address} with Not_found -> failwith "No Variable found in class");
  | VarFuncCall vfunc -> (try let res = (Hashtbl.find class_tbl.funcs vfunc.func) in 
  fprintf oc "era %s\n" vfunc.func;
  checkFuncParamsRec res.params vfunc.params {function_tbl=FNil; class_tbl=ClassTbl class_tbl; global_tbl=global_tbl} var_count const_tbl oc 1;
  fprintf oc "%s %s\n" "goSub" vfunc.func; (* Check initial address *)
  {rtipo=res.ftipo; address=0} with Not_found -> failwith "No function found in class");
  | VarArray varr ->  (try let res = (Hashtbl.find class_tbl.vars varr.name) in {rtipo=res.tipo; address=res.address} with Not_found -> failwith "No Variable found in class"); (* Need to implement arrays *)
  | Var2Array v2arr -> (try let res = (Hashtbl.find class_tbl.vars v2arr.name) in {rtipo=res.tipo; address=res.address} with Not_found -> failwith "No Variable found in class"); (* Need to implement arrays *)
  | VarPoint vpoint -> failwith "class in class not supported yet"; (* Need to do class in class *)

and pointVarLookup vp current_tbls var_count const_tbl oc = 
  match vp with
  | VarPoint vpoint -> (let v = variableLookupVarID vpoint.name current_tbls in
                        match v.tipo with 
                        | ClassTy -> (
                            try let tbl = (Hashtbl.find current_tbls.global_tbl v.id_class) in
                              ( match tbl with 
                              | ClaseT ct -> variableInClassLookup vpoint.inner ct var_count const_tbl oc current_tbls.global_tbl;
                              | _ -> failwith "element not part of class" )
                            with Not_found -> failwith "element not found");
                        | _ -> failwith "variable is not a class");
  | _ -> failwith "error";

and variableLookup var_id current_tbls var_count const_tbl oc= 
  match var_id with
  | VarID v -> let res = (variableLookupVarID v.name current_tbls) in {rtipo=res.tipo; address=res.address};
  | VarFuncCall vfunc -> (functionLookup (VarFuncCall vfunc) current_tbls var_count const_tbl oc);
  | VarArray varr ->  let res = (variableLookupVarID varr.name current_tbls) in {rtipo=res.tipo; address=res.address}; (* Need to implement arrays *)
  | Var2Array v2arr -> let res = (variableLookupVarID v2arr.name current_tbls) in {rtipo=res.tipo; address=res.address}; (* Need to implement arrays *)
  | VarPoint vpoint -> (pointVarLookup (VarPoint vpoint) current_tbls var_count const_tbl oc);;