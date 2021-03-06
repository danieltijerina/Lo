(*
  ExpValidator
  Modulo encargado de validar expresiones y realizar las acciones semanticas para cada expresion que se encuentre.
*)

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
    Hashtbl.add cte_tbl.integer e (let tmp = try Hashtbl.find var_count IntCte with Not_found -> failwith "hola" in tmp.base + Hashtbl.length cte_tbl.integer); update_count var_count IntCte; Hashtbl.find cte_tbl.integer e

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

let get_ptr_type ty =
  match ty with
  | IntTy -> IntPtr
  | FloatTy -> FloatPtr
  | CharTy -> CharPtr
  | StringTy -> StringPtr
  | BoolTy -> BoolPtr

let get_next_temporal var_count ty =
  let tmpty = get_temp_type ty in 
    let tmp = Hashtbl.find var_count tmpty in 
      update_count var_count tmpty;
      tmp.base + tmp.count

let get_next_pointer var_count ty =
  let tmpty = get_ptr_type ty in
    let tmp = Hashtbl.find var_count tmpty in
      update_count var_count tmpty;
      tmp.base + tmp.count

let rec process_var_expression exp tbls var_count cte_tbl oc =
  (variableLookup exp tbls var_count cte_tbl oc)

and process_const_expression exp var_count cte_tbl oc = 
  match exp with 
  | Int e -> let addr = (find_or_add_int_cte var_count cte_tbl e) in 
                { rtipo=IntTy; address=addr; dim1=1; dim2=1;}
  | Float e -> let addr = (find_or_add_float_cte var_count cte_tbl e) in 
                  { rtipo=FloatTy; address=addr; dim1=1; dim2=1;}
  | Bool e -> let addr = (find_or_add_bool_cte var_count cte_tbl e) in 
                {rtipo=BoolTy; address=addr; dim1=1; dim2=1; }
  | String e -> let addr = (find_or_add_string_cte var_count cte_tbl e) in 
                  {rtipo=StringTy; address=addr; dim1=1; dim2=1;}
  | Char e -> let addr = (find_or_add_char_cte var_count cte_tbl e) in 
                  {rtipo=CharTy; address=addr; dim1=1; dim2=1;}

and process_factor_expression exp tbls var_count cte_tbl oc =
  match exp with
  | Const e -> (process_const_expression e var_count cte_tbl oc)
  | FExp e -> process_pm_expression e tbls var_count cte_tbl oc
  | FVarId e -> process_var_expression e tbls var_count cte_tbl oc

and process_term_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Times e -> let left_ = (process_term_expression e.left tbls var_count cte_tbl oc) in
                let right_ = (process_factor_expression e.right tbls var_count cte_tbl oc) in
                  let t = times_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "*" left_.address right_.address nxt_temp; 
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | Div e -> let left_ = (process_term_expression e.left tbls var_count cte_tbl oc) in 
              let right_ = (process_factor_expression e.right tbls var_count cte_tbl oc) in
                let t = div_type_check left_.rtipo right_.rtipo in 
                  let nxt_temp = get_next_temporal var_count t in
                    fprintf oc "%s %d %d %d\n" "/" left_.address right_.address nxt_temp;
                    {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | Factor e -> process_factor_expression e tbls var_count cte_tbl oc;

and process_pm_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | Plus e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_term_expression e.right tbls var_count cte_tbl oc) in 
                  let t = sum_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "+" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};   
  | Mnius e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_term_expression e.right tbls var_count cte_tbl oc) in 
                  let t = sub_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "-" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | Termino e -> process_term_expression e tbls var_count cte_tbl oc

and process_logic_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | GreaterT e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in
                      let t = relational_type_check left_.rtipo right_.rtipo in
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" ">" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | LessT e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "<" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | GreaterE e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                      let t = relational_type_check left_.rtipo right_.rtipo in 
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" ">=" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | LessE e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in 
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "<=" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | Equal e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                  let t = relational_type_check left_.rtipo right_.rtipo in
                    let nxt_temp = get_next_temporal var_count t in
                      fprintf oc "%s %d %d %d\n" "==" left_.address right_.address nxt_temp;
                      {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | NotEqual e -> let left_ = (process_pm_expression e.left tbls var_count cte_tbl oc) in 
                    let right_ = (process_pm_expression e.right tbls var_count cte_tbl oc) in 
                      let t = relational_type_check left_.rtipo right_.rtipo in
                        let nxt_temp = get_next_temporal var_count t in
                          fprintf oc "%s %d %d %d\n" "!=" left_.address right_.address nxt_temp;
                          {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | OExp e -> process_pm_expression e tbls var_count cte_tbl oc

and process_and_expression exp tbls var_count cte_tbl oc = 
  match exp with
  | AndExp e -> let left_ = (process_or_expression e.left tbls var_count cte_tbl oc) in 
                  let right_ = (process_or_expression e.right tbls var_count cte_tbl oc) in 
                    let t = logical_type_check left_.rtipo right_.rtipo in
                      let nxt_temp = get_next_temporal var_count t in
                        fprintf oc "%s %d %d %d\n" "&&" left_.address right_.address nxt_temp;
                        {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
  | AExp e -> process_logic_expression e tbls var_count cte_tbl oc

and process_or_expression exp tbls var_count cte_tbl oc =
  match exp with
  | OrExp e -> let left_ = (process_and_expression e.left tbls var_count cte_tbl oc) in 
                  let right_ = (process_or_expression e.right tbls var_count cte_tbl oc) in 
                    let t = logical_type_check left_.rtipo right_.rtipo in
                      let nxt_temp = get_next_temporal var_count t in
                        fprintf oc "%s %d %d %d\n" "||" left_.address right_.address nxt_temp;
                        {rtipo=t; address=nxt_temp; dim1=1; dim2=1;};
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
    if fparam.dimension1 != ex.dim1 || fparam.dimension2 != ex.dim2 then failwith "Parameter dimensions do not correspond" 
    ex.dim2;
    fprintf oc "%s %d %d %d\n" "param" ex.address fparam.address (ex.dim1 * ex.dim2)

and variableLookupVarID var current_tbls= 
  match current_tbls.function_tbl with 
  | FuncTbl f -> (try (Hashtbl.find f.variables var)
                    with e -> 
                      match current_tbls.class_tbl with
                      | ClassTbl ct -> (try let var = (Hashtbl.find ct.vars var) in 
                      {name=var.name; tipo=var.tipo; address=(var.address - 50000); dimension1=var.dimension1; dimension2=var.dimension2; id_class=var.id_class} with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
                      | Nil -> raise e;)
  | FNil -> (match current_tbls.class_tbl with
            | ClassTbl ct -> (try (Hashtbl.find ct.vars var) with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
            | Nil -> failwith "No variable found";)

(*
  functionLookup
  Searches and returns function that might be inside a class or not.

  Parameters: f - Function to lookup. Tipo = Ast.var_id
              current_tbls - Tabla actual. Tipo = Util.current_tbls
              var_count - Cuenta global de variables. Tipo = (string, Ast.count_tbl) Hashtbl.t
              cte_tbl - Cuenta de constantes. Tipo Ast.cte_tbl
              oc - Archivo de salida donde se escribe el codigo intermedio
  Return: Util.quadInfo
*)
and functionLookup f current_tbls var_count const_tbl oc= 
  match f with 
  | VarFuncCall vf -> (
    match current_tbls.class_tbl with
      | ClassTbl ct -> (
        try let fres = (Hashtbl.find ct.funcs vf.func) in 
          fprintf oc "era %s\n" vf.func;
          checkFuncParamsRec fres.params vf.params current_tbls var_count const_tbl oc 1;
          fprintf oc "%s %s\n" "goSub" vf.func; (* Check initial address *)
          match fres.ftipo with
            | VoidTy -> {rtipo=fres.ftipo; address=0; dim1=fres.fdim1; dim2=fres.fdim2;}
            | _ -> let addr = get_next_temporal var_count fres.ftipo in fprintf oc "retVal %s %d %d\n" vf.func addr (fres.fdim1 * fres.fdim2); {rtipo=fres.ftipo; address=0; dim1=fres.fdim1; dim2=fres.fdim2;}
        with Not_found -> (
              try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in match tbl  with 
                                    | FuncT funct -> fprintf oc "era %s\n" vf.func;
                                      checkFuncParamsRec funct.params vf.params current_tbls var_count const_tbl oc 1;
                                      fprintf oc "%s %s\n" "goSub" vf.func; (* Check initial address *)
                                      match funct.ftipo with
                                        | VoidTy -> {rtipo=funct.ftipo; address=0; dim1=funct.fdim1; dim2=funct.fdim2;}
                                        | _ -> let addr = get_next_temporal var_count funct.ftipo in fprintf oc "retVal %s %d %d\n" vf.func addr (funct.fdim1 * funct.fdim2); {rtipo=funct.ftipo; address=addr; dim1=funct.fdim1; dim2=funct.fdim2;}
                                    | _ -> failwith "No function found"
              with Not_found -> failwith "No function found"
            ))
      | Nil -> (
        try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in 
          match tbl with 
            | FuncT funct -> fprintf oc "era %s\n" vf.func;
              checkFuncParamsRec funct.params vf.params current_tbls var_count const_tbl oc 1;
              fprintf oc "%s %s\n" "goSub" vf.func ; (* Check initial address *) 
              match funct.ftipo with
                | VoidTy -> {rtipo=funct.ftipo; address=0; dim1=funct.fdim1; dim2=funct.fdim2;}
                | _ -> let addr = get_next_temporal var_count funct.ftipo in fprintf oc "retVal %s %d %d\n" vf.func addr  (funct.fdim1 * funct.fdim2); {rtipo=funct.ftipo; address=addr; dim1=funct.fdim1; dim2=funct.fdim2;}
            | _ -> failwith "No function found"
        with Not_found -> failwith "No function found") )

(*
  variableInClassLookup
  Searches and returns a class attribute.

  Parameters: var_id - ID of the variable to lookup. Tipo = Ast.var_id
              current_tbls - Tabla actual. Tipo = Util.current_tbls
              var_count - Cuenta global de variables. Tipo = (string, Ast.count_tbl) Hashtbl.t
              const_tbl - Cuenta de constantes. Tipo Ast.cte_tbl
              oc - Archivo de salida donde se escribe el codigo intermedio
              global_tbl - Tabla global con informacion de todas las clases y funciones. Tipo = (string, Ast.high_level) Hashtbl.t
              class_idx = ID del objeto. Tipo = int
  Return: Util.quadInfo
*)
and variableInClassLookup var_id class_tbl var_count const_tbl oc current_tbls global_tbl class_idx=
  match var_id with
  | VarID v -> (try let res = (Hashtbl.find class_tbl.vars v.name) in {rtipo=res.tipo; address=res.address + 50000 + class_idx * 10000; dim1=res.dimension1; dim2=res.dimension2; } with Not_found -> failwith "No Variable found in class");
  | VarFuncCall vfunc -> (
    try let res = (Hashtbl.find class_tbl.funcs vfunc.func) in
      fprintf oc "class %d\n" (class_idx + 30000); 
      fprintf oc "era %s.%s\n" res.classInit vfunc.func;
      checkFuncParamsRec res.params vfunc.params {function_tbl=FNil; class_tbl=ClassTbl class_tbl; global_tbl=global_tbl} var_count const_tbl oc 1;
      fprintf oc "%s %s.%s\n" "goSub" res.classInit vfunc.func; (* Check initial address *)
      match res.ftipo with
        | VoidTy -> {rtipo=res.ftipo; address=0; dim1=1; dim2=1;}
        | _ -> let addr = get_next_temporal var_count res.ftipo in fprintf oc "retVal %s.%s %d %d\n" res.classInit vfunc.func addr  (res.fdim1 * res.fdim2); {rtipo=res.ftipo; address=addr; dim1=res.fdim1; dim2=res.fdim2;}
    with Not_found -> failwith "No function found in class");
  | VarArray varr ->  let index = process_pm_expression varr.expresion current_tbls var_count const_tbl oc in
                        if index.rtipo != IntTy then failwith "Array index must be int";
                        (* try *)let base = (Hashtbl.find class_tbl.vars varr.name) in
                          let next_ptr = get_next_pointer var_count base.tipo in
                            let next_tmp = get_next_temporal var_count IntTy in
                              fprintf oc "val %d %d -1\n" index.address base.dimension1;
                              fprintf oc "+ %d %d %d\n" (find_or_add_int_cte var_count const_tbl base.address) (find_or_add_int_cte var_count const_tbl (50000 + class_idx * 10000)) next_tmp;
                              fprintf oc "+ %d %d %d\n" next_tmp index.address next_ptr;
                              {rtipo=base.tipo; address=next_ptr; dim1=1; dim2=1;}
                        (* with Not_found -> failwith "No variable found in class" *)
  | Var2Array v2arr -> (try let res = (Hashtbl.find class_tbl.vars v2arr.name) in {rtipo=res.tipo; address=res.address + 50000 + class_idx * 10000; dim1=1; dim2=1;} with Not_found -> failwith "No Variable found in class"); (* Need to implement arrays *)
  | VarPoint vpoint -> failwith "class in class not supported yet"; (* Need to do class in class *)

and pointVarLookup vp current_tbls var_count const_tbl oc = 
  match vp with
  | VarPoint vpoint -> (let v = variableLookupVarID vpoint.name current_tbls in
                        match v.tipo with 
                        | ClassTy -> (
                            try let tbl = (Hashtbl.find current_tbls.global_tbl v.id_class) in
                              ( match tbl with 
                              | ClaseT ct -> variableInClassLookup vpoint.inner ct var_count const_tbl oc current_tbls current_tbls.global_tbl (v.address - 30000);
                              | _ -> failwith "element not part of class" )
                            with Not_found -> failwith "element not found");
                        | _ -> failwith "variable is not a class");
  | _ -> failwith "error";

  (*
  variableLookup
  Searches and returns a variable.

  Parameters: var_id - ID of the variable to lookup. Tipo = Ast.var_id
              current_tbls - Tabla actual. Tipo = Util.current_tbls
              var_count - Cuenta global de variables. Tipo = (string, Ast.count_tbl) Hashtbl.t
              const_tbl - Cuenta de constantes. Tipo Ast.cte_tbl
              oc - Archivo de salida donde se escribe el codigo intermedio
  Return: Util.quadInfo
*)
and variableLookup var_id current_tbls var_count const_tbl oc= 
  match var_id with
  | VarID v -> let res = (variableLookupVarID v.name current_tbls) in {rtipo=res.tipo; address=res.address; dim1=res.dimension1; dim2=res.dimension2;};
  | VarFuncCall vfunc -> (functionLookup (VarFuncCall vfunc) current_tbls var_count const_tbl oc);
  | VarArray varr -> let index = process_pm_expression varr.expresion current_tbls var_count const_tbl oc in
                        if index.rtipo != IntTy then failwith "Array index must be int";
                        let base = variableLookupVarID varr.name current_tbls in
                          let next_ptr = get_next_pointer var_count base.tipo in
                            fprintf oc "val %d %d -1\n" index.address base.dimension1;
                            fprintf oc "+ %d %d %d\n" (find_or_add_int_cte var_count const_tbl base.address) index.address next_ptr;
                            {rtipo=base.tipo; address=next_ptr; dim1=1; dim2=1;}
  | Var2Array v2arr ->  let index1 = process_pm_expression v2arr.expresion1 current_tbls var_count const_tbl oc in
                          if index1.rtipo != IntTy then failwith "Matrix first index must be int";
                          let index2 = process_pm_expression v2arr.expresion2 current_tbls var_count const_tbl oc in
                            if index2.rtipo != IntTy then failwith "Matrix second index must be int";
                            let base = variableLookupVarID v2arr.name current_tbls in
                              let next_ptr = get_next_pointer var_count base.tipo in
                                fprintf oc "val %d %d -1\n" index1.address base.dimension1;
                                fprintf oc "val %d %d -1\n" index2.address base.dimension2;
                                fprintf oc "+ %d %d %d\n" (find_or_add_int_cte var_count const_tbl base.address) index1.address next_ptr; 
                                let next_tmp = get_next_temporal var_count IntTy in
                                  fprintf oc "* %d %d %d\n" index2.address (find_or_add_int_cte var_count const_tbl base.dimension1) next_tmp;
                                  fprintf oc "+ %d %d %d\n" next_tmp next_ptr next_ptr;
                                  {rtipo=base.tipo; address=next_ptr; dim1=1; dim2=1;}

  | VarPoint vpoint -> (pointVarLookup (VarPoint vpoint) current_tbls var_count const_tbl oc);;