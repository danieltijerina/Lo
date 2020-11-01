open Ast
open VarTabl

type classTbl = 
  | ClassTbl of clase_tbl 
  | Nil;;

type current_tbls = 
  {
    function_tbl: (string, variable) Hashtbl.t;
    class_tbl: classTbl;
    global_tbl: (string, high_level) Hashtbl.t;
  }


let variableLookupVarID var current_tbls = 
  try (Hashtbl.find current_tbls.function_tbl var)
  with e -> 
    match current_tbls.class_tbl with
    | ClassTbl ct -> (try (Hashtbl.find ct.vars var) with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
    | Nil -> raise e;;

let functionLookup f current_tbls = 
  match f with 
  | VarFuncCall vf -> ( match current_tbls.class_tbl with 
                        | ClassTbl ct -> (try (Hashtbl.find ct.funcs vf.func).tipo with Not_found -> (
                                try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in match tbl  with 
                                                      | FuncT funct -> funct.tipo; 
                                                      | _ -> failwith "No function found"
                                with Not_found -> failwith "No function found"
                              ))
                        | Nil -> (try let tbl = (Hashtbl.find current_tbls.global_tbl vf.func) in match tbl  with 
                                                        | FuncT funct -> funct.tipo; 
                                                        | _ -> failwith "No function found"
                                  with Not_found -> failwith "No function found") )

let variableInClassLookup var_id class_tbl =
  match var_id with
  | VarID v -> (try (Hashtbl.find class_tbl.vars v.name).tipo with Not_found -> failwith "No Variable found in class");
  | VarFuncCall vfunc -> (try (Hashtbl.find class_tbl.funcs vfunc.func).tipo with Not_found -> failwith "No function found in class");
  | VarArray varr ->  (try (Hashtbl.find class_tbl.vars varr.name).tipo with Not_found -> failwith "No Variable found in class"); (* Need to implement arrays *)
  | Var2Array v2arr -> (try (Hashtbl.find class_tbl.vars v2arr.name).tipo with Not_found -> failwith "No Variable found in class"); (* Need to implement arrays *)
  | VarPoint vpoint -> failwith "class in class not supported yet";; (* Need to do class in class *)

let pointVarLookup vp current_tbls = 
  match vp with
  | VarPoint vpoint -> (let v = variableLookupVarID vpoint.name current_tbls in
                        match v.tipo with 
                        | ClassTy -> (
                            try let tbl = (Hashtbl.find current_tbls.global_tbl v.id_class) in
                              ( match tbl with 
                              | ClaseT ct -> variableInClassLookup vpoint.inner ct;
                              | _ -> failwith "element not part of class" )
                            with Not_found -> failwith "element not found");
                        | _ -> failwith "variable is not a class");
  | _ -> failwith "error";;


let rec variableLookup var_id current_tbls = 
  match var_id with
  | VarID v -> (variableLookupVarID v.name current_tbls).tipo;
  | VarFuncCall vfunc -> functionLookup (VarFuncCall vfunc) current_tbls;
  | VarArray varr ->  (variableLookupVarID varr.name current_tbls).tipo; (* Need to implement arrays *)
  | Var2Array v2arr -> (variableLookupVarID v2arr.name current_tbls).tipo; (* Need to implement arrays *)
  | VarPoint vpoint -> (pointVarLookup (VarPoint vpoint) current_tbls);;