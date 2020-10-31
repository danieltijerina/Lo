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
  try (Hashtbl.find current_tbls.function_tbl var).tipo
  with e -> 
    match current_tbls.class_tbl with
    | ClassTbl ct -> (try (Hashtbl.find ct.vars var).tipo with Not_found -> (failwith (String.concat var ["Variable "; " not found"] )));
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

let pointVarLookup vp current_tbls = 
  match vp with
  | VarPoint vpoint -> print_endline vpoint.name; VoidTy;;


let rec variableLookup var_id current_tbls = 
  match var_id with
  | VarID v -> variableLookupVarID v.name current_tbls;
  | VarFuncCall vfunc -> functionLookup (VarFuncCall vfunc) current_tbls;
  | VarArray varr ->  variableLookupVarID varr.name current_tbls; (* Need to implement arrays *)
  | Var2Array v2arr -> variableLookupVarID v2arr.name current_tbls; (* Need to implement arrays *)
  | VarPoint vpoint -> pointVarLookup (VarPoint vpoint) current_tbls;;