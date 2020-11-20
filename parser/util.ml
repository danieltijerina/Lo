open Ast
open VarTabl

type quadInfo = 
  {
    rtipo: type_def;
    address: int;
    dim1: int;
    dim2: int;
  }

type funcInfo =
  {
    variables: (string, variable) Hashtbl.t;
    var_count: (type_def, count_tbl) Hashtbl.t;
  }

type classTbl = 
  | ClassTbl of clase_tbl 
  | Nil;;

type functionTbl = 
  (* | FuncTbl of {variables: (string, variable) Hashtbl.t;
                count: (type_def, count_tbl) Hashtbl.t} *)
  | FuncTbl of funcInfo
  | FNil

type current_tbls = 
  {
    function_tbl: functionTbl;
    class_tbl: classTbl;
    global_tbl: (string, high_level) Hashtbl.t;
  }


