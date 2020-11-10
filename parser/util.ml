open Ast
open VarTabl

type quadInfo = 
  {
    rtipo: type_def;
    address: int;
  }

type classTbl = 
  | ClassTbl of clase_tbl 
  | Nil;;

type functionTbl = 
  | FuncTbl of (string, variable) Hashtbl.t
  | FNil

type current_tbls = 
  {
    function_tbl: functionTbl;
    class_tbl: classTbl;
    global_tbl: (string, high_level) Hashtbl.t;
  }


