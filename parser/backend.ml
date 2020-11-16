open Ast
open VarTabl
open ExpValidator
open Util
open Printf

let process_expression exp tbls var_count cte_tbl oc = 
  process_or_expression exp tbls var_count cte_tbl oc

(* Semantics *)

let print_counts oc k var_count =
  fprintf oc "%s\n" k;
  fprintf oc "IntTy %d\n" (Hashtbl.find var_count IntTy).count;
  fprintf oc "FloatTy %d\n" (Hashtbl.find var_count FloatTy).count;
  fprintf oc "CharTy %d\n" (Hashtbl.find var_count CharTy).count;
  fprintf oc "StringTy %d\n" (Hashtbl.find var_count StringTy).count;
  fprintf oc "BoolTy %d\n" (Hashtbl.find var_count BoolTy).count;
  fprintf oc "ClassTy %d\n" (Hashtbl.find var_count ClassTy).count;
  fprintf oc "IntTmp %d\n" (Hashtbl.find var_count IntTmp).count;
  fprintf oc "FloatTmp %d\n" (Hashtbl.find var_count FloatTmp).count;
  fprintf oc "CharTmp %d\n" (Hashtbl.find var_count CharTmp).count;
  fprintf oc "StringTmp %d\n" (Hashtbl.find var_count StringTmp).count;
  fprintf oc "BoolTmp %d\n" (Hashtbl.find var_count BoolTmp).count

let print_floating oc key value = 
  fprintf oc "%d %f\n" value key

let print_integers oc key value = 
  fprintf oc "%d %d\n" value key

let print_strings oc key value = 
  fprintf oc "%d %s\n" value key

let print_chars oc key value = 
  fprintf oc "%d %c\n" value key

let print_bools oc key value = 
  fprintf oc "%d %b\n" value key

let print_constants oc cte_table = 
  fprintf oc "\n$$$$ \n";
  fprintf oc "ints %d\n" (Hashtbl.length cte_table.integer);
  Hashtbl.iter (print_integers oc) cte_table.integer;
  fprintf oc "float %d\n" (Hashtbl.length cte_table.floating);
  Hashtbl.iter (print_floating oc) cte_table.floating;
  fprintf oc "string %d\n" (Hashtbl.length cte_table.strings);
  Hashtbl.iter (print_strings oc) cte_table.strings;
  fprintf oc "chars %d\n" (Hashtbl.length cte_table.characters);
  Hashtbl.iter (print_chars oc) cte_table.characters;
  fprintf oc "bools %d\n" (Hashtbl.length cte_table.booleans);
  Hashtbl.iter (print_bools oc) cte_table.booleans

let get_next_tag var_count = 
  let tmp = Hashtbl.find var_count JTag in 
    update_count var_count JTag;
    tmp.base + tmp.count

(* Match the variable declaration to add it to the table*)
let add_vars_to_tbl t var class_id tbl var_count cte_tbl =
  match var.id with
  | VDVarID v -> update_count var_count t; add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address= let tmp = Hashtbl.find var_count t in tmp.base + tmp.count - 1};
  | VDVarArray v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address=0} (* Need to add array part *)
  | VDVar2Array v -> add_element tbl v.name {name=v.name; tipo=t; id_class=class_id; address=0};; (* Need to add array part *)

let process_asignacion left right tbls var_count cte_tbl oc = 
  let leftVar = variableLookup left tbls var_count cte_tbl oc in
    let rightExp = process_expression right tbls var_count cte_tbl oc in
      assert_equal leftVar.rtipo rightExp.rtipo;
      fprintf oc "%s %d %d %d\n" "=" leftVar.address rightExp.address (-1)

let rec add_vars_to_func_tbl_rec t vars class_id tbl var_count cte_tbl oc = 
  match vars with
  | [] -> ();
  | (f::fs) -> 
      (* let new_var = add_vars_to_tbl t f class_id (match tbl.function_tbl with | FuncTbl f -> f.variables | FNil -> failwith "Not valid") var_count cte_tbl in *)
      let new_var = add_vars_to_tbl t f class_id (match tbl.function_tbl with | FuncTbl f -> f.variables | FNil -> failwith "Not valid") (match tbl.function_tbl with | FuncTbl f -> f.var_count | FNil -> failwith "Not valid") cte_tbl in
        let exp = f.right in
          match exp with
          | VDExp e ->  let tmp = process_expression e tbl var_count cte_tbl oc in
                          assert_equal tmp.rtipo t;
                          fprintf oc "%s %d %d %d\n" "=" new_var.address tmp.address (-1);
                          add_vars_to_func_tbl_rec t fs class_id tbl var_count cte_tbl oc
          | _ -> add_vars_to_func_tbl_rec t fs class_id tbl var_count cte_tbl oc

(* Expresion validation in class variable declaration is STILL missing *)
let rec add_vars_to_class_tbl_rec t vars class_id tbl var_count cte_tbl = 
  match vars with
  | [] -> ();
  | (f::fs) -> 
      add_vars_to_tbl t f class_id tbl.vars var_count cte_tbl;
      add_vars_to_class_tbl_rec t fs class_id tbl var_count cte_tbl

let process_print exp tbl var_count cte_tbl oc=
  let tmp = process_expression exp tbl var_count cte_tbl oc in
    match tmp.rtipo with
    | CharTy -> fprintf oc "%s %d %d %d\n" "print" tmp.address (-1) (-1)
    | StringTy -> fprintf oc "%s %d %d %d\n" "print" tmp.address (-1) (-1)
    | IntTy -> fprintf oc "%s %d %d %d\n" "print" tmp.address (-1) (-1)
    | FloatTy -> fprintf oc "%s %d %d %d\n" "print" tmp.address (-1) (-1)
    | _ -> failwith "Non-printable type"

let rec process_print_rec exps tbl var_count cte_tbl oc=
  match exps with
  | [] -> ()
  | (e::es) -> process_print e tbl var_count cte_tbl oc; process_print_rec es tbl var_count cte_tbl oc

let check_condition_type ty = 
  match ty with
  | BoolTy -> ()
  | _ -> failwith "Condition expression is not bool"

let process_condition exp tbl var_count cte_tbl oc=
  let exp_result = (process_expression exp tbl var_count cte_tbl oc) in
    check_condition_type exp_result.rtipo;
    exp_result
and check_else_block block = 
  match block with
  | [] -> false
  | _ -> true

let getFunctionTbl name tbl = 
  let ftbl = (Hashtbl.find tbl name) in
    match ftbl with
    | FuncT ftbl -> ftbl.variables
    | ClaseT ctbl -> failwith "Function not found"

let getFunctionTblInClass name classTbl = 
  match classTbl with 
    | ClaseT ctbl -> (Hashtbl.find ctbl.funcs name).variables
    | FuncT ftbl -> failwith "Classtbl is a function"

let getClaseTbl high_level =
  match high_level with
  | ClaseT ct -> ct
  | FuncT ft -> failwith "Should be a classtable"

(* Match estatuto to add variable to table *)
let rec add_func_elems_to_tbl elem tbls var_count cte_tbl ft jmp_count oc=
  match elem with 
  | Asigna a -> process_asignacion a.izq a.der tbls var_count cte_tbl oc; 
  | CondIf cif -> (let cond = process_condition cif.cond tbls var_count cte_tbl oc in
                    let next_tag = get_next_tag jmp_count in 
                      fprintf oc "%s %d %d\n" "gotoF" cond.address next_tag;
                      process_block cif.true_block tbls var_count cte_tbl ft jmp_count oc;
                      let else_block = check_else_block cif.false_block in 
                        match else_block with
                        | true -> ( let final_tag = get_next_tag jmp_count in 
                                      fprintf oc "%s %d\n" "goto" final_tag;
                                      fprintf oc "%s %d\n" "tag" next_tag;
                                      process_block cif.false_block tbls var_count cte_tbl ft jmp_count oc;
                                      fprintf oc "%s %d\n" "tag" final_tag;
                                      )
                        | false -> ( fprintf oc "%s %d\n" "tag" next_tag; );
                    )
  | Escritura e -> process_print_rec e tbls var_count cte_tbl oc;
  | EVar evar -> add_vars_to_func_tbl_rec evar.tipo evar.vars evar.id_class tbls var_count cte_tbl oc;
  | ForLoop floop -> process_for_loop floop tbls var_count cte_tbl ft jmp_count oc;
  | WhileLoop wloop -> (let starter_tag = get_next_tag jmp_count in
                          fprintf oc "%s %d\n" "tag" starter_tag; 
                          let cond = process_condition wloop.cond tbls var_count cte_tbl oc in
                            let end_tag = get_next_tag jmp_count in
                              fprintf oc "%s %d %d\n" "gotoF" cond.address end_tag;
                              process_block wloop.bloque tbls var_count cte_tbl ft jmp_count oc;
                              fprintf oc "%s %d\n" "goto" starter_tag;
                              fprintf oc "%s %d\n" "tag" end_tag; 
                              )
  | Return r -> assert_equal ft (process_expression r tbls var_count cte_tbl oc).rtipo; ();
  | Expresion ex -> assert_equal VoidTy (process_expression ex tbls var_count cte_tbl oc).rtipo; ();
(* Iterate through the function elements to add variables to the tbl *)
and add_func_elems_to_tbl_rec bloque tbls var_count cte_tbl ft jmp_count oc=
  match bloque with
  | [] -> ()
  | (f::fs) -> 
      add_func_elems_to_tbl f tbls var_count cte_tbl ft jmp_count oc;
      add_func_elems_to_tbl_rec fs tbls var_count cte_tbl ft jmp_count oc
(* Process blocks for conditions and loops *)
and process_block bloque tbl var_count cte_tbl ft jmp_count oc=
  add_func_elems_to_tbl_rec bloque tbl var_count cte_tbl ft jmp_count oc
and process_for_loop floop tbl var_count cte_tbl ft jmp_count oc= 
  variableLookup floop.init tbl;
  let starter_tag = get_next_tag jmp_count in
    fprintf oc "%s %d\n" "tag" starter_tag; 
    let cond = process_expression floop.cond tbl var_count cte_tbl oc in
      let end_tag = get_next_tag jmp_count in
        assert_equal BoolTy cond.rtipo;
        fprintf oc "%s %d %d\n" "gotoF" cond.address end_tag;
        process_block floop.bloque tbl var_count cte_tbl ft jmp_count oc;
        add_func_elems_to_tbl (Asigna floop.post) tbl var_count cte_tbl ft jmp_count oc;
        fprintf oc "%s %d\n" "goto" starter_tag;
        fprintf oc "%s %d\n" "tag" end_tag;;
        
(* Match the element of a class to a function and add the function elements to tbl *)
(* FALTA INICIALIZAR EL VARCOUNT DE LA FUNCION *)
let add_inner_fucs_of_class elem class_tbl tbl var_count cte_tbl jmp_count oc=
  match elem, class_tbl with
  | Fun f, ClaseT ct -> fprintf oc "ftag %s.%s\n" f.fname ct.name;
  add_func_elems_to_tbl_rec f.fbloque { function_tbl=FuncTbl({variables=getFunctionTblInClass f.fname class_tbl; var_count=Hashtbl.create 0;}); class_tbl=ClassTbl (getClaseTbl class_tbl); global_tbl=tbl} var_count cte_tbl f.tipo jmp_count oc; ();
  | CVar cv, ClaseT ct -> ();
  | _, _ -> assert false;;

(* Iterate through the class to check on functions *)
let rec add_inner_fucs_of_class_rec bloque class_tbl tbl var_count cte_tbl jmp_count oc = 
  match bloque with
  | [] -> ();
  | (f::fs) -> 
      add_inner_fucs_of_class f class_tbl tbl var_count cte_tbl jmp_count oc;
      add_inner_fucs_of_class_rec fs class_tbl tbl var_count cte_tbl jmp_count oc;;

(* Check elem to match class of function, then add all variables in functions to table*)
let add_inner_func_to_tbl elem tbl var_count cte_tbl jmp_count mem oc = 
  match elem with
  | Func f -> let fun_var_count = Hashtbl.find mem f.fname in 
                fprintf oc "ftag %s\n" f.fname;
                add_func_elems_to_tbl_rec f.fbloque { function_tbl=FuncTbl({variables=getFunctionTbl f.fname tbl; var_count=fun_var_count}); class_tbl=Nil; global_tbl=tbl} fun_var_count cte_tbl f.tipo jmp_count oc;
                (* add_func_elems_to_tbl_rec f.fbloque { function_tbl=FuncTbl(getFunctionTbl f.fname tbl); class_tbl=Nil; global_tbl=tbl} var_count cte_tbl f.tipo oc; *)
                fprintf oc "%s\n" "endFunc -1 -1 -1";
                (* fprintf oc "AHI TE VAN LOS DE LA FUNCION %s \n\n" f.fname; *)
                (* print_counts fun_var_count oc; *)
                (* fprintf oc "\nESOS FUERON TODOS LOS DE LA FUNCION \n\n"; *)
  | Clase c -> add_inner_fucs_of_class_rec c.bloque (Hashtbl.find tbl c.name) tbl var_count cte_tbl jmp_count oc;;

(* Processing a single element of each class *)
let add_class_att_to_table_inner elem class_tbl var_count cte_tbl = 
  match elem, class_tbl with
  | Fun f, ClaseT ctbl -> (* add_func class_tbl f.fname {name=f.fname; ftipo=f.tipo; variables=Hashtbl.create 0; params=(getVariablesFromParamsRec f.params);}; *) [];
  | CVar cv, ClaseT ctbl -> add_vars_to_class_tbl_rec cv.tipo cv.vars cv.id_class ctbl var_count cte_tbl; [];; (*If type is class, validate class type *)

(* Iterating through all the class variables and funcs *)
let rec add_class_att_to_table bloque class_tbl var_count cte_tbl = 
  match bloque with 
  | [] -> class_tbl;
  | (f::fs) -> 
      add_class_att_to_table_inner f class_tbl var_count cte_tbl;
      add_class_att_to_table fs class_tbl var_count cte_tbl;;

(* Processes single element of high order *)
let add_upper_prog_to_table elem table var_count cte_tbl mem = 
  match elem with
  | Func f -> add_high_level_element table elem mem
  | Clase c -> let class_tbl = add_high_level_element table elem mem in add_class_att_to_table c.bloque class_tbl var_count cte_tbl;;

(* Iterating through all the elemnts in tree *)
let rec semantic_main tree table var_count cte_tbl jmp_count mem oc = 
  match tree with 
  | Program [] -> table
  | Program (i :: j) ->
      add_upper_prog_to_table i table var_count cte_tbl mem; (* Adds high level funcs and classes to main table, including vars and funcs of class *)
      semantic_main (Program j) table var_count cte_tbl jmp_count mem oc; (* Iterate *)
      add_inner_func_to_tbl i table var_count cte_tbl jmp_count mem oc; (* Process functions of classes or funcs *)
      table;;

let initialize_jmp tbl =
  Hashtbl.add tbl JTag {count=0; base=0}

let print_mem mem oc =
  Hashtbl.iter (print_counts oc) mem
  
(* Main Semantic start *)
let semantic_start tree oc = 
  let main_table = Hashtbl.create 1234 in
    let var_count = Hashtbl.create 1234 in
      let cte_table = {integer=Hashtbl.create 123; floating=Hashtbl.create 123; strings=Hashtbl.create 123; characters=Hashtbl.create 123; booleans=Hashtbl.create 123} in
        let jmp_count = Hashtbl.create 123 in
          let mem = Hashtbl.create 123 in
            initialize_count var_count;
            initialize_jmp jmp_count;
            semantic_main tree main_table var_count cte_table jmp_count mem oc;
            fprintf oc "\n$$$\n";
            Hashtbl.iter (print_counts oc) mem;
            print_constants oc cte_table
            (* print_counts oc var_count *)
    