(* 
	CuboSemantico
	Modulo encargado de hacer respetar los tipos de datos en expresiones.
*)

open Ast
type op = CsPlus | CsMinus | CsTimes | CsDiv | CsLT | CsGT | CsLE | CsGE | CsNE | CsEE | CsAnd | CsOr;;

(* Operador + *)
let sum_type_check a b =
	match a, b with
	| IntTy, IntTy -> IntTy
	| IntTy, FloatTy -> FloatTy
	| FloatTy, IntTy -> FloatTy
	| FloatTy, FloatTy -> FloatTy
	| x, y -> failwith "Invalid opoerands for addition operator";;

(* Operador - *)
let sub_type_check a b =
	match a, b with
	| IntTy, IntTy -> IntTy
	| IntTy, FloatTy -> FloatTy
	| FloatTy, IntTy -> FloatTy
	| FloatTy, FloatTy -> FloatTy
	| x, y -> failwith "Invalid operands for subtraction operator";;

(* Operador * *)
let times_type_check a b =
	match a, b with
	| IntTy, IntTy -> IntTy
	| IntTy, FloatTy -> FloatTy
	| FloatTy, IntTy -> FloatTy
	| FloatTy, FloatTy -> FloatTy
	| x, y -> failwith "Invalid operands for multiplication operator";;

(* Operador / *)
let div_type_check a b =
	match a, b with
	| IntTy, IntTy -> IntTy
	| IntTy, FloatTy -> FloatTy
	| FloatTy, IntTy -> FloatTy
	| FloatTy, FloatTy -> FloatTy
	| x, y -> failwith "Invalid operands for division operator";;

(* Operadores < > <= >= != == *)
let relational_type_check a b =
	match a, b with
	| IntTy, FloatTy -> BoolTy
	| IntTy, IntTy -> BoolTy
	| FloatTy, FloatTy -> BoolTy
	| FloatTy, IntTy -> BoolTy
	| BoolTy, BoolTy -> BoolTy
	|x, y -> failwith "Invalid operands for relational operator";;

(* Operadores && || *)
let logical_type_check a b =
	match a, b with
	| BoolTy, BoolTy -> BoolTy
	| x, y -> failwith "Invalid operands for logical operator"

let type_check op arg1 arg2 =
	match op with
	| CsPlus -> sum_type_check arg1 arg2
	| CsMinus -> sub_type_check arg1 arg2
	| CsTimes -> times_type_check arg1 arg2
	| CsDiv -> div_type_check arg1 arg2
	| CsLT -> relational_type_check arg1 arg2
	| CsGT -> relational_type_check arg1 arg2
	| CsLE -> relational_type_check arg1 arg2
	| CsGE -> relational_type_check arg1 arg2
	| CsNE -> relational_type_check arg1 arg2
	| CsEE -> relational_type_check arg1 arg2
	| CsAnd -> logical_type_check arg1 arg2
	| CsOr -> logical_type_check arg1 arg2;;