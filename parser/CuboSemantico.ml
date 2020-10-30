type op = CsPlus | CsMinus | CsTimes | CsDiv | CsLT | CsGT | CsLE | CsGE | CsNE | CsEE | CsAnd | CsOr;;
type ttype = CsInt | CsFloat | CsChar | CsString | CsBool;;

(* Operador + *)
let sum_type_check a b =
	match a, b with
	| CsInt, CsInt -> CsInt
	| CsInt, CsFloat -> CsFloat
	| CsFloat, CsInt -> CsFloat
	| CsFloat, CsFloat -> CsFloat
	| x, y -> assert false;;

(* Operador - *)
let sub_type_check a b =
	match a, b with
	| CsInt, CsInt -> CsInt
	| CsInt, CsFloat -> CsFloat
	| CsFloat, CsInt -> CsFloat
	| CsFloat, CsFloat -> CsFloat
	| x, y -> assert false;;

(* Operador * *)
let times_type_check a b =
	match a, b with
	| CsInt, CsInt -> CsInt
	| CsInt, CsFloat -> CsFloat
	| CsFloat, CsInt -> CsFloat
	| CsFloat, CsFloat -> CsFloat
	| x, y -> assert false;;

(* Operador / *)
let div_type_check a b =
	match a, b with
	| CsInt, CsInt -> CsInt
	| CsInt, CsFloat -> CsFloat
	| CsFloat, CsInt -> CsFloat
	| CsFloat, CsFloat -> CsFloat
	| x, y -> assert false;;

(* Operadores < > <= >= != == *)
let relational_type_check a b =
	match a, b with
	| CsInt, CsFloat -> CsBool
	| CsInt, CsInt -> CsBool
	| CsFloat, CsFloat -> CsBool
	| CsFloat, CsInt -> CsBool
	|x, y -> assert false;;

(* Operadores && || *)
let logical_type_check a b =
	match a, b with
	| CsBool, CsBool -> CsBool
	| x, y -> assert false

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