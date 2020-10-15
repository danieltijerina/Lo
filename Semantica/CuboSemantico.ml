type op = Plus | Minus | Times | Div | LT | GT | LE | GE | NE | EE | And | Or;;
type ttype = Int | Float | Char | String | Bool;;

(* Operador + *)
let sum_type_check a b =
	match a, b with
	| Int, Int -> Int
	| Int, Float -> Float
	| Float, Int -> Float
	| Float, Float -> Float
	| x, y -> assert false;;

(* Operador - *)
let sub_type_check a b =
	match a, b with
	| Int, Int -> Int
	| Int, Float -> Float
	| Float, Int -> Float
	| Float, Float -> Float
	| x, y -> assert false;;

(* Operador * *)
let times_type_check a b =
	match a, b with
	| Int, Int -> Int
	| Int, Float -> Float
	| Float, Int -> Float
	| Float, Float -> Float
	| x, y -> assert false;;

(* Operador / *)
let div_type_check a b =
	match a, b with
	| Int, Int -> Int
	| Int, Float -> Float
	| Float, Int -> Float
	| Float, Float -> Float
	| x, y -> assert false;;

(* Operadores < > <= >= != == *)
let relational_type_check a b =
	match a, b with
	| Int, Float -> Bool
	| Int, Int -> Bool
	| Float, Float -> Bool
	| Float, Int -> Bool
	|x, y -> assert false;;

(* Operadores && || *)
let logical_type_check a b =
	match a, b with
	| Bool, Bool -> Bool
	| x, y -> assert false

let type_check op arg1 arg2 =
	match op with
	| Plus -> sum_type_check arg1 arg2
	| Minus -> sub_type_check arg1 arg2
	| Times -> times_type_check arg1 arg2
	| Div -> div_type_check arg1 arg2
	| LT -> relational_type_check arg1 arg2
	| GT -> relational_type_check arg1 arg2
	| LE -> relational_type_check arg1 arg2
	| GE -> relational_type_check arg1 arg2
	| NE -> relational_type_check arg1 arg2
	| EE -> relational_type_check arg1 arg2
	| And -> logical_type_check arg1 arg2
	| Or -> logical_type_check arg1 arg2;;