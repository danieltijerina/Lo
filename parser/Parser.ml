type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | INTTYPE
  | FLOATTYPE
  | CHARTYPE
  | STRINGTYPE
  | BOOLTYPE
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | EOL
  | EQ
  | GT
  | GE
  | LT
  | LE
  | NE
  | EE
  | AND
  | OR
  | PRINT
  | VAR
  | IF
  | ELSE
  | MAIN
  | FOR
  | WHILE
  | RETURN
  | IN
  | FUNC
  | CLASS
  | CONSTRUCTOR
  | COMMA
  | COLON
  | SEMICOLON
  | POINT
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 2 "Parser.mly"
open Printf
open Lexing

type variable = {
  name : string;
  tipo : string;
}

type funcion = {
  name : string;
  tipo : string;
  variables: (string, variable) Hashtbl.t;
}

type clase = {
  name : string;
  tipo : string;
  funcs : (string, funcion) Hashtbl.t;
  vars : (string, variable) Hashtbl.t;
}

type high_level = 
  | Clase of clase
  | Func of funcion

type op = Plus | Minus | Times | Div | LT | GT | LE | GE | NE | EE | And | Or;;
type ttype = Int | Float | Char | String | Bool

let var_table = Hashtbl.create 123
# 83 "Parser.ml"
let yytransl_const = [|
  260 (* INTTYPE *);
  261 (* FLOATTYPE *);
  262 (* CHARTYPE *);
  263 (* STRINGTYPE *);
  264 (* BOOLTYPE *);
  265 (* TRUE *);
  266 (* FALSE *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIV *);
  271 (* LBRACE *);
  272 (* RBRACE *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LBRACK *);
  276 (* RBRACK *);
  277 (* EOL *);
  278 (* EQ *);
  279 (* GT *);
  280 (* GE *);
  281 (* LT *);
  282 (* LE *);
  283 (* NE *);
  284 (* EE *);
  285 (* AND *);
  286 (* OR *);
  287 (* PRINT *);
  288 (* VAR *);
  289 (* IF *);
  290 (* ELSE *);
  291 (* MAIN *);
  292 (* FOR *);
  293 (* WHILE *);
  294 (* RETURN *);
  295 (* IN *);
  296 (* FUNC *);
  297 (* CLASS *);
  298 (* CONSTRUCTOR *);
  299 (* COMMA *);
  300 (* COLON *);
  301 (* SEMICOLON *);
  302 (* POINT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  303 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\003\000\004\000\
\007\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\005\000\005\000\011\000\011\000\012\000\012\000\009\000\009\000\
\013\000\013\000\013\000\013\000\013\000\006\000\006\000\014\000\
\014\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\016\000\023\000\023\000\023\000\023\000\025\000\026\000\
\026\000\018\000\027\000\027\000\021\000\021\000\017\000\029\000\
\029\000\028\000\028\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\024\000\024\000\024\000\031\000\031\000\031\000\
\032\000\032\000\032\000\033\000\033\000\033\000\033\000\033\000\
\034\000\034\000\034\000\034\000\035\000\035\000\035\000\036\000\
\036\000\036\000\010\000\010\000\010\000\037\000\037\000\037\000\
\038\000\038\000\019\000\020\000\022\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\002\000\007\000\003\000\
\003\000\001\000\001\000\001\000\002\000\002\000\002\000\000\000\
\007\000\006\000\004\000\002\000\002\000\000\000\006\000\005\000\
\001\000\001\000\001\000\001\000\001\000\003\000\002\000\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\001\000\004\000\007\000\004\000\002\000\001\000\003\000\003\000\
\000\000\005\000\003\000\001\000\001\000\003\000\006\000\002\000\
\000\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\004\000\004\000\003\000\001\000\002\000\002\000\000\000\003\000\
\003\000\000\000\005\000\005\000\006\000\003\000\002\000\000\000\
\004\000\000\000\011\000\005\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\102\000\000\000\002\000\003\000\
\004\000\000\000\000\000\000\000\001\000\005\000\006\000\000\000\
\000\000\000\000\008\000\000\000\025\000\026\000\027\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\009\000\014\000\015\000\000\000\
\000\000\021\000\000\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\076\000\077\000\078\000\
\079\000\080\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\034\000\035\000\036\000\
\038\000\039\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\073\000\074\000\017\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\095\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\045\000\030\000\032\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\086\000\085\000\000\000\094\000\000\000\092\000\091\000\075\000\
\000\000\000\000\000\000\000\000\000\000\000\000\101\000\000\000\
\000\000\000\000\083\000\000\000\061\000\063\000\062\000\064\000\
\066\000\065\000\054\000\059\000\068\000\069\000\071\000\072\000\
\023\000\000\000\000\000\000\000\093\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\081\000\000\000\000\000\
\047\000\042\000\082\000\089\000\088\000\097\000\051\000\050\000\
\000\000\000\000\000\000\000\000\100\000\000\000\000\000\000\000\
\055\000\000\000\000\000\000\000\000\000\048\000\056\000\000\000\
\000\000\043\000\000\000\000\000\000\000\099\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\032\000\052\000\019\000\033\000\
\034\000\035\000\027\000\039\000\036\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\115\000\185\000\146\000\087\000\201\000\088\000\089\000\090\000\
\091\000\092\000\099\000\137\000\059\000\104\000"

let yysindex = "\018\000\
\241\254\000\000\048\255\016\255\000\000\241\254\000\000\000\000\
\000\000\080\255\106\255\126\255\000\000\000\000\000\000\116\255\
\112\000\030\255\000\000\101\255\000\000\000\000\000\000\000\000\
\000\000\117\255\132\255\113\255\146\255\162\255\163\255\030\255\
\197\255\030\255\030\255\168\255\212\255\171\255\210\255\117\255\
\183\255\213\255\177\255\000\000\000\000\000\000\000\000\043\255\
\210\255\000\000\102\255\000\000\210\255\171\255\117\000\011\255\
\223\255\112\255\188\255\188\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\112\255\224\255\225\255\226\255\231\255\
\112\255\254\254\000\000\234\255\181\255\000\000\000\000\000\000\
\000\000\000\000\195\255\000\000\227\255\180\255\235\255\222\255\
\119\255\123\255\000\000\000\000\000\000\000\000\210\255\246\255\
\013\255\228\255\248\255\247\255\005\255\000\000\206\255\229\255\
\243\255\252\255\112\255\112\255\232\255\112\255\244\255\011\255\
\112\255\250\255\000\000\000\000\000\000\000\000\112\255\112\255\
\112\255\112\255\112\255\112\255\112\255\112\255\112\255\112\255\
\112\255\112\255\112\255\000\000\210\255\112\255\145\255\251\255\
\000\000\000\000\254\255\000\000\043\255\000\000\000\000\000\000\
\230\255\002\000\016\000\253\254\011\000\017\000\000\000\022\000\
\037\000\022\255\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\013\255\228\255\000\000\188\255\112\255\019\000\
\210\255\112\255\020\000\112\255\210\255\000\000\253\255\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\046\000\255\255\024\000\000\000\112\255\022\255\210\255\
\000\000\253\255\034\000\232\255\063\000\000\000\000\000\255\255\
\071\000\000\000\112\255\066\000\210\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\079\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\086\000\000\000\000\000\000\000\000\000\000\000\084\000\
\000\000\088\000\110\000\000\000\000\000\000\000\000\000\086\000\
\116\255\000\000\222\254\000\000\000\000\000\000\000\000\222\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\109\000\
\000\000\000\000\057\000\057\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\233\255\000\000\000\000\113\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\255\099\255\024\255\
\062\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\000\114\000\000\000\000\000\209\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\109\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\222\254\000\000\000\000\000\000\
\115\000\000\000\000\000\240\254\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\085\000\114\000\000\000\057\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\164\255\000\000\028\255\000\000\000\000\000\000\025\000\000\000\
\000\000\057\255\000\000\000\000\000\000\000\000\000\000\028\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\100\001\122\001\090\000\207\255\000\000\092\000\
\000\000\245\255\098\000\091\001\249\255\057\001\000\000\000\000\
\000\000\000\000\000\000\000\000\201\255\000\000\150\255\221\255\
\000\000\149\255\217\000\010\001\000\000\000\000\027\000\000\000\
\000\000\147\255\026\001\166\255\215\255\198\255"

let yytablesize = 394
let yytable = "\061\000\
\098\000\105\000\149\000\093\000\155\000\046\000\060\000\138\000\
\096\000\028\000\096\000\062\000\063\000\064\000\112\000\178\000\
\113\000\111\000\001\000\065\000\066\000\112\000\102\000\134\000\
\003\000\004\000\155\000\068\000\046\000\112\000\050\000\134\000\
\106\000\021\000\022\000\023\000\024\000\025\000\112\000\075\000\
\134\000\058\000\179\000\114\000\043\000\132\000\028\000\028\000\
\060\000\049\000\136\000\145\000\147\000\058\000\150\000\135\000\
\098\000\097\000\136\000\060\000\060\000\057\000\012\000\156\000\
\058\000\075\000\058\000\184\000\058\000\029\000\163\000\030\000\
\049\000\060\000\155\000\060\000\031\000\153\000\044\000\172\000\
\188\000\189\000\010\000\169\000\157\000\158\000\159\000\160\000\
\161\000\162\000\009\000\206\000\165\000\166\000\011\000\015\000\
\016\000\209\000\170\000\174\000\206\000\044\000\062\000\063\000\
\064\000\021\000\022\000\023\000\024\000\025\000\065\000\066\000\
\062\000\063\000\064\000\190\000\053\000\067\000\068\000\145\000\
\065\000\066\000\017\000\044\000\196\000\046\000\047\000\193\000\
\068\000\128\000\129\000\197\000\069\000\020\000\070\000\130\000\
\131\000\071\000\072\000\073\000\018\000\053\000\194\000\053\000\
\037\000\062\000\063\000\064\000\074\000\040\000\207\000\094\000\
\096\000\065\000\066\000\212\000\167\000\168\000\101\000\041\000\
\038\000\068\000\205\000\214\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\021\000\022\000\
\023\000\024\000\025\000\057\000\057\000\062\000\063\000\064\000\
\021\000\022\000\023\000\024\000\025\000\065\000\066\000\171\000\
\011\000\056\000\057\000\057\000\057\000\068\000\058\000\057\000\
\057\000\057\000\120\000\121\000\122\000\123\000\124\000\125\000\
\042\000\043\000\057\000\069\000\045\000\070\000\048\000\049\000\
\071\000\072\000\073\000\084\000\084\000\084\000\084\000\100\000\
\051\000\054\000\084\000\074\000\084\000\055\000\103\000\084\000\
\084\000\084\000\084\000\084\000\084\000\084\000\084\000\118\000\
\107\000\108\000\109\000\084\000\084\000\084\000\084\000\110\000\
\119\000\116\000\127\000\084\000\141\000\084\000\046\000\084\000\
\084\000\084\000\084\000\084\000\084\000\084\000\084\000\133\000\
\126\000\139\000\140\000\070\000\070\000\144\000\135\000\198\000\
\175\000\142\000\070\000\176\000\070\000\084\000\148\000\070\000\
\070\000\070\000\070\000\070\000\070\000\070\000\070\000\143\000\
\151\000\177\000\181\000\084\000\084\000\084\000\084\000\182\000\
\154\000\101\000\173\000\070\000\203\000\070\000\049\000\084\000\
\084\000\084\000\084\000\084\000\084\000\084\000\084\000\180\000\
\183\000\186\000\187\000\082\000\082\000\082\000\082\000\192\000\
\200\000\202\000\195\000\199\000\204\000\084\000\044\000\082\000\
\082\000\082\000\082\000\082\000\082\000\082\000\082\000\067\000\
\208\000\067\000\210\000\213\000\067\000\067\000\067\000\067\000\
\067\000\067\000\067\000\067\000\211\000\082\000\016\000\084\000\
\084\000\084\000\084\000\010\000\022\000\098\000\084\000\011\000\
\067\000\013\000\067\000\084\000\084\000\084\000\084\000\084\000\
\084\000\084\000\084\000\021\000\022\000\023\000\024\000\025\000\
\021\000\022\000\023\000\024\000\025\000\012\000\087\000\014\000\
\033\000\026\000\053\000\090\000\052\000\117\000\095\000\191\000\
\164\000\152\000"

let yycheck = "\049\000\
\056\000\060\000\109\000\053\000\114\000\022\001\048\000\098\000\
\043\001\017\000\045\001\001\001\002\001\003\001\017\001\019\001\
\019\001\073\000\001\000\009\001\010\001\017\001\058\000\019\001\
\040\001\041\001\136\000\017\001\045\001\017\001\038\000\019\001\
\068\000\004\001\005\001\006\001\007\001\008\001\017\001\051\000\
\019\001\018\001\046\001\046\001\047\001\095\000\054\000\055\000\
\018\001\022\001\046\001\107\000\108\000\030\001\110\000\043\001\
\112\000\047\001\046\001\029\001\030\001\019\001\047\001\119\000\
\022\001\077\000\043\001\046\001\045\001\040\001\126\000\042\001\
\045\001\043\001\184\000\045\001\047\001\113\000\022\001\135\000\
\171\000\172\000\035\001\133\000\120\000\121\000\122\000\123\000\
\124\000\125\000\001\000\199\000\128\000\129\000\047\001\006\000\
\017\001\204\000\134\000\141\000\208\000\045\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\001\001\002\001\003\001\174\000\018\001\016\001\017\001\175\000\
\009\001\010\001\017\001\032\000\180\000\034\000\035\000\177\000\
\017\001\011\001\012\001\181\000\031\001\018\001\033\001\013\001\
\014\001\036\001\037\001\038\001\015\001\043\001\178\000\045\001\
\044\001\001\001\002\001\003\001\047\001\018\001\200\000\054\000\
\055\000\009\001\010\001\211\000\130\000\131\000\047\001\047\001\
\044\001\017\001\198\000\213\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\004\001\005\001\
\006\001\007\001\008\001\016\001\017\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\047\001\
\047\001\017\001\031\001\019\001\033\001\017\001\022\001\036\001\
\037\001\038\001\023\001\024\001\025\001\026\001\027\001\028\001\
\047\001\047\001\047\001\031\001\016\001\033\001\047\001\004\001\
\036\001\037\001\038\001\011\001\012\001\013\001\014\001\001\001\
\015\001\043\001\018\001\047\001\020\001\017\001\043\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\045\001\
\017\001\017\001\017\001\011\001\012\001\013\001\014\001\017\001\
\022\001\016\001\029\001\043\001\047\001\045\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\018\001\
\030\001\018\001\020\001\011\001\012\001\018\001\043\001\019\001\
\043\001\045\001\018\001\018\001\020\001\045\001\047\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\045\001\
\045\001\018\001\018\001\011\001\012\001\013\001\014\001\018\001\
\047\001\047\001\045\001\043\001\046\001\045\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\045\001\
\020\001\045\001\020\001\011\001\012\001\013\001\014\001\045\001\
\034\001\020\001\047\001\047\001\045\001\045\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\018\001\
\047\001\020\001\020\001\018\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\022\001\045\001\016\001\011\001\
\012\001\013\001\014\001\016\001\015\001\045\001\018\001\016\001\
\043\001\006\000\045\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\004\001\005\001\006\001\007\001\008\001\
\004\001\005\001\006\001\007\001\008\001\016\001\018\001\006\000\
\016\001\018\001\040\000\018\001\018\001\077\000\018\001\175\000\
\127\000\112\000"

let yynames_const = "\
  INTTYPE\000\
  FLOATTYPE\000\
  CHARTYPE\000\
  STRINGTYPE\000\
  BOOLTYPE\000\
  TRUE\000\
  FALSE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  EOL\000\
  EQ\000\
  GT\000\
  GE\000\
  LT\000\
  LE\000\
  NE\000\
  EE\000\
  AND\000\
  OR\000\
  PRINT\000\
  VAR\000\
  IF\000\
  ELSE\000\
  MAIN\000\
  FOR\000\
  WHILE\000\
  RETURN\000\
  IN\000\
  FUNC\000\
  CLASS\000\
  CONSTRUCTOR\000\
  COMMA\000\
  COLON\000\
  SEMICOLON\000\
  POINT\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'b) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'main) in
    Obj.repr(
# 51 "Parser.mly"
         ("adecuado")
# 434 "Parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'main) in
    Obj.repr(
# 52 "Parser.mly"
         ("adecuado")
# 441 "Parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clase) in
    Obj.repr(
# 55 "Parser.mly"
        ()
# 448 "Parser.ml"
               : 'b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funcion) in
    Obj.repr(
# 56 "Parser.mly"
            ()
# 455 "Parser.ml"
               : 'b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'b) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clase) in
    Obj.repr(
# 57 "Parser.mly"
            ()
# 463 "Parser.ml"
               : 'b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'b) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'funcion) in
    Obj.repr(
# 58 "Parser.mly"
              ()
# 471 "Parser.ml"
               : 'b))
; (fun __caml_parser_env ->
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 62 "Parser.mly"
                                               ()
# 478 "Parser.ml"
               : 'main))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'class_bloque) in
    Obj.repr(
# 66 "Parser.mly"
                        (print_string "class "; print_endline _2)
# 486 "Parser.ml"
               : 'clase))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cb) in
    Obj.repr(
# 69 "Parser.mly"
                   ()
# 493 "Parser.ml"
               : 'class_bloque))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funcion) in
    Obj.repr(
# 72 "Parser.mly"
          ()
# 500 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constructor) in
    Obj.repr(
# 73 "Parser.mly"
                ()
# 507 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decvar) in
    Obj.repr(
# 74 "Parser.mly"
           ()
# 514 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'funcion) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cb) in
    Obj.repr(
# 75 "Parser.mly"
               ()
# 522 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constructor) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cb) in
    Obj.repr(
# 76 "Parser.mly"
                   ()
# 530 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decvar) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cb) in
    Obj.repr(
# 77 "Parser.mly"
              ()
# 538 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "Parser.mly"
                ()
# 544 "Parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'f1) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'f2) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 82 "Parser.mly"
                                     (print_string "func w params "; print_endline _2)
# 554 "Parser.ml"
               : 'funcion))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'f2) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 83 "Parser.mly"
                                    (print_string "func w/o params "; print_endline _2)
# 563 "Parser.ml"
               : 'funcion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'tipo) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'f1) in
    Obj.repr(
# 86 "Parser.mly"
                   (print_string _1; print_string " "; print_endline _2)
# 572 "Parser.ml"
               : 'f1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tipo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "Parser.mly"
            (print_string _1; print_string " "; print_endline _2)
# 580 "Parser.ml"
               : 'f1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tipo) in
    Obj.repr(
# 90 "Parser.mly"
             ()
# 587 "Parser.ml"
               : 'f2))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "Parser.mly"
                ()
# 593 "Parser.ml"
               : 'f2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'f1) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 94 "Parser.mly"
                                         (print_string "cons w params "; print_endline _2)
# 602 "Parser.ml"
               : 'constructor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 95 "Parser.mly"
                                        (print_string "cons w/o params "; print_endline _2)
# 610 "Parser.ml"
               : 'constructor))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "Parser.mly"
          ("int")
# 616 "Parser.ml"
               : 'tipo))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "Parser.mly"
              ("float")
# 622 "Parser.ml"
               : 'tipo))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "Parser.mly"
             ("char")
# 628 "Parser.ml"
               : 'tipo))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "Parser.mly"
               ("string")
# 634 "Parser.ml"
               : 'tipo))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "Parser.mly"
             ("bool")
# 640 "Parser.ml"
               : 'tipo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'b1) in
    Obj.repr(
# 106 "Parser.mly"
                   ()
# 647 "Parser.ml"
               : 'bloque))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "Parser.mly"
                  ()
# 653 "Parser.ml"
               : 'bloque))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'estatuto) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'b1) in
    Obj.repr(
# 110 "Parser.mly"
              ()
# 661 "Parser.ml"
               : 'b1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'estatuto) in
    Obj.repr(
# 111 "Parser.mly"
              ()
# 668 "Parser.ml"
               : 'b1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'asignacion) in
    Obj.repr(
# 114 "Parser.mly"
             ()
# 675 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condicion) in
    Obj.repr(
# 115 "Parser.mly"
              ()
# 682 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'escritura) in
    Obj.repr(
# 116 "Parser.mly"
              ()
# 689 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decvar) in
    Obj.repr(
# 117 "Parser.mly"
           ()
# 696 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'forloop) in
    Obj.repr(
# 118 "Parser.mly"
            ()
# 703 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'whileloop) in
    Obj.repr(
# 119 "Parser.mly"
              ()
# 710 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expresion) in
    Obj.repr(
# 120 "Parser.mly"
                        ()
# 717 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return) in
    Obj.repr(
# 121 "Parser.mly"
           ()
# 724 "Parser.ml"
               : 'estatuto))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'asignavar) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expresion) in
    Obj.repr(
# 124 "Parser.mly"
                                   ()
# 732 "Parser.ml"
               : 'asignacion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 127 "Parser.mly"
                                         ()
# 741 "Parser.ml"
               : 'asignavar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 128 "Parser.mly"
                         ()
# 749 "Parser.ml"
               : 'asignavar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'av1) in
    Obj.repr(
# 129 "Parser.mly"
           ()
# 757 "Parser.ml"
               : 'asignavar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "Parser.mly"
       ()
# 764 "Parser.ml"
               : 'asignavar))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'av2) in
    Obj.repr(
# 133 "Parser.mly"
               ()
# 772 "Parser.ml"
               : 'av1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'av2) in
    Obj.repr(
# 136 "Parser.mly"
               ()
# 780 "Parser.ml"
               : 'av2))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "Parser.mly"
                ()
# 786 "Parser.ml"
               : 'av2))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'e1) in
    Obj.repr(
# 140 "Parser.mly"
                                   ()
# 793 "Parser.ml"
               : 'escritura))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expresion) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e1) in
    Obj.repr(
# 143 "Parser.mly"
                     ()
# 801 "Parser.ml"
               : 'e1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expresion) in
    Obj.repr(
# 144 "Parser.mly"
              ()
# 808 "Parser.ml"
               : 'e1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'e_exp) in
    Obj.repr(
# 147 "Parser.mly"
        ()
# 815 "Parser.ml"
               : 'expresion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expresion) in
    Obj.repr(
# 148 "Parser.mly"
                       ()
# 823 "Parser.ml"
               : 'expresion))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expresion) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'bloque) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'c1) in
    Obj.repr(
# 151 "Parser.mly"
                                       ()
# 832 "Parser.ml"
               : 'condicion))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 154 "Parser.mly"
              ()
# 839 "Parser.ml"
               : 'c1))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "Parser.mly"
                ()
# 845 "Parser.ml"
               : 'c1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'c_exp) in
    Obj.repr(
# 158 "Parser.mly"
        ()
# 852 "Parser.ml"
               : 'e_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'c_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e_exp) in
    Obj.repr(
# 159 "Parser.mly"
                    ()
# 860 "Parser.ml"
               : 'e_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 162 "Parser.mly"
      ()
# 867 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 163 "Parser.mly"
               ()
# 875 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 164 "Parser.mly"
               ()
# 883 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 165 "Parser.mly"
               ()
# 891 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 166 "Parser.mly"
               ()
# 899 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 167 "Parser.mly"
               ()
# 907 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 168 "Parser.mly"
               ()
# 915 "Parser.ml"
               : 'c_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'termino) in
    Obj.repr(
# 171 "Parser.mly"
          ()
# 922 "Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termino) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 172 "Parser.mly"
                     ()
# 930 "Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termino) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 173 "Parser.mly"
                      ()
# 938 "Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 176 "Parser.mly"
         ()
# 945 "Parser.ml"
               : 'termino))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termino) in
    Obj.repr(
# 177 "Parser.mly"
                         ()
# 953 "Parser.ml"
               : 'termino))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termino) in
    Obj.repr(
# 178 "Parser.mly"
                       ()
# 961 "Parser.ml"
               : 'termino))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'varcte) in
    Obj.repr(
# 181 "Parser.mly"
         ()
# 968 "Parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 182 "Parser.mly"
             ()
# 975 "Parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 183 "Parser.mly"
                      ()
# 982 "Parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 186 "Parser.mly"
      ()
# 989 "Parser.ml"
               : 'varcte))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 187 "Parser.mly"
          ()
# 996 "Parser.ml"
               : 'varcte))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 188 "Parser.mly"
           ()
# 1003 "Parser.ml"
               : 'varcte))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "Parser.mly"
         ()
# 1009 "Parser.ml"
               : 'varcte))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "Parser.mly"
          ()
# 1015 "Parser.ml"
               : 'varcte))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'var1) in
    Obj.repr(
# 193 "Parser.mly"
                        ()
# 1023 "Parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 194 "Parser.mly"
                         ()
# 1031 "Parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 195 "Parser.mly"
                      ()
# 1039 "Parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 196 "Parser.mly"
       ()
# 1046 "Parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expresion) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var2) in
    Obj.repr(
# 199 "Parser.mly"
                 ()
# 1054 "Parser.ml"
               : 'var1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var2) in
    Obj.repr(
# 200 "Parser.mly"
            ()
# 1062 "Parser.ml"
               : 'var1))
; (fun __caml_parser_env ->
    Obj.repr(
# 201 "Parser.mly"
                ()
# 1068 "Parser.ml"
               : 'var1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expresion) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var2) in
    Obj.repr(
# 204 "Parser.mly"
                       ()
# 1076 "Parser.ml"
               : 'var2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var2) in
    Obj.repr(
# 205 "Parser.mly"
                  ()
# 1084 "Parser.ml"
               : 'var2))
; (fun __caml_parser_env ->
    Obj.repr(
# 206 "Parser.mly"
                ()
# 1090 "Parser.ml"
               : 'var2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'tipo) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'decvar1) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'decvar2) in
    Obj.repr(
# 210 "Parser.mly"
                                    ()
# 1100 "Parser.ml"
               : 'decvar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'decvar1) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'decvar2) in
    Obj.repr(
# 211 "Parser.mly"
                                    ()
# 1110 "Parser.ml"
               : 'decvar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var1) in
    Obj.repr(
# 212 "Parser.mly"
                                       ()
# 1119 "Parser.ml"
               : 'decvar))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 216 "Parser.mly"
                             ()
# 1126 "Parser.ml"
               : 'decvar1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 217 "Parser.mly"
                             ()
# 1133 "Parser.ml"
               : 'decvar1))
; (fun __caml_parser_env ->
    Obj.repr(
# 218 "Parser.mly"
                             ()
# 1139 "Parser.ml"
               : 'decvar1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'decvar1) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'decvar2) in
    Obj.repr(
# 222 "Parser.mly"
                             ()
# 1148 "Parser.ml"
               : 'decvar2))
; (fun __caml_parser_env ->
    Obj.repr(
# 223 "Parser.mly"
                             ()
# 1154 "Parser.ml"
               : 'decvar2))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'asignavar) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expresion) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'asignavar) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'expresion) in
    let _11 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 227 "Parser.mly"
                                                                                          ()
# 1165 "Parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expresion) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'bloque) in
    Obj.repr(
# 231 "Parser.mly"
                                       ()
# 1173 "Parser.ml"
               : 'whileloop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expresion) in
    Obj.repr(
# 235 "Parser.mly"
                              ()
# 1180 "Parser.ml"
               : 'return))
(* Entry init *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let init (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
