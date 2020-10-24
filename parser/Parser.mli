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

val init :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
