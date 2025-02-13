type token =
  | TRUE
  | FALSE
  | HOLE
  | PRIMITIVE
  | FUN
  | REC
  | LET
  | LOCAL
  | IN
  | DO
  | WRAP
  | UNWRAP
  | TYPE
  | INCLUDE
  | END
  | IF
  | THEN
  | ELSE
  | OR
  | AND
  | AS
  | EQUAL
  | COLON
  | SEAL
  | ARROW
  | DARROW
  | WITH
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | DOT
  | AT
  | TICK
  | COMMA
  | SEMI
  | EOF
  | NAME of (string)
  | SYM of (string)
  | TEXT of (string)
  | CHAR of (char)
  | NUM of (int)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.exp
