type token =
  | Plus
  | Times
  | Lpar
  | Rpar
  | EOL
  | Cst of (
# 6 "parser.mly"
        int
# 11 "parser.mli"
)
  | Id of (
# 7 "parser.mly"
        string
# 16 "parser.mli"
)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
