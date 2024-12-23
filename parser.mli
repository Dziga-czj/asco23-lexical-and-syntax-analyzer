type token =
  | Plus
  | Times
  | Lpar
  | Rpar
  | EOL
  | Comment
  | Cst_int of (
# 6 "parser.mly"
        int
# 12 "parser.mli"
)
  | Cst_float of (
# 7 "parser.mly"
        float
# 17 "parser.mli"
)
  | Cst_bool of (
# 8 "parser.mly"
        bool
# 22 "parser.mli"
)
  | Id of (
# 9 "parser.mly"
        string
# 27 "parser.mli"
)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
