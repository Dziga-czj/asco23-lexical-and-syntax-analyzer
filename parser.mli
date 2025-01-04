type token =
  | Plus
  | Times
  | Lpar
  | Rpar
  | EOL
  | Comment
  | Typeof
  | Exp
  | Div
  | Comp_lt
  | Comp_le
  | Comp_gt
  | Comp_ge
  | Eq
  | Double_eq
  | Triple_eq
  | Diff
  | Double_diff
  | And
  | Or
  | Type_num
  | Type_bool
  | Type_string
  | LBracket
  | RBracket
  | LAcc
  | RAcc
  | PVirgule
  | Virgule
  | DPoints
  | Var_decl
  | If
  | Else
  | While
  | Return
  | Type
  | Let
  | Const
  | Function
  | Cst_int of (
# 6 "parser.mly"
        int
# 45 "parser.mli"
)
  | Cst_float of (
# 7 "parser.mly"
        float
# 50 "parser.mli"
)
  | Cst_bool of (
# 8 "parser.mly"
        bool
# 55 "parser.mli"
)
  | Cst_string of (
# 9 "parser.mly"
        string
# 60 "parser.mli"
)
  | Id of (
# 10 "parser.mly"
        string
# 65 "parser.mli"
)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
