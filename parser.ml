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
  | Cst_int of (
# 6 "parser.mly"
        int
# 36 "parser.ml"
)
  | Cst_float of (
# 7 "parser.mly"
        float
# 41 "parser.ml"
)
  | Cst_bool of (
# 8 "parser.mly"
        bool
# 46 "parser.ml"
)
  | Cst_string of (
# 9 "parser.mly"
        string
# 51 "parser.ml"
)
  | Id of (
# 10 "parser.mly"
        string
# 56 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 63 "parser.ml"
let yytransl_const = [|
  257 (* Plus *);
  258 (* Times *);
  259 (* Lpar *);
  260 (* Rpar *);
  261 (* EOL *);
  262 (* Comment *);
  263 (* Typeof *);
  264 (* Exp *);
  265 (* Div *);
  266 (* Comp_lt *);
  267 (* Comp_le *);
  268 (* Comp_gt *);
  269 (* Comp_ge *);
  270 (* Eq *);
  271 (* Double_eq *);
  272 (* Triple_eq *);
  273 (* Diff *);
  274 (* Double_diff *);
  275 (* And *);
  276 (* Or *);
  277 (* Type_num *);
  278 (* Type_bool *);
  279 (* Type_string *);
  280 (* LBracket *);
  281 (* RBracket *);
  282 (* LAcc *);
  283 (* RAcc *);
  284 (* PVirgule *);
  285 (* Virgule *);
  286 (* DPoints *);
    0|]

let yytransl_block = [|
  287 (* Cst_int *);
  288 (* Cst_float *);
  289 (* Cst_bool *);
  290 (* Cst_string *);
  291 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\006\000\007\000\008\000\009\000\
\010\000\000\000\000\000\000\000\000\000\001\000\004\000\000\000\
\000\000"

let yydgoto = "\002\000\
\009\000\010\000"

let yysindex = "\004\000\
\253\254\000\000\253\254\000\000\000\000\000\000\000\000\000\000\
\000\000\001\255\013\255\253\254\253\254\000\000\000\000\014\255\
\014\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\255\
\008\255"

let yygindex = "\000\000\
\000\000\254\255"

let yytablesize = 32
let yytable = "\003\000\
\011\000\012\000\013\000\002\000\001\000\014\000\002\000\002\000\
\003\000\016\000\017\000\003\000\003\000\012\000\013\000\013\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\006\000\007\000\008\000"

let yycheck = "\003\001\
\003\000\001\001\002\001\001\001\001\000\005\001\004\001\005\001\
\001\001\012\000\013\000\004\001\005\001\001\001\002\001\002\001\
\004\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\031\001\032\001\033\001\034\001\035\001"

let yynames_const = "\
  Plus\000\
  Times\000\
  Lpar\000\
  Rpar\000\
  EOL\000\
  Comment\000\
  Typeof\000\
  Exp\000\
  Div\000\
  Comp_lt\000\
  Comp_le\000\
  Comp_gt\000\
  Comp_ge\000\
  Eq\000\
  Double_eq\000\
  Triple_eq\000\
  Diff\000\
  Double_diff\000\
  And\000\
  Or\000\
  Type_num\000\
  Type_bool\000\
  Type_string\000\
  LBracket\000\
  RBracket\000\
  LAcc\000\
  RAcc\000\
  PVirgule\000\
  Virgule\000\
  DPoints\000\
  "

let yynames_block = "\
  Cst_int\000\
  Cst_float\000\
  Cst_bool\000\
  Cst_string\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 20 "parser.mly"
             ( _1 )
# 195 "parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                         ( Plus(_1,_3) )
# 203 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                          ( Times(_1,_3) )
# 211 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                         ( Group(_2) )
# 218 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 26 "parser.mly"
                  ( Cst_int(_1) )
# 225 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 27 "parser.mly"
                    ( Cst_float(_1) )
# 232 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 28 "parser.mly"
                   ( Cst_bool(_1) )
# 239 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 29 "parser.mly"
                     ( Cst_string(_1))
# 246 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "parser.mly"
             ( Id(_1) )
# 253 "parser.ml"
               : 'expr))
(* Entry s *)
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
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.ast)
