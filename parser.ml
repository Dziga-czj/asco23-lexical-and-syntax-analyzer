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
# 12 "parser.ml"
)
  | Cst_float of (
# 7 "parser.mly"
        float
# 17 "parser.ml"
)
  | Cst_bool of (
# 8 "parser.mly"
        bool
# 22 "parser.ml"
)
  | Id of (
# 9 "parser.mly"
        string
# 27 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 34 "parser.ml"
let yytransl_const = [|
  257 (* Plus *);
  258 (* Times *);
  259 (* Lpar *);
  260 (* Rpar *);
  261 (* EOL *);
  262 (* Comment *);
    0|]

let yytransl_block = [|
  263 (* Cst_int *);
  264 (* Cst_float *);
  265 (* Cst_bool *);
  266 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\000\000\000\000\001\000\004\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000"

let yysindex = "\002\000\
\003\255\000\000\003\255\000\000\000\000\000\000\000\000\000\000\
\000\255\020\255\003\255\003\255\000\000\000\000\002\255\002\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\255\015\255"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 24
let yytable = "\010\000\
\011\000\012\000\001\000\012\000\013\000\003\000\000\000\015\000\
\016\000\004\000\005\000\006\000\007\000\002\000\000\000\003\000\
\002\000\002\000\003\000\003\000\011\000\012\000\000\000\014\000"

let yycheck = "\003\000\
\001\001\002\001\001\000\002\001\005\001\003\001\255\255\011\000\
\012\000\007\001\008\001\009\001\010\001\001\001\255\255\001\001\
\004\001\005\001\004\001\005\001\001\001\002\001\255\255\004\001"

let yynames_const = "\
  Plus\000\
  Times\000\
  Lpar\000\
  Rpar\000\
  EOL\000\
  Comment\000\
  "

let yynames_block = "\
  Cst_int\000\
  Cst_float\000\
  Cst_bool\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 18 "parser.mly"
             ( _1 )
# 111 "parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                         ( Plus(_1,_3) )
# 119 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                          ( Times(_1,_3) )
# 127 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                         ( Group(_2) )
# 134 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 26 "parser.mly"
                  ( Cst_int(_1) )
# 141 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 27 "parser.mly"
                    ( Cst_float(_1) )
# 148 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 28 "parser.mly"
                   ( Cst_bool(_1) )
# 155 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 29 "parser.mly"
             ( Id(_1) )
# 162 "parser.ml"
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
