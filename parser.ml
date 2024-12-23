type token =
  | Plus
  | Times
  | Lpar
  | Rpar
  | EOL
  | Cst of (
# 6 "parser.mly"
        int
# 11 "parser.ml"
)
  | Id of (
# 7 "parser.mly"
        string
# 16 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 23 "parser.ml"
let yytransl_const = [|
  257 (* Plus *);
  258 (* Times *);
  259 (* Lpar *);
  260 (* Rpar *);
  261 (* EOL *);
    0|]

let yytransl_block = [|
  262 (* Cst *);
  263 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\006\000\007\000\000\000\000\000\
\000\000\000\000\001\000\004\000\000\000\000\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\001\000\
\254\254\000\000\254\254\000\000\000\000\000\000\007\255\019\255\
\254\254\254\254\000\000\000\000\001\255\001\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\255\014\255"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 23
let yytable = "\008\000\
\003\000\001\000\010\000\004\000\005\000\013\000\014\000\009\000\
\010\000\002\000\000\000\011\000\002\000\002\000\003\000\000\000\
\000\000\003\000\003\000\009\000\010\000\000\000\012\000"

let yycheck = "\003\000\
\003\001\001\000\002\001\006\001\007\001\009\000\010\000\001\001\
\002\001\001\001\255\255\005\001\004\001\005\001\001\001\255\255\
\255\255\004\001\005\001\001\001\002\001\255\255\004\001"

let yynames_const = "\
  Plus\000\
  Times\000\
  Lpar\000\
  Rpar\000\
  EOL\000\
  "

let yynames_block = "\
  Cst\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 16 "parser.mly"
             ( _1 )
# 92 "parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                         ( Plus(_1,_3) )
# 100 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
                          ( Times(_1,_3) )
# 108 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                         ( Group(_2) )
# 115 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 24 "parser.mly"
              ( Cst(_1) )
# 122 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 25 "parser.mly"
             ( Id(_1) )
# 129 "parser.ml"
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
