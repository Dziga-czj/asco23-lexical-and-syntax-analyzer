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
# 45 "parser.ml"
)
  | Cst_float of (
# 7 "parser.mly"
        float
# 50 "parser.ml"
)
  | Cst_bool of (
# 8 "parser.mly"
        bool
# 55 "parser.ml"
)
  | Cst_string of (
# 9 "parser.mly"
        string
# 60 "parser.ml"
)
  | Id of (
# 10 "parser.mly"
        string
# 65 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 72 "parser.ml"
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
  287 (* Var_decl *);
  288 (* If *);
  289 (* Else *);
  290 (* While *);
  291 (* Return *);
  292 (* Type *);
  293 (* Let *);
  294 (* Const *);
  295 (* Function *);
    0|]

let yytransl_block = [|
  296 (* Cst_int *);
  297 (* Cst_float *);
  298 (* Cst_bool *);
  299 (* Cst_string *);
  300 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\006\000\006\000\006\000\
\007\000\007\000\004\000\004\000\004\000\004\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\012\000\012\000\013\000\013\000\014\000\014\000\014\000\014\000\
\009\000\009\000\011\000\011\000\011\000\008\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\003\000\000\000\001\000\003\000\
\000\000\002\000\005\000\003\000\003\000\009\000\001\000\002\000\
\003\000\003\000\005\000\007\000\005\000\002\000\003\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\004\000\
\001\000\003\000\003\000\005\000\001\000\003\000\003\000\005\000\
\001\000\003\000\001\000\004\000\003\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\025\000\026\000\027\000\059\000\065\000\000\000\000\000\002\000\
\003\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\029\000\000\000\046\000\000\000\
\017\000\047\000\000\000\000\000\018\000\000\000\000\000\000\000\
\023\000\000\000\012\000\013\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\061\000\000\000\
\050\000\000\000\000\000\062\000\063\000\064\000\000\000\058\000\
\000\000\000\000\000\000\000\000\048\000\060\000\000\000\000\000\
\000\000\021\000\011\000\000\000\052\000\000\000\000\000\000\000\
\000\000\020\000\010\000\000\000\000\000\014\000"

let yydgoto = "\002\000\
\021\000\022\000\023\000\024\000\025\000\000\000\129\000\111\000\
\037\000\026\000\027\000\032\000\035\000\038\000"

let yysindex = "\014\000\
\033\001\000\000\127\001\127\001\127\001\075\001\000\000\233\254\
\022\255\023\255\119\001\241\254\233\254\233\254\245\254\000\000\
\000\000\000\000\000\000\000\000\000\000\041\255\036\255\000\000\
\000\000\076\255\000\000\027\255\121\255\131\000\145\255\042\255\
\045\255\039\255\074\255\253\254\044\255\068\255\127\001\127\001\
\000\000\169\255\088\255\077\255\081\255\107\255\000\000\033\001\
\127\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
\127\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
\000\000\072\255\127\001\045\255\000\000\127\001\000\000\127\001\
\000\000\000\000\127\001\059\255\000\000\233\254\200\255\224\255\
\000\000\059\255\000\000\000\000\233\254\000\000\004\255\011\255\
\108\255\011\255\011\255\017\001\017\001\017\001\017\001\021\001\
\021\001\021\001\021\001\253\000\179\000\248\255\000\000\004\255\
\000\000\023\000\131\000\000\000\000\000\000\000\103\255\000\000\
\097\001\097\001\091\255\116\255\000\000\000\000\027\255\127\001\
\102\255\000\000\000\000\112\255\000\000\131\000\097\001\059\255\
\117\255\000\000\000\000\033\001\122\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\000\000\000\000\000\000\000\000\000\000\086\255\014\255\000\000\
\155\000\000\000\000\000\016\255\000\000\012\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\200\000\047\000\
\000\000\069\000\100\000\162\001\182\001\202\001\222\001\239\001\
\000\002\017\002\034\002\129\001\059\001\000\000\000\000\222\000\
\000\000\123\255\070\255\000\000\000\000\000\000\079\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\255\000\000\000\000\118\255\000\000\099\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\000\000\165\255\000\000\000\000\191\255\
\247\255\254\255\000\000\229\255\033\000\000\000"

let yytablesize = 831
let yytable = "\034\000\
\029\000\030\000\031\000\044\000\045\000\050\000\051\000\019\000\
\042\000\004\000\075\000\052\000\053\000\051\000\001\000\057\000\
\115\000\049\000\052\000\053\000\036\000\121\000\122\000\089\000\
\039\000\040\000\076\000\064\000\043\000\019\000\019\000\004\000\
\046\000\066\000\064\000\130\000\079\000\080\000\049\000\057\000\
\066\000\086\000\105\000\053\000\053\000\047\000\087\000\088\000\
\031\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\131\000\048\000\
\104\000\073\000\071\000\031\000\112\000\106\000\068\000\077\000\
\107\000\055\000\072\000\116\000\049\000\050\000\051\000\108\000\
\109\000\110\000\054\000\052\000\053\000\054\000\055\000\056\000\
\057\000\045\000\058\000\059\000\060\000\061\000\062\000\063\000\
\078\000\055\000\055\000\064\000\074\000\082\000\056\000\065\000\
\083\000\066\000\054\000\054\000\084\000\085\000\045\000\117\000\
\045\000\045\000\045\000\103\000\120\000\126\000\123\000\124\000\
\067\000\049\000\050\000\051\000\069\000\133\000\056\000\056\000\
\052\000\053\000\054\000\055\000\056\000\057\000\127\000\058\000\
\059\000\060\000\061\000\062\000\063\000\128\000\132\000\009\000\
\064\000\049\000\050\000\051\000\134\000\051\000\066\000\125\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\058\000\
\059\000\060\000\061\000\062\000\063\000\067\000\000\000\000\000\
\064\000\049\000\050\000\051\000\000\000\070\000\066\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\058\000\
\059\000\060\000\061\000\062\000\063\000\067\000\000\000\000\000\
\064\000\000\000\000\000\000\000\081\000\000\000\066\000\000\000\
\049\000\050\000\051\000\113\000\000\000\000\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\067\000\058\000\059\000\
\060\000\061\000\062\000\063\000\000\000\000\000\000\000\064\000\
\049\000\050\000\051\000\114\000\000\000\066\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\058\000\059\000\
\060\000\061\000\062\000\063\000\067\000\000\000\000\000\064\000\
\049\000\050\000\051\000\000\000\000\000\066\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\058\000\059\000\
\060\000\061\000\062\000\063\000\067\000\000\000\000\000\064\000\
\118\000\000\000\000\000\000\000\000\000\066\000\000\000\049\000\
\050\000\051\000\000\000\000\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\067\000\058\000\059\000\060\000\
\061\000\062\000\063\000\000\000\000\000\000\000\064\000\032\000\
\032\000\000\000\032\000\119\000\066\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\000\000\032\000\032\000\032\000\
\032\000\032\000\032\000\067\000\000\000\034\000\034\000\032\000\
\034\000\032\000\032\000\032\000\000\000\034\000\034\000\034\000\
\034\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
\034\000\000\000\000\000\032\000\000\000\034\000\000\000\034\000\
\034\000\034\000\000\000\000\000\033\000\033\000\000\000\033\000\
\000\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
\033\000\034\000\033\000\033\000\033\000\033\000\033\000\033\000\
\000\000\000\000\000\000\000\000\033\000\000\000\033\000\033\000\
\033\000\000\000\000\000\049\000\050\000\051\000\000\000\000\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\033\000\058\000\059\000\060\000\061\000\062\000\063\000\000\000\
\000\000\000\000\064\000\059\000\059\000\059\000\000\000\000\000\
\066\000\000\000\059\000\059\000\059\000\059\000\059\000\059\000\
\000\000\059\000\059\000\059\000\059\000\059\000\059\000\067\000\
\000\000\000\000\059\000\049\000\050\000\051\000\059\000\000\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\000\000\058\000\059\000\060\000\061\000\062\000\000\000\059\000\
\030\000\000\000\064\000\030\000\000\000\000\000\000\000\000\000\
\066\000\030\000\030\000\030\000\030\000\000\000\030\000\030\000\
\030\000\030\000\030\000\030\000\000\000\000\000\031\000\067\000\
\030\000\031\000\030\000\030\000\030\000\000\000\000\000\031\000\
\031\000\031\000\031\000\000\000\031\000\031\000\031\000\031\000\
\031\000\031\000\000\000\000\000\030\000\000\000\031\000\000\000\
\031\000\031\000\031\000\000\000\000\000\049\000\050\000\051\000\
\000\000\000\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\031\000\058\000\059\000\060\000\061\000\000\000\
\000\000\049\000\050\000\051\000\064\000\049\000\050\000\051\000\
\052\000\053\000\066\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\000\000\003\000\000\000\000\000\000\000\004\000\
\064\000\067\000\000\000\000\000\064\000\000\000\066\000\000\000\
\000\000\000\000\066\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\006\000\000\000\007\000\067\000\044\000\008\000\
\009\000\067\000\010\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\018\000\019\000\020\000\003\000\044\000\000\000\
\000\000\004\000\000\000\044\000\000\000\044\000\044\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\003\000\006\000\000\000\007\000\004\000\
\000\000\008\000\009\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\033\000\000\000\
\005\000\003\000\006\000\000\000\007\000\004\000\000\000\008\000\
\009\000\003\000\010\000\011\000\043\000\004\000\000\000\000\000\
\016\000\017\000\018\000\019\000\020\000\000\000\005\000\000\000\
\028\000\000\000\041\000\043\000\043\000\000\000\005\000\000\000\
\028\000\043\000\000\000\043\000\043\000\043\000\016\000\017\000\
\018\000\019\000\020\000\000\000\000\000\035\000\016\000\017\000\
\018\000\019\000\020\000\035\000\035\000\035\000\035\000\000\000\
\035\000\035\000\035\000\035\000\035\000\035\000\000\000\000\000\
\000\000\036\000\035\000\000\000\035\000\035\000\035\000\036\000\
\036\000\036\000\036\000\000\000\036\000\036\000\036\000\036\000\
\036\000\036\000\000\000\000\000\000\000\037\000\036\000\000\000\
\036\000\036\000\036\000\037\000\037\000\037\000\037\000\000\000\
\037\000\037\000\037\000\037\000\037\000\037\000\000\000\000\000\
\000\000\038\000\037\000\000\000\037\000\037\000\037\000\038\000\
\038\000\038\000\038\000\000\000\038\000\038\000\038\000\038\000\
\038\000\038\000\039\000\000\000\000\000\000\000\038\000\000\000\
\038\000\038\000\038\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\041\000\000\000\000\000\000\000\039\000\
\000\000\039\000\039\000\039\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\040\000\000\000\000\000\000\000\
\041\000\000\000\041\000\041\000\041\000\000\000\000\000\040\000\
\040\000\040\000\040\000\040\000\040\000\042\000\000\000\000\000\
\000\000\040\000\000\000\040\000\040\000\040\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\000\000\000\000\
\000\000\000\000\042\000\000\000\042\000\042\000\042\000"

let yycheck = "\006\000\
\003\000\004\000\005\000\013\000\014\000\002\001\003\001\005\001\
\011\000\005\001\014\001\008\001\009\001\003\001\001\000\004\001\
\082\000\004\001\008\001\004\001\044\001\113\000\114\000\051\000\
\003\001\003\001\030\001\024\001\044\001\027\001\028\001\027\001\
\044\001\030\001\024\001\127\000\039\000\040\000\025\001\028\001\
\030\001\048\000\070\000\028\001\029\001\005\001\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\128\000\028\001\
\067\000\027\001\025\001\070\000\078\000\072\000\044\001\028\001\
\075\000\004\001\030\001\085\000\001\001\002\001\003\001\021\001\
\022\001\023\001\004\001\008\001\009\001\010\001\011\001\012\001\
\013\001\004\001\015\001\016\001\017\001\018\001\019\001\020\001\
\029\001\028\001\029\001\024\001\027\001\014\001\004\001\028\001\
\028\001\030\001\028\001\029\001\028\001\003\001\025\001\004\001\
\027\001\028\001\029\001\044\001\014\001\120\000\028\001\004\001\
\045\001\001\001\002\001\003\001\004\001\132\000\028\001\029\001\
\008\001\009\001\010\001\011\001\012\001\013\001\033\001\015\001\
\016\001\017\001\018\001\019\001\020\001\030\001\026\001\026\001\
\024\001\001\001\002\001\003\001\027\001\027\001\030\001\119\000\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\045\001\255\255\255\255\
\024\001\001\001\002\001\003\001\255\255\029\001\030\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\045\001\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\255\255\030\001\255\255\
\001\001\002\001\003\001\004\001\255\255\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\045\001\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\255\255\255\255\024\001\
\001\001\002\001\003\001\004\001\255\255\030\001\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\045\001\255\255\255\255\024\001\
\001\001\002\001\003\001\255\255\255\255\030\001\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\045\001\255\255\255\255\024\001\
\025\001\255\255\255\255\255\255\255\255\030\001\255\255\001\001\
\002\001\003\001\255\255\255\255\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\013\001\045\001\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\255\255\024\001\001\001\
\002\001\255\255\004\001\029\001\030\001\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\045\001\255\255\001\001\002\001\025\001\
\004\001\027\001\028\001\029\001\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\045\001\255\255\025\001\255\255\027\001\
\028\001\029\001\255\255\255\255\001\001\002\001\255\255\004\001\
\255\255\255\255\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\045\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\025\001\255\255\027\001\028\001\
\029\001\255\255\255\255\001\001\002\001\003\001\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\013\001\
\045\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\024\001\001\001\002\001\003\001\255\255\255\255\
\030\001\255\255\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\045\001\
\255\255\255\255\024\001\001\001\002\001\003\001\028\001\255\255\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\255\255\045\001\
\001\001\255\255\024\001\004\001\255\255\255\255\255\255\255\255\
\030\001\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\255\255\001\001\045\001\
\025\001\004\001\027\001\028\001\029\001\255\255\255\255\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\045\001\255\255\025\001\255\255\
\027\001\028\001\029\001\255\255\255\255\001\001\002\001\003\001\
\255\255\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\013\001\045\001\015\001\016\001\017\001\018\001\255\255\
\255\255\001\001\002\001\003\001\024\001\001\001\002\001\003\001\
\008\001\009\001\030\001\255\255\008\001\009\001\010\001\011\001\
\012\001\013\001\255\255\003\001\255\255\255\255\255\255\007\001\
\024\001\045\001\255\255\255\255\024\001\255\255\030\001\255\255\
\255\255\255\255\030\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\255\255\028\001\045\001\004\001\031\001\
\032\001\045\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\003\001\020\001\255\255\
\255\255\007\001\255\255\025\001\255\255\027\001\028\001\029\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\003\001\026\001\255\255\028\001\007\001\
\255\255\031\001\032\001\255\255\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\255\255\
\024\001\003\001\026\001\255\255\028\001\007\001\255\255\031\001\
\032\001\003\001\034\001\035\001\004\001\007\001\255\255\255\255\
\040\001\041\001\042\001\043\001\044\001\255\255\024\001\255\255\
\026\001\255\255\028\001\019\001\020\001\255\255\024\001\255\255\
\026\001\025\001\255\255\027\001\028\001\029\001\040\001\041\001\
\042\001\043\001\044\001\255\255\255\255\004\001\040\001\041\001\
\042\001\043\001\044\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\255\255\004\001\025\001\255\255\027\001\028\001\029\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\004\001\025\001\255\255\
\027\001\028\001\029\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\255\255\004\001\025\001\255\255\027\001\028\001\029\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\004\001\255\255\255\255\255\255\025\001\255\255\
\027\001\028\001\029\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\004\001\255\255\255\255\255\255\025\001\
\255\255\027\001\028\001\029\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\004\001\255\255\255\255\255\255\
\025\001\255\255\027\001\028\001\029\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\004\001\255\255\255\255\
\255\255\025\001\255\255\027\001\028\001\029\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\255\255\255\255\025\001\255\255\027\001\028\001\029\001"

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
  Var_decl\000\
  If\000\
  Else\000\
  While\000\
  Return\000\
  Type\000\
  Let\000\
  Const\000\
  Function\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_instr_list) in
    Obj.repr(
# 26 "parser.mly"
                        ( _1 )
# 480 "parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 31 "parser.mly"
         ( I_or_D_decl _1 )
# 487 "parser.ml"
               : 'decl_instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 32 "parser.mly"
          ( I_or_D_instr _1 )
# 494 "parser.ml"
               : 'decl_instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl_instr) in
    Obj.repr(
# 35 "parser.mly"
               ( [_1] )
# 501 "parser.ml"
               : 'decl_instr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl_instr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_instr_list) in
    Obj.repr(
# 36 "parser.mly"
                                        ( _1 :: _3 )
# 509 "parser.ml"
               : 'decl_instr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
        ( [] )
# 515 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
         ( [_1] )
# 522 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 43 "parser.mly"
                            ( _1 :: _3 )
# 530 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
        ( None )
# 536 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 47 "parser.mly"
                        ( Some _2 )
# 543 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( Type_alias (Id _2, _4) )
# 551 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 51 "parser.mly"
                          ( Let_decl _2 )
# 558 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 52 "parser.mly"
                            ( Const_decl _2 )
# 565 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'bindings) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'return_type) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'decl_instr_list) in
    Obj.repr(
# 53 "parser.mly"
                                                                         ( 
    Func_decl (Id(_2), _4, _6, _8) 
)
# 577 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
             ( Empty )
# 583 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                  ( Pt_virgule (_1) )
# 590 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'decl_instr_list) in
    Obj.repr(
# 63 "parser.mly"
                              ( Bloc _2 )
# 597 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 64 "parser.mly"
                               ( Var_decl _2 )
# 604 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 65 "parser.mly"
                            ( If (_3, _5) )
# 612 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'instr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( If_else (_3, _5, _7) )
# 621 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 67 "parser.mly"
                               ( While (_3, _5) )
# 629 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                    ( Return None )
# 635 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                         ( Return (Some _2) )
# 642 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
              ( Cst (Cst_float (float_of_int _1)) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 75 "parser.mly"
                ( Cst (Cst_float _1) )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 76 "parser.mly"
               ( Cst (Cst_bool _1) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                 ( Cst (Cst_string _1) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'left_mem) in
    Obj.repr(
# 78 "parser.mly"
               ( _1 )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Par _2 )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Add (_1, _3) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                      ( Sub (_1, _3) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                      ( Mul (_1, _3) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                    ( Div (_1, _3) )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                    ( Exp (_1, _3) )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                        ( LT (_1, _3) )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                        ( LE (_1, _3) )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                        ( GT (_1, _3) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                        ( GE (_1, _3) )
# 756 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                          ( Eq (_1, _3) )
# 764 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Diff (_1, _3) )
# 772 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                          ( Triple_eq (_1, _3) )
# 780 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                            ( Double_diff (_1, _3) )
# 788 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                    ( Conj (_1, _3) )
# 796 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                   ( Disj (_1, _3) )
# 804 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                  ( Typeof _2 )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 96 "parser.mly"
                                  ( Tab _2 )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'obj_list) in
    Obj.repr(
# 97 "parser.mly"
                         ( Object _2 )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 98 "parser.mly"
                               ( Func_call (_1, _3) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
         ( [_1] )
# 840 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 103 "parser.mly"
                           ( _1 :: _3 )
# 848 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                    ( [(_1, _3)] )
# 856 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'obj_list) in
    Obj.repr(
# 109 "parser.mly"
                                     ( (_1, _3) :: _5 )
# 865 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
       ( Binding (_1, None, None) )
# 872 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 113 "parser.mly"
                         ( Binding (_1, Some _3, None) )
# 880 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
               ( Binding (_1, None, Some _3) )
# 888 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                 ( Binding (_1, Some _3, Some _5) )
# 897 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 118 "parser.mly"
            ( [_1] )
# 904 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bindings) in
    Obj.repr(
# 119 "parser.mly"
                             ( _1 :: _3 )
# 912 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
         ( Left_mem (Id _1) )
# 919 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                  ( Left_mem (ArrayAccess (_1, _3)) )
# 927 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
                      ( Left_mem (FieldAccess (_1, _3)) )
# 935 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
             ( TypeNum )
# 941 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
              ( TypeBool )
# 947 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
                ( TypeString )
# 953 "parser.ml"
               : 'type_expr))
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
