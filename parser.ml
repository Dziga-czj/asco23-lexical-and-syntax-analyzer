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
\000\000\000\000\000\000\000\000\000\000\048\000\060\000\000\000\
\000\000\000\000\021\000\011\000\000\000\000\000\052\000\000\000\
\000\000\008\000\000\000\000\000\020\000\010\000\000\000\000\000\
\014\000"

let yydgoto = "\002\000\
\021\000\022\000\023\000\024\000\025\000\117\000\132\000\111\000\
\037\000\026\000\027\000\032\000\035\000\038\000"

let yysindex = "\016\000\
\034\001\000\000\128\001\128\001\128\001\076\001\000\000\234\254\
\023\255\026\255\120\001\245\254\234\254\234\254\255\254\000\000\
\000\000\000\000\000\000\000\000\000\000\036\255\016\255\000\000\
\000\000\073\255\000\000\002\255\122\255\132\000\146\255\039\255\
\064\255\068\255\069\255\253\254\070\255\071\255\128\001\128\001\
\000\000\170\255\085\255\074\255\076\255\102\255\000\000\034\001\
\128\001\128\001\128\001\128\001\128\001\128\001\128\001\128\001\
\128\001\128\001\128\001\128\001\128\001\128\001\128\001\128\001\
\000\000\062\255\128\001\064\255\000\000\128\001\000\000\128\001\
\000\000\000\000\128\001\254\254\000\000\234\254\201\255\225\255\
\000\000\254\254\000\000\000\000\063\255\000\000\004\255\015\255\
\105\255\015\255\015\255\018\001\018\001\018\001\018\001\022\001\
\022\001\022\001\022\001\254\000\180\000\249\255\000\000\004\255\
\000\000\024\000\132\000\000\000\000\000\000\000\099\255\000\000\
\098\001\098\001\086\255\087\255\111\255\000\000\000\000\002\255\
\128\001\084\255\000\000\000\000\063\255\090\255\000\000\132\000\
\098\001\000\000\254\254\095\255\000\000\000\000\034\001\100\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\000\000\000\000\000\000\000\000\000\000\083\255\011\255\000\000\
\156\000\000\000\000\000\038\255\000\000\094\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\124\255\000\000\201\000\048\000\
\000\000\070\000\101\000\163\001\183\001\203\001\223\001\240\001\
\001\002\018\002\035\002\130\001\060\001\000\000\000\000\223\000\
\000\000\109\255\043\255\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\139\255\000\000\000\000\000\000\000\000\
\000\000\003\255\000\000\000\000\124\255\118\255\000\000\051\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\000\000\167\255\020\000\000\000\188\255\
\247\255\254\255\000\000\221\255\030\000\000\000"

let yytablesize = 832
let yytable = "\034\000\
\029\000\030\000\031\000\044\000\045\000\050\000\051\000\019\000\
\042\000\004\000\075\000\052\000\053\000\115\000\049\000\089\000\
\001\000\051\000\108\000\109\000\110\000\036\000\052\000\122\000\
\123\000\039\000\076\000\064\000\040\000\019\000\019\000\004\000\
\043\000\066\000\105\000\049\000\079\000\080\000\064\000\133\000\
\047\000\086\000\046\000\048\000\066\000\068\000\087\000\088\000\
\031\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\134\000\071\000\
\104\000\053\000\053\000\031\000\112\000\106\000\055\000\055\000\
\107\000\049\000\050\000\051\000\054\000\054\000\056\000\056\000\
\052\000\053\000\054\000\055\000\056\000\057\000\045\000\058\000\
\059\000\060\000\061\000\062\000\063\000\072\000\073\000\074\000\
\064\000\077\000\082\000\078\000\065\000\083\000\066\000\084\000\
\085\000\103\000\116\000\045\000\118\000\045\000\045\000\045\000\
\121\000\124\000\126\000\125\000\129\000\067\000\128\000\131\000\
\135\000\057\000\049\000\050\000\051\000\069\000\137\000\006\000\
\136\000\052\000\053\000\054\000\055\000\056\000\057\000\051\000\
\058\000\059\000\060\000\061\000\062\000\063\000\007\000\009\000\
\130\000\064\000\049\000\050\000\051\000\127\000\000\000\066\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\058\000\059\000\060\000\061\000\062\000\063\000\067\000\000\000\
\000\000\064\000\049\000\050\000\051\000\000\000\070\000\066\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\058\000\059\000\060\000\061\000\062\000\063\000\067\000\000\000\
\000\000\064\000\000\000\000\000\000\000\081\000\000\000\066\000\
\000\000\049\000\050\000\051\000\113\000\000\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\067\000\058\000\
\059\000\060\000\061\000\062\000\063\000\000\000\000\000\000\000\
\064\000\049\000\050\000\051\000\114\000\000\000\066\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\058\000\
\059\000\060\000\061\000\062\000\063\000\067\000\000\000\000\000\
\064\000\049\000\050\000\051\000\000\000\000\000\066\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\058\000\
\059\000\060\000\061\000\062\000\063\000\067\000\000\000\000\000\
\064\000\119\000\000\000\000\000\000\000\000\000\066\000\000\000\
\049\000\050\000\051\000\000\000\000\000\000\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\067\000\058\000\059\000\
\060\000\061\000\062\000\063\000\000\000\000\000\000\000\064\000\
\032\000\032\000\000\000\032\000\120\000\066\000\000\000\000\000\
\032\000\032\000\032\000\032\000\032\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\067\000\000\000\034\000\034\000\
\032\000\034\000\032\000\032\000\032\000\000\000\034\000\034\000\
\034\000\034\000\034\000\000\000\034\000\034\000\034\000\034\000\
\034\000\034\000\000\000\000\000\032\000\000\000\034\000\000\000\
\034\000\034\000\034\000\000\000\000\000\033\000\033\000\000\000\
\033\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\033\000\034\000\033\000\033\000\033\000\033\000\033\000\
\033\000\000\000\000\000\000\000\000\000\033\000\000\000\033\000\
\033\000\033\000\000\000\000\000\049\000\050\000\051\000\000\000\
\000\000\000\000\000\000\052\000\053\000\054\000\055\000\056\000\
\057\000\033\000\058\000\059\000\060\000\061\000\062\000\063\000\
\000\000\000\000\000\000\064\000\059\000\059\000\059\000\000\000\
\000\000\066\000\000\000\059\000\059\000\059\000\059\000\059\000\
\059\000\000\000\059\000\059\000\059\000\059\000\059\000\059\000\
\067\000\000\000\000\000\059\000\049\000\050\000\051\000\059\000\
\000\000\000\000\000\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\058\000\059\000\060\000\061\000\062\000\000\000\
\059\000\030\000\000\000\064\000\030\000\000\000\000\000\000\000\
\000\000\066\000\030\000\030\000\030\000\030\000\000\000\030\000\
\030\000\030\000\030\000\030\000\030\000\000\000\000\000\031\000\
\067\000\030\000\031\000\030\000\030\000\030\000\000\000\000\000\
\031\000\031\000\031\000\031\000\000\000\031\000\031\000\031\000\
\031\000\031\000\031\000\000\000\000\000\030\000\000\000\031\000\
\000\000\031\000\031\000\031\000\000\000\000\000\049\000\050\000\
\051\000\000\000\000\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\031\000\058\000\059\000\060\000\061\000\
\000\000\000\000\049\000\050\000\051\000\064\000\049\000\050\000\
\051\000\052\000\053\000\066\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\003\000\000\000\000\000\000\000\
\004\000\064\000\067\000\000\000\000\000\064\000\000\000\066\000\
\000\000\000\000\000\000\066\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\006\000\000\000\007\000\067\000\044\000\
\008\000\009\000\067\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\003\000\044\000\
\000\000\000\000\004\000\000\000\044\000\000\000\044\000\044\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\003\000\006\000\000\000\007\000\
\004\000\000\000\008\000\009\000\000\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\033\000\
\000\000\005\000\003\000\006\000\000\000\007\000\004\000\000\000\
\008\000\009\000\003\000\010\000\011\000\043\000\004\000\000\000\
\000\000\016\000\017\000\018\000\019\000\020\000\000\000\005\000\
\000\000\028\000\000\000\041\000\043\000\043\000\000\000\005\000\
\000\000\028\000\043\000\000\000\043\000\043\000\043\000\016\000\
\017\000\018\000\019\000\020\000\000\000\000\000\035\000\016\000\
\017\000\018\000\019\000\020\000\035\000\035\000\035\000\035\000\
\000\000\035\000\035\000\035\000\035\000\035\000\035\000\000\000\
\000\000\000\000\036\000\035\000\000\000\035\000\035\000\035\000\
\036\000\036\000\036\000\036\000\000\000\036\000\036\000\036\000\
\036\000\036\000\036\000\000\000\000\000\000\000\037\000\036\000\
\000\000\036\000\036\000\036\000\037\000\037\000\037\000\037\000\
\000\000\037\000\037\000\037\000\037\000\037\000\037\000\000\000\
\000\000\000\000\038\000\037\000\000\000\037\000\037\000\037\000\
\038\000\038\000\038\000\038\000\000\000\038\000\038\000\038\000\
\038\000\038\000\038\000\039\000\000\000\000\000\000\000\038\000\
\000\000\038\000\038\000\038\000\000\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\041\000\000\000\000\000\000\000\
\039\000\000\000\039\000\039\000\039\000\000\000\000\000\041\000\
\041\000\041\000\041\000\041\000\041\000\040\000\000\000\000\000\
\000\000\041\000\000\000\041\000\041\000\041\000\000\000\000\000\
\040\000\040\000\040\000\040\000\040\000\040\000\042\000\000\000\
\000\000\000\000\040\000\000\000\040\000\040\000\040\000\000\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\000\000\
\000\000\000\000\000\000\042\000\000\000\042\000\042\000\042\000"

let yycheck = "\006\000\
\003\000\004\000\005\000\013\000\014\000\002\001\003\001\005\001\
\011\000\005\001\014\001\008\001\009\001\082\000\004\001\051\000\
\001\000\003\001\021\001\022\001\023\001\044\001\008\001\113\000\
\114\000\003\001\030\001\024\001\003\001\027\001\028\001\027\001\
\044\001\030\001\070\000\025\001\039\000\040\000\024\001\129\000\
\005\001\048\000\044\001\028\001\030\001\044\001\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\131\000\025\001\
\067\000\028\001\029\001\070\000\078\000\072\000\028\001\029\001\
\075\000\001\001\002\001\003\001\028\001\029\001\028\001\029\001\
\008\001\009\001\010\001\011\001\012\001\013\001\004\001\015\001\
\016\001\017\001\018\001\019\001\020\001\030\001\027\001\027\001\
\024\001\028\001\014\001\029\001\028\001\028\001\030\001\028\001\
\003\001\044\001\044\001\025\001\004\001\027\001\028\001\029\001\
\014\001\028\001\004\001\029\001\033\001\045\001\121\000\030\001\
\026\001\028\001\001\001\002\001\003\001\004\001\027\001\004\001\
\135\000\008\001\009\001\010\001\011\001\012\001\013\001\027\001\
\015\001\016\001\017\001\018\001\019\001\020\001\004\001\026\001\
\125\000\024\001\001\001\002\001\003\001\120\000\255\255\030\001\
\255\255\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\045\001\255\255\
\255\255\024\001\001\001\002\001\003\001\255\255\029\001\030\001\
\255\255\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\045\001\255\255\
\255\255\024\001\255\255\255\255\255\255\028\001\255\255\030\001\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\045\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\255\255\
\024\001\001\001\002\001\003\001\004\001\255\255\030\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\045\001\255\255\255\255\
\024\001\001\001\002\001\003\001\255\255\255\255\030\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\045\001\255\255\255\255\
\024\001\025\001\255\255\255\255\255\255\255\255\030\001\255\255\
\001\001\002\001\003\001\255\255\255\255\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\045\001\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\255\255\255\255\024\001\
\001\001\002\001\255\255\004\001\029\001\030\001\255\255\255\255\
\009\001\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\045\001\255\255\001\001\002\001\
\025\001\004\001\027\001\028\001\029\001\255\255\009\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\045\001\255\255\025\001\255\255\
\027\001\028\001\029\001\255\255\255\255\001\001\002\001\255\255\
\004\001\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\045\001\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\255\255\255\255\025\001\255\255\027\001\
\028\001\029\001\255\255\255\255\001\001\002\001\003\001\255\255\
\255\255\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\045\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\024\001\001\001\002\001\003\001\255\255\
\255\255\030\001\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\045\001\255\255\255\255\024\001\001\001\002\001\003\001\028\001\
\255\255\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\018\001\019\001\255\255\
\045\001\001\001\255\255\024\001\004\001\255\255\255\255\255\255\
\255\255\030\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\001\001\
\045\001\025\001\004\001\027\001\028\001\029\001\255\255\255\255\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\045\001\255\255\025\001\
\255\255\027\001\028\001\029\001\255\255\255\255\001\001\002\001\
\003\001\255\255\255\255\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\013\001\045\001\015\001\016\001\017\001\018\001\
\255\255\255\255\001\001\002\001\003\001\024\001\001\001\002\001\
\003\001\008\001\009\001\030\001\255\255\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\003\001\255\255\255\255\255\255\
\007\001\024\001\045\001\255\255\255\255\024\001\255\255\030\001\
\255\255\255\255\255\255\030\001\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\255\255\028\001\045\001\004\001\
\031\001\032\001\045\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\003\001\020\001\
\255\255\255\255\007\001\255\255\025\001\255\255\027\001\028\001\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\024\001\003\001\026\001\255\255\028\001\
\007\001\255\255\031\001\032\001\255\255\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\044\001\
\255\255\024\001\003\001\026\001\255\255\028\001\007\001\255\255\
\031\001\032\001\003\001\034\001\035\001\004\001\007\001\255\255\
\255\255\040\001\041\001\042\001\043\001\044\001\255\255\024\001\
\255\255\026\001\255\255\028\001\019\001\020\001\255\255\024\001\
\255\255\026\001\025\001\255\255\027\001\028\001\029\001\040\001\
\041\001\042\001\043\001\044\001\255\255\255\255\004\001\040\001\
\041\001\042\001\043\001\044\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\004\001\025\001\255\255\027\001\028\001\029\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\255\255\004\001\025\001\
\255\255\027\001\028\001\029\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\004\001\025\001\255\255\027\001\028\001\029\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\004\001\255\255\255\255\255\255\025\001\
\255\255\027\001\028\001\029\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\004\001\255\255\255\255\255\255\
\025\001\255\255\027\001\028\001\029\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\004\001\255\255\255\255\
\255\255\025\001\255\255\027\001\028\001\029\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\004\001\255\255\
\255\255\255\255\025\001\255\255\027\001\028\001\029\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\255\255\025\001\255\255\027\001\028\001\029\001"

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
# 483 "parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 31 "parser.mly"
         ( I_or_D_decl _1 )
# 490 "parser.ml"
               : 'decl_instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 32 "parser.mly"
          ( I_or_D_instr _1 )
# 497 "parser.ml"
               : 'decl_instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl_instr) in
    Obj.repr(
# 35 "parser.mly"
               ( [_1] )
# 504 "parser.ml"
               : 'decl_instr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl_instr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_instr_list) in
    Obj.repr(
# 36 "parser.mly"
                                        ( _1 :: _3 )
# 512 "parser.ml"
               : 'decl_instr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
        ( [] )
# 518 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
         ( [_1] )
# 525 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 43 "parser.mly"
                            ( _1 :: _3 )
# 533 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
        ( None )
# 539 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 47 "parser.mly"
                        ( Some _2 )
# 546 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( Type_alias (Id _2, _4) )
# 554 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 51 "parser.mly"
                          ( Let_decl _2 )
# 561 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 52 "parser.mly"
                            ( Const_decl _2 )
# 568 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'param_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'return_type) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'decl_instr_list) in
    Obj.repr(
# 53 "parser.mly"
                                                                           ( 
    Func_decl (Id _2, _4, _8) 
)
# 580 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
             ( Empty )
# 586 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                  ( Expr (_1) )
# 593 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'decl_instr_list) in
    Obj.repr(
# 63 "parser.mly"
                              ( Block _2 )
# 600 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bindings) in
    Obj.repr(
# 64 "parser.mly"
                               ( VarDecl _2 )
# 607 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 65 "parser.mly"
                            ( If (_3, _5, Nil) )
# 615 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'instr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( If (_3, _5, _7) )
# 624 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 67 "parser.mly"
                               ( While (_3, _5) )
# 632 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                    ( Return None )
# 638 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                         ( Return (Some _2) )
# 645 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
              ( Cst (Cst_float (float_of_int _1)) )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 75 "parser.mly"
                ( Cst (Cst_float _1) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 76 "parser.mly"
               ( Cst (Cst_bool _1) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                 ( Cst (Cst_string _1) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'left_mem) in
    Obj.repr(
# 78 "parser.mly"
               ( _1 )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Par _2 )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Add (_1, _3) )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                      ( Sub (_1, _3) )
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                      ( Mul (_1, _3) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                    ( Div (_1, _3) )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                    ( Exp (_1, _3) )
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                        ( LT (_1, _3) )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                        ( LE (_1, _3) )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                        ( GT (_1, _3) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                        ( GE (_1, _3) )
# 759 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                          ( Eq (_1, _3) )
# 767 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Diff (_1, _3) )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                          ( Triple_eq (_1, _3) )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                            ( Double_diff (_1, _3) )
# 791 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                    ( Conj (_1, _3) )
# 799 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                   ( Disj (_1, _3) )
# 807 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                  ( Typeof _2 )
# 814 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 96 "parser.mly"
                                  ( Tab _2 )
# 821 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'obj_list) in
    Obj.repr(
# 97 "parser.mly"
                         ( Object _2 )
# 828 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 98 "parser.mly"
                               ( Func_call (_1, _3) )
# 836 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
         ( [_1] )
# 843 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 103 "parser.mly"
                           ( _1 :: _3 )
# 851 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                    ( [(_1, _3)] )
# 859 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'obj_list) in
    Obj.repr(
# 109 "parser.mly"
                                     ( (_1, _3) :: _5 )
# 868 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
       ( Binding (_1, None, None) )
# 875 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 113 "parser.mly"
                         ( Binding (_1, Some _3, None) )
# 883 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
               ( Binding (_1, None, Some _3) )
# 891 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                 ( Binding (_1, Some _3, Some _5) )
# 900 "parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 118 "parser.mly"
            ( [_1] )
# 907 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bindings) in
    Obj.repr(
# 119 "parser.mly"
                             ( _1 :: _3 )
# 915 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
         ( Left_mem (Id _1) )
# 922 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                  ( Left_mem (ArrayAccess (_1, _3)) )
# 930 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
                      ( Left_mem (FieldAccess (_1, _3)) )
# 938 "parser.ml"
               : 'left_mem))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
             ( TypeNum )
# 944 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
              ( TypeBool )
# 950 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
                ( TypeString )
# 956 "parser.ml"
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
