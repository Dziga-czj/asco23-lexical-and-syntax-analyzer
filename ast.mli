
type id = Id of string

type const =    
  | Cst_int of int
  | Cst_float of float
  | Cst_bool of bool
  | Cst_string of string

type prim =
  | Num of int
  | Bool of bool
  | Str of string

type type_ = 
  | Identifiant
  | Constante
  | Number
  | Boolean
  | String
  | Tableau
  | Any
  | Object
  | Union

(* and sers a ce que il reconaisse tout les types en même temps pour pouvoir les utiliser mutuellement *)
and left = 
  | Id of string
  | Tab_affect of (expr * expr) (* e[e] *)
  | Point_sep of (expr * id) (* e.i *)

and expr = 
  | Par of expr
  | Cst of const
  | Left_mem of left
  | Obj of (id * expr) list
  | Tab of expr list
  | Func_call of (expr * expr list)
  | Typeof of expr
  | Plus of expr (* +e *)
  | Minus of expr (* -e *)
  | Exp of expr * expr
  | Mul of expr * expr 
  | Div of expr * expr 
  | Add of expr * expr (* e+e *)
  | Sub of expr * expr (* e-e *)
  | GT of expr * expr (* > *)
  | GE of expr * expr (* >= *)
  | LT of expr * expr (* < *)
  | LE of expr * expr (* <= *)  
  | Eq of expr * expr (* == *)  
  | Diff of expr * expr (* != *)  
  | Triple_eq of expr * expr (* === *)  
  | Double_diff of expr * expr (* !== *)  
  | Conj of expr * expr (* && *)  
  | Disj of expr * expr (* || *)  
  | Affect of left * expr (* l = e *)

and instr = 
  | Vide
  | Pt_virgule of expr
  | Bloc of inst_or_decl list
  | Var_decl of binding list
  | If of expr * instr
  | If_else of expr * instr * instr
  | While of expr * instr
  | Simple_return
  | Return of expr

and binding =  
  | Bind_Simple of id (* var a*)
  | Bind_Double of id * type_ * expr (* var a : type = expr; *)
  | Bind_typed of id * type_ (* var a : type;*)
  | Bind_expr of id * expr (* var a = expr; *)

and inst_or_decl = 
  | I_or_D_instr of instr 
  | I_or_D_decl of decl

and decl = 
  | Type_alias of id * type_
  | Let_decl of binding list
  | Const_decl of binding list
  | Func_decl of id * binding list * type_ option * inst_or_decl list

and ast = inst_or_decl list

val affiche : ast -> unit
