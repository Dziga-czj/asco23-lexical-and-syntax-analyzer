
type id = Id of string

type const =    
  | Cst_int of int
  | Cst_float of float
  | Cst_bool of bool
  | Cst_string of string

and type_ = 
  | Identifiant of string
  | Constante of const
  | Number
  | Boolean
  | String
  | Tableau of type_
  | Any
  | Object of (id * type_ option) list
  | Union of type_ list

(* and sers a ce que il reconaisse tout les types en même temps pour pouvoir les utiliser mutuellement *)
and left = 
  | Left_id of string
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
  | Empty
  | Pt_virgule of expr
  | Bloc of inst_or_decl list
  | Var_decl of binding list
  | If of expr * instr * instr option
  | While of expr * instr
  | Return of expr option

and binding = Binding of id * type_ option * expr option

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

val print_ast : ast -> unit
val print_expr : expr -> unit
val print_decl : decl -> unit
val print_instr : instr -> unit