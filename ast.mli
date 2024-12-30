
type id = Id of string

type const =    
  | Cst_float of float
  | Cst_bool of bool
  | Cst_string of string

type prim =
  | Number of int 
  | Bool of bool
  | String of string

type ast = Nil

type type_ = 
  | Id of string

(* and sers a ce que il reconaisse tout les types en même temps pour pouvoir les utiliser mutuellement *)
and left = 
  | Id of string
  | Tab_affect of (expr * expr) (* e[e] *)
  | Point_sep of (expr * id) (* e.i *)

and expr = 
  | Par of expr
  | Cst of const
  | Left_mem of left
  | Object of (id * expr) list
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
  | Id of string
  | Const of const
  | Primitif of prim

and decl = Nil

(* j'ai copié collé le type dans ast.ml pour que ca compile *)


val affiche : ast -> unit
