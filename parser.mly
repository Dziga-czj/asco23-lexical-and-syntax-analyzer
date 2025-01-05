%{
    open Ast
%}

%token Plus Times Lpar Rpar Point EOL Comment Typeof Exp Div Comp_lt Comp_le Comp_gt Comp_ge Eq Double_eq Triple_eq Diff Double_diff And Or Type_num Type_bool Type_string LBracket RBracket LAcc RAcc PVirgule Virgule DPoints Var_decl If Else While Return Type Let Const Function
%token <int> Cst_int
%token <float> Cst_float
%token <bool> Cst_bool
%token <string> Cst_string
%token <string> Id

%left Or
%left And
%left Plus Minus
%left Times Div
%right Exp
%left LBracket DPoints 
%nonassoc Eq Double_eq Diff Double_diff Triple_eq Comp_lt Comp_le Comp_gt Comp_ge

%start s
%type <Ast.ast> s
%%
(* Entrée principale du programme *)

s : 
  | decl_instr_list EOL { $1 } (* prend une liste de déclarations ou instructions *)

(* Déclarations et instructions *)

decl_instr :
  | decl { I_or_D_decl $1 } (* une déclaration *)
  | instr { I_or_D_instr $1 } (* une instruction *)

decl_instr_list :
  | decl_instr { [$1] } (* une seule déclaration ou instruction *)
  | decl_instr PVirgule decl_instr_list { $1 :: $3 } (* plusieurs decl/instr séparées par des ; *)

return_type :
    |   { None } 
    | DPoints type_expr { Some $2 } 

decl : 
  | Type Id Eq type_expr PVirgule { Type_alias (Id $2, $4) }
  | Let bindings PVirgule { Let_decl $2 }
  | Const bindings PVirgule { Const_decl $2 }
  | Function Id Lpar bindings Rpar return_type LAcc decl_instr_list RAcc { 
    Func_decl (Id($2), $4, $6, $8) 
}


(* Instructions *)

instr :
  | PVirgule { Empty } (* ; *)
  | expr PVirgule { Pt_virgule ($1) } (* e; *)
  | LAcc decl_instr_list RAcc { Bloc $2 } (* { ... } *)
  | Var_decl bindings PVirgule { Var_decl $2 } (* var bs; *)
  | If Lpar expr Rpar instr { If ($3, $5) } (* if (e) i *)
  | If Lpar expr Rpar instr Else instr { If_else ($3, $5, $7) } (* if (e) i1 else i2 *)
  | While Lpar expr Rpar instr { While ($3, $5) } (* while (e) i *)
  | Return PVirgule { Return None } (* return; *)
  | Return expr PVirgule { Return (Some $2) } (* return e; *)

(* Expressions *)

expr : 
    | Cst_int { Cst (Cst_float (float_of_int $1)) }
    | Cst_float { Cst (Cst_float $1) }
    | Cst_bool { Cst (Cst_bool $1) }
    | Cst_string { Cst (Cst_string $1) }
    | Lpar expr Rpar { Par $2 } 
    | Typeof expr %prec Typeof { Typeof $2 }
    | Minus expr %prec Minus { Minus $2 }
    | Plus expr %prec Plus { Plus $2 }
    | LBracket expr_list RBracket { Tab $2 }
    | LAcc obj_list RAcc { Obj $2 }
    | left_mem { $1 }
    | expr Plus expr { Add ($1, $3) }
    | expr Minus expr { Sub ($1, $3) }
    | expr Times expr { Mul ($1, $3) }
    | expr Div expr { Div ($1, $3) }
    | expr Exp expr { Exp ($1, $3) }
    | expr Comp_lt expr { LT ($1, $3) }
    | expr Comp_le expr { LE ($1, $3) }
    | expr Comp_gt expr { GT ($1, $3) }
    | expr Comp_ge expr { GE ($1, $3) }
    | expr Double_eq expr { Eq ($1, $3) }
    | expr Diff expr { Diff ($1, $3) }
    | expr Triple_eq expr { Triple_eq ($1, $3) }
    | expr Double_diff expr { Double_diff ($1, $3) }
    | expr And expr { Conj ($1, $3) }
    | expr Or expr { Disj ($1, $3) }
    | expr Lpar expr_list Rpar { Func_call ($1, $3) }
    | left_mem Eq expr { $1 }


expr_list :
  | { [] } 
  | expr expr_list_2 { $1::$2 } 

expr_list_2 :
  |  { [] }
  | Virgule expr_list { $2 }

(* Objets *)

obj_list :
  | Id DPoints expr { [(Id $1, $3)] } 
  | Id DPoints expr Virgule obj_list { (Id $1, $3) :: $5 } 

binding :
  | Id { Bind_Simple (Id $1) }  (* var a *)
  | Id DPoints type_expr { Bind_typed (Id $1, $3) }  (* var a : t *)
  | Id Eq expr { Bind_expr (Id $1, $3) }  (* var a = e *)
  | Id DPoints type_expr Eq expr { Bind_Double (Id $1, $3, $5) }  (* var a : t = e *)

bindings :
  | binding { [$1] } (* une déclaration *)
  | binding Virgule bindings { $1 :: $3 } (* plusieurs déclarations *)

left_mem :
    | Id { Left_mem (Id $1) }
    | expr LBracket expr RBracket { Left_mem (Tab_affect ($1, $3)) }
    | expr Point Id { Left_mem (Point_sep ($1, Id $3)) }

type_expr : 
  | Type_num { Number }
  | Type_bool { Boolean }
  | Type_string { String }

