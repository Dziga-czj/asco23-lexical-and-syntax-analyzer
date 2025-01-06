%{
    open Ast
%}

%token Plus EOF Times Minus Any Lpar Rpar Point Comment Barre Typeof Exp Div Comp_lt Comp_le Comp_gt Comp_ge Eq Double_eq Triple_eq Diff Double_diff And Or Type_num Type_bool Type_string LBracket RBracket LAcc RAcc PVirgule Virgule DPoints Var_decl If Else While Return Type Let Const Function
%token <int> Cst_int
%token <float> Cst_float
%token <bool> Cst_bool
%token <string> Cst_string
%token <string> Id
%token Comment


%left Or
%left And
%left Plus Minus
%left Typeof
%left Times Div
%right Exp
%right Point
%left DPoints
%right LBracket 
%left Lpar
%left Virgule PVirgule

%nonassoc ELSE If Barre
%nonassoc Eq Double_eq Diff Double_diff Triple_eq Comp_lt Comp_le Comp_gt Comp_ge

%start s
%type <Ast.ast> s
%%
(* Entrée principale du programme *)

s : decl_instr_list EOF { $1 } (* prend une liste de déclarations ou instructions *)

(* Déclarations et instructions *)

decl_instr :
  | decl { print_decl $1; print_newline (); flush stdout; I_or_D_decl $1 } (* une déclaration *)
  | instr { print_instr $1; print_newline (); flush stdout; I_or_D_instr $1 } (* une instruction *)

decl_instr_list :
  | { [] }
  | decl_instr decl_instr_list { $1::$2 }

decl : 
  | Type Id Eq type_expr PVirgule { Type_alias (Id $2, $4) }
  | Let bindings PVirgule { Let_decl $2 }
  | Const bindings PVirgule { flush stdout; Const_decl $2 }
  | Function Id Lpar possible_empty_bindings Rpar to_ LAcc decl_instr_list RAcc { Func_decl (Id($2), $4, $6, $8) }

possible_empty_bindings:
  | { [] }
  | bindings { $1 }

(* Instructions *)
instr:
  | instr_mached { $1 }
  | instr_non_mached { $1 }

eo :
| Eq expr { Some $2 }
| { None }

instr_mached :
  | PVirgule { Empty } (* ; *)
  | expr PVirgule { Pt_virgule ($1) } (* e; *)
  | LAcc decl_instr_list RAcc { Bloc $2 } (* { ... } *)
  | Var_decl bindings PVirgule { Var_decl ($2) } (* var bs; *)
  | If Lpar expr Rpar instr_mached Else instr_mached { If ($3, $5, None)  } (* if (e) i *)
  | While Lpar expr Rpar instr_mached { While ($3, $5) } (* while (e) i *)
  | Return PVirgule { Return None } (* return; *)
  | Return expr PVirgule { Return (Some $2) } (* return e; *)

instr_non_mached :
  | If Lpar expr Rpar instr { If ($3, $5, None) } (* if (e) i else i *)
  | If Lpar expr Rpar instr_mached Else instr_non_mached { If ($3, $5, Some $7) } (* if (e) i else i *)


(* Expressions *)

expr : 
    | Lpar expr Rpar { Par $2 } 
    | Cst_int { Cst (Cst_float (float_of_int $1)) }
    | Cst_float { Cst (Cst_float $1) }
    | Cst_bool { Cst (Cst_bool $1) }
    | Cst_string { Cst (Cst_string $1) }
    | left_mem { Left_mem $1 }
    | LAcc obj_list RAcc { Obj $2 }
    | LBracket tab_expr_list RBracket { Tab $2 }
    | expr Lpar expr_list Rpar { Func_call ($1, $3) }
    | Typeof expr %prec Typeof { Typeof $2 }
    | Minus expr %prec Minus { Minus $2 }
    | Plus expr %prec Plus { Plus $2 }
    | expr Exp expr { Exp ($1, $3) }
    | expr Times expr { Mul ($1, $3) }
    | expr Div expr { Div ($1, $3) }
    | expr Plus expr { Add ($1, $3) }
    | expr Minus expr { Sub ($1, $3) }
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
    | left_mem Eq expr { Affect ($1, $3) }

tab_expr_list:
  | { [] }
  | expr { [$1] }
  | expr Virgule tab_expr_list { $1::$3 }


expr_list :
  | { [] } 
  | expr { [$1] }
  | expr Virgule expr_list { $1::$3 } 


(* Objets *)

pt_v_or_virgule :
  | PVirgule { }
  | Virgule { }

obj_list :
  | Id DPoints expr { [(Id $1, $3)] } 
  | Id DPoints expr pt_v_or_virgule obj_list { (Id $1, $3) :: $5 } 

to_:
  | { None }
  | DPoints type_expr { Some $2 }

eo:
  | { None }
  | Eq expr { Some $2 }

binding :
  | Id to_ eo {Binding (Id $1, $2, $3) }  (* var a *)


bindings :
  | binding { [$1] } (* une déclaration *)
  | binding Virgule bindings { $1 :: $3 } (* plusieurs déclarations *)


(* Accès mémoire *)

left_mem :
    | Id { Left_id $1 }
    | expr LBracket expr RBracket { Tab_affect ($1, $3) }
    | expr Point Id { Point_sep ($1, Id $3) }

type_expr : 
  | Id { Identifiant($1)}
  | Cst_string { Constante(Cst_string $1) }
  | Cst_bool { Constante(Cst_bool $1) }
  | Cst_float { Constante(Cst_float $1) }
  | Cst_int { Constante(Cst_int $1) }
  | Type_num { Number }
  | Type_bool { Boolean }
  | Type_string { String }
  | type_expr LBracket RBracket { Tableau $1 }
  | Any { Any}
  | LAcc type_obj RAcc { Object $2 }
  | type_expr Barre union_type { Union ($1::$3) }


type_obj :
  | { [] }
  | id_or_type pt_v_or_virgule type_obj { $1::$3 }


union_type :
  | type_expr { [$1] }
  | type_expr Barre union_type { $1::$3 }

id_or_type :
  | Id { (Id $1, None) }
  | Id DPoints type_expr { (Id $1, Some $3) }




