%{
    open Ast
%}

%token Plus Times Lpar Rpar EOL Comment Typeof Exp Div Comp_lt Comp_le Comp_gt Comp_ge Eq Double_eq Triple_eq Diff Double_diff And Or Type_num Type_bool Type_string LBracket RBracket LAcc RAcc PVirgule Virgule DPoints Var_decl If Else While Return Type Let Const Function
%token <int> Cst_int
%token <float> Cst_float
%token <bool> Cst_bool
%token <string> Cst_string
%token <string> Id

%left Or
%left And
%left Eq Double_eq Diff Double_diff Triple_eq
%left Comp_lt Comp_le Comp_gt Comp_ge
%left Plus Minus
%left Times Div
%right Exp

%start s
%type <Ast.program> s
%%
(* Entrée principale du programme *)

s : 
  | decl_instr_list EOL { Program $1 } (* prend une liste de déclarations ou instructions *)

(* Déclarations et instructions *)

decl_instr :
  | decl { Decl $1 } (* une déclaration *)
  | instr { Instr $1 } (* une instruction *)

decl_instr_list :
  | decl_instr { [$1] } (* une seule déclaration ou instruction *)
  | decl_instr PVirgule decl_instr_list { $1 :: $3 } (* plusieurs decl/instr séparées par des ; *)

(* Déclarations *)

param_list :
    |   { [] } 
    | Id { [$1] } 
    | Id Virgule param_list { $1 :: $3 } 

return_type :
    |   { None } 
    | DPoints type_expr { Some $2 } 

decl :
  | Type Id Eq type_expr PVirgule { TypeAlias ($2, $4) } 
  | Let bindings PVirgule { LetDecl $2 } 
  | Const bindings PVirgule { ConstDecl $2 } 
  | Function Id Lpar param_list Rpar return_type LAcc decl_instr_list RAcc { 
      FuncDecl ($2, $4, $6, $8) 
    } (* function i(as) to { dis } voir énoncé *)

(* Instructions *)

instr :
  | PVirgule { Empty } (* ; *)
  | expr PVirgule { Expr ($1) } (* e; *)
  | LAcc decl_instr_list RAcc { Block $2 } (* { ... } *)
  | Var_decl bindings PVirgule { VarDecl $2 } (* var bs; *)
  | If Lpar expr Rpar instr { If ($3, $5, Nil) } (* if (e) i *)
  | If Lpar expr Rpar instr Else instr { If ($3, $5, $7) } (* if (e) i1 else i2 *)
  | While Lpar expr Rpar instr { While ($3, $5) } (* while (e) i *)
  | Return PVirgule { Return None } (* return; *)
  | Return expr PVirgule { Return (Some $2) } (* return e; *)

(* Expressions *)

expr : 
    | Cst_int { Cst (Cst_float (float_of_int $1)) }
    | Cst_float { Cst (Cst_float $1) }
    | Cst_bool { Cst (Cst_bool $1) }
    | Cst_string { Cst (Cst_string $1) }
    | left_mem { $1 } 
    | Lpar expr Rpar { Par $2 }
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
    | Typeof expr { Typeof $2 }
    | LBracket expr_list RBracket { Tab $2 }
    | LAcc obj_list RAcc { Object $2 }
    | expr Lpar expr_list Rpar { Func_call ($1, $3) }


expr_list :
  | expr { [$1] } 
  | expr Virgule expr_list { $1 :: $3 } 

(* Objets *)

obj_list :
  | Id DPoints expr { [($1, $3)] } 
  | Id DPoints expr Virgule obj_list { ($1, $3) :: $5 } 

binding :
  | Id { Binding ($1, None, None) } (* i *)
  | Id DPoints type_expr { Binding ($1, Some $3, None) } (* i : t *)
  | Id Eq expr { Binding ($1, None, Some $3) } (* i = e *)
  | Id DPoints type_expr Eq expr { Binding ($1, Some $3, Some $5) } (* i : t = e *)

bindings :
  | binding { [$1] } (* une déclaration *)
  | binding Virgule bindings { $1 :: $3 } (* plusieurs déclarations *)

left_mem :
    | Id { Left_mem (Id $1) }
    | expr LBracket expr RBracket { Left_mem (ArrayAccess ($1, $3)) }
    | expr DPoints Id { Left_mem (FieldAccess ($1, $3)) }

type_expr : 
  | Type_num { TypeNum }
  | Type_bool { TypeBool }
  | Type_string { TypeString }

