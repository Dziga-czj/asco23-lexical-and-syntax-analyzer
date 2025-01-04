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
%type <Ast.ast> s
%%
(*grammaire à action*)


s : expr EOL { $1 }
(* a compléter avec les types dans ast.ml (remplir le type en fonction de la tête que ca a, tu peux les changer bien entendu) *)
expr : 
    | Cst_int { Cst (Cst_float (float_of_int $1)) }
    | Cst_float { Cst (Cst_float $1) }
    | Cst_bool { Cst (Cst_bool $1) }
    | Cst_string { Cst (Cst_string $1) }
    | Id { Left_mem (Id $1) }
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

obj_list :
    | Id DPoints expr { [($1, $3)] }
    | Id DPoints expr Virgule obj_list { ($1, $3) :: $5 }

instr : 
    | PVirgule { Vide }
    | Var_decl Id Eq expr { Affect (Id $2, $4) }
    | Const Id Eq expr { Affect (Id $2, $4) }
    | Let Id Eq expr { Affect (Id $2, $4) }
    | Return expr { $2 }
    | If expr instr { If ($2, $3, Nil) }
    | If expr instr Else instr { If ($2, $3, $5) }
    | While expr instr { While ($2, $3) }
    | Function Id Lpar param_list Rpar LAcc instr_list RAcc { 
        Func_call (Id $2, $7)
      }

param_list : 
    | Id { [$1] }
    | Id Virgule param_list { $1 :: $3 }

instr_list : 
    | instr { [$1] }
    | instr PVirgule instr_list { $1 :: $3 }

