%{
    open Ast
%}

%token Plus Times Lpar Rpar EOL Comment Typeof Exp Div Comp_lt Comp_le Comp_gt Comp_ge Eq Double_eq Triple_eq Diff Double_diff And Or Type_num Type_bool Type_string LBracket RBracket LAcc RAcc PVirgule Virgule DPoints Var_decl If Else While Return Type Let Const Function
%token <int> Cst_int
%token <float> Cst_float
%token <bool> Cst_bool
%token <string> Cst_string
%token <string> Id

%left Plus
%right Times
%start s
%type <Ast.ast> s
%%
(*grammaire à action*)


s : expr EOL { $1 }
(* a compléter avec les types dans ast.ml (remplir le type en fonction de la tête que ca a, tu peux les changer bien entendu) *)
expr :  
        | expr Plus expr { Plus($1,$3) }
        | expr Times expr { Times($1,$3) }
        | Lpar expr Rpar { Group($2) }
        | Cst_int { Cst_int($1) }
        | Cst_float { Cst_float($1) }
        | Cst_bool { Cst_bool($1) }
        | Cst_string { Cst_string($1)}
        | Id { Id($1) }

