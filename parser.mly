%{
    open Ast
%}

%token Plus Times Lpar Rpar EOL Comment
%token <int> Cst_int
%token <float> Cst_float
%token <bool> Cst_bool
%token <string> Id
%left Plus
%right Times
%start s
%type <Ast.ast> s
%%
(*grammaire à action*)


s : expr EOL { $1 }



expr :  
        | expr Plus expr { Plus($1,$3) }
        | expr Times expr { Times($1,$3) }
        | Lpar expr Rpar { Group($2) }
        | Cst_int { Cst_int($1) }
        | Cst_float { Cst_float($1) }
        | Cst_bool { Cst_bool($1) }
        | Id { Id($1) }

