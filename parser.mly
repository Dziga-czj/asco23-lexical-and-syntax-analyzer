%{
    open Ast
%}

%token Plus Times Lpar Rpar EOL
%token <int> Cst
%token <string> Id
%left Plus
%right Times
%start s
%type <Ast.ast> s
%%
(*grammaire à action*)
s : expr EOL { $1 }

expr :
        | expr Plus expr { Printf.printf "t\n";Plus($1,$3) }
        | expr Times expr { Times($1,$3) }
        | Lpar expr Rpar { Printf.printf "tes\n";Group($2) }
        | Cst { Printf.printf "%d " $1;Cst($1) }
        | Id { Id($1) }

