{
    open Parser
}

rule decoupe = parse
| '+' { Plus }
| '*' { Times }
| '(' { Lpar }
| ')' { Rpar }
| ['0'-'9']+ as i { Cst(int_of_string (i)) }
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as s { Id(s)}
| "\n" { EOL }
| eof { EOL }

{
    (*trailer*)
}