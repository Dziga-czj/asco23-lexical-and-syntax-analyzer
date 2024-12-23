{
    open Parser
    let int_of_string2 i = int_of_string (String.concat "" (String.split_on_char '_' i)) 
    let int_of_string3 beg i = int_of_string (beg ^ (String.sub (String.concat "" (String.split_on_char '_' i)) 2 (String.length i - 2)))
    let float_of_string2 i = float_of_string (String.concat "" (String.split_on_char '_' i))
}

rule decoupe = parse
| "//"*'\n' {Comment}
| "/*"*"*\\" {Comment}
| '+' { Plus }
| '*' { Times }
| '(' { Lpar }
| ')' { Rpar }
| '\n' { EOL }
| eof { EOL }
(*------------------------- constantes -------------------------*)
(* const decimales *)
| ['1'-'9']['0'-'9''_']* as i { Cst_int (int_of_string2 i) }
| "0" { Cst_int 0 } 

(* const bin *)
| "0b" ['0'-'1''_']+ as i { Cst_int (int_of_string3 "0b" i) }
| "0B" ['0'-'1''_']+ as i { Cst_int (int_of_string3 "0b" i) }

(* const oct *)
| "0o" ['0'-'7''_']+ as i { Cst_int (int_of_string3 "0o" i) }
| "0O" ['0'-'7''_']+ as i { Cst_int (int_of_string3 "0o" i) }

(* hex *)
| "0x" ['0'-'9' 'a'-'f' 'A'-'F''_']+ as i { Cst_int (int_of_string3 "0x" i) }
| "0X" ['0'-'9' 'a'-'f' 'A'-'F''_']+ as i { Cst_int (int_of_string3 "0x" i) }

(* const float *)
| ['0'-'9''_']+ '.' ['0'-'9''_']* (['e''E'] ['+' '-']? ['0'-'9''_']+)? as i { Cst_float (float_of_string2 i) }
(* dec exp *)
| '.' ['0'-'9''_']+ (['e''E'] ['+' '-']? ['0'-'9''_']+)? as i { Cst_float (float_of_string2 i)}
(* exp *)
| ['0'-'9''_']+ ['e''E'] ['+' '-']? ['0'-'9''_']+ as i { Cst_float (float_of_string2 i) }

| "True"|"False" as b { Cst_bool (if b = "True" then true else false)}
(* identifiants *)
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as s { Id(s)}


{
    (*trailer*)
}
