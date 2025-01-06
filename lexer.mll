{
    open Parser
    let int_of_string2 i = int_of_string (String.concat "" (String.split_on_char '_' i)) 
    let int_of_string3 beg i = 
        let l = (String.concat "" (String.split_on_char '_' i)) in
        int_of_string l
    let float_of_string2 i = 
        let l = (String.concat "" (String.split_on_char '_' i)) in
        float_of_string l
}

rule decoupe = parse
| "//" [^'\n']* '\n' { decoupe lexbuf } 
| "/*" ( [^'*'] | ('*' [^'/']) )* "*/" { decoupe lexbuf }
| " " { decoupe lexbuf } 
| "-" { Minus }
| '+' { Plus }
| '*' { Times }
| '(' { Lpar }
| ')' { Rpar }
| '\t' { decoupe lexbuf }
| '\n' { decoupe lexbuf }
| '\r' { decoupe lexbuf }
| eof { EOF }
| "typeof" { Typeof }
| "**" { Exp }
| '/' { Div }
| '<' { Comp_lt }
| '>' { Comp_gt }
| "<=" { Comp_le }
| ">=" { Comp_gt }
| "=" { Eq }
| "==" { Double_eq }
| "===" { Triple_eq }
| "!=" { Diff }
| "!==" { Double_diff }
| "&&" { And }
| "||" { Or }
| "number" { Type_num }
| "boolean" { Type_bool }
| "string" { Type_string }
| '[' { LBracket }
| ']' { RBracket }
| '{' { LAcc }
| '}' { RAcc }
| ';' { PVirgule }
| ',' { Virgule }
| ':' { DPoints }
| "var" { Var_decl }
| "if" { If }
| "else" { Else }
| "while" { While }
| "return" { Return }
| "type" { Type }
| "let" { Let }
| "const" { Const }
| "function" { Function }
| "any" { Any }
| "." { Point}
| "|" { Barre }
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
| "true"|"false" as b { Cst_bool (if b = "true" then true else false)}
(* identifiants *)
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as s { Id(s)}

| '"'([^'"']|"\\\"")*'"' as s { Cst_string (String.sub s 1 ((String.length s) -2)) }
| "'"([^''']|"\\'")*"'" as s { Cst_string (String.sub s 1 ((String.length s) -2)) }
| _ as c { failwith ("Unexpected character: " ^ Char.escaped c) }
{
    (*trailer*)
}
