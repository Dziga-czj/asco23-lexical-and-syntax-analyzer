let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    let a = Parser.s (Lexer.decoupe) lexbuf in
    if a = [] then exit 0;
    Ast.affiche a;
  done
