let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    let a = Parser.s (Lexer.decoupe) lexbuf in
    Ast.affiche a;
    print_newline ()
  done
