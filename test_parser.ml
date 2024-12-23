let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    Printf.printf "test\n";
    let a = Parser.s (Lexer.decoupe) lexbuf in
    Printf.printf "test\n";
    Ast.affiche a;
    print_newline ()
  done
