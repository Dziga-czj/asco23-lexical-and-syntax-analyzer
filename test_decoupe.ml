let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    ignore(Lexer.decoupe lexbuf);
  done
