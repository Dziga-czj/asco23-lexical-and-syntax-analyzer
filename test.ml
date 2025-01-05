let lexbuf = Lexing.from_channel stdin

type file = string * string list

exception End_of_file

let _ =
  let a = [
    ("OK", ["comments.ts";"constants.ts";"consts.ts";"decls.ts";"expr.ts";"instr.ts";"scope.ts";"subtypes.ts";"typecheck.ts";"types.ts"]);
    ("KO", ["comments1.ts";"constants10.ts";"constants3.ts" ; "constants6.ts" ; "constants9.ts" ; "scope2.ts" ; "typecheck1.ts" ; "types2.ts";
    "const1.ts";"constants11.ts"  ;"constants4.ts"  ;"constants7.ts" ; "decls1.ts"; "scope3.ts";  "typecheck2.ts";
    "constants1.ts"  ;"constants2.ts"   ;"constants5.ts" ; "constants8.ts" ; "scope1.ts";"scope4.ts"  ;"types1.ts"])

  ] in

  

  let rec aux = function
    | [] -> ()
    | (s,l)::q -> 
      List.iter (fun x -> 
        let file = "tests/"^s^"/"^x in
        Printf.printf "testing %s\n" file;
        let lexbuf = Lexing.from_channel (open_in file) in
        try 
          while true do
            let r = Parser.s (Lexer.decoupe) lexbuf in
            if r = [] then raise End_of_file
          done
        with End_of_file -> ()
      ) l
      in aux a