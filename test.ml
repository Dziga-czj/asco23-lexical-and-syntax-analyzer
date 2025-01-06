let lexbuf = Lexing.from_channel stdin

exception End_of_file

let testing s l =
  List.iter (fun x -> 
    let file = "tests/"^s^"/"^x in
    Printf.printf "testing %s\n\n" file;
    flush stdout;
    let f = open_in file in
    let lexbuf = Lexing.from_channel f in
    try 
      while true do
        let r = Parser.s (Lexer.decoupe) lexbuf in
        if r = [] then raise End_of_file
      done
    with End_of_file -> close_in f;
  ) l


let _ =
  let a = [
    ("OK", ["comments.ts";"constants.ts";"consts.ts";"decls.ts";"expr.ts";"instr.ts";"scope.ts";"subtypes.ts";"typecheck.ts";"types.ts"]);
    ("KO", ["comments1.ts";
    "constants1.ts"  ;"constants2.ts"   ;"constants3.ts" ; "constants4.ts" ;"constants5.ts";"constants6.ts" ; "constants7.ts" ; "constants8.ts" ; "constants9.ts"  ;"constants10.ts"  ;"constants11.ts" ; 
    "types1.ts"; "types2.ts";
    "typecheck1.ts" ; "typecheck2.ts";
    "decls1.ts"; 
    "const1.ts";
    
    "scope1.ts";"scope2.ts" ; "scope3.ts"; "scope4.ts"])
  ] in

  

  let rec aux = function
    | [] -> ()
    | (s,l)::q -> 
      begin
        if s = "OK" then begin
          testing s l;
        end
        else
          let rec testing_error s l =
            match l with
            | [] -> ()
            | t::q -> 
                let file = "tests/"^s^"/"^t in
                Printf.printf "testing %s\n" file;
                flush stdout;
                let f = open_in file in
                let lexbuf = Lexing.from_channel f in
                try 
                  let r = Parser.s (Lexer.decoupe) lexbuf in
                  if r = [] then raise End_of_file
                with e -> begin match e with
                          | End_of_file -> Printf.printf "KO test failed\n\n"; flush stdout; close_in f;
                          | _ -> Printf.printf "KO test failed successfully \n\n"; flush stdout; close_in f; testing_error s q end
          in testing_error s l;
        
      end;
      aux q
      in aux a