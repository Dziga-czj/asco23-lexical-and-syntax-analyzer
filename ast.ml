type ast =
  | Group of ast
  | Plus of ast * ast
  | Times of ast * ast
  | Cst of int
  | Id of string

     
let print_sep l =
  List.iter print_string l

let rec print_sep_spec = function
  | [] -> ()
  | [x] -> print_string "|-"
  | x :: q -> print_string x; print_sep_spec q
    
let rec aff_aux l a =
  
  print_sep_spec l;
  match a with
  | Group(a) -> 
    Printf.printf("trouvé\n");
    print_string "(";
    aff_aux (l @ [")\n"]) a;
  | Plus(a1, a2) ->
    print_string "Plus\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["  "]) a2
  | Times(a1, a2) ->
    print_string "Fois\n";
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["| "]) a1;
    print_sep (l @ ["|\n"]);
    aff_aux (l @ ["  "]) a2
  | Cst i -> Printf.printf "Cte(%i)\n" i

  | Id s -> Printf.printf "Id(%s)\n" s

let affiche = aff_aux []
  
