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
    
let rec aff_aux a =
  match a with
  | Group(a1) -> 
    print_string "(";
    aff_aux a1;
    print_string ")";
  | Plus(a1, a2) ->
    aff_aux a1;
    print_string "+";
    aff_aux a2;
  | Times(a1, a2) ->
    aff_aux a1;
    print_string "*";
    aff_aux a2;
  | Cst i -> Printf.printf "Cte(%i)" i

  | Id s -> Printf.printf "Id(%s)" s

let affiche = aff_aux
  
