type ast =
  | Group of ast
  | Plus of ast * ast
  | Times of ast * ast
  | Cst_int of int
  | Cst_float of float
  | Cst_bool of bool
  | Cst_string of string
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
  | Cst_int i -> Printf.printf "Cst(%i)" i
  | Cst_bool i -> if i then Printf.printf "Cst_bool(true)" else Printf.printf "Cst_bool(false)"
  | Cst_float i -> Printf.printf "Cst_float(%f)" i
  | Cst_string s -> print_string s; print_newline ();
  | Id s -> Printf.printf "Id(%s)" s

let affiche = aff_aux
  
