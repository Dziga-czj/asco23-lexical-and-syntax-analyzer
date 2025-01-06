(* Exemple d'un test d'AST simple avec des variables et des affectations *)

open Scope_checker

let test_ast = [
  I_or_D_decl (Let_decl [
    Binding (Id "x", None, Some (Cst (Cst_int 10)))
  ]);
  I_or_D_instr (Var_decl [
    Binding (Id "y", None, None)
  ]);
  I_or_D_instr (Affect (Left_id "x", Cst (Cst_int 20)));
  I_or_D_instr (Affect (Left_id "z", Cst (Cst_int 30)))  (* "z" n'est pas déclaré ! *)
]

let () =
  try
    let _ = check_scope test_ast in
    print_endline "Test passed!"
  with
  | Failure msg -> Printf.printf "Test failed: %s\n" msg
