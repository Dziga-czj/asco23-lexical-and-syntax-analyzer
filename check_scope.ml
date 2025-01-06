open Ast

exception Undeclared_variable of string

module Table = Map.Make(String)

(*last : "ordre" de déclaration. toujours le dernier available*)
type table_type = { last: int; table: Table.t}

(* idée : pour chaque bloc, déclaration de fonction etc on relance la recherche avec la table courante 
  donc si c'est local, on aura toutes les déclarations d'avant et on sauvegarde pas la table
  si c'est global, on mets a jour la table,
  etc
*)
let add_id i t =
  if Table.mem i t.table then t
  else let tb' = Table.add i t.table t.last in
    {last = t.last +1; table = tb'}

let add_b_list b_list t =
  match b_list with
  | [] -> t
  |  (Binding(i, typ, exp))::q ->
    if Table.mem i t.table then
      add_b_list q t
    else
      let updated_table = Table.add i t.table t.last in
      let updated_table = { t with last = t.last + 1; table = updated_table } in
      add_b_list q updated_table

let check_scope_binding (Binding (id, type_opt, expr_opt)) table =
  let table_after_type = 
    match type_opt with
    | None -> table
    | Some t -> check_scope_type t table
  in
  match expr_opt with
  | None -> table_after_type
  | Some expr -> check_scope_expr expr table_after_type


let check_scope_decl d table : Table.t =
  match d with 
  | Type_alias (i, t) -> let table_after_type = check_scope_type t table in 
    add_id i table_after_type
  | Let_decl (b_list) -> add_b_list b_list table
  | Const_decl (b_list) -> add_b_list b_list table
  (*pour la fonction on ajoute l'id a la table globale
  on ajoute les paramètres a la table avec add_b_list
  puis on check les instr et decl dans la fonction*)
  | Func_decl (i, b_list, typ, inst_or_decl_l) -> 
    let table' = add_id i table in
    let table' = add_b_list b_list table' in 
    check_i_or_d_l_scope inst_or_decl_l table'

let check_scope_instr i table =
  match i with 
    | Empty -> table
    | Pt_virgule expr -> check_scope_expr expr table
    | Bloc inst_or_decl_list -> check_i_or_d_l_scope inst_or_decl_list table
    | Var_decl b_list -> add_b_list b_list table
    | If (cond, then_instr, else_instr_opt) -> 
      let table_after_cond = check_scope_expr cond table in
      let table_after_then = check_scope_instr then_instr table_after_cond in
      (match else_instr_opt with
        | None -> table_after_then
        | Some else_instr -> check_scope_instr else_instr table_after_then)
    | While (cond, body_instr) -> 
        let table_after_cond = check_scope_expr cond table in
        check_scope_instr body_instr table_after_cond
    | Return expr_opt -> 
      (match expr_opt with
      | None -> table
      | Some expr -> check_scope_expr expr table)


let rec check_scope_expr e table =
  match e with
  | Par expr -> check_scope_expr expr table  
  | Cst _ -> table  
  | Left_mem left -> 
    check_scope_left left table  
  | Obj obj_list -> 
    List.fold_left (fun acc (_, expr) -> check_scope_expr expr acc) table obj_list
  | Tab expr_list -> 
    List.fold_left (fun acc expr -> check_scope_expr expr acc) table expr_list
  | Func_call (expr, args) -> 
    let table_after_func = check_scope_expr expr table in
    List.fold_left (fun acc arg -> check_scope_expr arg acc) table_after_func args
  | Typeof expr -> 
    check_scope_expr expr table  
  | Plus (expr1, expr2) | Minus (expr1, expr2) 
  | Exp (expr1, expr2) | Mul (expr1, expr2) 
  | Div (expr1, expr2) -> 
    (* affecte pas la table *)
    let table_after_expr1 = check_scope_expr expr1 table in
    check_scope_expr expr2 table_after_expr1
  | Add (expr1, expr2) | Sub (expr1, expr2) 
  | GT (expr1, expr2) | GE (expr1, expr2) 
  | LT (expr1, expr2) | LE (expr1, expr2) 
  | Eq (expr1, expr2) | Diff (expr1, expr2) 
  | Triple_eq (expr1, expr2) | Double_diff (expr1, expr2) 
  | Conj (expr1, expr2) | Disj (expr1, expr2) -> 
    (* affecte pas la table *)
    let table_after_expr1 = check_scope_expr expr1 table in
    check_scope_expr expr2 table_after_expr1

  | Affect (left, expr) -> 
    (* Vérifie si l'identifiant à gauche de l'affectation est bien dans la table, puis l'expression à droite *)
    let table_after_left = check_scope_left left table in
    check_scope_expr expr table_after_left


let check_i_or_d_l_scope l table =
  let rec aux curr acc table =
    match curr with
    | [] -> acc
    | t::q -> match t with
              | I_or_D_instr i -> let acc' = check_scope_instr i table in aux q acc' table'
              | I_or_D_decl d -> let table' = check_scope_decl d table in aux q acc' table'

  in aux a true table

let check_left_scope l table =
  match l with
  | Left_id i ->
      if Table.mem i table.table then table
      else failwith ("id not declared")
  | Tab_affect (arr_expr, index_expr) ->
      (* check l'expr du tableau et de l'index*)
      let table_after_arr = check_scope_expr arr_expr table in
      check_scope_expr index_expr table_after_arr
  | Point_sep (obj_expr, _) ->
      (* check l'expr de l'objet *)
      check_scope_expr obj_expr table

let rec check_scope_type t table =
  match t with
  | Identifiant i ->
      (* Vérifie si le type référencé par l'identifiant est bien déclaré dans la table *)
      if Table.mem i table.table then table
      else failwith ("Type " ^ i ^ " not declared")
  | Constante _ -> table
  | Number | Boolean | String | Any -> table
  | Tableau sub_type ->
      check_scope_type sub_type table
  | Object fields ->
      List.fold_left
        (fun acc (_, field_type_opt) ->
          match field_type_opt with
          | None -> acc
          | Some field_type -> check_scope_type field_type acc)
        table
        fields
  | Union type_list ->
      List.fold_left
        (fun acc sub_type -> check_scope_type sub_type acc)
        table
        type_list



let check_scope a =
  check_i_or_d_l_scope a Table.empty
    
(* Cas de test *)

let ast1 = [
  I_or_D_decl (Let_decl [Binding (Id "x", Some Number, Some (Cst (Cst_int 5)))])
]

let ast2 = [
  I_or_D_instr (Pt_virgule (Left_mem (Left_id "y")))
]

let ast3 = [
  I_or_D_decl (
    Func_decl (
      Id "myFunc",
      [Binding (Id "a", Some Number, None); Binding (Id "b", Some Number, None)],
      Some Number,
      [
        I_or_D_instr (Var_decl [Binding (Id "x", Some Number, Some (Add (Left_mem (Left_id "a"), Left_mem (Left_id "b"))))]);
        I_or_D_instr (Return (Some (Left_mem (Left_id "x"))))
      ]
    )
  )
]

let ast4 = [
  I_or_D_decl (Type_alias (Id "MyType", Number));
  I_or_D_decl (Let_decl [Binding (Id "y", Some (Identifiant "MyType"), Some (Cst (Cst_int 10)))])
]

(* Fonction de test *)
let test_check_scope ast =
  try
    let _ = check_scope ast in
    print_endline "Scope check passed!"
  with
  | Failure msg -> print_endline ("Scope check failed: " ^ msg)
  | Undeclared_variable var -> print_endline ("Undeclared variable: " ^ var)

let () =
  print_endline "Testing AST 1...";
  test_check_scope ast1;

  print_endline "Testing AST 2...";
  test_check_scope ast2;

  print_endline "Testing AST 3...";
  test_check_scope ast3;

  print_endline "Testing AST 4...";
  test_check_scope ast4
