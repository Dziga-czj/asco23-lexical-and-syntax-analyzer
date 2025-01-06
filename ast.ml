
type id = Id of string

type const =    
  | Cst_int of int
  | Cst_float of float
  | Cst_bool of bool
  | Cst_string of string

and type_ = 
  | Identifiant of string
  | Constante of const
  | Number
  | Boolean
  | String
  | Tableau of type_
  | Any
  | Object of (id * type_ option) list
  | Union of type_ list

(* and sers a ce que il reconaisse tout les types en même temps pour pouvoir les utiliser mutuellement *)
and left = 
  | Left_id of string
  | Tab_affect of (expr * expr) (* e[e] *)
  | Point_sep of (expr * id) (* e.i *)

and expr = 
  | Par of expr
  | Cst of const
  | Left_mem of left
  | Obj of (id * expr) list
  | Tab of expr list
  | Func_call of (expr * expr list)
  | Typeof of expr
  | Plus of expr (* +e *)
  | Minus of expr (* -e *)
  | Exp of expr * expr
  | Mul of expr * expr 
  | Div of expr * expr 
  | Add of expr * expr (* e+e *)
  | Sub of expr * expr (* e-e *)
  | GT of expr * expr (* > *)
  | GE of expr * expr (* >= *)
  | LT of expr * expr (* < *)
  | LE of expr * expr (* <= *)  
  | Eq of expr * expr (* == *)  
  | Diff of expr * expr (* != *)  
  | Triple_eq of expr * expr (* === *)  
  | Double_diff of expr * expr (* !== *)  
  | Conj of expr * expr (* && *)  
  | Disj of expr * expr (* || *)  
  | Affect of left * expr (* l = e *)

and instr = 
  | Empty
  | Pt_virgule of expr
  | Bloc of inst_or_decl list
  | Var_decl of binding list
  | If of expr * instr * instr option
  | While of expr * instr
  | Return of expr option

and binding = Binding of id * type_ option * expr option

and inst_or_decl = 
  | I_or_D_instr of instr 
  | I_or_D_decl of decl

and decl = 
  | Type_alias of id * type_
  | Let_decl of binding list
  | Const_decl of binding list
  | Func_decl of id * binding list * type_ option * inst_or_decl list

and ast = inst_or_decl list

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

(* ________________________________________PRINTING___________________________________________________*)
let rec print_id (Id s) = print_string s

and print_const = function
  | Cst_int i -> print_string "cst:";print_int i
  | Cst_float f -> print_string "cst:";print_float f
  | Cst_bool b -> print_string "cst:";print_string (string_of_bool b)
  | Cst_string s -> print_string "cst:";print_string s

and print_type = function
  | Identifiant s -> print_string s
  | Constante c -> print_const c
  | Number -> print_string "Number"
  | Boolean -> print_string "Boolean"
  | String -> print_string "String"
  | Tableau t -> print_string "Tableau of "; print_type t
  | Any -> print_string "Any"
  | Object champs -> print_string "Object of "; print_champs champs
  | Union types -> print_string "Union of "; List.iter print_type types

and print_champs = function
  | [] -> ()
  | [(id, t_opt)] -> print_id id; print_string ": "; (match t_opt with None -> () | Some t -> print_type t)
  | (id, t_opt) :: q -> print_id id; print_string ": "; (match t_opt with None -> () | Some t -> print_type t); print_string ", "; print_champs q

and print_left = function
  | Left_id s -> print_string s
  | Tab_affect (e1, e2) -> print_string "Tab_affect of "; print_expr e1; print_string ", "; print_expr e2
  | Point_sep (e, Id s) -> print_string "Point_sep of "; print_expr e; print_string ", "; print_string s

and print_expr = function
  | Par e -> print_string "Par of "; print_expr e
  | Cst c -> print_const c
  | Left_mem l -> print_left l
  | Obj id_expr_list -> print_string "Obj of "; List.iter (fun (id, e) -> print_id id; print_string ": "; print_expr e) id_expr_list
  | Tab expr_list -> print_string "Tab of "; List.iter print_expr expr_list
  | Func_call (e, expr_list) -> print_string "Func_call of "; print_expr e; print_string ", "; List.iter print_expr expr_list
  | Typeof e -> print_string "Typeof of "; print_expr e
  | Plus e -> print_string "Plus of "; print_expr e
  | Minus e -> print_string "Minus of "; print_expr e
  | Exp (e1, e2) -> print_string "Exp of "; print_expr e1; print_string ", "; print_expr e2
  | Mul (e1, e2) -> print_string "Mul of "; print_expr e1; print_string ", "; print_expr e2
  | Div (e1, e2) -> print_string "Div of "; print_expr e1; print_string ", "; print_expr e2
  | Add (e1, e2) -> print_string "Add of "; print_expr e1; print_string ", "; print_expr e2
  | Sub (e1, e2) -> print_string "Sub of "; print_expr e1; print_string ", "; print_expr e2
  | GT (e1, e2) -> print_string "GT of "; print_expr e1; print_string ", "; print_expr e2
  | GE (e1, e2) -> print_string "GE of "; print_expr e1; print_string ", "; print_expr e2
  | LT (e1, e2) -> print_string "LT of "; print_expr e1; print_string ", "; print_expr e2
  | LE (e1, e2) -> print_string "LE of "; print_expr e1; print_string ", "; print_expr e2
  | Eq (e1, e2) -> print_string "Eq of "; print_expr e1; print_string ", "; print_expr e2
  | Diff (e1, e2) -> print_string "Diff of "; print_expr e1; print_string ", "; print_expr e2
  | Triple_eq (e1, e2) -> print_string "Triple_eq of "; print_expr e1; print_string ", "; print_expr e2
  | Double_diff (e1, e2) -> print_string "Double_diff of "; print_expr e1; print_string ", "; print_expr e2
  | Conj (e1, e2) -> print_string "Conj of "; print_expr e1; print_string ", "; print_expr e2
  | Disj (e1, e2) -> print_string "Disj of "; print_expr e1; print_string ", "; print_expr e2
  | Affect (l, e) -> print_string "Affect of "; print_left l; print_string ", "; print_expr e

and print_instr = function
  | Empty -> print_string "Empty"
  | Pt_virgule e -> print_expr e; print_string "Pt_virgule"; 
  | Bloc inst_or_decl_list -> print_string "Bloc of "; List.iter print_inst_or_decl inst_or_decl_list
  | Var_decl bindings -> print_string "Var_decl of "; print_bindings bindings
  | If (e, i, None) -> print_string "If of "; print_expr e; print_string ", "; print_instr i
  | If (e, i, Some i2) -> print_string "If of "; print_expr e; print_string ", "; print_instr i; print_string ", "; print_instr i2
  | While (e, i) -> print_string "While of "; print_expr e; print_string ", "; print_instr i
  | Return None -> print_string "Return None"
  | Return Some e -> print_string "Return of "; print_expr e

and print_binding (Binding (id, t_opt, e_opt)) =
  print_id id;
  (match t_opt with
  | None -> ()
  | Some t -> print_string ": "; print_type t);
  (match e_opt with
  | None -> ()
  | Some e -> print_string " = "; print_expr e)

and print_bindings bindings = List.iter print_binding bindings

and print_inst_or_decl = function
  | I_or_D_instr i -> print_instr i
  | I_or_D_decl d -> print_decl d

and print_decl = function
  | Type_alias (id, t) -> print_string "Type_alias of "; print_id id; print_string ", "; print_type t
  | Let_decl bindings -> print_string "Let_decl of "; print_bindings bindings
  | Const_decl bindings -> print_string "Const_decl of "; print_bindings bindings
  | Func_decl (id, bindings, t_opt, inst_or_decl_list) ->
    print_string "Func_decl of "; print_id id; print_string ", "; print_bindings bindings;
    (match t_opt with
    | None -> ()
    | Some t -> print_string ": "; print_type t);
    print_string ", "; List.iter print_inst_or_decl inst_or_decl_list

and print_ast ast = List.iter print_inst_or_decl ast