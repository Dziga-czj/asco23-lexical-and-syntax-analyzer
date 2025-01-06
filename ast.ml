
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


module Table = Map.Make(String)

type decl_info = {
  decl_type: [ `Var | `Let | `Const ]; 
  mutable_flag: bool;                   
}

type table = decl_info Table.t   

  let check_scope ast =
    (* Pile de scopes : chaque scope est une table locale *)
    let rec push_scope stack = Table.empty :: stack
    and pop_scope = function
      | [] -> failwith "Scope stack underflow"
      | _::q -> q
    in
  
    let rec find_in_scopes id = function
      | [] -> None
      | scope::rest -> 
          (match Table.find_opt id scope with
           | Some info -> Some info
           | None -> find_in_scopes id rest)
    in
  
    (* Vérifie les affectations *)
    let check_assignment left_expr stack =
      match left_expr with
      | Left_id id -> (
          match find_in_scopes id stack with
          | Some { decl_type = `Const; _ } -> 
              failwith ("Cannot assign to constant variable: " ^ id)
          | Some _ -> ()  (* OK *)
          | None -> failwith ("Variable not declared: " ^ id)
        )
      | _ -> ()  (* Ignorer les autres cas d'affectation pour le moment *)
    in
  
    let rec aux curr stack =
      match curr with
      | [] -> true
      | t::q -> (
          match t with
          | I_or_D_instr instr -> 
              let stack' = check_scope_instr instr stack in
              aux q stack'
          | I_or_D_decl decl -> 
              let stack' = check_scope_decl decl stack in
              aux q stack'
        )
    and check_scope_instr instr stack =
      match instr with
      | Var_decl bindings -> 
          let current_scope = List.hd stack in
          let updated_scope = 
            List.fold_left (fun scope (Binding (Id id, typ, _)) ->
              Table.add id { decl_type = `Var; mutable_flag = true; decl_type_opt = typ } scope
            ) current_scope bindings
          in
          updated_scope :: (List.tl stack)
      | Bloc block -> 
          let stack' = push_scope stack in
          let _ = aux block stack' in
          pop_scope stack'
      | Func_call (Left_id id, _) -> (
          match find_in_scopes id stack with
          | Some { decl_type = `Let; _ } -> stack  (* OK : fonction trouvée *)
          | None -> failwith ("Function not declared: " ^ id)
        )
      | _ -> stack  (* Autres instructions *)
      let check_scope_instr instr stack =
  match instr with
  | Var_decl bindings -> 
      let current_scope = List.hd stack in
      let updated_scope = 
        List.fold_left (fun scope (Binding (Id id, typ, _)) ->
          Table.add id { decl_type = `Var; mutable_flag = true; decl_type_opt = typ } scope
        ) current_scope bindings
      in
      updated_scope :: (List.tl stack)
  | Affect (left, _) -> 
      check_assignment left stack; stack  (* Vérification des affectations *)
  | Pt_virgule _ -> stack  (* Le point-virgule ne change rien à la pile de scopes *)
  | Bloc block -> 
      let stack' = push_scope stack in
      let _ = aux block stack' in
      pop_scope stack'  (* Bloc de code avec un scope local *)
  | Func_call (left, _) -> 
      (* Vérification si la fonction a été déclarée dans le scope *)
      let id = match left with
        | Left_id id -> id
        | _ -> failwith "Invalid function call expression"
      in
      match find_in_scopes id stack with
      | Some { decl_type = `Let; _ } -> ()  (* OK, la fonction est déclarée *)
      | _ -> failwith ("Function not declared: " ^ id);
      stack
  | _ -> stack  (* Autres instructions non traitées *)

    and check_scope_decl decl stack =
      match decl with
      | Let_decl bindings -> 
          let current_scope = List.hd stack in
          let updated_scope = 
            List.fold_left (fun scope (Binding (Id id, typ, _)) ->
              Table.add id { decl_type = `Let; mutable_flag = true; decl_type_opt = typ } scope
            ) current_scope bindings
          in
          updated_scope :: (List.tl stack)
      | Const_decl bindings -> 
          let current_scope = List.hd stack in
          let updated_scope = 
            List.fold_left (fun scope (Binding (Id id, typ, _)) ->
              Table.add id { decl_type = `Const; mutable_flag = false; decl_type_opt = typ } scope
            ) current_scope bindings
          in
          updated_scope :: (List.tl stack)
      | Func_decl (Id id, bindings, return_type, body) -> 
          (* Ajouter la fonction elle-même au scope courant *)
          let current_scope = List.hd stack in
          let func_scope = Table.add id { decl_type = `Let; mutable_flag = false; decl_type_opt = return_type } current_scope in
          
          (* Créer un nouveau scope pour les paramètres de la fonction *)
          let param_scope = 
            List.fold_left (fun scope (Binding (Id param_id, typ, _)) ->
              Table.add param_id { decl_type = `Let; mutable_flag = true; decl_type_opt = typ } scope
            ) Table.empty bindings
          in
  
          (* Traiter le corps de la fonction avec un nouveau scope *)
          let stack' = param_scope :: (func_scope :: (List.tl stack)) in
          let _ = aux body stack' in
          stack
      | _ -> stack
    in
  
    (* Initialisation avec un scope global *)
    aux ast [Table.empty]
  


    
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
