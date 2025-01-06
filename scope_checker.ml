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

let check_scope_decl decl table =
  match decl with
  | Let_decl bindings ->
      List.fold_left (fun t (Binding (Id id, _, _)) ->
        let decl_info = { decl_type = `Let; mutable_flag = true } in
        Table.add id decl_info t
      ) table bindings
  | Const_decl bindings ->
      List.fold_left (fun t (Binding (Id id, _, _)) ->
        let decl_info = { decl_type = `Const; mutable_flag = false } in
        Table.add id decl_info t
      ) table bindings
  | Type_alias (Id id, _) ->
      let decl_info = { decl_type = `Let; mutable_flag = false } in
      Table.add id decl_info table
  | Func_decl (Id id, _, _, _) ->
      let decl_info = { decl_type = `Let; mutable_flag = false } in
      Table.add id decl_info table
  | _ -> table  (* Si ce n'est pas une déclaration *)


let check_scope_instr instr table =
  match instr with
  | Var_decl bindings -> 
      List.fold_left (fun t (Binding (Id id, _, _)) ->
        let decl_info = { decl_type = `Var; mutable_flag = true } in
        Table.add id decl_info t
      ) table bindings
  | _ -> table  (* Autres instructions non traitées *)

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
        | Some _ -> ()  
        | None -> failwith ("Variable not declared: " ^ id)
      )
    | _ -> ()  
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
          List.fold_left (fun scope (Binding (Id id, _, _)) ->
            Table.add id { decl_type = `Var; mutable_flag = true } scope
          ) current_scope bindings
        in
        updated_scope :: (List.tl stack)
    | Bloc block -> 
        let stack' = push_scope stack in
        let _ = aux block stack' in
        pop_scope stack'
    | _ -> stack  
  and check_scope_decl decl stack =
    match decl with
    | Let_decl bindings -> 
        let current_scope = List.hd stack in
        let updated_scope = 
          List.fold_left (fun scope (Binding (Id id, _, _)) ->
            Table.add id { decl_type = `Let; mutable_flag = true } scope
          ) current_scope bindings
        in
        updated_scope :: (List.tl stack)
    | Const_decl bindings -> 
        let current_scope = List.hd stack in
        let updated_scope = 
          List.fold_left (fun scope (Binding (Id id, _, _)) ->
            Table.add id { decl_type = `Const; mutable_flag = false } scope
          ) current_scope bindings
        in
        updated_scope :: (List.tl stack)
    | _ -> stack
  in

  aux ast [Table.empty]
