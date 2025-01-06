open Ast 

module Table = Map.Make(String)

exception Not_declared of string
exception Type_error of string

let refresh_type i t tab =
  if (Table.mem i tab) then Table.update i t tab
  else Table.add i t tab

let get_type i tab =
  if (Table.mem i tab) then Table.find i tab
  else raise Not_declared (i)

let rec equal_types t1 t2 =
  if t2 = Any || t1 = Any then true
  else
  match t1 with
  | Any -> true
  | Number -> t2 = Number
  | Boolean -> t2 = Boolean
  | String -> t2 = String
  | Identifiant (s1) -> match t2 with
    | Identifiant (s2) -> s1 = s2
    | _ -> false
  | Constante (c1) -> 
    match t2 with
    | Constante (c2) -> c1 = c2
    | _ -> false
  | Tableau t1' -> 
    match t2 with
    | Tableau t2' -> equal_types t1' t2'
    | _ -> false
  | Union l -> 
    match t2 with
    | Union l2 -> if List.length l2 = List.length l1 then List.for_all2 equal_types l l2
    else false
    | _ -> false
  | Object l -> 
    match t2 with
    | Object l2 -> if List.length l2 = List.length l1 then List.for_all2 (fun (Id i', t') (Id i2', t2') -> i' = i2' && equal_types t' t2') l l2
    else false
    | _ -> false

let get_const_type c =
  match c with
  | Cst_int _ -> Number
  | Cst_float _ -> Number
  | Cst_bool _ -> Boolean
  | Cst_string _ -> String

let is_num_type e =
  let t = check_exp_type e tab in
  if not equal_types t Number then raise Type_error ("not a number")
  else Number

let is_num_type2 e1 e2 =
  if is_num_type e1 = Number && is_num_type e2 = Number then Number
  else raise Type_error ("not a number")

let check_tab_type l =
  match List.length l with
  | 0 -> (true,Any)
  | 1 -> (truen, check_exp_type (List.hd l))
  | _ ->
  List.fold_left (fun (res,prev) x -> 
    if not res then (false, prev)
    else
    let t = check_exp_type x in
    (equal_types t prev, t)
  ) (true, check_exp_type (List.hd l))  (List.tl l)



let check_left_mem_type lef tab =
  match lef with
  | Left_id (i) -> get_type i tab
  | Tab_affect (e1,e2) -> let t2 = check_exp_type e2 tab in
                          if t2 <> Number then raise Type_error ("Tab_affect_index_not_number")
                          else
                            match e1 with
                            | Cst c -> match c with
                                  | Cst_string s -> get_type c tab
                                  | _ -> raise Type_error ("Tab_affect")
                            | e1 -> check_exp_type e1 tab 
                          
  | Point_sep (e,i) -> (*id,object,union*)
                            let t2 = get_type i tab in
                            let t = check_exp_type e tab in
                            let t' = match t with
                            | Identifiant (s) -> let t' = get_type s tab in if t2 <> t' then raise Type_error ("Point_sep")
                                                else t2
                            | Union l -> failwith "TODO"
                            | Object l -> List.fold_left (fun acc x ->
                              let (i2, t_opt) = x in
                              let t2 = match t_opt with
                              | None -> Any 
                              | Some typ -> typ
                              in
                              if i = i2 then if not equal_types t2 t then raise Type_error ("Point_sep")
                                            else t2
                              else acc
                              ) Any l
                            in if not equal_types t' t then raise Type_error ("Point_sep")
                            else t
                              

let check_exp_type e' tab =
  match e' with
  | Par (e) -> check_exp_type e tab
  | Cst c -> get_const_type
  | Left_mem lef -> check_left_mem_type lef tab
  | Obj l -> get_obj_type l tab
  | Tab l -> let (res, t) = check_tab_type l in 
            if not res then raise Type_error ("Tab")
            else t
  | Func_call (e,l) -> let t = match e with
                      | Cst c -> match c with
                                  | Cst_string s -> get_type c tab
                                  | _ -> raise Type_error ("Func_call_on_non_func")
                      | e -> check_exp_type e tab
                      in
                      List.iter (fun x -> check_exp_type x tab) l;
                      t
 
  | Typeof (e) -> check_exp_type e tab
  | Plus e -> is_num_type e
  | Minus e -> is_num_type e
  | Exp (e1 ,e2) -> is_num_type2 e1 e2
  | Mul (e1 ,e2) -> is_num_type2 e1 e2
  | Div (e1 ,e2) -> is_num_type2 e1 e2
  | Add (e1 ,e2) -> is_num_type2 e1 e2
  | Sub (e1 ,e2) -> is_num_type2 e1 e2
  | GT (e1 ,e2) -> let t = is_num_type2 e1 e2 in Boolean
  | GE (e1 ,e2) -> let t = is_num_type2 e1 e2 in Boolean
  | LT (e1 ,e2) -> let t = is_num_type2 e1 e2 in Boolean
  | LE (e1 ,e2) -> let t = is_num_type2 e1 e2 in Boolean
  | Eq (e1 ,e2) -> let t1 = check_exp_type e1 tab in
                  let t2 = check_exp_type e2 tab in
                  Boolean
  | Diff (e1 ,e2) -> let t1 = check_exp_type e1 tab in
                  let t2 = check_exp_type e2 tab in
                  Boolean
                  (*idem que == par simplicité*)
  | Triple_eq (e1 ,e2) -> let t1 = check_exp_type e1 tab in
                            let t2 = check_exp_type e2 tab in
                            Boolean
                             (*idem que != par simplicité*)
  | Double_diff (e1 ,e2) -> let t1 = check_exp_type e1 tab in
                            let t2 = check_exp_type e2 tab in
                            Boolean
  | Conj (e1 ,e2) ->  let t1 = check_exp_type e1 tab in
                      let t2 = check_exp_type e2 tab in
                      if not equal_types t1 Boolean || not equal_types t2 Boolean then raise Type_error ("Conj")
                      else Boolean
  | Disj (e1 ,e2) -> let t1 = check_exp_type e1 tab in
                      let t2 = check_exp_type e2 tab in
                      if not equal_types t1 Boolean || not equal_types t2 Boolean then raise Type_error ("Conj")
                      else Boolean
  | Affect (l , e) -> let t1 = check_left_type l tab in
                    let t2 = check_exp_type e tab in
                    if not equal_types t1 t2 then raise Type_error ("Affect")
                    else t1

let test_types i t tab =
  if not (Table.mem i tab) then raise Not_declared (i)
  else
  let t1 = Table.find i tab in (*fait une erreur automatiquement si ne trouve pas donc pas déclaré*)
  equal_types t1 t



let set_b_list_type l inp_table = 

    List.fold_left (fun table x ->

      let Binding(i, t_opt, exp_opt) = x in
      let table' = 
        match t_opt with
        | None -> refresh_type i Any table
        | Some typ -> refresh_type i typ table
      in
      let table' =
        match exp_opt with
        | None -> table'
        | Some exp -> test_types i (check_exp_type exp table') table'
      in
      table'
    
    ) inp_table l 



let check_type_instr i tab =
  match i with
  | Empty -> tab
  | Pt_virgule e -> ignore(check_exp_type e tab); tab
  | Bloc l -> List.fold_left (fun table x -> check_type_instr x table) tab l
  | Var_decl bl -> set_b_list_type bl tab
  | If (e, i, i_opt) -> let t = check_exp_type e tab in
                        if t <> Boolean then raise Type_error ("If")
                        else
                        let table' = check_type_instr i tab in
                        match i_opt with
                        | None -> table'
                        | Some i' -> check_type_instr i' table
  | While (e,i) -> let t = check_exp_type e tab in
                  if t <> Boolean then raise Type_error ("While")
                  else check_type_instr i tab
  | Return e -> match e with
                | None -> tab
                | Some e' -> let _ = check_exp_type e' tab in tab


let check_type_decl d table =
  match d with
  | Type_alias (i, t) -> refresh_type i t table
  | Let_decl (b_list) -> set_b_list_type b_list table
  | Const_decl (b_list) -> set_b_list_type b_list table
  | Func_decl (i, b_list, typ, inst_or_decl_l) -> failwith "TODO"
                                                  



let check_type a =
  check_i_or_d_list_type a (Table.empty)

let check_i_or_d_list_type l table =
  List.fold_left (fun table x ->
    let res = 
      match x with
      | I_or_D_instr i -> check_type_instr i table 
      | I_or_D_decl d -> check_type_decl d table
    in
    res
    ) table l
