open Ast 

module Table = Map.Make(String)

type tab = { table: Table.t }

exception Not_declared of string

let refresh_type i t tab =
  if Table.mem i table then Table.update i t table
  else Table.add i t table

(* type_ = 
  | Identifiant of string
  | Constante of const
  | Number
  | Boolean
  | String
  | Tableau of type_
  | Any
  | Object of (id * type_ option) list
  | Union of type_ list
  *)

let equal_types t1 t2 =
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
  | Tableau of t1' -> 
    match t2 with
    | Tableau of t2' -> equal_types t1' t2'
    | _ -> false
  | Union of l -> 
    match t2 with
    | Union of l2 -> if List.length l2 = List.length l1 then List.for_all2 equal_types l l2
    else false
    | _ -> false
  | Object of l -> 
    match t2 with
    | Object of l2 -> if List.length l2 = List.length l1 then List.for_all2 (fun (Id i', t') (Id i2', t2') -> i' = i2' && equal_types t' t2') l l2
    else false
    | _ -> false



let test_types i t tab =
  if not Table.mem i tab then raise Not_declared (i)
  else
  let t1 = Table.find i tab in
  equal_types t1 t


let check_b_list_type l inp_table = 

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
      
    
    ) inp_table l 



let check_type_instr i =
  match i with
  | 
  | 


let check_type_decl d table =
  match d with
  | Type_alias (i, t) -> refresh_type i t table
  | Let_decl (b_list) -> check_b_list_type b_list table
  | Const_decl (b_list) -> check_b_list_type b_list table
  | Func_decl of (i, b_list, typ, inst_or_decl_l) ->


let check_type a =

let check_i_or_d_list_type l =
  List.fold_left (fun table x ->
    let res = 
      match x with
      | I_or_D_instr i -> check_type_instr i table 
      | I_or_D_decl d -> check_type_decl d table
    in

    ) Table.empty l
