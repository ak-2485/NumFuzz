exception ElementaryOperation of string

open Translate_ast

let rec check_elem_helper (e : expr) =
  match e with
  | ENum _ | ESymbol _ | EConstant _ -> false
  | EIf (e1, e2, e3) ->
      check_elem_helper e1 || check_elem_helper e2 || check_elem_helper e3
  | ELet (lst, exp) ->
      List.exists (fun (_, expr) -> check_elem_helper expr) lst
      || check_elem_helper exp
  | EApp (e1, e2) -> check_elem_helper e1 || check_elem_helper e2
  | EBang (_, expr) -> check_elem_helper expr
  | EArray lst -> List.exists check_elem_helper lst
  | EOP (op, _) -> (
      match op with
      | Plus | Times | Sqrt | Divide -> true
      | Equals | GreaterThan | Cast -> false)
  | ERef (expr, _) -> check_elem_helper expr

let check_elementary (core : fpcore) =
  let body = match core with FPCore (_, _, _, b) -> b in
  if check_elem_helper body then
    raise (ElementaryOperation "FPCore uses elementary operation")

let get_core_name = function FPCore (s, _, _, _) -> s

let elem_names (s : string) =
  match s with "addfp" | "mulfp" | "sqrtfp" | "divfp" -> true | _ -> false

(** ignores addfp/mulfp/divfp/ sqrtfp *)
let rec check_prog (prog : program) =
  match prog with
  | [] -> ()
  | core :: t ->
      let name = get_core_name core in
      if name = None then (
        check_elementary core;
        check_prog t)
      else if elem_names (Option.get name) then check_prog t
      else check_elementary core;
      check_prog t

let op_names (s : symbol) =
  match s with
  | "addfp" -> Some Plus
  | "divfp" -> Some Divide
  | "mulfp" -> Some Times
  | "sqrtfp" -> Some Sqrt
  | _ -> None

let check_app_elem e1 e2 =
  match e1 with
  | ESymbol s -> (
      let op = op_names s in
      if op = None then None
      else
        match Option.get op with
        | Sqrt -> Some (EOP (Sqrt, [ e2 ]))
        | _ ->
            let arr_list =
              match e2 with EArray l -> l | _ -> failwith "error in checkapp"
            in
            let el1, el2 = (List.nth arr_list 0, List.nth arr_list 1) in
            Some (EOP (Option.get op, [ el1; el2 ])))
  | _ -> None

let rec transform_ast_elem expr =
  match expr with
  | ENum _ -> expr
  | ESymbol _ -> expr
  | EOP (fpop, e_lst) -> EOP (fpop, List.map transform_ast_elem e_lst)
  | EIf (e1, e2, e3) ->
      EIf (transform_ast_elem e1, transform_ast_elem e2, transform_ast_elem e3)
  | ELet (lst, e) ->
      ELet
        ( List.map (fun (symbol, exp) -> (symbol, transform_ast_elem exp)) lst,
          transform_ast_elem e )
  | EArray lst -> EArray (List.map transform_ast_elem lst)
  | ERef (e, lst) -> ERef (transform_ast_elem e, lst)
  | EConstant _ -> expr
  | EBang (p_list, e) -> EBang (p_list, transform_ast_elem e)
  | EApp (e1, e2) ->
      Option.default
        (EApp (transform_ast_elem e1, transform_ast_elem e2))
        (check_app_elem e1 e2)

let transform_body (core : fpcore) f =
  match core with FPCore (s, a, p, b) -> FPCore (s, a, p, f b)
