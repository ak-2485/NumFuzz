open Translate_ast

(** [unwind_app] takes an expression of a function application [e] and 
  returns the list of arguments being passed into the function.
  Should be called with [args] as nil [] *)
let rec unwind_app (e : expr) (args : expr list) : symbol * expr list =
  match e with
  | EApp (e1, e2) -> unwind_app e1 (e2 :: args)
  | ESymbol name -> (name, args)
  | _ -> failwith "Unable to parse function call while inlining"

(** [build_arg_sub_map] takes a list [vars] and a list [args] and creates an 
  association list mapping the nth element of [vars] to the nth element of [lists].
  Requires: [vars] and [args] are the same length *)

let rec build_arg_sub_map (vars : argument list) (args : expr list)
    (map : (string * expr) list) : (string * expr) list =
  match (vars, args) with
  | vh :: vt, ah :: at ->
      let v_name = match vh with ASymbol s -> s | Array (s, _) -> s in
      build_arg_sub_map vt at ((v_name, ah) :: map)
  | [], [] -> map
  | _ ->
      failwith "Failed inline: function called with wrong number of arguments."

(** [substitute_args_rec] takes a map [subst_map] from parameter names to the 
  argument passed in for that parameter and substitutes for all occurences of 
  the parameters in [body] *)

let rec substitute_args_rec (subst_map : (string * expr) list) (body : expr) :
    expr =
  let substitute_args_rec' = substitute_args_rec subst_map in
  match body with
  | ENum n -> ENum n
  | ESymbol s -> (
      match List.assoc_opt s subst_map with Some e -> e | None -> ESymbol s)
  | EOP (op, e's) -> EOP (op, List.map (fun e -> substitute_args_rec' e) e's)
  | EIf (e1, e2, e3) ->
      EIf
        ( substitute_args_rec' e1,
          substitute_args_rec' e2,
          substitute_args_rec' e3 )
  | ELet (args, e) ->
      ELet
        ( List.map (fun (s, e) -> (s, substitute_args_rec' e)) args,
          substitute_args_rec' e )
  | EArray e's -> EArray (List.map (fun e -> substitute_args_rec' e) e's)
  | ERef (e, d's) -> ERef (substitute_args_rec' e, d's)
  | EConstant c -> EConstant c
  | EApp (e1, e2) -> EApp (substitute_args_rec' e1, substitute_args_rec' e2)
  | EBang _ -> body
  | EFor ((s, e1), e2) ->
      EFor ((s, substitute_args_rec' e1), substitute_args_rec' e2)
(* CHECK THIS? *)

(** [substitute_args] takes a function definition [func] and a list of 
  arguments [arg] and returns the function body with the arguments substituded 
in for the corresponding parameter names.*)

let substitute_args (func : fpcore) (args : expr list) : expr =
  match func with
  | FPCore (_, vars, _, body) ->
      let subst_map = build_arg_sub_map vars args [] in
      substitute_args_rec subst_map body

(** [inline_expr] takes a mapping [dict] from function names to definitions and 
inlines all occurences of those functions in [e] *)
let rec inline_expr (dict : (string * fpcore) list) (e : expr) : expr =
  let inline_expr' = inline_expr dict in
  match e with
  | ENum n -> ENum n
  | ESymbol s -> ESymbol s
  | EOP (op, e's) -> EOP (op, List.map (fun e -> inline_expr' e) e's)
  | EIf (e1, e2, e3) -> EIf (inline_expr' e1, inline_expr' e2, inline_expr' e3)
  | ELet (args, e) ->
      ELet (List.map (fun (s, e) -> (s, inline_expr' e)) args, inline_expr' e)
  | EArray e's -> EArray (List.map (fun e -> inline_expr' e) e's)
  | ERef (e, d's) -> ERef (inline_expr' e, d's)
  | EConstant c -> EConstant c
  | EApp _ -> (
      let name, args = unwind_app e [] in
      match List.assoc_opt name dict with
      | Some func_def -> substitute_args func_def args
      | None -> e)
  | EBang _ -> e
  | EFor ((s, e1), e2) -> EFor ((s, inline_expr' e1), inline_expr' e2)

(** [inline] takes in a program [prog] (list of fpcore's) and inlines all function 
  definitions into the last function. If multiples functions are defined with 
  the same name, the latest definition is used. *)

let inline (prog : fpcore list) : fpcore =
  let prog_rev = List.rev prog in
  let dict =
    prog_rev |> List.tl
    |> List.map (fun x ->
           match x with
           | FPCore (name, _, _, _) -> (
               match name with Some s -> (s, x) | None -> ("", x)))
  in
  match List.hd prog_rev with
  | FPCore (name, args, props, e) ->
      FPCore (name, args, props, inline_expr dict e)

let next_var =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    "$" ^ string_of_int !counter

(* let replace_map args size anon_func_map =
   let ctr = next_var () in EFor (ctr, ENum (float_of_int max), ) *)
