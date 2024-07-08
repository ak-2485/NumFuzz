open Syntax
open Translate_ast
open Translate_inline

(** [string_of prec prec] is the string representation of precision [prec].*)
let string_of_prec = function
  | Binary64 -> "binary64"
  | Binary32 -> "binary32"
  | Binary16 -> "binary16"
  | Real -> "real"

(** [string_of_prop_inline prop] returns the string representation of [prop] according
to the FPcore syntax. *)
let string_of_prop_inline = function
  | Prec p -> ":precision " ^ string_of_prec p
  | PRound -> ":round toPositive"

(** [string_of_prop_core] is the same as [string_of_prop_lst] but a newline character
is inserted after every element in the property list. *)
let rec string_of_prop_core = function
  | [] -> ""
  | p :: t -> string_of_prop_inline p ^ "\n" ^ string_of_prop_core t

(** This function returns a string representation of a list of properties that occurs
within an expression. There are no new line character in between each property. *)
let rec string_of_prop_lst (lst : property list) =
  match lst with
  | [] -> ""
  | p :: t -> string_of_prop_inline p ^ " " ^ string_of_prop_lst t

(** [op_names s] returns the operation that a floating-point operation variable name
represents. In NumFuzz, floating point operations (addfp,divfp,etc) are functions,
but which we can inline in FPCore by using an appropiate rounding context
and the equivalent operation.*)
let op_names (s : symbol) =
  match s with
  | "addfp" | "addfp_64" -> Some (Plus, Binary64)
  | "divfp" | "divfp_64" -> Some (Divide, Binary64)
  | "mulfp" | "mulfp_64" -> Some (Times, Binary64)
  | "sqrtfp" | "sqrtfp_64" -> Some (Sqrt, Binary64)
  | "addfp_32" -> Some (Plus, Binary32)
  | "divfp_32" -> Some (Divide, Binary32)
  | "mulfp_32" -> Some (Times, Binary32)
  | "sqrtfp_32" -> Some (Sqrt, Binary32)
  | _ -> None

(** [rnd_and_prec] is the list containing the FPCore properties for floating point
operations translated from NumFuzz, which are assumed to occur with binary 64 precision
and round towards positive infinity. *)
let rnd_and_prec = [ Prec Binary64; PRound ]

(** [translate_op] translates a numfuzz operation [op] to its FPcore equivalent. *)
let translate_op (op : op) : fpop =
  match op with
  | AddOp -> Plus
  | MulOp -> Times
  | SqrtOp -> Sqrt
  | DivOp -> Divide
  | GtOp -> GreaterThan
  | EqOp -> Equals

(** [unwind_abs] takes a term [t] and 
  returns the list of parameters and the body of the function.
  Should be called with [args] as nil [] *)
let rec unwind_abs (t : term) (args : argument list) : argument list * term =
  match t with
  | TmAbs (_, b_i, _, t') -> unwind_abs t' (ASymbol b_i.b_name :: args)
  | _ -> (args, t)

(** [unwind_app_tm] takes a term [t] and 
    returns the list of arguments being passed into the function & the function name.
    If it does not parse as a normal function application, returns no name.
    Should be called with [args] as nil [] *)
let rec unwind_app_tm (t : term) (args : term list) : symbol * term list =
  match t with
  | TmApp (_, t1, t2) -> unwind_app_tm t1 (t2 :: args)
  | TmPrim (_, tprim) -> (
      match tprim with PrimTString str -> (str, args) | _ -> ("", args))
  | TmVar (_, v_i) -> (v_i.v_name, args)
  | _ -> ("", args)

(** [translate] converts a NumFuzz term [prog] into an equivalent FPCore program*)
let rec translate (prog : term) : program =
  match prog with
  | TmAbs _ ->
      let arg_list, body = get_arguments prog in
      [ FPCore (None, arg_list, [], translate_expr body) ]
  | TmLet (_, bind, _, t1, t2) -> (
      match t1 with
      | TmAbs _ ->
          let arg_list, body = get_arguments t1 in
          FPCore (Some bind.b_name, arg_list, [], translate_expr body)
          :: translate t2
      | _ -> [])
  | _ -> []

(** [arg_of_typ ty] returns [Some 2] if [typ] is either [TyAmpersand] or [TyTensor].
    Returns [None] otherwise.*)
and arg_of_typ (typ : ty) =
  match typ with TyAmpersand _ | TyTensor _ -> Some 2 | _ -> None

(** [get_arguments prog] returns a pair of list of arguments and body of [prog],
provided that [prog] is a "chain" of lambda expressions, which represents a multi-argument
function. Otherwise, the function returns [(\[\],prog)].

For example, calling [get_arguments] on an expression of the form [lambda x. lambda y. e] 
returns [(\[x;y\],e)]. *)
and get_arguments (prog : term) : argument list * term =
  match prog with
  | TmAbs (_, b_info, ty, t) ->
      let dim = arg_of_typ ty in
      let curr_arg =
        if dim = None then ASymbol b_info.b_name
        else Array (b_info.b_name, [ Option.get dim ])
      in
      let next_arg, next_t = get_arguments t in
      (curr_arg :: next_arg, next_t)
  | _ -> ([], prog)

(** [translate_expr] converts a NumFuzz function body [body] into its equivalent FPCore expression. 
Requires: [body] has no TMAbs terms, as FPCore does not support nested functions *)
and translate_expr (body : term) : expr =
  let rec translate_expr' subst_map anon_func_map body =
    match body with
    | TmVar (_, var_i) -> (
        match List.assoc_opt var_i.v_name subst_map with
        | Some e -> e
        | None -> ESymbol var_i.v_name)
    | TmTens (_, t1, t2) | TmAmpersand (_, t1, t2) ->
        EArray
          [
            translate_expr' subst_map anon_func_map t1;
            translate_expr' subst_map anon_func_map t2;
          ]
    | TmTensDest (_, b_i1, b_i2, t1, t2) ->
        let tens = translate_expr' subst_map anon_func_map t1 in
        ELet
          ( [
              (b_i1.b_name, ERef (tens, [ EInt 0 ]));
              (b_i2.b_name, ERef (tens, [ EInt 1 ]));
            ],
            translate_expr' subst_map anon_func_map t2 )
    | TmInl (_, t) ->
        EArray [ EConstant True; translate_expr' subst_map anon_func_map t ]
    | TmInr (_, t) ->
        EArray [ EConstant False; translate_expr' subst_map anon_func_map t ]
    | TmUnionCase (_, t1, b_i2, t2, b_i3, t3) ->
        let v1 =
          ERef (translate_expr' subst_map anon_func_map t1, [ EInt 1 ])
        in
        EIf
          ( ERef (translate_expr' subst_map anon_func_map t1, [ EInt 0 ]),
            ELet
              ([ (b_i2.b_name, v1) ], translate_expr' subst_map anon_func_map t2),
            ELet
              ([ (b_i3.b_name, v1) ], translate_expr' subst_map anon_func_map t3)
          )
    | TmPrim (_, tprim) -> (
        match tprim with
        | PrimTUnit -> EFloat (-1.0)
        | PrimTNum n -> EFloat n
        | PrimTString str -> ESymbol str
        | PrimTFun _ ->
            failwith "Reached unreachable PrimTFun clause."
            (* Check with Ariel ^ *))
    | TmRnd64 (_, t) ->
        EBang
          ( [ Prec Binary64; PRound ],
            EOP (Cast, [ translate_expr' subst_map anon_func_map t ]) )
    | TmRnd32 (_, t) ->
        EBang
          ( [ Prec Binary32; PRound ],
            EOP (Cast, [ translate_expr' subst_map anon_func_map t ]) )
    | TmRnd16 (_, t) ->
        EBang
          ( [ Prec Binary16; PRound ],
            EOP (Cast, [ translate_expr' subst_map anon_func_map t ]) )
    | TmRet (_, t) -> translate_expr' subst_map anon_func_map t
    | TmApp _ ->
        (* Check for map/fold application here *)
        let name, arg_terms = unwind_app_tm body [] in
        let args =
          List.map (translate_expr' subst_map anon_func_map) arg_terms
        in
        (* if Str.(string_match (regexp "map[0-9]+") name 0) then *)
        if String.length name >= 3 && String.sub name 0 3 = "map" then
          let size =
            4
            (* int_of_string (String.sub name 3 (String.length name - 3)) *)
          in
          replace_map args size anon_func_map
        else if String.length name >= 4 && String.sub name 0 4 = "fold" then
          let size =
            4
            (* int_of_string (String.sub name 3 (String.length name - 3)) *)
          in
          replace_fold args size anon_func_map
          (* Not a map or a fold; general case *)
        else EApp (ESymbol name, args)
    | TmAbs _ -> failwith "FPCore does not support nested functions."
    | TmAmp1 (_, t) ->
        ERef (translate_expr' subst_map anon_func_map t, [ EInt 0 ])
    | TmAmp2 (_, t) ->
        ERef (translate_expr' subst_map anon_func_map t, [ EInt 0 ])
    | TmBox (_, _, t) -> translate_expr' subst_map anon_func_map t
    | TmLet (_, b_i, _, t1, t2) -> (
        (* Check if t1 is a normal expression or an anonymous function *)
        match t1 with
        | TmAbs _ ->
            let args, func_body = unwind_abs t1 [] in
            let new_mapping =
              ( b_i.b_name,
                (args, translate_expr' subst_map anon_func_map func_body) )
            in
            translate_expr' subst_map (new_mapping :: anon_func_map) t2
        | _ ->
            ELet
              ( [ (b_i.b_name, translate_expr' subst_map anon_func_map t1) ],
                translate_expr' subst_map anon_func_map t2 ))
    | TmLetBind (_, b_i, t1, t2) | TmBoxDest (_, b_i, t1, t2) ->
        translate_expr'
          ((b_i.b_name, translate_expr' subst_map anon_func_map t1) :: subst_map)
          anon_func_map t2
    | TmOp (_, op, t) ->
        translate_expr_op (translate_op op)
          (translate_expr' subst_map anon_func_map t)
  in
  translate_expr' [] [] body

(** [translate_expr_op] converts a NumFuzz operator application [op] [t] into its FPCore equivalent*)
and translate_expr_op op (t : expr) =
  match op with
  | Plus | Times | Divide | Equals | Minus | GreaterThan ->
      EOP (op, [ ERef (t, [ EInt 0 ]); ERef (t, [ EInt 0 ]) ])
  | Sqrt | Cast -> EOP (op, [ t ])

(** [string_of_name] takes a string option [name] and returns the string with a trailing space,
 or an empty string in the None case *)
let string_of_name (name : symbol option) : string =
  match name with Some s -> s ^ " " | None -> ""

(** [sting_of_dim_list] converts a dimension list [ds] into a space-seperated string*)
let rec string_of_dim_list (ds : dimension list) : string =
  match ds with
  | [] -> ""
  | h :: t -> (" " ^ string_of_int h) ^ string_of_dim_list t

(** [string_of_args] converts an argument list [args] into a space-seperated string, 
conforming to the FPCore notation for Array argument names *)
let rec string_of_args (args : argument list) : string =
  match args with
  | [] -> ""
  | arg :: tl ->
      (match arg with
      | ASymbol x -> x
      | Array (x, ds) -> "(" ^ x ^ string_of_dim_list ds ^ ")")
      ^ (if tl == [] then "" else " ")
      ^ string_of_args tl

(** [string_of_op op] is the string representation of an FPCore operation [op]. *)
let string_of_op (op : fpop) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Sqrt -> "sqrt"
  | Equals -> "=="
  | GreaterThan -> ">"
  | Cast -> "cast"

(** [check_app e1 e2] returns an non-empty [Option] value if [e1] is 
  a variable name that represents one of the basic floating operations (addfp,sqrtfp,divfp,mulfp)
and returns an equivalent FPCore term that does not call these functions. If [e1] is anything
else, this function returns [None]. *)
let check_app e1 args =
  match e1 with
  | ESymbol s -> (
      let op = op_names s in
      if op = None then None
      else
        match Option.get op with
        | Sqrt, prec -> Some (EBang ([ Prec prec; PRound ], EOP (Sqrt, args)))
        | _ -> (
            match args with
            | EArray arr_list :: [] ->
                let el1, el2 = (List.nth arr_list 0, List.nth arr_list 1) in
                let actual_op, prec = Option.get op in
                Some
                  (EBang ([ Prec prec; PRound ], EOP (actual_op, [ el1; el2 ])))
            | _ -> None))
  | _ -> None

(** [check_app_elem] is the same as [check_app] but it doesn't add precision or
rounding annotations. *)
let check_app_elem e1 args =
  match e1 with
  | ESymbol s -> (
      let op = op_names s in
      if op = None then None
      else
        match Option.get op with
        | Sqrt, _ -> Some (EOP (Sqrt, args))
        | _ ->
            let arr_list =
              match args with
              | [ EArray l ] -> l
              | _ -> failwith "error in checkapp"
            in
            let el1, el2 = (List.nth arr_list 0, List.nth arr_list 1) in
            Some (EOP (Option.get op |> fst, [ el1; el2 ])))
  | _ -> None

(** [string_of_expr] converts an FPCore expression [e] into a string in FPCore syntax *)
let rec string_of_expr (e : expr) : string =
  match e with
  | EFloat n -> string_of_float n
  | EInt n -> string_of_int n
  | ESymbol str -> str
  | EOP (op, e's) ->
      "(" ^ string_of_op op
      ^ List.fold_left (fun acc a -> acc ^ " " ^ string_of_expr a) "" e's
      ^ ")"
  | EIf (e1, e2, e3) ->
      "(if " ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ " "
      ^ string_of_expr e3 ^ ")"
  | ELet (args, e) ->
      "(let (" ^ string_of_let_args args ^ ") " ^ string_of_expr e ^ ")"
  | EArray vals ->
      "(array "
      ^ List.fold_left (fun acc a -> acc ^ " " ^ string_of_expr a) "" vals
      ^ ")"
  | ERef (e, ds) ->
      "(ref " ^ string_of_expr e
      ^ List.fold_left (fun acc a -> acc ^ " " ^ string_of_expr a) "" ds
      ^ ")"
  | EConstant c -> ( match c with True -> "TRUE" | False -> "FALSE")
  | EApp (e1, e's) ->
      "(" ^ string_of_expr e1 ^ " "
      ^ List.fold_left (fun acc a -> acc ^ " " ^ string_of_expr a) "" e's
      ^ ")"
  | EBang (p_lst, e) ->
      "(! " ^ string_of_prop_lst p_lst ^ " " ^ string_of_expr e ^ ")"
  | ETensor (s1, e1, lst, e2) ->
      "(tensor* " ^ "( [ " ^ s1 ^ " " ^ string_of_expr e1 ^ " ] ) \n" ^ "( "
      ^ List.fold_left
          (fun acc (s, e1', e2') ->
            acc ^ " [ " ^ s ^ " " ^ string_of_expr e1' ^ " "
            ^ string_of_expr e2' ^ " ]")
          "" lst
      ^ " ) " ^ string_of_expr e2 ^ " )"
  | EFor (s1, e1, lst, e2) ->
      "(for " ^ "( [ " ^ s1 ^ " " ^ string_of_expr e1 ^ " ] ) \n" ^ "( "
      ^ List.fold_left
          (fun acc (s, e1', e2') ->
            acc ^ " [ " ^ s ^ " " ^ string_of_expr e1' ^ " "
            ^ string_of_expr e2' ^ " ]")
          "" lst
      ^ " ) " ^ string_of_expr e2 ^ " )"

(** [string_of_let_args] converts the bindings of a let expression into an FPCore string *)
and string_of_let_args (args : (symbol * expr) list) : string =
  match args with
  | (s, e) :: tl ->
      "[" ^ s ^ " " ^ string_of_expr e ^ "]" ^ string_of_let_args tl
  | [] -> ""

(** [string_of_fpcore] is the string representation a single FPCore.*)
let string_of_fpcore (prog : fpcore) : string =
  match prog with
  | FPCore (name, args, p_lst, e) ->
      "(FPCore " ^ string_of_name name ^ "(" ^ string_of_args args ^ ")\n"
      ^ string_of_prop_core p_lst ^ string_of_expr e ^ ")"

(** [string_of_program prog] is the string representation of a list of FPCores.*)
let string_of_program (prog : fpcore list) =
  List.fold_left (fun acc x -> acc ^ string_of_fpcore x ^ "\n\n") "" prog

(** [get_last lst] is the last element of [lst]. If [lst] is empty, this function
raises [Failure]. *)
let get_last lst = List.rev lst |> List.hd

(** [remove_last lst] is [lst] with its last element removed. *)
let rec remove_last lst =
  match lst with [] | [ _ ] -> [] | h :: t -> h :: remove_last t

(** [transform_body core f] applies [f] to the body of [core]. *)
let transform_body f (core : fpcore) =
  match core with FPCore (s, a, p, b) -> FPCore (s, a, p, f b)

(** [add_prop prop_lst core] appends [prop_lst] to the list of properties in
[core]. *)
let add_prop prop_lst = function
  | FPCore (s, arg_lst, p_lst, expr) ->
      FPCore (s, arg_lst, prop_lst @ p_lst, expr)

(** [transform_ast expr check] is [expr] but with every instance one of the floating operations 
  (addfp, sqrtfp, mulfp, divfp)
  inlined away with an equivalent operation. How this is done is specified by [check],
  which is applied to [e1 e2] when [expr] matches [EApp (e1,e2)]. *)
let rec transform_ast expr check =
  match expr with
  | EFloat _ | EInt _ -> expr
  | ESymbol _ -> expr
  | EOP (fpop, e_lst) ->
      EOP (fpop, List.map (fun x -> transform_ast x check) e_lst)
  | EIf (e1, e2, e3) ->
      EIf
        (transform_ast e1 check, transform_ast e2 check, transform_ast e3 check)
  | ELet (lst, e) ->
      ELet
        ( List.map (fun (symbol, exp) -> (symbol, transform_ast exp check)) lst,
          transform_ast e check )
  | EArray lst -> EArray (List.map (fun x -> transform_ast x check) lst)
  | ERef (e, lst) -> ERef (transform_ast e check, lst)
  | EConstant _ -> expr
  | EBang (p_list, e) -> EBang (p_list, transform_ast e check)
  | EApp (e1, e's) ->
      Option.default
        (EApp
           ( transform_ast e1 check,
             List.map (fun x -> transform_ast x check) e's ))
        (check e1 e's)
  | ETensor (s, e1, l, e2) ->
      ETensor
        ( s,
          transform_ast e1 check,
          List.map
            (fun (s, e1, e2) ->
              (s, transform_ast e1 check, transform_ast e2 check))
            l,
          transform_ast e2 check )
  | EFor (s, e1, l, e2) ->
      EFor
        ( s,
          transform_ast e1 check,
          List.map
            (fun (s, e1, e2) ->
              (s, transform_ast e1 check, transform_ast e2 check))
            l,
          transform_ast e2 check )

(** [check_elementary core] is [()] if any of its subexpressions contains
  an expression of the form [EOP (op , exp)] where [op] is either [Plus],[Times],[Sqrt],
   or [Divide]. Raises [ElementaryOperation] otherwise.  *)
let check_elementary (core : fpcore) =
  let rec check_elem_helper (e : expr) =
    match e with
    | EFloat _ | EInt _ | ESymbol _ | EConstant _ -> false
    | EIf (e1, e2, e3) ->
        check_elem_helper e1 || check_elem_helper e2 || check_elem_helper e3
    | ELet (lst, exp) ->
        List.exists (fun (_, expr) -> check_elem_helper expr) lst
        || check_elem_helper exp
    | EApp (e1, e2) -> check_elem_helper e1 || List.exists check_elem_helper e2
    | EBang (_, expr) -> check_elem_helper expr
    | EArray lst -> List.exists check_elem_helper lst
    | EOP (op, lst) ->
        (match op with
        | Plus | Times | Sqrt | Divide -> true
        | Minus | Equals | GreaterThan | Cast -> false)
        || List.exists check_elem_helper lst
    | ERef (expr, _) -> check_elem_helper expr
    | ETensor (_, e1, lst, e2) | EFor (_, e1, lst, e2) ->
        check_elem_helper e1
        || List.exists
             (fun (_, e1, e2) -> check_elem_helper e1 || check_elem_helper e2)
             lst
        || check_elem_helper e2
  in
  let body = match core with FPCore (_, _, _, b) -> b in
  if check_elem_helper body then
    raise (ElementaryOperation "FPCore uses elementary operation")

(** [get_core_name (FPCore (s,_,_,_))] is [s].*)
let get_core_name = function FPCore (s, _, _, _) -> s

(** [elem_names s] is [true] if [s] is [addfp,mulfp,sqrt], or [divfp]. 
   [false] otherwise. *)
let elem_names (s : string) =
  match s with "addfp" | "mulfp" | "sqrtfp" | "divfp" -> true | _ -> false

(** [check_prog prog] applies [check_elementary] to every [FPCore] in prog
whose name is [None] or not one of the floating point operations in [elem_names].

Hence, if any of the elements in [prog] contains an elementary operation and is
not a floating point operation, this function raises an exception. *)
let rec check_prog (prog : program) =
  match prog with
  | [] -> ()
  | core :: t ->
      let name = get_core_name core in
      if name = None || not (elem_names (Option.get name)) then (
        check_elementary core;
        check_prog t)
      else check_prog t

(** [handle_flag prog flag] is [prog] but converted into a program
whose calls to imported functions have been inlined if [flag] is either [NaiveInline]
or [SmartInline]. *)
let handle_flag prog flag =
  match flag with
  | Default -> List.map (add_prop [ Prec Real ]) prog
  | SmartInline ->
      let last = get_last prog in
      let new_core =
        [
          add_prop [ Prec Real ]
            (transform_body (fun x -> transform_ast x check_app) last);
        ]
      in
      [ inline (remove_last prog @ new_core) ]
  | NaiveInline -> [ inline prog ]
  | Decimal ->
      check_prog prog;
      let new_prog =
        List.map (transform_body (fun x -> transform_ast x check_app_elem)) prog
      in
      [ inline new_prog |> add_prop rnd_and_prec ]

(** [export_prog] takes a NumFuzz [prog], converts it into FPCore with inlining/smart 
substituion as dictated by [flag], and prints the resulting FPCore program to [outfile]*)
let export_prog (prog : term) (outfile : string) (flag : translate_flag) : unit
    =
  let oc = open_out outfile in
  let translated = translate prog in
  let transformed = handle_flag translated flag in
  let data = string_of_program transformed in
  Printf.fprintf oc "%s\n" data;
  close_out oc
