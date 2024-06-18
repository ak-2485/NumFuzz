open Support.FileInfo
open Syntax

type symbol = string
type precision = Binary64 | Binary32 | Real
type translate_flag = Default | NaiveInline | SmartInline

type fpcore = FPCore of (symbol option * argument list * property list * expr)
and dimension = int
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | ENum of float
  | ESymbol of symbol
  | EOP of (fpop * expr list)
  | EIf of (expr * expr * expr)
  | ELet of (symbol * expr) list * expr
  | EArray of expr list
  | ERef of expr * dimension list
  | EConstant of constant
  | EApp of expr * expr
  | EBang of property list * expr

and data = DSymbol of symbol | DNum of float
and fpop = Plus | Times | Divide | Sqrt | Equals | GreaterThan | Cast
and constant = True | False
and property = Prec of precision | PRound

type program = fpcore list

let string_of_prec = function
  | Binary64 -> "binary64"
  | Binary32 -> "binary32"
  | Real -> "real"

let string_of_prop_inline = function
  | Prec p -> ":precision " ^ string_of_prec p
  | PRound -> ":round toPositive"

let rec string_of_prop_core = function
  | [] -> ""
  | p :: t -> string_of_prop_inline p ^ "\n" ^ string_of_prop_core t

let rec string_of_prop_lst (lst : property list) =
  match lst with
  | [] -> ""
  (* extra space for the last element actually works*)
  | p :: t -> string_of_prop_inline p ^ " " ^ string_of_prop_lst t

let op_names (s : symbol) =
  match s with
  | "addfp" -> Some Plus
  | "divfp" -> Some Divide
  | "mulfp" -> Some Times
  | _ -> None

let rnd_and_prec = [ Prec Binary64; PRound ]

let get_name (inf : info) =
  match inf with FI (sym, _, _) -> sym | UNKNOWN -> ""

let translate_op (op : op) : fpop =
  match op with
  | AddOp -> Plus
  | MulOp -> Times
  | SqrtOp -> Sqrt
  | DivOp -> Divide
  | GtOp -> GreaterThan
  | EqOp -> Equals

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

(** if type is a pair, returns appropiate dimension *)
and arg_of_typ (typ : ty) =
  match typ with TyAmpersand _ | TyTensor _ -> Some 2 | _ -> None

(* Assumes that [prog] has outermost TmAbs *)
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

and translate_expr (body : term) : expr =
  match body with
  | TmVar (_, var_i) -> ESymbol var_i.v_name
  | TmTens (_, t1, t2) | TmAmpersand (_, t1, t2) ->
      EArray [ translate_expr t1; translate_expr t2 ]
  | TmTensDest (_, b_i1, b_i2, t1, t2) ->
      let tens = translate_expr t1 in
      ELet
        ( [
            (b_i1.b_name, ERef (tens, [ 0 ])); (b_i2.b_name, ERef (tens, [ 1 ]));
          ],
          translate_expr t2 )
  | TmInl (_, t) -> EArray [ EConstant True; translate_expr t ]
  | TmInr (_, t) -> EArray [ EConstant False; translate_expr t ]
  | TmUnionCase (_, t1, b_i2, t2, b_i3, t3) ->
      let v1 = ERef (translate_expr t1, [ 1 ]) in
      EIf
        ( ERef (translate_expr t1, [ 0 ]),
          ELet ([ (b_i2.b_name, v1) ], translate_expr t2),
          ELet ([ (b_i3.b_name, v1) ], translate_expr t3) )
  | TmPrim (_, tprim) -> (
      match tprim with
      | PrimTUnit -> ENum (-1.0)
      | PrimTNum n -> ENum n
      | PrimTString str -> ESymbol str
      | PrimTFun _ ->
          failwith "Reached unreachable PrimTFun clause."
          (* Check with Ariel ^ *))
  | TmRnd (_, t) -> EBang (rnd_and_prec, EOP (Cast, [ translate_expr t ]))
  | TmRet (_, t) -> translate_expr t
  | TmApp (_, t1, t2) -> EApp (translate_expr t1, translate_expr t2)
  | TmAbs _ -> failwith "FPCore does not support nested functions."
  | TmAmp1 (_, t) -> ERef (translate_expr t, [ 0 ])
  | TmAmp2 (_, t) -> ERef (translate_expr t, [ 1 ])
  | TmBox (_, _, t) -> translate_expr t
  | TmBoxDest (_, b_i, t1, t2)
  | TmLet (_, b_i, _, t1, t2)
  | TmLetBind (_, b_i, t1, t2) ->
      ELet ([ (b_i.b_name, translate_expr t1) ], translate_expr t2)
  | TmOp (_, op, t) -> translate_expr_op (translate_op op) (translate_expr t)

and translate_expr_op op (t : expr) =
  match op with
  | Plus | Times | Divide | Equals | GreaterThan ->
      EOP (op, [ ERef (t, [ 0 ]); ERef (t, [ 1 ]) ])
  | Sqrt | Cast -> EOP (op, [ t ])

let string_of_name (name : symbol option) : string =
  match name with Some s -> s ^ " " | None -> ""

let rec string_of_dim_list (ds : dimension list) : string =
  match ds with
  | [] -> ""
  | h :: t -> (" " ^ string_of_int h) ^ string_of_dim_list t

let rec string_of_args (args : argument list) : string =
  match args with
  | [] -> ""
  | arg :: tl ->
      (match arg with
      | ASymbol x -> x
      | Array (x, ds) -> "(" ^ x ^ string_of_dim_list ds ^ ")")
      ^ (if tl == [] then "" else " ")
      ^ string_of_args tl

let string_of_op (op : fpop) : string =
  match op with
  | Plus -> "+"
  | Times -> "*"
  | Divide -> "/"
  | Sqrt -> "sqrt"
  | Equals -> "=="
  | GreaterThan -> ">"
  | Cast -> "cast"

let check_app e1 e2 flag =
  if flag = Default then None
  else if flag = NaiveInline then None
  else
    match e1 with
    | ESymbol s ->
        let op = op_names s in
        if op = None then None
        else
          let arr_list =
            match e2 with EArray l -> l | _ -> failwith "error in check_app"
          in
          let el1, el2 = (List.nth arr_list 0, List.nth arr_list 1) in
          Some (EBang (rnd_and_prec, EOP (Option.get op, [ el1; el2 ])))
    | _ -> None

let rec string_of_expr (e : expr) : string =
  match e with
  | ENum n -> string_of_float n
  | ESymbol str -> str
  | EOP (op, e's) ->
      "(" ^ string_of_op op ^ " "
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
  | ERef (e, ds) -> "(ref " ^ string_of_expr e ^ string_of_dim_list ds ^ ")"
  | EConstant c -> ( match c with True -> "TRUE" | False -> "FALSE")
  | EApp (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | EBang (p_lst, e) ->
      "(! " ^ string_of_prop_lst p_lst ^ " " ^ string_of_expr e ^ ")"

and string_of_let_args (args : (symbol * expr) list) : string =
  match args with
  | (s, e) :: tl ->
      "[" ^ s ^ " " ^ string_of_expr e ^ "]" ^ string_of_let_args tl
  | [] -> ""

let string_of_fpcore (prog : fpcore) : string =
  match prog with
  | FPCore (name, args, p_lst, e) ->
      "(FPCore " ^ string_of_name name ^ "(" ^ string_of_args args ^ ")\n"
      ^ string_of_prop_core p_lst ^ string_of_expr e ^ ")"

let string_of_program (prog : fpcore list) =
  List.fold_left (fun acc x -> acc ^ string_of_fpcore x ^ "\n\n") "" prog

let rec get_last = function
  | _ :: _ :: t -> get_last t
  | h :: [] -> Some h
  | [] -> None

let add_prop prop_lst = function
  | FPCore (s, arg_lst, p_lst, expr) ->
      FPCore (s, arg_lst, prop_lst @ p_lst, expr)

(* Takes in a program (list of functions) and inlines all functions into the last function.
   If multiples functions are defined with the same name, the latest definition is used. *)

let rec unwind_app (e : expr) (args : expr list) : symbol * expr list =
  match e with
  | EApp (e1, e2) -> unwind_app e1 (e2 :: args)
  | ESymbol name -> (name, args)
  | _ -> failwith "Unable to parse function call while inlining"

let rec build_arg_sub_map (vars : argument list) (args : expr list)
    (map : (string * expr) list) : (string * expr) list =
  match (vars, args) with
  | vh :: vt, ah :: at ->
      let v_name = match vh with ASymbol s -> s | Array (s, _) -> s in
      build_arg_sub_map vt at ((v_name, ah) :: map)
  | [], [] -> map
  | _ ->
      failwith "Failed inline: function called with wrong number of arguments."

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

let substitute_args (func : fpcore) (args : expr list) : expr =
  match func with
  | FPCore (_, vars, _, body) ->
      let subst_map = build_arg_sub_map vars args [] in
      substitute_args_rec subst_map body

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

let rec transform_ast expr =
  match expr with
  | ENum _ -> expr
  | ESymbol _ -> expr
  | EOP (fpop, e_lst) -> EOP (fpop, List.map (fun x -> transform_ast x) e_lst)
  | EIf (e1, e2, e3) ->
      EIf (transform_ast e1, transform_ast e2, transform_ast e3)
  | ELet (lst, e) ->
      ELet
        ( List.map (fun (symbol, exp) -> (symbol, transform_ast exp)) lst,
          transform_ast e )
  | EArray lst -> EArray (List.map (fun x -> transform_ast x) lst)
  | ERef (e, lst) -> ERef (transform_ast e, lst)
  | EConstant _ -> expr
  | EBang (p_list, e) -> EBang (p_list, transform_ast e)
  | EApp (e1, e2) ->
      let e = check_app e1 e2 SmartInline in
      if e = None then EApp (transform_ast e1, transform_ast e2)
      else Option.get e

let handle_flag prog flag =
  match flag with
  | Default -> prog
  | SmartInline ->
      let last = Option.get (get_last prog) in
      let s, arg_list, p_list, body =
        match last with
        | FPCore (s, arg_list, p_list, b) -> (s, arg_list, p_list, b)
      in
      let body = transform_ast body in
      let new_core =
        [ add_prop [ Prec Real ] (FPCore (s, arg_list, p_list, body)) ]
      in
      [ inline new_core ]
  | NaiveInline -> [ inline prog ]

let export_prog (prog : term) (outfile : string) (flag : translate_flag) : unit
    =
  let oc = open_out outfile in
  let translated = translate prog in
  let transformed = handle_flag translated flag in
  let data = string_of_program transformed in
  Printf.fprintf oc "%s\n" data;
  close_out oc
