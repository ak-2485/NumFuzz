open Support.FileInfo
open Syntax

type symbol = string

type fpcore = FPCore of (symbol option * argument list * property list * expr)
and dimension = int
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | ENum of float
  | ESymbol of symbol
  | EOP of (fpop * expr)
  | EIf of (expr * expr * expr)
  | ELet of (symbol * expr) list * expr
  | EArray of expr list
  | ERef of expr * dimension list
  | EConstant of constant
  | EApp of expr * expr

and fpop = Plus | Times | Divide | Sqrt | Equals | GreaterThan | Round
and constant = True | False
and property

type program = fpcore list

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
  | TmAbs (inf, bind, typ, t) ->
      let arg_list, body = get_arguments prog in
      [ FPCore (None, arg_list, [], translate_expr body) ]
  | TmLet (inf, bind, typ, t1, t2) -> (
      match t1 with
      | TmAbs _ ->
          let arg_list, body = get_arguments prog in
          FPCore (Some bind.b_name, arg_list, [], translate_expr body)
          :: translate t2
      | _ -> [ FPCore (None, [], [], translate_expr prog) ])
  | _ -> [ FPCore (None, [], [], translate_expr prog) ]

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
      | PrimTFun (str, ty) ->
          failwith "Reached unreachable PrimTFun clause."
          (* Check with Ariel ^ *))
  | TmRnd (_, t) -> EOP (Round, translate_expr t)
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
  | TmOp (_, op, t) -> EOP (translate_op op, translate_expr t)

let rec string_of_dim_list (ds : dimension list) : string =
  match ds with
  | [] -> ""
  | h :: t -> (" " ^ string_of_int h) ^ string_of_dim_list t

let rec string_of_args (args : argument list) : string =
  match args with
  | [] -> ""
  | arg :: tl ->
      " "
      ^ (match arg with
        | ASymbol x -> "(" ^ x ^ ")"
        | Array (x, ds) -> "(" ^ x ^ string_of_dim_list ds ^ ")")
      ^ string_of_args tl

let export_fpcore (prog : fpcore) : string =
  match prog with
  | FPCore (name, args, props, e) -> "(FPCore" ^ "name" ^ string_of_args args
