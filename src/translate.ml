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
  | TmAbs _ ->
      let arg_list, body = get_arguments prog in
      [ FPCore (None, arg_list, [], translate_expr body) ]
  | TmLet (_, bind, _, t1, t2) -> (
      match t1 with
      | TmAbs _ ->
          let arg_list, body = get_arguments t1 in
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
      | PrimTFun _ ->
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
      " "
      ^ (match arg with
        | ASymbol x -> x
        | Array (x, ds) -> "(" ^ x ^ string_of_dim_list ds ^ ")")
      ^ string_of_args tl

let string_of_op (op : fpop) : string =
  match op with
  | Plus -> "+"
  | Times -> "*"
  | Divide -> "/"
  | Sqrt -> "sqrt"
  | Equals -> "=="
  | GreaterThan -> ">"
  | Round -> "round"

let rec string_of_expr (e : expr) : string =
  match e with
  | ENum n -> string_of_float n
  | ESymbol str -> str
  | EOP (op, e) -> "(" ^ string_of_op op ^ " " ^ string_of_expr e ^ ")"
  | EIf (e1, e2, e3) ->
      "( if " ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ " "
      ^ string_of_expr e3 ^ " )"
  | ELet (args, e) ->
      "(( " ^ string_of_let_args args ^ " ) " ^ string_of_expr e ^ " )"
  | EArray vals ->
      "( array "
      ^ List.fold_left (fun acc a -> acc ^ " " ^ string_of_expr a) "" vals
      ^ " )"
  | ERef (e, ds) -> "( ref " ^ string_of_expr e ^ string_of_dim_list ds ^ " )"
  | EConstant c -> ( match c with True -> "TRUE" | False -> "FALSE")
  | EApp (e1, e2) -> "( " ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ " )"

and string_of_let_args (args : (symbol * expr) list) : string =
  match args with
  | (s, e) :: tl ->
      "[ " ^ s ^ " " ^ string_of_expr e ^ " ]" ^ string_of_let_args tl
  | [] -> ""

let string_of_fpcore (prog : fpcore) : string =
  match prog with
  | FPCore (name, args, _, e) ->
      "(FPCore " ^ string_of_name name ^ "(" ^ string_of_args args ^ ")\n"
      ^ string_of_expr e ^ ")"

let rec string_of_program (prog : fpcore list) : string =
  match prog with
  | h :: tl -> string_of_fpcore h ^ "\n\n" ^ string_of_program tl
  | [] -> ""

let export_prog (prog : term) (outfile : string) : unit =
  let oc = open_out outfile in
  let data = prog |> translate |> string_of_program in
  Printf.fprintf oc "%s\n" data;
  close_out oc
