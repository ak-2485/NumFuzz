open Syntax
open Support.FileInfo
open Translate_ast
open Translate_inline

(** [string_of prec prec] is the string representation of precision [prec].*)
let string_of_prec = function
  | Binary64 -> "binary64"
  | Binary32 -> "binary32"
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
  | "addfp" -> Some Plus
  | "divfp" -> Some Divide
  | "mulfp" -> Some Times
  | "sqrtfp" -> Some Sqrt
  | _ -> None

(** [rnd_and_prec] is the list containing the FPCore properties for floating point
operations translated from NumFuzz, which are assumed to occur with binary 64 precision
and round towards positive infinity. *)
let rnd_and_prec = [ Prec Binary64; PRound ]

let get_name (inf : info) =
  match inf with FI (sym, _, _) -> sym | UNKNOWN -> ""

(** [translate_op] translates a numfuzz operation [op] to its FPcore equivalent. *)
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

(** [string_of_op op] is the string representation of an FPCore operation [op]. *)
let string_of_op (op : fpop) : string =
  match op with
  | Plus -> "+"
  | Times -> "*"
  | Divide -> "/"
  | Sqrt -> "sqrt"
  | Equals -> "=="
  | GreaterThan -> ">"
  | Cast -> "cast"

(** [check_app e1 e2] returns an non-empty [Option] value if [e1] is 
  a variable name that represents one of the basic floating operations (addfp,sqrtfp,divfp,mulfp)
and returns an equivalent FPCore term that does not call these functions. If [e1] is anything
else, this function returns [None].*)
let check_app e1 e2 =
  match e1 with
  | ESymbol s -> (
      let op = op_names s in
      if op = None then None
      else
        match Option.get op with
        | Sqrt -> Some (EBang (rnd_and_prec, EOP (Sqrt, [ e2 ])))
        | _ ->
            let arr_list =
              match e2 with EArray l -> l | _ -> failwith "error in checkapp"
            in
            let el1, el2 = (List.nth arr_list 0, List.nth arr_list 1) in
            Some (EBang (rnd_and_prec, EOP (Option.get op, [ el1; el2 ]))))
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

(** [add_prop prop_lst core] appends [prop_lst] to the list of properties in
[core]. *)
let add_prop prop_lst = function
  | FPCore (s, arg_lst, p_lst, expr) ->
      FPCore (s, arg_lst, prop_lst @ p_lst, expr)

(** [transform_ast expr] is [expr] but with every instance one of the floating operations 
  (addfp, sqrtfp, mulfp, divfp)
  inlined away with an equivalent operation. 
  For instance, [mulfp (x,y)] is translated as a multiplying x and y in FPCore but in a
  context with binary 64 precision and rounding towards positive infinity. *)
let rec transform_ast expr =
  match expr with
  | ENum _ -> expr
  | ESymbol _ -> expr
  | EOP (fpop, e_lst) -> EOP (fpop, List.map transform_ast e_lst)
  | EIf (e1, e2, e3) ->
      EIf (transform_ast e1, transform_ast e2, transform_ast e3)
  | ELet (lst, e) ->
      ELet
        ( List.map (fun (symbol, exp) -> (symbol, transform_ast exp)) lst,
          transform_ast e )
  | EArray lst -> EArray (List.map transform_ast lst)
  | ERef (e, lst) -> ERef (transform_ast e, lst)
  | EConstant _ -> expr
  | EBang (p_list, e) -> EBang (p_list, transform_ast e)
  | EApp (e1, e2) ->
      Option.default
        (EApp (transform_ast e1, transform_ast e2))
        (check_app e1 e2)

(** [handle_flag prog flag] is [prog] but converted into a program
whose calls to imported functions have been inlined if [flag] is either [NaiveInline]
or [SmartInline]. *)
let handle_flag prog flag =
  match flag with
  | Default -> List.map (add_prop [ Prec Real ]) prog
  | SmartInline ->
      let length = List.length prog in
      print_int length;
      let last = get_last prog in
      let s, arg_list, p_list, body = match last with FPCore s -> s in
      let body = transform_ast body in
      let new_core =
        [ add_prop [ Prec Real ] (FPCore (s, arg_list, p_list, body)) ]
      in
      [ inline (remove_last prog @ new_core) ]
  | NaiveInline -> [ inline prog ]

let export_prog (prog : term) (outfile : string) (flag : translate_flag) : unit
    =
  let oc = open_out outfile in
  let translated = translate prog in
  let transformed = handle_flag translated flag in
  let data = string_of_program transformed in
  Printf.fprintf oc "%s\n" data;
  close_out oc
