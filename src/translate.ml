open Support.FileInfo
open Syntax

type symbol = string

type fpcore = FPCore of (symbol * argument list * property list * expr)
and dimension = int
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | ENum of float
  | ESymbol of symbol
  | EOP of (op * expr)
  | EIf of (expr * expr * expr)
  | ELet of (symbol * expr) list * expr
  | EArray of expr list

and op = Plus | Times | Divide | Sqrt | Equals | LessThan
and property

type program = fpcore list

let get_name (inf : info) =
  match inf with FI (sym, _, _) -> sym | UNKNOWN -> ""

let rec translate (prog : term) : program =
  match prog with
  | TmAbs (inf, bind, typ, t) ->
      let arg_list, body = get_arguments prog in
      [ FPCore ("", arg_list, [], translate_body body) ]
  | TmLet (inf, bind, typ, t1, t2) -> (
      match t1 with
      | TmAbs _ ->
          let arg_list, body = get_arguments prog in
          FPCore ("", arg_list, [], translate_body body) :: translate t2
      | _ ->
          failwith
            "FPCore does not support expressions outside of function bodies"
          (* ([ translate_function (get_name inf) bind.b_name typ t1 ] @ translate t2) *)
      )
  | _ ->
      failwith "FPCore does not support expressions outside of function bodies"

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

and translate_body (body : term) : expr = ENum 0.0
