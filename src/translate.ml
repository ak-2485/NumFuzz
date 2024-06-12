open Support.FileInfo
open Syntax

type symbol = string

type fpcore = FPCore of (symbol * argument list * property list * expr)
and dimension = Num of int | DSymbol of symbol
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | ENum of float
  | ESymbol of symbol
  | OP of (op * expr)
  | If of (expr * expr * expr)
  | Let of symbol * expr * expr
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

(* Assumes that [prog] has outermost TmAbs *)
and get_arguments (prog : term) : argument list * term =
  ([], TmPrim (UNKNOWN, PrimTUnit))

and translate_body (body : term) : expr = ENum 0.0
