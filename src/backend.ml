(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Format

open Syntax

(*
  We compile to native Caml code
*)

let header =
"open Prim
open Bag
open Db_sources
open Init

let query =
"

let body =
"
let () = exit (fuzz_main query)"

let paren s = "(" ^ s ^ ")"

let gen_primitive prim =
  match prim with
    PrimTUnit        -> "()"
  | PrimTNum(r)      -> string_of_float  r
  | PrimTString(s)   -> "\"" ^ s ^ "\""
  | PrimTFun(f, _ty) -> f

let gen_op op =
  match op with
    AddOp        -> "add"
  | MulOp        -> "mul"
  | DivOp        -> "div"
  | SubOp        -> "sub"

(* Avoid clashes with ML names *)
let ml_n n = "_" ^ n
let ml_b b = "_" ^ b.b_name

let rec gen_term ppf t =
  match t with
      (* TODO: not very happy using debug information for this, generate
         names from the index in the future *)
      TmVar (_, v)  ->
        begin
          match v.v_name with
          | "p_inl"      -> fprintf ppf "Left"
          | "p_inr"      -> fprintf ppf "Right"
          | _           ->  fprintf ppf "%s" (ml_n v.v_name)
        end
    (* Will be represented as applications soon *)
    | TmTens (_,  e1, e2) ->
      fprintf ppf "(%a, %a)" gen_term e1 gen_term e2

    | TmTensDest (_,  b_x, b_y, tm_e1, tm_e2) ->
      fprintf ppf "(let (%s,%s) =  %a in@\n@[%a@])"
        (ml_b b_x) (ml_b b_y) gen_term tm_e1 gen_term tm_e2

    (* Regular stuff for fuzz *)
    | TmPrim (_, prim) -> fprintf ppf "%s" (gen_primitive prim)

    (* let bi = e1 in e2 *)
    | TmLet (_, bi, _sty, e1, e2) -> fprintf ppf "(let %s = %a in@\n%a)" (ml_b bi) gen_term e1 gen_term e2

    | TmAdd (_, x, y) -> fprintf ppf "(Add %s %s)"  (ml_n x.v_name) (ml_n y.v_name)



let gen_program ppf t =
  fprintf ppf "%s@\n" header;
  gen_term ppf t;
  fprintf ppf "%s@." body
