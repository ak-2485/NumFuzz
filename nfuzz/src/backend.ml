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
  | SqrtOp       -> "sqrt"
  | DivOp        -> "div"


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

    | TmInl (_i, e1) -> fprintf ppf "inl %a" gen_term e1
    | TmInr (_i, e1) -> fprintf ppf "inr %a" gen_term e1
    | TmUnionCase (_, tm_e, bi_l, tm_l, bi_r, tm_r) ->
      fprintf ppf "(match %a with @[<v>| Left %s -> %a @,| Right %s -> %a@])"
        gen_term tm_e (ml_b bi_l) gen_term tm_l (ml_b bi_r) gen_term tm_r

    (* Regular stuff for fuzz *)
    | TmPrim (_, prim) -> fprintf ppf "%s" (gen_primitive prim)

    (* Round *)
    | TmRnd (_, v) -> fprintf ppf "rnd(%a)" gen_term v

    | TmApp (_, f, e)  ->
      (* Some (hacky) optimizations for the OCaml translation *)
      begin
        match f with
        (* Binary operations *)
        | TmApp (_, TmVar(_, v), e') ->
          begin
            match v.v_name with
            | "op_add"      -> fprintf ppf "(%a +. %a)" gen_term e' gen_term e
            | "op_sub"      -> fprintf ppf "(%a -. %a)" gen_term e' gen_term e
            | "op_mul"      -> fprintf ppf "(%a *. %a)" gen_term e' gen_term e
            | "op_div"      -> fprintf ppf "(%a /. %a)" gen_term e' gen_term e
            | "string_concat" -> fprintf ppf "(%a ^ %a)" gen_term e' gen_term e
            | _             -> fprintf ppf "(@[%a@ %a@])" gen_term f gen_term e
          end
        | _ -> fprintf ppf "(@[%a@ %a@])" gen_term f gen_term e
      end
    | TmAbs (_, b, _sty, body) ->
      fprintf ppf "(fun %s ->@\n @[%a@])" (ml_b b) gen_term body

    | TmAmpersand (_i, e1, e2) -> fprintf ppf "(%a,%a)" gen_term e1 gen_term e2
    | TmAmp1 (_i, e1) -> fprintf ppf "Proj1 %a" gen_term e1
    | TmAmp2 (_i, e1) -> fprintf ppf "Proj2 %a" gen_term e1

    | TmBox(_i, _s1, e1) -> fprintf ppf "\n[%a@\n]" gen_term e1
    | TmBoxDest (_i, b_x, tm_e1, tm_e2) ->
      fprintf ppf "(let \n[%s\n] =  %a in@\n@[%a@])"
        (ml_b b_x) gen_term tm_e1 gen_term tm_e2


    (* let bi = e1 in e2 *)
    | TmLet (_, bi, _sty, e1, e2) -> fprintf ppf "(let %s = %a in@\n%a)" (ml_b bi) gen_term e1 gen_term e2

    | TmLetBind (_, bi, e1, e2) -> fprintf ppf "(letM %s = %a in@\n%a)" (ml_b bi) gen_term e1 gen_term e2

    | TmOp (_, op, e1) -> fprintf ppf "(%s %a)" (gen_op op) gen_term e1



let gen_program ppf t =
  fprintf ppf "%s@\n" header;
  gen_term ppf t;
  fprintf ppf "%s@." body
