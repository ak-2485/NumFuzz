(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Pretty printing module. Currently it uses the standard Format facility. *)

open Format

open Ctx
open Constr
open Syntax

open Support.Options

(**********************************************************************)
(* Unicode handling *)

module Symbols = struct
  type pp_symbols =
      Inf
    | Forall
    | Exists
    | Arrow
    | DblArrow
    | Lollipop
    | Tensor
    | Union
    | Bang
    | Nat
    | Int
    | Num
    | Mu
    | Lambda
    | BigLambda
    | Fuzzy
    | SubTau
    | Vdash
    | Geq
    | Lub

  let pp_symbol_table s = match s with
      Inf      -> ("inf",     "∞")
    | Forall   -> ("forall ", "∀")
    | Exists   -> ("exits " , "∃")
    | Arrow    -> ("->",      "→")
    | DblArrow -> ("=>",      "⇒")
    | Lollipop -> ("-o",      "⊸")
    | Tensor   -> ("x",       "⊗")
    | Union    -> ("+",       "⊕")
    | Bang     -> ("!",       "!")
    | Nat      -> ("nat",     "ℕ")
    | Int      -> ("int",     "ℤ")
    | Num      -> ("num",     "ℝ")
    | Mu       -> ("mu",      "μ")
    | Lambda   -> ("\\",      "λ")
    | BigLambda   -> ("\\!",  "Λ")
    | Fuzzy    -> ("circle",  "◯")
    | SubTau   -> ("_t",      "ₜ")
    | Vdash    -> ("|-",      "⊢")
    | Geq      -> (">=",      "≥")
    | Lub      -> ("U",       "⊔")

  let string_of_symbol s =
    let select = if !debug_options.unicode then snd else fst in
    select (pp_symbol_table s)
end

let u_sym x = Symbols.string_of_symbol x

(**********************************************************************)
(* Helper functions for pretty printing *)

let rec pp_list pp fmt l = match l with
    []         -> fprintf fmt ""
  | csx :: []  -> fprintf fmt "%a" pp csx
  | csx :: csl -> fprintf fmt "%a,@ %a" pp csx (pp_list pp) csl

(* Not worth it, we usually need very custom printers in option cases *)

(* let pp_option pp fmt o = match o with *)
(*   | None   -> fprintf fmt "" *)
(*   | Some v -> fprintf fmt "%a" pp v *)

(* Study this concept *)

(* Limit a formatter: (limit_boxes 3 pp_type) *)
let limit_boxes ?(n=(!debug_options).pr_level) pp = fun fmt ->
  let mb      = Format.pp_get_max_boxes fmt () in
  let con fmt = Format.pp_set_max_boxes fmt mb in
  (* print_string "eo"; print_string (string_of_int n); print_newline (); *)
  Format.pp_set_max_boxes fmt n;
  kfprintf con fmt "%a" pp

(**********************************************************************)
(* Pretty printing for variables *)

let pp_name fmt (bt, n) =
  let pf = fprintf fmt in
  match bt with
    BiVar    -> pf  "%s" n

let pp_vinfo fmt v =
  let vi = (v.v_type, v.v_name) in
  match !debug_options.var_output with
      PrVarName  -> fprintf fmt "%a" pp_name vi
    | PrVarIndex -> fprintf fmt "[%d/%d]" v.v_index v.v_size
    | PrVarBoth  -> fprintf fmt "%a:[%d/%d]" pp_name vi v.v_index v.v_size

let pp_binfo fmt b = pp_name fmt (b.b_type, b.b_name)

(* Kinds *)
let pp_kind fmt _k = fprintf fmt "%s" (u_sym Symbols.Num)

(**********************************************************************)
(* Pretty printing for sensitivities *)

let rec pp_si fmt s =
  match s with
  | SiConst flt            -> fprintf fmt "%s" (M.get_formatted_str flt)
  | SiVar   v              -> pp_vinfo fmt v
  | SiAdd (si1, si2)       -> fprintf fmt "(%a + %a)" pp_si si1 pp_si si2
  | SiMult(si1, si2)       -> fprintf fmt "(%a * %a)" pp_si si1 pp_si si2
  | SiDiv(si1, si2)        -> fprintf fmt "(%a / %a)" pp_si si1 pp_si si2
  | SiInfty                -> fprintf fmt "%s" (u_sym Symbols.Inf)
  | SiLub  (s1, s2)        -> fprintf fmt "(%a @<1>%s %a)" pp_si s1 (u_sym Symbols.Lub) pp_si s2


let pp_si_op fmt o =
  match o with
  | None    -> fprintf fmt "?"
  | Some si -> pp_si fmt si

(**********************************************************************)
(* Pretty printing for constraints *)

let pp_tyvar_ctx_elem ppf (v, k) =
  if !debug_options.full_context then
    fprintf ppf "%a :%s %a" pp_vinfo v (u_sym Symbols.SubTau) pp_kind k
  else
    fprintf ppf "%a" pp_vinfo v

let pp_tyvar_ctx = pp_list pp_tyvar_ctx_elem

let pp_si_eq fmt cs_eq = match cs_eq with
  | SiEq (si1, si2) -> fprintf fmt "@[%a@] = @[%a@]" pp_si si1 pp_si si2

let pp_ctx_eq =
  pp_list pp_si_eq

let pp_cs fmt cs =
  fprintf fmt "@[<h>%a@] | @[<h>%a@] %s @[%a@] %s @[%a@]" pp_tyvar_ctx cs.c_kind_ctx
    pp_ctx_eq cs.c_cs (u_sym Symbols.Vdash)
    pp_si cs.c_upper (u_sym Symbols.Geq) pp_si cs.c_lower

(**********************************************************************)
(* Pretty printing for types *)

(* Primitive types *)
let pp_primtype fmt ty = match ty with
    PrimNum     -> fprintf fmt "@<1>%s" (u_sym Symbols.Num)
  | PrimUnit    -> fprintf fmt "()"
  | PrimString  -> fprintf fmt "string"

(* Helper for our sensitivity annotated arrows *)
let pp_arrow fmt s = match s with
    SiConst a        ->
      if a = (M.make_from_float 1.0) then
        fprintf fmt "@<1>%s" (u_sym Symbols.Lollipop)
      else
        fprintf fmt "@<1>%s[%a]" (u_sym Symbols.Lollipop) pp_si s
  | SiInfty          -> fprintf fmt "@<1>%s" (u_sym Symbols.Arrow)
  | si               -> fprintf fmt "@<1>%s[%a]" (u_sym Symbols.Lollipop) pp_si si

(* Main printer *)
let rec pp_type ppf ty = match ty with
  | TyVar v                  -> pp_vinfo ppf v
  | TyPrim tp                -> fprintf ppf "%a" pp_primtype tp
  (* ADT *)
  | TyUnion(ty1, ty2)       -> fprintf ppf "(%a @<1>%s @[<h>%a@])" pp_type ty1 (u_sym Symbols.Union)  pp_type ty2
  | TyTensor(ty1, ty2)      -> fprintf ppf "(%a @<1>%s @[<h>%a@])" pp_type ty1 (u_sym Symbols.Tensor) pp_type ty2
  | TyAmpersand(ty1, ty2)   -> fprintf ppf "(%a & @[<h>%a@])" pp_type ty1 pp_type ty2
  (* Funs *)
  | TyLollipop(ty1, ty2) -> fprintf ppf "(@[<hov>%a %a@ %a@])" pp_type ty1 pp_arrow (SiConst (M.make_from_float 1.0)) pp_type ty2
  | TyMonad(si,ty1) -> fprintf ppf "(M[%a] @[<h>%a@])" pp_si si pp_type ty1
  | TyBang(si,ty1) -> fprintf ppf "(@<1>%s[%a] @[<h>%a@])" (u_sym Symbols.Bang)  pp_si si pp_type ty1

let pp_type_list = pp_list pp_type

(**********************************************************************)
(* Pretty printing for contexts *)

(* let pp_bi_ctx ppf ( *)

let pp_var_ctx_elem ppf (v, ty) =
  if !debug_options.full_context then
    fprintf ppf "%a : @[%a@]" pp_vinfo v pp_type ty
  else
    fprintf ppf "%a" pp_vinfo v

let pp_var_ctx   = pp_list pp_var_ctx_elem

(* Primitives to drop *)
let n_prim = 37

let rec ldrop n l = if n = 0 then l else ldrop (n-1) (List.tl l)

let pp_context ppf ctx =
  fprintf ppf "Type Context: [@[<v>%a@]]@\nTerm Context: [@[<v>%a@]@]"
    pp_tyvar_ctx (List.rev ctx.tyvar_ctx)
    pp_var_ctx   (ldrop n_prim (List.rev ctx.var_ctx))

(**********************************************************************)
(* Pretty printing for terms *)

(* This will be useful in the future *)

(* Operators *)
let binary_op_table =
  [("op_lor", "||");
   ("op_land", "&&");
   ("op_eq",   "==");
   ("op_neq",  "!=");
   ("op_lt",   "<");
   ("op_gt",   ">");
   ("op_lte",  "<=");
   ("op_gte",  ">=");
   ("op_add",  "+");
   ("op_sub",  "-");
   ("op_mul",  "*");
   ("op_div",  "/")]

let is_binary_op s = List.mem_assoc s binary_op_table

let string_of_op s = List.assoc s binary_op_table

let string_of_op2 fop = match fop with
  AddOp -> "add"
  | MulOp -> "mul"

let string_of_term_prim t = match t with
    PrimTUnit         -> "()"
  | PrimTNum f        -> string_of_float f
  | PrimTString s     -> ("\"" ^ s ^ "\"")
  | PrimTFun(s, _)    -> ("primitive " ^ s)

let pp_colon ppf s = match s with
    SiConst a        ->
      if a = (M.make_from_float 1.0) then
        fprintf ppf " :[]"
      else
        fprintf ppf " :[%a]" pp_si s
  | SiInfty          -> fprintf ppf " :"
  | si               -> fprintf ppf " :[%a]" pp_si si

let pp_maybe_si_type ppf osity =
  if !debug_options.pr_ann then
    match osity with
    | None         -> fprintf ppf ""
    | Some(si, ty) -> fprintf ppf "%a %a" pp_colon si pp_type ty
  else
    fprintf ppf ""

let pp_si_type ppf (si, ty) =
  if !debug_options.pr_ann then
    fprintf ppf "%a %a" pp_colon si pp_type ty
  else
    fprintf ppf ""

let pp_maybe_type ppf oty =
  if !debug_options.pr_ann then
    match oty with
      None    -> fprintf ppf ""
    | Some ty -> fprintf ppf ": %a" pp_type ty
  else
    fprintf ppf ""

(* let open_box n =  *)
(*   print_string  *)
(* Term pretty printing *)
let rec pp_term ppf t =
  match t with
    TmVar(_, v)             -> fprintf ppf "%a" pp_vinfo v
  (* Primitive terms *)
  | TmPrim(_, pt)           -> fprintf ppf "%s" (string_of_term_prim pt)

  (* Rounding *)
  | TmRnd(_, tm1)           -> fprintf ppf "rnd(%a)" pp_term tm1

  (* Tensor and & *)
  | TmTens(_, tm1, tm2)     -> fprintf ppf "(@[%a@], @[%a@])" pp_term tm1 pp_term tm2
  | TmTensDest(_, x, y, tm, term) -> fprintf ppf "@[<v>let (%a,%a) : = @[%a@];@,@[%a@]@]" pp_binfo x pp_binfo y pp_term tm pp_term term

  | TmAmpersand(_, tm1, tm2)          -> fprintf ppf "(<@[%a@], @[%a@]>)" pp_term tm1 pp_term tm2
  | TmAmp1(_, tm1)      -> fprintf ppf "Proj1 @[%a@]" pp_term tm1
  | TmAmp2(_, tm1)      -> fprintf ppf "Proj2 @[%a@]" pp_term tm1

  (* OP *)
  | TmOp(_, op, tm1)    -> fprintf ppf "%s(%a)" (string_of_op2 op) pp_term tm1

  (* Box *)
  | TmBox (_,_s,tm1)                -> fprintf ppf "\n[%a\n]" pp_term tm1
  | TmBoxDest (_, x, tm, term)      -> fprintf ppf "@[<v>let \n[%a\n] : = @[%a@];@,@[%a@]@]" pp_binfo x pp_term tm pp_term term


  (* Data type manipulation *)

  (* Regular Abstraction and Application *)
  | TmAbs(_, a_n, ty_a, tm) ->
    fprintf ppf "@<1>%s (%a%a) {@\n@[<hov 1> %a@]@\n}"
      (u_sym Symbols.Lambda) pp_binfo a_n pp_type ty_a pp_term tm

  | TmApp(_, tm1, tm2)         -> print_special_app ppf tm1 tm2

  | TmLet(_, n, _sty, tm1, tm2) ->
    fprintf ppf "@[<v>@[<hov>%a =@;<1 1>@[%a@]@];@,@[%a@]@]" pp_binfo n pp_term tm1 pp_term tm2

  | TmLetBind(_, x, tm1, tm2) ->
    fprintf ppf "@[<v>let %a : = @[%a@];@,@[%a@]@]" pp_binfo x pp_term tm1 pp_term tm2

  (* Case expressions *)
  | TmUnionCase(_, tm, ln, ltm, rn, rtm) ->
    (* Alternative using vertical boxes *)
    fprintf ppf "case @[%a@] of {@\n   inl(%a) @<1>%s @[%a@]@\n | inr(%a) @<1>%s @[%a@]@\n}"
      pp_term tm
      pp_binfo ln (u_sym Symbols.DblArrow) pp_term ltm
      pp_binfo rn (u_sym Symbols.DblArrow) pp_term rtm



(* We print some applications in an special way, note that this relies on debug information *)
and print_special_app ppf tm1 tm2 =
  let regular_print tm1 tm2 = fprintf ppf "(%a@;<1 1>@[<hov>%a@])" pp_term tm1 pp_term tm2 in
  match tm1 with
    (* Binary operations *)
    TmApp(_, TmVar(_, v), op1) ->
      if is_binary_op v.v_name then
        fprintf ppf "(@[%a@] %s@ @[%a@])" pp_term op1 (string_of_op v.v_name) pp_term tm2
      else if v.v_name = "tensor_pair" then
        fprintf ppf "(%a, %a)" pp_term op1 pp_term tm2
      else
        regular_print tm1 tm2
  | TmApp(_, TmApp(_, TmVar(_, v), cond), op1) ->
    if v.v_name = "if_then_else" then
      fprintf ppf "@[<v>if @[<hov>%a@] then@ @[<h>%a@]@,else@ @[%a@]@]" pp_term cond pp_term op1 pp_term tm2
    else
      regular_print tm1 tm2
  | _ -> regular_print tm1 tm2
