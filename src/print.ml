(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* ---------------------------------------------------------------------- *)
(* Pretty printing *)
(* ---------------------------------------------------------------------- *)

open Format

open Constr
open Syntax

open Support.Options

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
    | Eps

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
    | Eps      -> ("e",       "ε")

  let string_of_symbol s =
    let select = if !debug_options.unicode then snd else fst in
    select (pp_symbol_table s)
end

let u_sym x = Symbols.string_of_symbol x

(* Helper functions for pretty printing *)

let rec pp_list pp fmt l = match l with
    []         -> fprintf fmt ""
  | csx :: []  -> fprintf fmt "%a" pp csx
  | csx :: csl -> fprintf fmt "%a,@ %a" pp csx (pp_list pp) csl

(* Not worth it, we usually need very custom printers in option cases *)

(* Limit a formatter: (limit_boxes 3 pp_type) *)
let limit_boxes ?(n=(!debug_options).pr_level) pp = fun fmt ->
  let mb      = Format.pp_get_max_boxes fmt () in
  let con fmt = Format.pp_set_max_boxes fmt mb in
  Format.pp_set_max_boxes fmt n;
  kfprintf con fmt "%a" pp

(* Pretty printing for variables *)

let pp_name fmt n =
  let pf = fprintf fmt in pf  "%s" n

let pp_vinfo fmt v =
  let vi = v.v_name in
  match !debug_options.var_output with
      PrVarName  -> fprintf fmt "%a" pp_name vi
    | PrVarIndex -> fprintf fmt "[%d/%d]" v.v_index v.v_size
    | PrVarBoth  -> fprintf fmt "%a:[%d/%d]" pp_name vi v.v_index v.v_size

let pp_vinfo_ind fmt v =
  fprintf fmt "[%d]" v.v_index

let pp_binfo fmt b = pp_name fmt b.b_name

(* Pretty printing for sensitivities *)

let machine_eps = 2.**(-53.)
let rec pp_si fmt s =
  match s with
  | SiConst flt            -> fprintf fmt "[%s]" (string_of_float (flt))
  | SiAdd (si1, si2)       -> fprintf fmt "(%a + %a)" pp_si si1 pp_si si2
  | SiMult(si1, si2)       -> fprintf fmt "(%a * %a)" pp_si si1 pp_si si2
  | SiDiv(si1, si2)        -> fprintf fmt "(%a / %a)" pp_si si1 pp_si si2
  | SiInfty                -> fprintf fmt "%s" (u_sym Symbols.Inf)
  | SiLub  (s1, s2)        -> fprintf fmt "(%a @<1>%s %a)" pp_si s1 (u_sym Symbols.Lub) pp_si s2

let pp_si_op fmt o =
  match o with
  | None    -> fprintf fmt "?"
  | Some si -> pp_si fmt (Simpl.si_simpl_compute si)

(* Pretty printing for types *)

(* Primitive types *)
let pp_primtype fmt ty = match ty with
    PrimNum     -> fprintf fmt "@<1>%s" (u_sym Symbols.Num)
  | PrimDNum    -> fprintf fmt "@<1>d%s" (u_sym Symbols.Num)
  | PrimUnit    -> fprintf fmt "()"

(* Helper for our sensitivity annotated arrows *)
let pp_arrow fmt s = match s with
    SiConst a        ->
      if a = (1.0) then
        fprintf fmt "@<1>%s" (u_sym Symbols.Lollipop)
      else
        fprintf fmt "@<1>%s[%a]" (u_sym Symbols.Lollipop) pp_si s
  | SiInfty          -> fprintf fmt "@<1>%s" (u_sym Symbols.Arrow)
  | si               -> fprintf fmt "@<1>%s[%a]" (u_sym Symbols.Lollipop) pp_si si

(* Main printer *)
let rec pp_type ppf ty = match ty with
  | TyPrim tp                -> fprintf ppf "%a" pp_primtype tp
  | TyUnion(ty1, ty2)       -> fprintf ppf "(%a @<1>%s @[<h>%a@])" 
    pp_type ty1 (u_sym Symbols.Union)  pp_type ty2
  | TyTensor(ty1, ty2)      -> fprintf ppf "(%a @<1>%s @[<h>%a@])" 
    pp_type ty1 (u_sym Symbols.Tensor) pp_type ty2
 
let pp_type_list = pp_list pp_type

(* Pretty printing for contexts *)

let pp_index_sis ppf (i,si) = 
  fprintf ppf "(%d , @[%a@])" i pp_si si

let pp_var_ctx_ind_elem ppf (v, ty) =
  if !debug_options.full_context then
    fprintf ppf "%a : @[%a@]" pp_vinfo_ind v pp_type ty
  else
    fprintf ppf "%a" pp_vinfo v

let pp_var_ctx_ind   = pp_list pp_var_ctx_ind_elem

let pp_var_si_ctx_elem ppf ((v, ty), si) =
    fprintf ppf "%a :[%a] @[%a@]" pp_vinfo v pp_si si pp_type ty

let pp_var_si_ctx   = pp_list pp_var_si_ctx_elem

let pp_var_ctx_elem ppf (v, ty) =
  if !debug_options.full_context then
    fprintf ppf "%a : @[%a@]" pp_vinfo v pp_type ty
  else
    fprintf ppf "%a" pp_vinfo v

let pp_var_ctx_elem_si ppf ((v, ty), si) =
  if !debug_options.full_context then
    fprintf ppf "%a :%a @[%a@]" pp_vinfo v pp_si_op si pp_type ty
  else
    fprintf ppf "%a" pp_vinfo v

let pp_var_ctx  = pp_list pp_var_ctx_elem
let pp_var_ctx_si  = pp_list pp_var_ctx_elem_si

(* Primitives to drop *)
let n_prim = 37

let rec ldrop n l = if n = 0 then l else ldrop (n-1) (List.tl l)

(* Pretty printing for terms *)

(* Operators *)
let binary_op_table =
  [("op_lor",  "||");
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
    AddOp  -> "add"
  | MulOp  -> "mul"
  | DivOp  -> "div"
  | SubOp  -> "sub"

let string_of_term_prim t = match t with
    PrimTUnit         -> "()"
  | PrimTNum f        -> string_of_float f

let pp_colon ppf s = match s with
    SiConst a        ->
      (* if a = (M.make_from_float 1.0) then *)
      if a = ( 1.0) then
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

(* Term pretty printing *)
let rec pp_term ppf t =
  match t with
    TmVar(_, v)             -> fprintf ppf "%a" pp_vinfo v
  | TmDVar(_, v)            -> fprintf ppf "%a" pp_vinfo v
  | TmDisc(_, tm)            -> fprintf ppf "!%a" pp_term tm
  | TmPrim(_, pt)           -> fprintf ppf "%s" (string_of_term_prim pt)
  | TmTens(_, tm1, tm2)     -> fprintf ppf "(@[%a@], @[%a@])" pp_term tm1 pp_term tm2
  | TmTensDest(_, x, y, tm, term) -> fprintf ppf "@[<v>let (%a, %a) : = @[%a@];@,@[%a@]@]" 
    pp_binfo x pp_binfo y pp_term tm pp_term term
  | TmTensDDest(_, x, y, tm, term) -> fprintf ppf "@[<v>dlet (%a, %a) : = @[%a@];@,@[%a@]@]" 
    pp_binfo x pp_binfo y pp_term tm pp_term term

  (* Ops *)
  | TmAdd(_, x, y)    -> fprintf ppf "Add %a %a" pp_vinfo x pp_vinfo y
  | TmSub(_, x, y)    -> fprintf ppf "Sub %a %a" pp_vinfo x pp_vinfo y
  | TmMul(_, x, y)    -> fprintf ppf "Mul %a %a" pp_vinfo x pp_vinfo y
  | TmDiv(_, x, y)    -> fprintf ppf "Div %a %a" pp_vinfo x pp_vinfo y
  | TmDMul(_, x, y)    -> fprintf ppf "DMul %a %a" pp_vinfo x pp_vinfo y
  | TmLet(_, n, _sty, tm1, tm2) ->
    fprintf ppf "@[<v>@[<hov>%a =@;<1 1>@[%a@]@];@,@[%a@]@]" pp_binfo n pp_term tm1 pp_term tm2
  | TmDLet(_, n, _sty, tm1, tm2) ->
    fprintf ppf "@[<v>@[<hov>%a =@;<1 1>@[%a@]@];@,@[%a@]@]" pp_binfo n pp_term tm1 pp_term tm2
  (* Case *)
  | TmInl(_, ty, tm_l) -> fprintf ppf "inl @[%a@] @[%a@]" pp_type ty pp_term tm_l
  | TmInr(_, ty, tm_r) -> fprintf ppf "inr @[%a@] @[%a@]" pp_type ty pp_term tm_r
  | TmUnionCase(_, tm, ln, ltm, rn, rtm) ->
    fprintf ppf "case @[%a@] of {@\n   inl(%a) @<1>%s @[%a@]@\n | inr(%a) @<1>%s @[%a@]@\n}"
      pp_term tm
      pp_binfo ln (u_sym Symbols.DblArrow) pp_term ltm
      pp_binfo rn (u_sym Symbols.DblArrow) pp_term rtm


