(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Support.FileInfo

(*module M = Mlmpfr*)

(* ---------------------------------------------------------------------- *)
(* Abstract Syntax Tree for sensitivities, terms and types                *)
(* ---------------------------------------------------------------------- *)

(* Binders are represented using Debruijn notation *)

(* Different types of variable binding, for debug purposes *)
type fuzz_binding = BiVar (* Regular variable *)

(* To keep things simple, we include some meta-information about
   variables that in spirit should go in each particular case, for
   instance v_interesting make only sense for SiEvars ....
*)
type var_info = {
  (* Indexes start a 0 *)
  v_index : int;
  (* Debug fields *)
  v_name : string;
  v_size : int;
  v_type : fuzz_binding;
}

(* Default varinfo *)
let dvi = { v_index = -1; v_name = "deadbeef"; v_size = -1; v_type = BiVar }

(* The name is printed on screen, but it is ignored for all purposes
*)

(* Helper to modify the index *)
let var_shift o n v =
  {
    v with
    v_index = (if o <= v.v_index then v.v_index + n else v.v_index);
    v_size = v.v_size + n;
  }

(* All of the fields are debug information *)
type binder_info = {
  b_name : string;
  b_size : int; (* How many outside binders we had when this binder was found *)
  b_type : fuzz_binding;
  b_prim : bool;
}

(* Kinds for type variables *)
type kind = Sens

(* Part 1: Sizes and Sensitivities *)

(* Sensitivities *)
type si =
  | SiInfty
  | SiConst of float
  (*  | SiConst of M.mpfr_float*)
  | SiVar of var_info
  | SiAdd of si * si
  | SiMult of si * si
  | SiDiv of si * si
  | SiLub of si * si
(* We only allow to sup to happen over the first variable *)

(* Map over the variables of a sensitivity type *)
let rec si_map n f si =
  let smf = si_map n f in
  match si with
  | SiVar v -> f n v
  | SiConst c -> SiConst c
  | SiAdd (x, y) -> SiAdd (smf x, smf y)
  | SiMult (x, y) -> SiMult (smf x, smf y)
  | SiDiv (x, y) -> SiDiv (smf x, smf y)
  | SiInfty -> SiInfty
  | SiLub (s1, s2) -> SiLub (smf s1, smf s2)

(* Shifts all the variables greater or equal than o by n *)
let si_shift o n si =
  let f o v = SiVar (var_shift o n v) in
  si_map o f si

(* Substitution e[x/t], t depends on the number of binders traversed
   and correctly shifts t *)
let si_subst_shift x t n v =
  if x + n = v.v_index then si_shift 0 n t else SiVar (var_shift (x + n) (-1) v)

(* Substitution si[t/x] for sens vars *)
let si_subst x t si = si_map 0 (si_subst_shift x t) si

type si_cs = SiEq of (si * si)

let cs_shift n d cs =
  match cs with SiEq (s1, s2) -> SiEq (si_shift n d s1, si_shift n d s2)

(* Types *)

(* Primitive types *)
type ty_prim = PrimNum | PrimUnit | PrimString

(* Strings in the binders just for debug purposes *)
type ty =
  (* variables used in bindings *)
  | TyVar of var_info
  (* Primitive types *)
  | TyPrim of ty_prim
  (* ADT *)
  | TyUnion of ty * ty
  | TyTensor of ty * ty
  | TyAmpersand of ty * ty
  (* Functional type *)
  | TyLollipop of ty * ty
  (* Monadic type *)
  | TyMonad of si * ty
  (* Comonadic type *)
  | TyBang of si * ty

(* map over types, first argument: action on vars, second argument
   action on evars, third argument action on sensitivities, 4th on sizes *)
let rec ty_map n fv fsi ty =
  match ty with
  | TyVar v -> fv n v
  | TyPrim tp -> TyPrim tp
  (* ADT *)
  | TyUnion (ty1, ty2) -> TyUnion (ty_map n fv fsi ty1, ty_map n fv fsi ty2)
  | TyTensor (ty1, ty2) -> TyTensor (ty_map n fv fsi ty1, ty_map n fv fsi ty2)
  | TyAmpersand (ty1, ty2) ->
      TyAmpersand (ty_map n fv fsi ty1, ty_map n fv fsi ty2)
  (* *)
  | TyLollipop (ty1, ty2) ->
      TyLollipop (ty_map n fv fsi ty1, ty_map n fv fsi ty2)
  | TyMonad (si1, ty1) -> TyMonad (fsi n si1, ty_map n fv fsi ty1)
  | TyBang (si1, ty1) -> TyBang (fsi n si1, ty_map n fv fsi ty1)

let ty_shift o n ty =
  let fv k v = TyVar (var_shift k n v) in
  let fsi k si = si_shift k n si in
  ty_map o fv fsi ty

let ty_subst_shift x t k v =
  if x + k = v.v_index then ty_shift 0 k t else TyVar (var_shift (x + k) (-1) v)

(* Substitution ty[t/x] for type vars *)
let ty_subst x t ty =
  let f_si k si = si_shift (x + k) (-1) si in
  let f_ty = ty_subst_shift x t in
  ty_map 0 f_ty f_si ty

(* XXX: This used to be wrong, double-check again *)
(* Substitution ty[s/x] for sensitivity vars *)
let ty_si_subst x si ty =
  let f_si k = si_subst (x + k) (si_shift 0 k si) in
  let f_ty k v = TyVar (var_shift (x + k) (-1) v) in
  ty_map 0 f_ty f_si ty

(*********************************************************************)
(* Terms                                                             *)

(* Primitive Terms *)
type term_prim =
  | PrimTUnit
  | PrimTNum of float
  | PrimTString of string
  | PrimTFun of string * ty

let type_of_prim t =
  match t with
  | PrimTUnit -> TyPrim PrimUnit
  | PrimTNum _ -> TyPrim PrimNum
  | PrimTString _ -> TyPrim PrimString
  | PrimTFun (_, ty) -> ty

type op = AddOp | SubOp | MulOp | SqrtOp | DivOp | GtOp | EqOp
type op_core = AddOpCore | MulOpCore | SqrtOpCore | DivOpCore | GtOpCore | EqOpCore

type term =
  | TmVar of info * var_info
  (*  *)
  | TmTens of info * term * term
  | TmTensDest of info * binder_info * binder_info * term * term
  | TmInl of info * term
  | TmInr of info * term
  | TmUnionCase of info * term * binder_info * term * binder_info * term
  (*                      t  of { inl(x)     => tm1  | inl(y)     => tm2  } *)
  (* Primitive terms *)
  | TmPrim of info * term_prim
  (* Rounding *)
  | TmRnd64 of info * term
  | TmRnd32 of info * term
  | TmRnd16 of info * term
  (* Ret *)
  | TmRet of info * term
  (* Regular Abstraction and Applicacion *)
  | TmApp of info * term * term
  | TmAbs of info * binder_info * ty * term
  (* & constructor and eliminator *)
  | TmAmpersand of info * term * term
  | TmAmp1 of info * term
  | TmAmp2 of info * term
  (* Box constructor and elim *)
  | TmBox of info * si * term
  | TmBoxDest of info * binder_info * term * term
  (* Regular sequencing *)
  | TmLet of info * binder_info * ty option * term * term
  (* Monadic sequencing *)
  | TmLetBind of info * binder_info * term * term
  (* Basic ops *)
  | TmOp of info * op * term

type term_core =
  | TmVarCore of info * var_info
  (*  *)
  | TmTensCore of info * term_core * term_core
  | TmTensDestCore of info * binder_info * binder_info * term_core * term_core
  | TmInlCore of info * term_core
  | TmInrCore of info * term_core
  | TmUnionCaseCore of info * term_core * binder_info * term_core * binder_info * term_core
  (*                      t  of { inl(x)     => tm1  | inl(y)     => tm2  } *)
  (* Primitive core terms *)
  | TmPrimCore of info * term_prim
  (* Rounding *)
  | TmRnd64Core of info * term_core
  | TmRnd32Core of info * term_core
  | TmRnd16Core of info * term_core
  (* Ret *)
  | TmRetCore of info * term_core
  (* Regular Abstraction and Applicacion *)
  | TmAppCore of info * term_core * term_core
  | TmAbsCore of info * binder_info * ty * term_core
  (* & constructor and eliminator *)
  | TmAmpersandCore of info * term_core * term_core
  | TmAmp1Core of info * term_core
  | TmAmp2Core of info * term_core
  (* Box constructor and elim *)
  | TmBoxCore of info * si * term_core
  | TmBoxDestCore of info * binder_info * term_core * term_core
  (* Regular sequencing *)
  | TmLetCore of info * binder_info * ty option * term_core * term_core
  (* Monadic sequencing *)
  | TmLetBindCore of info * binder_info * term_core * term_core
  (* Basic ops *)
  | TmOpCore of info * op_core * term_core

let map_prim_ty n f p =
  match p with
  | PrimTUnit -> p
  | PrimTNum _ -> p
  | PrimTString _ -> p
  | PrimTFun (s, ty) -> PrimTFun (s, f n ty)

let rec map_term_ty_aux n ft fsi tm =
  let tf n = map_term_ty_aux n ft fsi in
  let opf = Option.map (ft n) in
  match tm with
  | TmVar (i, v) -> TmVar (i, v)
  | TmPrim (i, p) -> TmPrim (i, map_prim_ty n ft p)
  | TmRnd64 (i, tm1) | TmRnd32 (i, tm1) | TmRnd16 (i, tm1) ->
      TmRnd64 (i, tf n tm1)
  | TmRet (i, tm1) -> TmRet (i, tf n tm1)
  (*  *)
  | TmTens (i, tm1, tm2) -> TmTens (i, tf n tm1, tf n tm2)
  | TmTensDest (i, bi_x, bi_y, tm, tm_i) ->
      TmTensDest (i, bi_x, bi_y, tf n tm, tf n tm_i)
  (*  *)
  | TmInl (i, tm_l) -> TmInl (i, tf n tm_l)
  | TmInr (i, tm_r) -> TmInr (i, tf n tm_r)
  | TmUnionCase (i, tm, bi_l, tm_l, bi_r, tm_r) ->
      TmUnionCase (i, tf n tm, bi_l, tf n tm_l, bi_r, tf n tm_r)
  (*  *)
  | TmAbs (i, bi, ty, tm) -> TmAbs (i, bi, ft n ty, tf n tm)
  | TmApp (i, tm1, tm2) -> TmApp (i, tf n tm1, tf n tm2)
  (*  *)
  | TmAmpersand (i, tm1, tm2) -> TmAmpersand (i, tf n tm1, tf n tm2)
  | TmAmp1 (i, tm1) -> TmAmp1 (i, tf n tm1)
  | TmAmp2 (i, tm1) -> TmAmp2 (i, tf n tm1)
  (*  *)
  | TmBox (i, si, tm1) -> TmBox (i, fsi n si, tf n tm1)
  | TmBoxDest (i, bi, tm1, tm2) -> TmBoxDest (i, bi, tf n tm1, tf n tm2)
  (*  *)
  | TmLet (i, bi, orty, tm, tm_i) -> TmLet (i, bi, opf orty, tf n tm, tf n tm_i)
  (*  *)
  | TmLetBind (i, bi, tm, tm_i) -> TmLetBind (i, bi, tf n tm, tf n tm_i)
  (*  *)
  | TmOp (i, opi, tm) -> TmOp (i, opi, tf n tm)

let map_term_ty fty fsi tm = map_term_ty_aux 0 fty fsi tm

(* Substitution of annotations in expressions *)

(* tm[t/x] *)
let term_ty_subst x t tm =
  let tsub k = ty_subst (x + k) t in
  let sisub k = si_shift k (-1) in
  map_term_ty tsub sisub tm

let term_si_subst x s tm =
  let tsub k = ty_shift k (-1) in
  let sisub k = si_subst (x + k) s in
  map_term_ty tsub sisub tm

(************************************************************************)
(* File info extraction *)

let tmInfo t =
  match t with
  | TmVar (fi, _) -> fi
  | TmPrim (fi, _) -> fi
  | TmRnd64 (fi, _) | TmRnd32 (fi, _) | TmRnd16 (fi, _) -> fi
  | TmRet (fi, _) -> fi
  (* *)
  | TmTens (fi, _, _) -> fi
  | TmTensDest (fi, _, _, _, _) -> fi
  (* *)
  | TmInl (fi, _) -> fi
  | TmInr (fi, _) -> fi
  | TmUnionCase (fi, _, _, _, _, _) -> fi
  (* *)
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  (* *)
  | TmAmpersand (fi, _, _) -> fi
  | TmAmp1 (fi, _) -> fi
  | TmAmp2 (fi, _) -> fi
  (* *)
  | TmBox (fi, _, _) -> fi
  | TmBoxDest (fi, _, _, _) -> fi
  (*  *)
  | TmLet (fi, _, _, _, _) -> fi
  (*  *)
  | TmLetBind (fi, _, _, _) -> fi
  (*  *)
  | TmOp (fi, _, _) -> fi
