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

(* map over types, first argument: action on vars, second argument
   action on evars, third argument action on sensitivities, 4th on sizes *)
let rec ty_map n fv fsi ty =
  match ty with
  | TyVar v -> fv n v
  | TyPrim tp -> TyPrim tp
  (* ADT *)
  | TyUnion (ty1, ty2) -> TyUnion (ty_map n fv fsi ty1, ty_map n fv fsi ty2)
  | TyTensor (ty1, ty2) -> TyTensor (ty_map n fv fsi ty1, ty_map n fv fsi ty2)

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

type op = AddOp | MulOp | DivOp | SubOp

type term =
  | TmVar of info * var_info
  | TmDVar of info * var_info
  (*  *)
  | TmTens of info * term * term
  | TmTensDest of info * binder_info * binder_info * term * term
  (* Primitive terms *)
  | TmPrim of info * term_prim
  (* Case *)
  | TmInl of info * ty * term
  | TmInr of info * ty * term
  | TmUnionCase of info * term * binder_info * term * binder_info * term
  (*                      t  of { inl(x)     => tm1  | inl(y)     => tm2  } *)
  (* Regular sequencing *)
  | TmLet of info * binder_info * ty option * term * term
  (* Basic ops *)
  | TmAdd of info * var_info * var_info
  | TmSub of info * var_info * var_info
  | TmDiv of info * var_info * var_info
  | TmMul of info * var_info * var_info

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
  | TmDVar (i, v) -> TmDVar (i, v)
  | TmPrim (i, p) -> TmPrim (i, map_prim_ty n ft p)
  | TmTens (i, tm1, tm2) -> TmTens (i, tf n tm1, tf n tm2)
  | TmTensDest (i, bi_x, bi_y, tm, tm_i) ->
      TmTensDest (i, bi_x, bi_y, tf n tm, tf n tm_i)
  | TmInl (i, ty, tm_l) -> TmInl (i, ty, tf n tm_l)
  | TmInr (i, ty, tm_r) -> TmInr (i, ty, tf n tm_r)
  | TmUnionCase (i, tm, bi_l, tm_l, bi_r, tm_r) ->
      TmUnionCase (i, tf n tm, bi_l, tf n tm_l, bi_r, tf n tm_r)
  | TmLet (i, bi, orty, tm, tm_i) -> TmLet (i, bi, opf orty, tf n tm, tf n tm_i)
  (*  *)
  | TmAdd (i, x, y) -> TmAdd (i, x, y)
  | TmSub (i, x, y) -> TmSub (i, x, y)
  | TmDiv (i, x, y) -> TmDiv (i, x, y)
  | TmMul (i, x, y) -> TmMul (i, x, y)

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
  | TmDVar (fi, _) -> fi
  | TmPrim (fi, _) -> fi
  | TmTens (fi, _, _) -> fi
  | TmTensDest (fi, _, _, _, _) -> fi
  (*  *)
  | TmInl (fi, _, _) -> fi
  | TmInr (fi, _, _) -> fi
  | TmUnionCase (fi, _, _, _, _, _) -> fi
  | TmLet (fi, _, _, _, _) -> fi
  (*  *)
  | TmAdd (fi, _, _) -> fi
  | TmSub (fi, _, _) -> fi
  | TmDiv (fi, _, _) -> fi
  | TmMul (fi, _, _) -> fi
