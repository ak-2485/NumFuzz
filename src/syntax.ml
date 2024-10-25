(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Support.FileInfo

(* ---------------------------------------------------------------------- *)
(* Abstract syntax tree for sensitivities, terms and types                *)
(* ---------------------------------------------------------------------- *)

(* Binders are represented using Debruijn notation *)

type var_info = {
  (* Indices start at 0 *)
  v_index : int;
  v_name : string;
  v_size : int;
}

(* Helper to modify the index *)
let var_shift o n v = {
    v with
    v_index = (if o <= v.v_index then v.v_index + n else v.v_index);
    v_size = v.v_size + n;
  }

type binder_info = {
  b_name : string;
  (* How many outside binders we had when this binder was found *)
  b_size : int; 
  b_prim : bool;
}

(* Sensitivities *)
type si =
  | SiInfty
  | SiConst of float
  | SiAdd of si * si
  | SiMult of si * si
  | SiDiv of si * si
  | SiLub of si * si

(* Map over the variables of a sensitivity type *)
let rec si_map n f si =
  let smf = si_map n f in
  match si with
  | SiConst c -> SiConst c
  | SiAdd (x, y) -> SiAdd (smf x, smf y)
  | SiMult (x, y) -> SiMult (smf x, smf y)
  | SiDiv (x, y) -> SiDiv (smf x, smf y)
  | SiInfty -> SiInfty
  | SiLub (s1, s2) -> SiLub (smf s1, smf s2)

(* Primitive types *)
type ty_prim = PrimNum | PrimUnit | PrimString

(* Types *)
type ty =
  | TyPrim of ty_prim
  | TyUnion of ty * ty
  | TyTensor of ty * ty

(* Primitive Terms *)
type term_prim =
  | PrimTUnit
  | PrimTNum of float

let type_of_prim t =
  match t with
  | PrimTUnit -> TyPrim PrimUnit
  | PrimTNum _ -> TyPrim PrimNum

(* Terms *)
type op = AddOp | MulOp | DivOp | SubOp

type term =
  | TmVar of info * var_info
  | TmDVar of info * var_info
  (* Tensor *)
  | TmTens of info * term * term
  | TmTensDest of info * binder_info * binder_info * term * term
  | TmTensDDest of info * binder_info * binder_info * term * term
  (* Primitive terms *)
  | TmPrim of info * term_prim
  (* Case *)
  | TmInl of info * ty * term
  | TmInr of info * ty * term
  | TmUnionCase of info * term * binder_info * term * binder_info * term
  (* Let bindings *)
  | TmLet of info * binder_info * ty option * term * term
  (* Basic ops *)
  | TmAdd of info * var_info * var_info
  | TmSub of info * var_info * var_info
  | TmDiv of info * var_info * var_info
  | TmMul of info * var_info * var_info
  | TmDMul of info * var_info * var_info

(* File info extraction *)
let tmInfo t =
  match t with
  | TmVar (fi, _) -> fi
  | TmDVar (fi, _) -> fi
  | TmPrim (fi, _) -> fi
  | TmTens (fi, _, _) -> fi
  | TmTensDest (fi, _, _, _, _) -> fi
  | TmTensDDest (fi, _, _, _, _) -> fi
  | TmInl (fi, _, _) -> fi
  | TmInr (fi, _, _) -> fi
  | TmUnionCase (fi, _, _, _, _, _) -> fi
  | TmLet (fi, _, _, _, _) -> fi
  | TmAdd (fi, _, _) -> fi
  | TmSub (fi, _, _) -> fi
  | TmDiv (fi, _, _) -> fi
  | TmMul (fi, _, _) -> fi
  | TmDMul (fi, _, _) -> fi
