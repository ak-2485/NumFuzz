(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Ctx
open Syntax
open Constr

open Support.Error
open Support.FileInfo

module Opt = Support.Options
module P   = Print

(* Type checking and synthesis engine for Fuzz. *)

(* Errors *)
type ty_error_elem =
| SensErrorLe  of si * si
| SensErrorLt  of si * si
| SensErrorEq  of si * si
| NotDisjoint  of si * si
| SensErrorDiv of si * si
| MoreGeneral  of ty * ty
| TypeMismatch of ty * ty
| TypeInst     of ty * ty
| CannotApply  of ty * ty
| OccursCheck  of var_info * ty
| WrongShape   of ty * string
| WrongKind    of kind * kind
| NotSubtype   of ty * ty
| Internal     of string

let ty_seq = ref 0

let typing_error fi = error_msg    Opt.TypeChecker fi
let typing_error_pp = error_msg_pp Opt.TypeChecker

let ty_warning fi = message 1 Opt.TypeChecker fi
let ty_info    fi = message 2 Opt.TypeChecker fi
let ty_info2   fi = message 3 Opt.TypeChecker fi
let ty_debug   fi = message 4 Opt.TypeChecker fi
let ty_debug2  fi = message 5 Opt.TypeChecker fi
let ty_debug3  fi = message 6 Opt.TypeChecker fi

type 'a ty_error =
  Right of 'a
| Left  of ty_error_elem withinfo

(* Native @@ is already in ocaml 4.0 *)
let (@@) x y = x y

(* Reader/Error monad for type-checking *)
type 'a checker = context -> context -> 'a ty_error

let (>>=) (m : 'a checker) (f : 'a -> 'b checker) : 'b checker =
  fun ctx dctx ->
    match m ctx dctx with
    | Right res -> f res ctx dctx
    | Left e    -> Left e

let (>>) m f = m >>= fun _ -> f

let return (x : 'a) : 'a checker = fun _ctx _dctx -> Right x

let get_ctx : context checker =
  fun ctx _ -> Right ctx 

let get_dctx : context checker =
  fun _ dctx -> Right dctx 

let get_ctx_length : int checker =
  get_ctx                             >>= fun ctx ->
  return @@ List.length ctx.var_ctx

let get_ty_ctx_length : int checker =
  get_ctx                             >>= fun ctx ->
  return @@ List.length ctx.tyvar_ctx

let with_new_ctx (f : context -> context) (m : 'a checker) : 'a checker =
  fun ctx dctx -> m (f ctx) dctx

let with_new_dctx (f : context -> context) (m : 'a checker) : 'a checker =
  fun ctx dctx -> m ctx (f dctx)

let with_new_ctx2 (f : context -> context -> context) (computed_ctx : context) 
  (m : 'a checker) : 'a checker =
  fun current_ctx -> m (f computed_ctx current_ctx)

let fail (i : info) (e : ty_error_elem) : 'a checker = fun _ _ ->
  Left { i = i; v = e }

let si_div (si1: si) (si2: si) =
  Simpl.si_simpl_compute (SiDiv(si1, si2))

let check_sens_div' (sil : si) (sir: si) : bool =
  let sil' = Simpl.si_simpl_compute sil in
  let sir' = Simpl.si_simpl_compute sir in
  match sil', sir' with
  | SiConst _, SiConst _ -> true
  | _, _ -> false

let check_sens_div i (sil : si) (sir : si) : unit checker =
  if check_sens_div' sil sir then
    return ()
  else
    fail i @@ SensErrorDiv(sil, sir)

let check_sens_leq i (sil : si) (sir : si) : unit checker =
  if post_si_leq sil sir then
    return()
  else
    fail i @@ SensErrorLe(sil, sir)

let check_sens_lt i (sil : si) (sir : si) : unit checker =
  if post_si_lt sil sir then
    return ()
  else
    fail i @@ SensErrorLt(sil, sir)

let check_sens_eq  i (sil : si) (sir : si) : unit checker =
  if post_si_eq sil sir then
    return ()
  else
    fail i @@ SensErrorEq(sil, sir)

(* Constants *)
let si_zero  = SiConst  0.0
let si_one   = SiConst  1.0
let si_hlf   = SiConst  0.5
let si_infty = SiInfty

(* Type of sensitivities augmented with □, with corresponding
   functions. In the type-checking algorithm, □ represents a binding
   that doesn't have an assigned sensitivity, which must be
   later on. *)
type bsi = si option

(* A list only with one sensitivities *)
let ones (n : int) : bsi list =
  let rec aux n l =
    if n = 0 then l else aux (n - 1) (Some si_one :: l) in
  aux n []

(* A list only with zero sensitivities *)
let zeros (n : int) : bsi list =
  let rec aux n l =
    if n = 0 then l else aux (n - 1) (None :: l) in
  aux n []

(* A list with zero sensitivities, except for one variable *)
(* Note that this has to be kept in sync with the actual ctx *)
let singleton (n : int) (v : var_info) (si : si) : bsi list =
  let rec aux n l =
    if n = 0 then l
    else let si = if n = v.v_index + 1 then Some si else None in
         aux (n - 1) (si :: l) in
  aux n []

let binop_ctx (n : int) (v1 : var_info) (v2 : var_info) s1 s2 : bsi list =
  let rec aux n l =
    if n = 0 then l
    else let si = if n = v1.v_index + 1 then Some s1 else
                  if n = v2.v_index + 1 then Some s2 else None in
                  aux (n - 1) (si :: l) in
  aux n []

let zero_list_op (n : int) (v1 : var_info) (v2 : var_info) s1 s2 : bsi list =
  let rec aux n l =
    if n = 0 then l
    else let si = if n = v1.v_index + 1 then Some s1 else
                  if n = v2.v_index + 1 then Some s2 else None in
         aux (n - 1) (si :: l) in
  aux n []

(* Extension of operations on regular sensitivities to augmented
   sensitivities *)

let add_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiAdd (si1, si2))
  | _, _ -> None

let mult_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiMult (si1, si2))
  | _, _ -> None

let div_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiDiv (si1, si2))
  | _, _ -> None

(* we don't want to use this in bean because it maps None si to zero *)
let si_of_bsi (bsi : bsi) : si =
  Option.default si_zero bsi

let lub_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiLub(Simpl.si_simpl_compute  si1, Simpl.si_simpl_compute  si2))
  | Some si, None
  | None, Some si -> Some (Simpl.si_simpl_compute si)
  | None, None -> None

let cs_add_eq (sil : si) (sir : si) =
  extend_cs (SiEq (sil, sir))

module TypeSub = struct

  let check_prim_sub (i : info) (ty_f : ty_prim) (ty_a : ty_prim) : unit checker =
    match ty_f, ty_a with
    | _   when ty_f = ty_a -> return ()
    | _                    -> fail i @@ NotSubtype (TyPrim ty_f, TyPrim ty_a)


  (* Check whether ty_1 is a subtype of ty_2, generating the necessary
     constraints along the way. *)
  let rec check_type_sub (i : info) (ty_1 : ty) (ty_2 : ty) : unit checker =
    let fail = fail i @@ NotSubtype (ty_1, ty_2) in
    match ty_1, ty_2 with
    | TyVar v1, TyVar v2   ->
      if v1 = v2 then return () else fail

    | TyPrim p1, TyPrim p2 -> check_prim_sub i p1 p2

    | TyUnion(tyl1, tyl2), TyUnion(tyr1, tyr2) ->
      check_type_sub i tyl1 tyr1 >>
      check_type_sub i tyl2 tyr2

    | TyTensor(tyl1, tyl2), TyTensor(tyr1, tyr2) ->
      check_type_sub i tyl1 tyr1 >>
      check_type_sub i tyl2 tyr2

    | _, _ -> fail

    let check_type_sub' i (sty : ty)  (oty_x : ty option) =
      match oty_x with
      | Some ty' -> check_type_sub i sty ty'
      | None -> return ()

    let rec check_type_eq (i : info) (ty_1 : ty) (ty_2 : ty) : unit checker =
      let fail = fail i @@ TypeMismatch (ty_1, ty_2) in
      match ty_1, ty_2 with
      | TyVar v1, TyVar v2   ->
        if v1 = v2 then return () else fail

      | TyPrim p1, TyPrim p2 -> check_prim_sub i p1 p2

      | TyUnion(tyl1, tyl2), TyUnion(tyr1, tyr2) ->
        check_type_eq i tyl1 tyr1 >>
        check_type_eq i tyl2 tyr2

      | TyTensor(tyl1, tyl2), TyTensor(tyr1, tyr2) ->
        check_type_eq i tyl1 tyr1 >>
        check_type_eq i tyl2 tyr2

      | _, _ -> fail

  let check_ty_union i sityl sityr =
          check_type_eq i sityl sityr >>
          return sityl

  (* Check whether ty is compatible (modulo subtyping) with annotation
     ann, returning the resulting type. *)
  let check_type_ann (i : info) (ann : ty option) (ty : ty) : ty checker =
    match ann with
    | Some ty' -> check_type_sub i ty ty' >> return ty'
    | None -> return ty

  (* Checks for types of different shapes *)
  let check_fuzz_shape i ty = fail i @@ WrongShape (ty, "fuzzy")

  let check_tensor_shape i ty =
    match ty with
    | TyTensor(ty1, ty2) -> return (ty1, ty2)
    | _                  -> fail i @@ WrongShape (ty, "tensor")

  let check_union_shape i ty =
    match ty with
    | TyUnion(ty1, ty2) -> return (ty1, ty2)
    | _                 -> fail i @@ WrongShape (ty, "union")

 
  let check_sized_nat_shape i ty = fail i @@ WrongShape (ty, "nat")

end

open TypeSub

(* Extend the context with a value binding and run a computation. The
   computation is assumed to produce a list of results, one for each
   variable in the extended context. That list is destructed, and the
   result corresponding to the new variable is returned separately for
   convenience. *)
let with_extended_ctx (i : info) (v : string) (ty : ty) (m : ('a * 'b list) checker) :
    ('a * 'b * 'b list) checker =
  with_new_ctx (extend_var v ty) m >>= fun (res, res_ext_ctx) ->
  match res_ext_ctx with
  | res_v :: res_ctx -> return (res, res_v, res_ctx)
  | [] -> fail i @@ Internal "Computation on extended context didn't produce enough results"

(* Similar to the one above, but with two variables. vx has index 1 in
   the extended context, while vy has index 0. The order of the
   returned results matches those of the arguments. *)
let with_extended_ctx_2 (i : info)
    (vx : string) (tyx : ty) (vy : string) (tyy : ty)
    (m : ('a * 'b list) checker) : ('a * 'b * 'b * 'b list) checker =
  with_new_ctx (fun ctx -> extend_var vy tyy (extend_var vx tyx ctx)) m >>= fun (res, res_ext_ctx) ->
  match res_ext_ctx with
  | res_y :: res_x :: res_ctx -> return (res, res_x, res_y, res_ctx)
  | _ -> fail i @@ Internal "Computation on extended context didn't produce enough results"

(* extends the discrete context with 2 variables *)
let with_extended_dctx_2 (i : info)
    (vx : string) (tyx : ty) (vy : string) (tyy : ty)
    (m : ('a * 'b list) checker) : ('a * 'b list) checker =
  with_new_dctx (fun dctx -> extend_var vy tyy (extend_var vx tyx dctx)) m

let intersect_bsi (a : bsi) (b : bsi) : bool = 
   not (a == None) && not (b == None)

let check_disjoint i (ctx1 : bsi list) (ctx2 : bsi list) : unit checker = 
  let s1 = List.find_opt (fun a -> a == true) (List.map2 intersect_bsi ctx1 ctx2) in
  match s1 with 
    | Some _   -> fail i @@ Internal "Context check failed" 
    | None     -> return ()

let get_var_ty (v : var_info) : ty checker =
  get_ctx >>= fun ctx ->
  return @@ snd (access_var ctx v.v_index)

let get_dvar_ty (v : var_info) : ty checker =
  get_dctx >>= fun dctx ->
  return @@ snd (access_var dctx v.v_index)

let shift_sens (s : bsi) (l :  bsi list) :  bsi list =
  List.map (add_bsi s) l

let rec union_ctx'  (ctx :  (bsi * bsi) list) : bsi list =
   match ctx with 
    | (Some s1, Some s2) :: l -> (lub_bsi (Some s1) (Some s2)) :: union_ctx' l
    | (Some s1, _) :: l -> Some s1 :: union_ctx' l
    | (_, Some s2) :: l -> Some s2 :: union_ctx' l
    | (None, None) :: l -> None :: union_ctx' l
    | [] -> []

(* if contexts are not disjoint, takes greater of error bounds *)
let union_ctx (ctx1 : bsi list) (ctx2 :  bsi list) : bsi list =
  union_ctx' (List.combine ctx1 ctx2)

let scale_sens (bsi : bsi) (bsis : bsi list) : bsi list =
  List.map (mult_bsi bsi) bsis

let bsi_sens (bsis : bsi list) : si list =
  List.map si_of_bsi bsis

(**********************************************************************)
(* Main typing routines                                               *)
(**********************************************************************)
let kind_of (i : info) (si : si) : kind checker =
  ty_debug i "--> [%3d] Enter kind_of: @[%a@]" !ty_seq
    (Print.limit_boxes Print.pp_si) si; incr ty_seq;

  let ck k = return k in

  (match si with
  | SiInfty       -> return Sens
  | SiConst _     -> return Sens

  | SiVar   v     ->
    get_ctx >>= fun ctx ->
    ck @@ snd @@ access_ty_var ctx v.v_index

  | SiAdd  (_ , _)
  | SiMult (_ , _)
  | SiDiv (_ , _)
  | SiLub  (_ , _) -> return Sens

  ) >>= fun k ->

  decr ty_seq;
  (* We limit pp_term *)
  ty_debug i "<-- [%3d] Exit kind_of: @[%a@] with kind @[%a@]" !ty_seq
    (Print.limit_boxes Print.pp_si) si Print.pp_kind k;
  return k

(* Given a term t and a context ctx for that term, check whether t is
   typeable under ctx, returning a type for t, a list of synthesized
   sensitivities for ctx, and a list of constraints that need to be
   satisfied in order for the type to be valid. Raises an error if it
   detects that no typing is possible. *)

let rec type_of (t : term) : (ty * bsi list) checker =

  ty_debug (tmInfo t) "--> [%3d] Enter type_of: @[%a@]" !ty_seq
    (Print.limit_boxes Print.pp_term) t; incr ty_seq;

  (match t with
  (* Variables *)
  | TmVar(_i, x)  ->
    get_ctx_length              >>= fun len ->
    get_var_ty  x               >>= fun ty_x  ->
    (* variable typed with zero backward error *)
    return (ty_x, singleton len x si_zero)

  | TmDVar(_i, x)  ->
    get_ctx_length              >>= fun len ->
    get_dvar_ty  x              >>= fun ty_x  ->
    (* empty linear context *)
    return (ty_x, zeros len)

  (* Primitive terms *)
  | TmPrim(_, pt) ->
    get_ctx_length >>= fun len ->
    return (type_of_prim pt, zeros len)

  (* Standard let-binding *)
  (* let (x : oty_x) = e in f *)
  | TmLet(i, x, oty_x, tm_e, tm_f)         ->

    type_of tm_e >>= fun (ty_e, ctx_e)  ->

    with_extended_ctx i x.b_name ty_e (type_of tm_f) >>= fun (ty_f, si_x, ctx_f) ->

    check_disjoint i ctx_e ctx_f >>

    return (ty_e, union_ctx (shift_sens si_x ctx_e) ctx_f)

  (* Tensor product*)
  | TmTens(i, tm_e, tm_f) ->

    type_of tm_e >>= fun (ty_e, ctx_e) ->
    type_of tm_f >>= fun (ty_f, ctx_f) ->

    check_disjoint i ctx_e ctx_f >>

    return (TyTensor(ty_e, ty_f), union_ctx ctx_e ctx_f)

  (* let (x,y) = e in f *)
  | TmTensDest(i, x, y, tm_e, tm_f) ->

    type_of tm_e >>= fun (ty_e, ctx_e) ->
    check_tensor_shape i ty_e >>= fun (ty_x, ty_y) ->

    (* Extend context with x and y *)
    with_extended_ctx_2 i x.b_name ty_x y.b_name ty_y 
      (type_of tm_f) >>= fun (ty_f, si_x, si_y, ctx_f) ->

    check_disjoint i ctx_e ctx_f >> 
    let si = lub_bsi si_x si_y in

    return (ty_f, union_ctx (shift_sens si ctx_e) ctx_f)
  
  (* dlet (x,y) = e in f *)
  | TmTensDDest(i, x, y, tm_e, tm_f) ->

    type_of tm_e >>= fun (ty_e, ctx_e) ->
    check_tensor_shape i ty_e >>= fun (ty_x, ty_y) ->

    (* Extend context with x and y *)
    with_extended_dctx_2 i x.b_name ty_x y.b_name ty_y 
      (type_of tm_f) >>= fun (ty_f, ctx_f) ->

    check_disjoint i ctx_e ctx_f >> 
    return (ty_f, union_ctx ctx_e ctx_f)
  
  | TmInl(_i, ty_r, tm_l)      ->

      type_of tm_l >>= fun (ty, ctx) ->
      return (TyUnion(ty, ty_r), ctx)

  | TmInr(_i, ty_l, tm_r)      ->

      type_of tm_r >>= fun (ty, ctx) ->
      return (TyUnion(ty_l, ty), ctx)

  (* case v of (x.e_l | y.f_r) *)
  | TmUnionCase(i, v, b_x, e_l, b_y, f_r) ->

    type_of v >>= fun (ty_v, ctx_v) ->
    check_union_shape i ty_v >>= fun (ty1, ty2) ->

    with_extended_ctx i b_x.b_name ty1 (type_of e_l) >>= fun (tyl, si_x, ctx_l) ->
    with_extended_ctx i b_y.b_name ty2 (type_of f_r) >>= fun (tyr, si_y, ctx_r) ->
    (* check that e_l and f_r have the same type *)
    check_ty_union i tyl tyr >>= fun ty_exp ->
    (* check that domains are disjoint *)
    check_disjoint i ctx_v ctx_l >> 
    check_disjoint i ctx_v ctx_r >> 

    (* take lub of x, y error *)
    let si = lub_bsi si_x si_y in
    (* non-disjoint union of left and right contexts *)
    let ctx_union = union_ctx ctx_l ctx_r in
    return (ty_exp, union_ctx (shift_sens si ctx_v) ctx_union)
  (* Ops *)
  | TmAdd(i, x, y) ->

    ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
      P.pp_vinfo x x.v_index;
    ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
      P.pp_vinfo y y.v_index;

    get_ctx_length              >>= fun len ->
    return (TyPrim PrimNum,  binop_ctx len x y si_one si_one)
  
  | TmSub(i, x, y) ->

    ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
      P.pp_vinfo x x.v_index;
    ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
      P.pp_vinfo y y.v_index;

    get_ctx_length              >>= fun len ->
    return (TyPrim PrimNum,  binop_ctx len x y si_one si_one)
  
  | TmMul(i, x, y) ->

      ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
        P.pp_vinfo x x.v_index;
      ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
        P.pp_vinfo y y.v_index;
  
      get_ctx_length              >>= fun len ->
      return (TyPrim PrimNum,  binop_ctx len x y si_hlf si_hlf)
  | TmDiv(i, x, y) ->
    
      ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
        P.pp_vinfo x x.v_index;
      ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
        P.pp_vinfo y y.v_index;
  
      get_ctx_length              >>= fun len ->
      return (TyPrim PrimNum,  binop_ctx len x y si_hlf si_hlf)

  | TmDMul(i, z, x) ->

      ty_debug i "### In case, [%3d] index for binder @[%a@] is @[%d@]" !ty_seq 
        P.pp_vinfo x x.v_index;

      get_ctx_length              >>= fun len ->
      return (TyPrim PrimNum, singleton len x si_hlf)
  ) >>= fun (ty, sis) ->

  decr ty_seq;
  (* We limit pp_term *)
  ty_debug (tmInfo t) "<-- [%3d] Exit type_of : @[%a@] with type @[%a@]" !ty_seq
    (Print.limit_boxes Print.pp_term) t Print.pp_type ty;

  (* TODO: pretty printer for sensitivities *)
  (* ty_debug2 (tmInfo t) "<-- Context: @[%a@]" Print.pp_context ctx; *)

  return (ty, sis)

open Format
open Print

let pp_tyerr ppf s = match s with
  | SensErrorEq (si1, si2)  -> fprintf ppf "EEE [%3d] Cannot satisfy constraint %a = %a" !ty_seq pp_si si1 pp_si si2
  | SensErrorLe (si1, si2)  -> fprintf ppf "EEE [%3d] Cannot satisfy constraint %a <= %a" !ty_seq pp_si si1 pp_si si2
  | SensErrorLt (si1, si2)  -> fprintf ppf "EEE [%3d] Cannot satisfy constraint %a < %a" !ty_seq pp_si si1 pp_si si2
  | NotDisjoint (si1, si2)  -> fprintf ppf "EEE [%3d] Some linear context error %a and %a" !ty_seq pp_si si1 pp_si si2
  | SensErrorDiv (_si1, _si2)  -> fprintf ppf "Some sens div error" 
  | MoreGeneral(ty1, ty2) -> fprintf ppf "EEE [%3d] %a is not more general than %a"     !ty_seq pp_type ty1 pp_type ty2
  | TypeInst(ty1, ty2)    -> fprintf ppf "EEE [%3d] Type %a is not instance of %a"      !ty_seq pp_type ty1 pp_type ty2
  | TypeMismatch(ty1, ty2)-> fprintf ppf "EEE [%3d] Cannot unify %a with %a"        !ty_seq pp_type ty1 pp_type ty2
  | CannotApply(ty1, ty2) -> fprintf ppf "EEE [%3d] Cannot apply %a to %a"    !ty_seq pp_type ty1 pp_type ty2
  | OccursCheck(v, ty)    -> fprintf ppf "EEE [%3d] Cannot build infinite type %a = %a" !ty_seq pp_vinfo v pp_type ty
  | WrongShape(ty, sh)    -> fprintf ppf "EEE [%3d] Type %a has wrong shape, expected %s type." !ty_seq pp_type ty sh
  | WrongKind(k1, k2)     -> fprintf ppf "EEE [%3d] Kind mismatch expected %a found %a." !ty_seq pp_kind k1 pp_kind k2
  | NotSubtype(ty1,ty2)   -> fprintf ppf "EEE [%3d] %a is not a subtype of %a" !ty_seq pp_type ty1 pp_type ty2
  | Internal s            -> fprintf ppf "EEE [%3d] Internal error: %s" !ty_seq s

(* Equivalent to run *)
let get_type program context dcontext =
  match type_of program context dcontext with
  | Right (ty, sis) -> (ty, sis)
  | Left e ->
    typing_error_pp e.i pp_tyerr e.v
