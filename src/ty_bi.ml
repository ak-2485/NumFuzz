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
type 'a checker = context -> 'a ty_error

let (>>=) (m : 'a checker) (f : 'a -> 'b checker) : 'b checker =
  fun ctx ->
    match m ctx with
    | Right res -> f res ctx
    | Left e    -> Left e

let (>>) m f = m >>= fun _ -> f

let return (x : 'a) : 'a checker = fun _ctx -> Right x

let get_ctx : context checker =
  fun ctx -> Right ctx

let get_ctx_length : int checker =
  get_ctx                             >>= fun ctx ->
  return @@ List.length ctx.var_ctx

let get_ty_ctx_length : int checker =
  get_ctx                             >>= fun ctx ->
  return @@ List.length ctx.tyvar_ctx

let with_new_ctx (f : context -> context) (m : 'a checker) : 'a checker =
  fun ctx -> m (f ctx)

let fail (i : info) (e : ty_error_elem) : 'a checker = fun _ ->
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
let singleton (n : int) (v : var_info) s : bsi list =
  let rec aux n l =
    if n = 0 then l
    else let si = if n = v.v_index + 1 then Some s else None in
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
  | Some si, None
  | None, Some si -> Some si
  | None, None -> None

let mult_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiMult (si1, si2))
  | _, _ -> None

let div_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiDiv (si1, si2))
  | _, _ -> None

let si_of_bsi (bsi : bsi) : si =
  Option.default si_zero bsi

let lub_bsi (bsi1 : bsi) (bsi2 : bsi) : bsi =
  match bsi1, bsi2 with
  | Some si1, Some si2 -> Some (SiLub (si1, si2))
  | Some si, None
  | None, Some si -> Some si
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

  (* Check that the type is an application and can be applied to
     arg. Return the result and sensitivity of the application *)
  let check_ty_app_shape i ty_arr =
    match ty_arr with
    | _                   -> fail i @@ CannotApply(ty_arr, ty_arr)

  let check_fuzz_shape i ty = fail i @@ WrongShape (ty, "fuzzy")

  let check_tensor_shape i ty =
    match ty with
    | TyTensor(ty1, ty2) -> return (ty1, ty2)
    | _                  -> fail i @@ WrongShape (ty, "tensor")

  let check_sens_disjoint' bsi1 bsi2 = 
    match bsi1,bsi2 with
      | Some si1, Some si2 -> 
        if (si1 == si_zero) == false then post_si_leq si_zero si2 else
        if (si2 == si_zero) == false then post_si_leq si_zero si1 else
        false
      | None, None -> true
      | _, _ ->  false

  let rec check_sens_disjoint i (bsi1 : bsi list) (bsi2 : bsi list) =
    match bsi1, bsi2 with
    | (sbsi1 :: l1), (sbsi2 :: l2) -> 
        if check_sens_disjoint' sbsi1 sbsi2 then return () else 
          fail i @@ Internal "Some linear context problem" >>
        check_sens_disjoint i l1 l2
    | [] , [] -> return ()
    | _ , _ -> fail i @@ Internal "Some linear context problem"


let check_is_num' ty : bool =
  match ty with
  | TyPrim PrimNum -> true
  | _ -> false

let check_is_num i ty : unit checker =
  if check_is_num' ty then
    return ()
  else
    fail i @@ WrongShape (ty, "num")

  let check_union_shape i ty =
    match ty with
    | TyUnion(ty1, ty2) -> return (ty1, ty2)
    | _                 -> fail i @@ WrongShape (ty, "union")

 
  let check_sized_nat_shape i ty = fail i @@ WrongShape (ty, "nat")

end

open TypeSub

(* Extend the context with a type binding and run a computation *)
let with_extended_ty_ctx (v : string) (k : kind) (m : 'a checker) : 'a checker =
  with_new_ctx (extend_ty_var v k) m

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

let get_var_ty (v : var_info) : ty checker =
  get_ctx >>= fun ctx ->
  return @@ snd (access_var ctx v.v_index)

let shift_sens (bsi : bsi) (bsis : bsi list) : bsi list =
  List.map (add_bsi bsi) bsis

let add_sens (bsis1 : bsi list) (bsis2 : bsi list) : bsi list =
  List.map2 add_bsi bsis1 bsis2

let scale_sens (bsi : bsi) (bsis : bsi list) : bsi list =
  List.map (mult_bsi bsi) bsis

let lub_sens (bsis1 : bsi list) (bsis2 : bsi list) : bsi list =
  List.map2 lub_bsi bsis1 bsis2

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

let rec type_of (t : term) : (ty * bsi list) checker  =

  ty_debug (tmInfo t) "--> [%3d] Enter type_of: @[%a@]" !ty_seq
    (Print.limit_boxes Print.pp_term) t; incr ty_seq;

  (match t with
  (* Variables *)
  | TmVar(_i, x)  ->
    get_ctx_length              >>= fun len ->
    get_var_ty x                >>= fun ty_x  ->
    return (ty_x, singleton len x si_zero)

  (* Primitive terms *)
  | TmPrim(_, pt) ->
    get_ctx_length >>= fun len ->
    return (type_of_prim pt, zeros len)

  (* Standard let-binding *)
  (* x : oty_x = tm_x ; e *)
  | TmLet(i, x, oty_x, tm_x, e)         ->

    type_of tm_x >>= fun (ty_x, sis_x)  ->

    ty_info2 i "### Type of binder %a is %a" Print.pp_binfo x Print.pp_type ty_x;

    check_type_sub' i ty_x oty_x >>

    with_extended_ctx i x.b_name ty_x (type_of e) >>= fun (ty_e, si_x, sis_e) ->

    ty_debug i "*** Context let: @[%a@]" (Print.pp_list Print.pp_si) (bsi_sens sis_e); 
    ty_debug i "### In case, [%3d] Inferred sensitivity for binder @[%a@] is @[%a@]" !ty_seq P.pp_binfo x P.pp_si (si_of_bsi si_x);

    let si_x =  (si_of_bsi si_x) in
    let si_x = Simpl.si_simpl si_x in
    let si_x = Simpl.si_simpl_compute si_x in

    return (ty_e, add_sens (shift_sens (Some si_x) sis_x) sis_e)

  (* Defs *)
  | TmDef(i, e)                   ->
    type_of e >>= fun (ty_e, sis_e)  ->
    ty_debug i "*** Context DEF: @[%a@]" (Print.pp_list Print.pp_si) (bsi_sens sis_e); 

    return (ty_e,sis_e)

  (* Tensor product*)
  | TmTens(i, e1, e2) ->

    type_of e1 >>= fun (ty1, sis1) ->
    type_of e2 >>= fun (ty2, sis2) ->

    check_sens_disjoint i sis1 sis2 >>

    return @@ (TyTensor(ty1, ty2), add_sens sis1 sis2)

  (* let (x,y) = v in e *)
  | TmTensDest(i, x, y, v, e) ->

    type_of v >>= fun (ty_v, sis_v) ->
    check_tensor_shape i ty_v >>= fun (ty_x, ty_y) ->

    (* Extend context with x and y *)
    with_extended_ctx_2 i x.b_name ty_x y.b_name ty_y 
      (type_of e) >>= fun (ty_e, si_x, si_y, sis_e) ->

    check_sens_eq i (si_of_bsi si_x) (si_of_bsi si_y) >>
    check_sens_disjoint i sis_v sis_e >> 

    return (ty_e, add_sens (shift_sens si_x sis_v) sis_e)

  (* Case analysis *)
  (* case v of inl(x) => e_l | inr(y) => f_r *)
  | TmUnionCase(i, v, b_x, e_l, b_y, f_r)      ->

    type_of v >>= fun (ty_v, sis_v) ->

    check_union_shape i ty_v >>= fun (ty1, ty2) ->

    with_extended_ctx i b_x.b_name ty1 (type_of e_l) >>= fun (tyl, si_x, sis_l) ->

    with_extended_ctx i b_y.b_name ty2 (type_of f_r) >>= fun (tyr, si_y, sis_r) ->

    check_ty_union i tyl tyr >>= fun ty_exp ->

    ty_debug (tmInfo v) "### In case, [%3d] Inferred sensitivity for binder @[%a@] is @[%a@]" !ty_seq P.pp_binfo b_x P.pp_si (si_of_bsi si_x);

    ty_debug (tmInfo v) "*** Context: @[%a@]" (Print.pp_list Print.pp_si) (bsi_sens sis_v); 

      let si_x = si_of_bsi si_x in
      let si_y = si_of_bsi si_y in

      check_sens_eq i si_x si_y >>

      let theta = (lub_sens sis_l sis_r) in

      if (post_si_eq si_x si_zero) then
        return (ty_exp, add_sens theta (scale_sens (Some si_one) sis_v))
      else
        return (ty_exp, add_sens theta (scale_sens (Some si_x) sis_v))

  | TmInl(_i, tm_l)      ->

      type_of tm_l >>= fun (ty, sis) ->
      return (TyUnion(ty, TyPrim PrimUnit), sis)

  | TmInr(_i, tm_r)      ->

      type_of tm_r >>= fun (ty, sis) ->
      return (TyUnion(TyPrim PrimUnit, ty), sis)

  (* Ops *)
  | TmAdd(_i, x, y) ->
    (*type_of tm_x >>= fun (ty_x, sis_x) ->
    type_of tm_y >>= fun (ty_y, sis_y) ->*)
    get_ctx_length >>= fun len ->
    return (TyPrim PrimNum, zero_list_op len x y si_hlf si_hlf)

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
let get_type program context =
  match type_of program context with
  | Right (ty, sis) -> (ty,sis)
  | Left e ->
    typing_error_pp e.i pp_tyerr e.v
