open Syntax
open Support.Error
open Support.Options

let rec lower_ty (sugared_ty : ty) : ty =
  match sugared_ty with
  | TyPrim p -> 
      (match p with
      | PrimNum -> TyTensor (TyPrim PrimNum, TyPrim PrimNum)
      | _ -> sugared_ty)
  | TyVar x -> TyVar x
  | TyTensor (ty1, ty2) -> TyTensor (lower_ty ty1, lower_ty ty2)
  | TyAmpersand (ty1, ty2) -> TyAmpersand (lower_ty ty1, lower_ty ty2)
  | TyUnion (ty1, ty2) -> TyUnion (lower_ty ty1, lower_ty ty2)
  | TyLollipop (ty1, ty2) -> TyLollipop (lower_ty ty1, lower_ty ty2)
  | TyBang (si, ty) -> TyBang (si, lower_ty ty)
  | TyMonad (si, ty) -> TyMonad (si, lower_ty ty)

let lower_bound_tm_with_ty ((sugared_tm, sugared_ty) : (term * ty)) : (term * ty) = 
  (sugared_tm, lower_ty sugared_ty)

(*let rec lower_bound_tm_with_ty ((sugared_tm, sugared_ty) : (term * ty)) : (term * ty) = *)
(*  match (sugared_tm, sugared_ty) with*)
(*  | (_, TyPrim p) -> *)
(*      (match p with*)
(*      (*| PrimNum -> TyUnion (TyPrim PrimNum, TyPrim PrimNum)*)*)
(*      | _ -> (sugared_tm, TyPrim p))*)
(*  | (_, TyVar x) -> (sugared_tm, TyVar x)*)
(*  | (TmUnionCase (finfo, tm, binfo1, tm1, binfo2, tm2), TyUnion (ty1, ty2)) -> *)
(*      let (tm1', ty1') = lower_bound_tm_with_ty tm1 ty1 in *)
(*      let (tm2', ty2') = lower_bound_tm_with_ty tm2 ty2 in*)
(*      (TmUnionCase (finfo, tm, binfo1, tm1', binfo2, tm2'), TyUnion (ty1', ty2'))*)
(*  | (TmTens (finfo, tm1, tm2), TyTensor (ty1, ty2)) -> *)
(*      let (tm1', ty1') = lower_bound_tm_with_ty tm1 ty1 in *)
(*      let (tm2', ty2') = lower_bound_tm_with_ty tm2 ty2 in*)
(*      (TmTens (finfo, tm1', tm2'), TyTensor (ty1', ty2'))*)
(*  | (TmAmpersand (finfo, tm1, tm2), TyAmpersand (ty1, ty2)) -> *)
(*      let (tm1', ty1') = lower_bound_tm_with_ty tm1 ty1 in *)
(*      let (tm2', ty2') = lower_bound_tm_with_ty tm2 ty2 in*)
(*      (TmAmpersand (finfo, tm1', tm2'), TyAmpersand (ty1', ty2'))*)
(*  | (TmAbs (finfo, binfo, ty, tm), TyLollipop (ty1, ty2)) -> *)
(*      let (tm', ty') = lower_bound_tm_with_ty tm ty in*)
(*      let (_, ty1') = lower_bound_tm_with_ty (TmVar (finfo, binfo), ty1) in')*)
(*      (TmAbs (finfo, binfo, ty', tm'), TyLollipop (ty1, ty2))*)
(*  | TyMonad (si, ty) -> (sugared_tm, TyMonad (si, (lower_bound_tm_with_ty ty)))*)
(*  | TyBang (si, ty) -> (sugared_tm, TyBang (si, (lower_bound_tm_with_ty ty)))*)

(* Lower the program into core representation. Debug information is kept the same. 
   There are three interesting cases:
     1. We see a float (constant). Replace this float, r, with a paired float (a, b) such that a >= 0 and b >= 0 and a-b = r.
     2. We see a lambda. If the variables bound by the lambda are constructed with floats, replace them with paired floats. Update the types accordingly.
     3. (maybe not necessary) We see a let ("regular sequencing"). Treat the same as the lambda case.
     4. (not necessary) We see a TmOp. Recursively replace this with paired operations.
   At every point that we see a lambda, we recursively replace each variable with a paired variable.
   Types and terms are lowered separately
 *)
let rec lower_term (program: term) : term = 
  match program with
  (* interesting cases *)
  | TmPrim (finfo, t) ->
      (match t with 
      | PrimTNum f -> 
          if f > 0.0 then 
            TmTens (finfo, TmPrim (finfo, PrimTNum f), TmPrim (finfo, PrimTNum 0.0))
          else 
            TmTens (finfo, TmPrim (finfo, PrimTNum 0.0), TmPrim (finfo, PrimTNum (Float.neg f)))
      | _ -> program)
  | TmAbs (finfo, binfo, ty, tm) -> 
      TmAbs (finfo, binfo, lower_ty ty, lower_term tm)
  | TmLet (finfo, binfo, ty_option, tm1, tm2) -> 
      (match ty_option with 
      | None -> TmLet (finfo, binfo, None, lower_term tm1, lower_term tm2)
      | Some ty -> TmLet (finfo, binfo, Some (lower_ty ty), lower_term tm1, lower_term tm2))

  | TmOp (finfo, op, tm) -> TmOp (finfo, op, lower_term tm)

  (* boring cases *)
  | TmTens (finfo, tm1, tm2) -> TmTens (finfo, lower_term tm1, lower_term tm2)
  | TmVar (finfo, x) -> TmVar (finfo, x) (* should have been recursievely replaced in the lambda/binding cases *)
  | TmTensDest (a, b, c, tm1, tm2) -> TmTensDest (a, b, c, lower_term tm1, lower_term tm2)
  | TmInl (finfo, tm) -> TmInl (finfo, lower_term tm)
  | TmInr (finfo, tm) -> TmInr (finfo, lower_term tm)
  | TmUnionCase (finfo, tm1, c, tm2, e, tm3) -> TmUnionCase (finfo, lower_term tm1, c, lower_term tm2, e, lower_term tm3)
  | TmRnd64 (finfo, tm) -> TmRnd64 (finfo, lower_term tm)
  | TmRnd32 (finfo, tm) -> TmRnd32 (finfo, lower_term tm)
  | TmRnd16 (finfo, tm) -> TmRnd16 (finfo, lower_term tm)
  | TmRet (finfo, tm) -> TmRet (finfo, lower_term tm)
  | TmApp (finfo, tm1, tm2) -> TmApp (finfo, lower_term tm1, lower_term tm2)
  | TmAmpersand (finfo, tm1, tm2) -> TmAmpersand (finfo, lower_term tm1, lower_term tm2)
  | TmAmp1 (finfo, tm) -> TmAmp1 (finfo, lower_term tm)
  | TmAmp2 (finfo, tm) -> TmAmp2 (finfo, lower_term tm)
  | TmBox (finfo, b, tm) -> TmBox (finfo, b, lower_term tm)
  | TmBoxDest (finfo, b, tm1, tm2) -> TmBoxDest (finfo, b, lower_term tm1, lower_term tm2)
  | TmLetBind (finfo, b, tm1, tm2) -> TmLetBind (finfo, b, lower_term tm1, lower_term tm2)

let cast_op_to_core (op: op) finfo : op_core =
  match op with
  | AddOp -> AddOpCore
  | SubOp -> 
      error_msg General finfo "Subtraction is not supported as a core operation."
  | MulOp -> MulOpCore
  | SqrtOp -> SqrtOpCore
  | DivOp -> DivOpCore
  | GtOp -> GtOpCore
  | EqOp -> EqOpCore

let rec cast_term_to_core (program: term) : term_core =
  match program with
  | TmPrim (finfo, x) -> TmPrimCore (finfo, x)
  | TmVar (finfo, x) -> TmVarCore (finfo, x)
  | TmLet (finfo, b, c, tm1, tm2) -> TmLetCore (finfo, b, c, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmAbs (finfo, b, c, tm) -> TmAbsCore (finfo, b, c, cast_term_to_core tm)
  | TmRnd16 (finfo, tm) -> TmRnd16Core (finfo, cast_term_to_core tm)
  | TmRnd32 (finfo, tm) -> TmRnd32Core (finfo, cast_term_to_core tm)
  | TmRnd64 (finfo, tm) -> TmRnd64Core (finfo, cast_term_to_core tm)
  | TmRet (finfo, tm) -> TmRetCore (finfo, cast_term_to_core tm)
  | TmOp (finfo, op, tm) -> TmOpCore (finfo, cast_op_to_core op finfo, cast_term_to_core tm) (* handle this later *)
  | TmBox (finfo, b, tm) -> TmBoxCore (finfo, b, cast_term_to_core tm)
  | TmAmp1 (finfo, tm) -> TmAmp1Core (finfo, cast_term_to_core tm) 
  | TmAmp2 (finfo, tm) -> TmAmp2Core (finfo, cast_term_to_core tm)
  | TmInr (finfo, tm) -> TmInrCore (finfo, cast_term_to_core tm)
  | TmInl (finfo, tm) -> TmInlCore (finfo, cast_term_to_core tm)
  | TmApp (finfo, tm1, tm2) -> TmAppCore (finfo, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmTens (finfo, tm1, tm2) -> TmTensCore (finfo, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmLetBind (finfo, b, tm1, tm2) -> TmLetBindCore (finfo, b, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmTensDest (finfo, b, c, tm1, tm2) -> TmTensDestCore (finfo, b, c, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmAmpersand (finfo, tm1, tm2) -> TmAmpersandCore (finfo, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmBoxDest (finfo, b, tm1, tm2) -> TmBoxDestCore (finfo, b, cast_term_to_core tm1, cast_term_to_core tm2)
  | TmUnionCase (finfo, tm1, c, tm2, e, tm3) -> TmUnionCaseCore (finfo, cast_term_to_core tm1, c, cast_term_to_core tm2, e, cast_term_to_core tm3)

let lower_term_to_core (program: term) : term_core =
  cast_term_to_core (lower_term program)

let lift_core_op_to_op (op: op_core) : op =
  match op with
  | AddOpCore -> AddOp
  | MulOpCore -> MulOp
  | SqrtOpCore -> SqrtOp
  | DivOpCore -> DivOp
  | GtOpCore -> GtOp
  | EqOpCore -> EqOp

let rec lift_core_to_term (core_program : term_core) : term = 
  match core_program with
  | TmPrimCore (finfo, x) -> TmPrim (finfo, x)
  | TmVarCore (finfo, x) -> TmVar (finfo, x)
  | TmLetCore (finfo, b, c, tm1, tm2) -> TmLet (finfo, b, c, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmAbsCore (finfo, b, c, tm) -> TmAbs (finfo, b, c, lift_core_to_term tm)
  | TmRnd16Core (finfo, tm) -> TmRnd16 (finfo, lift_core_to_term tm)
  | TmRnd32Core (finfo, tm) -> TmRnd32 (finfo, lift_core_to_term tm)
  | TmRnd64Core (finfo, tm) -> TmRnd64 (finfo, lift_core_to_term tm)
  | TmRetCore (finfo, tm) -> TmRet (finfo, lift_core_to_term tm)
  | TmOpCore (finfo, op, tm) -> TmOp (finfo, lift_core_op_to_op op, lift_core_to_term tm) (* handle this later *)
  | TmBoxCore (finfo, b, tm) -> TmBox (finfo, b, lift_core_to_term tm)
  | TmAmp1Core (finfo, tm) -> TmAmp1 (finfo, lift_core_to_term tm) 
  | TmAmp2Core (finfo, tm) -> TmAmp2 (finfo, lift_core_to_term tm)
  | TmInrCore (finfo, tm) -> TmInr (finfo, lift_core_to_term tm)
  | TmInlCore (finfo, tm) -> TmInl (finfo, lift_core_to_term tm)
  | TmAppCore (finfo, tm1, tm2) -> TmApp (finfo, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmTensCore (finfo, tm1, tm2) -> TmTens (finfo, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmLetBindCore (finfo, b, tm1, tm2) -> TmLetBind (finfo, b, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmTensDestCore (finfo, b, c, tm1, tm2) -> TmTensDest (finfo, b, c, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmAmpersandCore (finfo, tm1, tm2) -> TmAmpersand (finfo, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmBoxDestCore (finfo, b, tm1, tm2) -> TmBoxDest (finfo, b, lift_core_to_term tm1, lift_core_to_term tm2)
  | TmUnionCaseCore (finfo, tm1, c, tm2, e, tm3) -> TmUnionCase (finfo, lift_core_to_term tm1, c, lift_core_to_term tm2, e, lift_core_to_term tm3)
