open Syntax
open Support.Error
open Support.Options

(* Assume that we do not have subtraction. 
   If we encounter a subtraction operation, fail loudly. 
   Otherwise, term_core contains the same information as term.*)
let lower_op_to_core (op: op) finfo : op_core =
  match op with
  | AddOp -> AddOpCore
  | SubOp -> 
      error_msg General finfo "Subtraction is not supported as a core operation."
  | MulOp -> MulOpCore
  | SqrtOp -> SqrtOpCore
  | DivOp -> DivOpCore
  | GtOp -> GtOpCore
  | EqOp -> EqOpCore

(* Lower a program of type term to core, assuming that 
   subtraction is not used. *)
let rec lower_term_to_core (program: term) : term_core =
  match program with
  | TmPrim (finfo, x) -> TmPrimCore (finfo, x)
  | TmVar (finfo, x) -> TmVarCore (finfo, x)
  | TmLet (finfo, b, c, tm1, tm2) -> TmLetCore (finfo, b, c, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmAbs (finfo, b, c, tm) -> TmAbsCore (finfo, b, c, lower_term_to_core tm)
  | TmRnd16 (finfo, tm) -> TmRnd16Core (finfo, lower_term_to_core tm)
  | TmRnd32 (finfo, tm) -> TmRnd32Core (finfo, lower_term_to_core tm)
  | TmRnd64 (finfo, tm) -> TmRnd64Core (finfo, lower_term_to_core tm)
  | TmRet (finfo, tm) -> TmRetCore (finfo, lower_term_to_core tm)
  | TmOp (finfo, op, tm) -> TmOpCore (finfo, lower_op_to_core op finfo, lower_term_to_core tm) (* handle this later *)
  | TmBox (finfo, b, tm) -> TmBoxCore (finfo, b, lower_term_to_core tm)
  | TmAmp1 (finfo, tm) -> TmAmp1Core (finfo, lower_term_to_core tm) 
  | TmAmp2 (finfo, tm) -> TmAmp2Core (finfo, lower_term_to_core tm)
  | TmInr (finfo, tm) -> TmInrCore (finfo, lower_term_to_core tm)
  | TmInl (finfo, tm) -> TmInlCore (finfo, lower_term_to_core tm)
  | TmApp (finfo, tm1, tm2) -> TmAppCore (finfo, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmTens (finfo, tm1, tm2) -> TmTensCore (finfo, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmLetBind (finfo, b, tm1, tm2) -> TmLetBindCore (finfo, b, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmTensDest (finfo, b, c, tm1, tm2) -> TmTensDestCore (finfo, b, c, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmAmpersand (finfo, tm1, tm2) -> TmAmpersandCore (finfo, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmBoxDest (finfo, b, tm1, tm2) -> TmBoxDestCore (finfo, b, lower_term_to_core tm1, lower_term_to_core tm2)
  | TmUnionCase (finfo, tm1, c, tm2, e, tm3) -> TmUnionCaseCore (finfo, lower_term_to_core tm1, c, lower_term_to_core tm2, e, lower_term_to_core tm3)

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
