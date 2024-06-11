open Syntax

let infer_arg_type (ty : ty option) (t : term) (ctx : Ctx.context) : ty =
  match ty with
  | Some t -> t
  | None -> (
      match Ty_bi.type_of t ctx with
      | Right (ty, _sis) -> ty
      | Left _ -> failwith "Failed type inference of let expression.")

let rec convert (tree : term) (ctx : Ctx.context) : term =
  match tree with
  | TmLet (i, b_i, typ_o, t1, t2) ->
      let typ = infer_arg_type typ_o t1 ctx in
      let new_ctx = Ctx.extend_var b_i.b_name typ ctx in
      TmLet (i, b_i, Some typ, t1, convert t2 new_ctx)
  | TmVar _ -> tree
  | TmTens (i, t1, t2) -> TmTens (i, convert t1 ctx, convert t2 ctx)
  | TmTensDest (i, b_i1, b_i2, t1, t2) ->
      TmTensDest (i, b_i1, b_i2, convert t1 ctx, convert t2 ctx)
  | TmInl (i, t) -> TmInl (i, convert t ctx)
  | TmInr (i, t) -> TmInr (i, convert t ctx)
  | TmUnionCase (i, t1, b_i1, t2, b_i2, t3) ->
      TmUnionCase (i, convert t1 ctx, b_i1, convert t2 ctx, b_i2, convert t3 ctx)
  | TmPrim _ -> tree
  | TmRnd (i, t) -> TmRnd (i, convert t ctx)
  | TmRet (i, t) -> TmRet (i, convert t ctx)
  | TmApp (i, t1, t2) -> TmApp (i, convert t1 ctx, convert t2 ctx)
  | TmAbs (i, b_i, ty, t) -> TmAbs (i, b_i, ty, convert t ctx)
  | TmAmpersand (i, t1, t2) -> TmAmpersand (i, convert t1 ctx, convert t2 ctx)
  | TmAmp1 (i, t) -> TmAmp1 (i, convert t ctx)
  | TmAmp2 (i, t) -> TmAmp2 (i, convert t ctx)
  | TmBox (i, si, t) -> TmBox (i, si, convert t ctx)
  | TmBoxDest (i, b_i, t1, t2) ->
      TmBoxDest (i, b_i, convert t1 ctx, convert t2 ctx)
  | TmLetBind (i, b_i, t1, t2) ->
      TmLetBind (i, b_i, convert t1 ctx, convert t2 ctx)
  | TmOp (i, op, t) -> TmOp (i, op, convert t ctx)
