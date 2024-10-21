/* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: GNU GPL V3.
   See the LICENSE file for details on licensing.
*/
%{
open Syntax
open Support.FileInfo

let parser_error   fi = Support.Error.error_msg   Support.Options.Parser fi

  let dummy_ty  = TyPrim PrimUnit

(* look for a variable in the current context *)
let existing_var fi id ctx =
  match Ctx.lookup_var id ctx with
      None              -> parser_error fi "Identifier %s is unbound" id
    | Some (var, _bi)   -> var

let existing_tyvar fi id ctx =
  match Ctx.lookup_tyvar id ctx with
      None            -> parser_error fi "Type %s is unbound" id
    | Some (var, bi)  -> (var, bi)

(* return Var or DVar *)
let var_or_dvar fi id ctx dctx = 
  match Ctx.lookup_var id ctx with
      None              -> 
        (match Ctx.lookup_var id dctx with
          None              -> parser_error fi "Identifier %s is unbound" id
        | Some (var, _bi)   -> TmDVar(fi, var))
    | Some (var, _bi)   -> TmVar(fi, var)

(* Wrap extend here in order to avoid mutually recursive
   dependencies *)
let extend_var id ctx =
  Ctx.extend_var id dummy_ty ctx

let extend_var_ty id ty ctx =
  Ctx.extend_var id ty ctx

(* Create a new binder *)
let nb_var  n  = {b_name = n; b_type = BiVar;  b_size = -1; b_prim = false;}
let nb_var_ty  n ty : binder_info = {b_name = n; b_type = ty;  b_size = -1; b_prim = false;}

(*
let rec list_to_term l body = match l with
    []                    -> body
  | (ty, n, i) :: tml -> TmAbs (i, nb_var n, ty, list_to_term tml body) *)

(*let from_args_to_term arg_list body = list_to_term arg_list body*)

(*
let rec list_to_type l ret_ty = match l with
    []                        -> TyLollipop (TyPrim PrimUnit, ret_ty) (* Not yet allowed constant function *)
  | (ty, _n, _i) :: []    -> TyLollipop (ty, ret_ty)
  | (ty, _n, _i) :: tyl   -> TyLollipop (ty, list_to_type tyl ret_ty)

let from_args_to_type arg_list oty = match oty with
  | Some ty -> Some (list_to_type arg_list ty)
  | None -> oty *)


%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* Keyword tokens */
%token <Support.FileInfo.info> ADD
%token <Support.FileInfo.info> ADDOP
%token <Support.FileInfo.info> COLON
%token <Support.FileInfo.info> COMMA
%token <Support.FileInfo.info> DBLARROW
%token <Support.FileInfo.info> DIVOP
%token <Support.FileInfo.info> DLET
%token <Support.FileInfo.info> ELSE
%token <Support.FileInfo.info> EQUAL
%token <Support.FileInfo.info> EQOP
%token <Support.FileInfo.info> EOF
(* %token <Support.FileInfo.info> FALSE *)
%token <Support.FileInfo.info> FUNCTION
%token <Support.FileInfo.info> FUN
%token <Support.FileInfo.info> GT
%token <Support.FileInfo.info> GTOP
%token <Support.FileInfo.info> IF
%token <Support.FileInfo.info> INL
%token <Support.FileInfo.info> INF 
%token <Support.FileInfo.info> INR
%token <Support.FileInfo.info> LBRACE
%token <Support.FileInfo.info> LBRACK
%token <Support.FileInfo.info> LET
%token <Support.FileInfo.info> LOLLIPOP
%token <Support.FileInfo.info> LPAREN
%token <Support.FileInfo.info> LT
%token <Support.FileInfo.info> MULOP
%token <Support.FileInfo.info> NUM
%token <Support.FileInfo.info> BOOL
%token <Support.FileInfo.info> OF
%token <Support.FileInfo.info> PIPE
%token <Support.FileInfo.info> PROJ1
%token <Support.FileInfo.info> PROJ2
%token <Support.FileInfo.info> RBRACE
%token <Support.FileInfo.info> RBRACK
%token <Support.FileInfo.info> RET
%token <Support.FileInfo.info> RPAREN
(*Mixed precision rounding *)
%token <Support.FileInfo.info> SEMI
(* %token <Support.FileInfo.info> SENS *)
%token <Support.FileInfo.info> STRING
%token <Support.FileInfo.info> SUBOP
%token <Support.FileInfo.info> THEN
%token <Support.FileInfo.info> TICK
(* %token <Support.FileInfo.info> TRUE *)
%token <Support.FileInfo.info> UNIONCASE


/* Identifier and constant value tokens */
%token <string Support.FileInfo.withinfo> ID
%token <string Support.FileInfo.withinfo> D_ID
%token <float Support.FileInfo.withinfo> FLOATV
%token <string Support.FileInfo.withinfo> STRINGV

/* ---------------------------------------------------------------------- */
/* Fuzz grammar                                                           */
/* ---------------------------------------------------------------------- */

%start body
%type <Ctx.context * Ctx.context * Syntax.term > body
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition                                     */
/* ---------------------------------------------------------------------- */

body :
    // TODO: make discrete arguments optional
    LBRACE TyArguments RBRACE TICK LBRACE TyArguments RBRACE Term EOF
      { 
        let (args, ctx_args) = ($2 Ctx.empty_context) in
        let (dargs, ctx_dargs) = ($6 Ctx.empty_context) in
        (ctx_args, ctx_dargs, $8 ctx_args ctx_dargs)
      }

Term :
  (* values *)
    Val
      { $1 }
  (* tensor product elimination *)
  | DLET LPAREN ID COMMA ID RPAREN EQUAL Term SEMI Term
      { fun ctx dctx ->
        let dctx_x  = extend_var $3.v dctx   in
        let dctx_xy = extend_var $5.v dctx_x in
        TmTensDDest($1, (nb_var $3.v), (nb_var $5.v), $8 ctx dctx, $10 ctx dctx_xy)
      }
  | LET LPAREN ID COMMA ID RPAREN EQUAL Term SEMI Term
      { fun ctx dctx ->
        let ctx_x  = extend_var $3.v ctx   in
        let ctx_xy = extend_var $5.v ctx_x in
        TmTensDest($1, (nb_var $3.v), (nb_var $5.v), $8 ctx dctx, $10 ctx_xy dctx)
      }
  (* case analysis *)
  | UNIONCASE Val OF LBRACE INL LPAREN ID RPAREN DBLARROW Term PIPE INR LPAREN ID RPAREN DBLARROW Term RBRACE
      { fun ctx dctx ->
        let ctx_l = extend_var $7.v  ctx in
        let ctx_r = extend_var $14.v ctx in
        TmUnionCase($1, $2 ctx dctx, nb_var $7.v, $10 ctx_l dctx, nb_var $14.v, $17 ctx_r dctx) }
  (* let expression *)
  | LET ID MaybeType EQUAL Term SEMI Term
      { fun ctx dctx ->
        let ctx' = extend_var $2.v ctx in
        TmLet($2.i, (nb_var $2.v), $3 ctx, $5 ctx dctx, $7 ctx' dctx)
      }
  (* primitive ops *)
  | ADDOP ID ID
      { fun ctx _dctx -> 
        let x = existing_var $2.i $2.v ctx in
        let y = existing_var $3.i $3.v ctx in
        TmAdd($1, x, y) }
  | SUBOP ID ID
      { fun ctx _dctx -> 
        let x = existing_var $2.i $2.v ctx in
        let y = existing_var $3.i $3.v ctx in
        TmSub($1, x, y) }
  | MULOP ID ID
      { fun ctx _dctx -> 
        let x = existing_var $2.i $2.v ctx in
        let y = existing_var $3.i $3.v ctx in
        TmMul($1, x, y) }
  | DIVOP ID ID
      { fun ctx _dctx -> 
        let x = existing_var $2.i $2.v ctx in
        let y = existing_var $3.i $3.v ctx in
        TmDiv($1, x, y) }
  (* extra *)
  | LPAREN Term RPAREN
    { $2 }

Argument :
    LPAREN ID COLON Type RPAREN
      { fun ctx -> ([($4 ctx, $2.v, $2.i)], extend_var $2.v ctx) }

TyArgument :
    LPAREN ID COLON Type RPAREN
      { fun ctx -> ([($4 ctx, $2.v, $2.i)], extend_var_ty $2.v ($4 ctx) ctx) }

(* Arguments returns a tuple (arg, ctx), where arg is the list of arguments. *)
Arguments :
    Argument
      { $1 }
  | Argument Arguments
      { fun ctx ->
          let (l,  ctx')  = $1 ctx in
          let (l2, ctx'') = $2 ctx' in
          (l @ l2, ctx'')
      }

TyArguments :
    TyArgument
      { $1 }
  | TyArgument TyArguments
      { fun ctx ->
          let (l,  ctx')  = $1 ctx in
          let (l2, ctx'') = $2 ctx' in
          (l @ l2, ctx'')
      }

(* Sugar for n-ary tuples *)
PairSeq:
    Val COMMA Val
      { fun ctx dctx -> TmTens($2, $1 ctx dctx, $3 ctx dctx)  }
  | Val COMMA PairSeq
      { fun ctx dctx -> TmTens($2, $1 ctx dctx, $3 ctx dctx)  }

Val:
    LPAREN RPAREN
      { fun _ctx _dctx -> TmPrim ($1, PrimTUnit) }
  | ID
      { fun ctx dctx -> var_or_dvar $1.i $1.v ctx dctx }
  | INL Type Val
      { fun ctx dctx -> TmInl($1, $2 ctx, $3 ctx dctx)  }
  | INR Type Val
      { fun ctx dctx -> TmInr($1, $2 ctx, $3 ctx dctx)  }
  | LPAREN PairSeq RPAREN
      { fun ctx dctx -> $2 ctx dctx}
  | STRINGV
      { fun _cx _dctx -> TmPrim($1.i, PrimTString $1.v) }
  | FLOATV
      { fun _cx _dctx -> TmPrim($1.i, PrimTNum $1.v) }
  (* extra *)
  | LPAREN Val RPAREN
    { $2 }

MaybeType:
    {fun _ctx -> None}
  | COLON Type
      {fun ctx -> Some ($2 ctx)}

ColType :
  | COLON Type
      { fun ctx -> ($2 ctx) }

Type :
    ComplexType
      { $1 }

ComplexType :
    AType ADD ComplexType
      { fun ctx -> TyUnion($1 ctx, $3 ctx) }
  | AType
      { $1 }

TPairSeq:
    Type COMMA Type
      { fun ctx -> TyTensor($1 ctx, $3 ctx) }
  | Type COMMA TPairSeq
      { fun ctx -> TyTensor($1 ctx, $3 ctx) }

AType :
    LPAREN Type RPAREN
      { $2 }
  | ID
      {	fun ctx -> let (v, _) = existing_tyvar $1.i $1.v ctx in
                   TyVar v
      }
  | NUM
      { fun _cx -> TyPrim PrimNum }
  | BOOL 
      { fun _cx -> TyUnion(TyPrim PrimUnit, TyPrim PrimUnit) }
  | STRING
      { fun _cx -> TyPrim PrimString }
  | LPAREN RPAREN
      { fun _cx -> TyPrim PrimUnit }
  | LPAREN TPairSeq RPAREN
      { fun ctx -> $2 ctx }
