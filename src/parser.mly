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
      None            -> parser_error fi "Identifier %s is unbound" id
    | Some (var, _bi) -> var

let existing_tyvar fi id ctx =
  match Ctx.lookup_tyvar id ctx with
      None            -> parser_error fi "Type %s is unbound" id
    | Some (var, bi)  -> (var, bi)

(* Wrap extend here in order to avoid mutually recursive
   dependencies *)
let extend_var id ctx =
  Ctx.extend_var id dummy_ty ctx


(* Create a new binder *)
let nb_var   n = {b_name = n; b_type = BiVar;  b_size = -1; b_prim = false;}

let rec list_to_term l body = match l with
    []                    -> body
  | (ty, n, i) :: tml -> TmAbs (i, nb_var n, ty, list_to_term tml body)

let from_args_to_term arg_list body = list_to_term arg_list body

let rec list_to_type l ret_ty = match l with
    []                        -> TyLollipop (TyPrim PrimUnit, ret_ty) (* Not yet allowed constant function *)
  | (ty, _n, _i) :: []    -> TyLollipop (ty, ret_ty)
  | (ty, _n, _i) :: tyl   -> TyLollipop (ty, list_to_type tyl ret_ty)

let from_args_to_type arg_list oty = match oty with
  | Some ty -> Some (list_to_type arg_list ty)
  | None -> oty


%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* Keyword tokens */
%token <Support.FileInfo.info> ADD
%token <Support.FileInfo.info> ADDOP
%token <Support.FileInfo.info> AMP
%token <Support.FileInfo.info> BANG
%token <Support.FileInfo.info> COLON
%token <Support.FileInfo.info> COMMA
%token <Support.FileInfo.info> DBLARROW
%token <Support.FileInfo.info> DIVOP
%token <Support.FileInfo.info> ELSE
%token <Support.FileInfo.info> EM
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
%token <Support.FileInfo.info> RND16
%token <Support.FileInfo.info> RND32
%token <Support.FileInfo.info> RND64
%token <Support.FileInfo.info> SEMI
(* %token <Support.FileInfo.info> SENS *)
%token <Support.FileInfo.info> SQRTOP
%token <Support.FileInfo.info> STRING
%token <Support.FileInfo.info> THEN
(* %token <Support.FileInfo.info> TRUE *)
%token <Support.FileInfo.info> UNIONCASE


/* Identifier and constant value tokens */
%token <string Support.FileInfo.withinfo> ID
%token <float Support.FileInfo.withinfo> FLOATV
%token <float Support.FileInfo.withinfo> EPS
%token <float Support.FileInfo.withinfo> EPS16
%token <float Support.FileInfo.withinfo> EPS32
%token <float Support.FileInfo.withinfo> EPS64
%token <string Support.FileInfo.withinfo> STRINGV

/* ---------------------------------------------------------------------- */
/* Fuzz grammar                                                           */
/* ---------------------------------------------------------------------- */

%start body
%type < Syntax.term > body
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition                                     */
/* ---------------------------------------------------------------------- */

body :
    Term EOF
      { $1 Ctx.empty_context }

Term :
  (* values *)
    Val
      { $1 }
  (* application *)
  | Val Val
      { fun ctx ->
        let e1 = $1 ctx in
        let e2 = $2 ctx in
        TmApp(tmInfo e1, e1, e2)
      }
  (* projections; Cartesian prod. elimination *)
  | PROJ1 Val
      { fun ctx -> TmAmp1($1, $2 ctx)}
  | PROJ2 Val
      { fun ctx -> TmAmp2($1, $2 ctx)}
  (* tensor product elimination *)
  | LET LPAREN ID COMMA ID RPAREN EQUAL Val SEMI Term
      { fun ctx ->
        let ctx_x  = extend_var $3.v ctx   in
        let ctx_xy = extend_var $5.v ctx_x in
        TmTensDest($1, (nb_var $3.v), (nb_var $5.v), $8 ctx, $10 ctx_xy)
      }
  (* case analysis *)
  | UNIONCASE Val OF LBRACE INL LPAREN ID RPAREN DBLARROW Term PIPE INR LPAREN ID RPAREN DBLARROW Term RBRACE
      { fun ctx ->
        let ctx_l = extend_var $7.v  ctx in
        let ctx_r = extend_var $14.v ctx in
        TmUnionCase($1, $2 ctx, nb_var $7.v, $10 ctx_l, nb_var  $14.v, $17 ctx_r) }
  (* sugar for conditionals *)
  | IF Val THEN LBRACE Term RBRACE ELSE LBRACE Term RBRACE
      { fun ctx ->
        let ctx_l = extend_var "unit" ctx in
        let ctx_r = extend_var "unit" ctx in
        TmUnionCase($1, $2 ctx, nb_var "unit" , $5 ctx_l, nb_var "unit" , $9 ctx_r)
      }
  (* co-monadic let-binder *)
  | LET LBRACK ID RBRACK EQUAL Val SEMI Term
      { fun ctx ->
        let ctx_x  = extend_var $3.v ctx   in
        TmBoxDest($1, (nb_var $3.v), $6 ctx, $8 ctx_x)
      }
  (* monadic let-binder *)
  | LET ID EQUAL Val SEMI Term
      { fun ctx ->
        let ctx' = extend_var $2.v ctx in
        TmLetBind($2.i, (nb_var $2.v), $4 ctx, $6 ctx')
      }
  (* pure let-binder *)
  | ID MaybeType EQUAL Term SEMI Term
      { fun ctx ->
        let ctx' = extend_var $1.v ctx in
        TmLet($1.i, nb_var $1.v, $2 ctx, $4 ctx, $6 ctx')
      }
  (* sugar for top level functions *)
  | FUNCTION ID Arguments MaybeType LBRACE Term RBRACE Term
      { fun ctx ->
        let (args, ctx_args) = $3 ctx                 in
        let ctx_let          = extend_var $2.v ctx    in
        let f_term           = from_args_to_term args ($6 ctx_args) in
        let f_type           = from_args_to_type args ($4 ctx_args) in
        TmLet($2.i, nb_var $2.v, f_type , f_term, $8 ctx_let)
      }
  (* primitive ops *)
  | ADDOP Val
      { fun ctx -> TmOp($1, AddOp, $2 ctx) }
  | MULOP Val
      { fun ctx -> TmOp($1, MulOp, $2 ctx) }
  | DIVOP Val
      { fun ctx -> TmOp($1, DivOp, $2 ctx) }
  | SQRTOP Val
      { fun ctx -> TmOp($1, SqrtOp, $2 ctx) }
  | GTOP Val
      { fun ctx -> TmOp($1, GtOp, $2 ctx) }
  | EQOP Val
      { fun ctx -> TmOp($1, EqOp, $2 ctx) }
  (* hacked sugar for n-ary applications; should fix with appropriate let-  binding *)
  | Term Val
      { fun ctx ->
        let e1 = $1 ctx in
        let e2 = $2 ctx in
        TmApp(tmInfo e1, e1, e2)
      }
  (* extra *)
  | LPAREN Term RPAREN
    { $2 }

Argument :
    LPAREN ID COLON Type RPAREN
      { fun ctx -> ([($4 ctx, $2.v, $2.i)], extend_var $2.v ctx) }

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

(* Sugar for n-ary tuples *)
PairSeq:
    Val COMMA Val
      { fun ctx -> TmTens($2, $1 ctx, $3 ctx)  }
  | Val COMMA PairSeq
      { fun ctx -> TmTens($2, $1 ctx, $3 ctx)  }

Val:
    LPAREN RPAREN
      { fun _cx -> TmPrim ($1, PrimTUnit) }
  | ID
      { fun ctx -> TmVar($1.i, existing_var $1.i $1.v ctx) }
  | INL Val
      { fun ctx -> TmInl($1, $2 ctx)  }
  | INR Val
      { fun ctx -> TmInr($1, $2 ctx)  }
  | LPAREN PairSeq RPAREN
      { fun ctx -> $2 ctx }
  | LT; v1 = Val; COMMA; v2 = Val; GT
      { fun ctx -> TmAmpersand($1, v1 ctx, v2 ctx) }
  | LBRACK Val SensTerm RBRACK
      { fun ctx -> TmBox($1, $3 ctx, $2 ctx) }
      
  | RND64 Val
      { fun ctx -> TmRnd64($1, $2 ctx) } (* CHECK WITH ARIEL / CHANGE EVENTUALLY *)
  | RND32 Val 
      { fun ctx -> TmRnd32($1, $2 ctx) }
  | RND16 Val 
      { fun ctx -> TmRnd16($1, $2 ctx) }    
//   | RND Val
//       { fun ctx -> TmRnd64($1, $2 ctx) } (* CHECK WITH ARIEL / CHANGE EVENTUALLY *)
  | RET Val
      { fun ctx -> TmRet($1, $2 ctx) }
  | FUN LPAREN ID ColType RPAREN LBRACE Term RBRACE
      {
        fun ctx -> TmAbs($1, nb_var $3.v, $4 ctx, $7 (extend_var $3.v ctx ))
      }
  | STRINGV
      { fun _cx -> TmPrim($1.i, PrimTString $1.v) }
  | FLOATV
      { fun _cx -> TmPrim($1.i, PrimTNum $1.v) }
  (* extra *)
  | LPAREN Val RPAREN
    { $2 }


/* Sensitivities and sizes */
SensTerm :
  | LBRACE SensTerm RBRACE
      { fun ctx -> $2 ctx}
  | SensAtomicTerm
      { $1 }

SensAtomicTerm :
    ID
      { fun ctx ->
        let (v, _k) = existing_tyvar $1.i $1.v ctx in SiVar v
      }
  | FLOATV
      { fun _cx -> SiConst ( $1.v) }
  | INF
      { fun _cx -> SiInfty  }
  | EPS
      { fun _cx -> SiConst ( $1.v) }
  | EPS16
      { fun _cx -> SiConst ( $1.v) }
  | EPS32 
      { fun _cx -> SiConst ( $1.v) }
  | EPS64
      { fun _cx -> SiConst ( $1.v) }

MaybeType:
    {fun _ctx -> None}
  | COLON Type
      {fun ctx -> Some ($2 ctx)}

(*
MaybeSens:
    {fun _ctx -> None}
  | SensTerm
      {fun ctx -> Some ($1 ctx)}
*)

ColType :
  | COLON Type
      { fun ctx -> ($2 ctx) }

Type :
    ComplexType
      { $1 }

ComplexType :
    AType ADD ComplexType
      { fun ctx -> TyUnion($1 ctx, $3 ctx) }
  | AType AMP ComplexType
      { fun ctx -> TyAmpersand($1 ctx, $3 ctx) }
  | AType LOLLIPOP ComplexType
      { fun ctx -> TyLollipop($1 ctx, $3 ctx) }
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
  | BANG LBRACK SensTerm RBRACK Type
      { fun ctx -> TyBang ($3 ctx, $5 ctx) }
  | EM LBRACK SensTerm RBRACK Type
      { fun ctx -> TyMonad ($3 ctx, $5 ctx) }
  | LPAREN TPairSeq RPAREN
      { fun ctx -> $2 ctx }
  | LT Type COMMA Type GT
      { fun ctx -> TyAmpersand($2 ctx, $4 ctx) }
