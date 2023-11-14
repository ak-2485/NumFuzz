/* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*/
%{
open Syntax
open Support.FileInfo

let parser_error   fi = Support.Error.error_msg   Support.Options.Parser fi
(* let parser_warning fi = Support.Error.message   1 Support.Options.Parser fi *)
(* let parser_info    fi = Support.Error.message   2 Support.Options.Parser fi *)

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
let nb_var   n = {b_name = n; b_type = BiVar;   b_size = -1; b_prim = false;}


(*
let mk_infix ctx info op t1 t2 =
  match Ctx.lookup_var op ctx with
      None      -> parser_error info "The primitive infix operator %s is not in scope" op
    | Some(v,_) -> TmApp(info, TmApp(info, TmVar(info, v), t1), t2)
*)

(* This takes a *reversed* list of arguments *)
let rec mk_prim_app_args info v arglist = match arglist with
  | []        -> v
  | (o :: op) -> TmApp(info, (mk_prim_app_args info v op), o)

let mk_prim_app ctx info prim arglist =
  match Ctx.lookup_var prim ctx with
    None      -> parser_error info "Primitive %s is not in scope" prim
  | Some(v,_) -> mk_prim_app_args info (TmVar(info, v)) (List.rev arglist)

let mk_prim_app_ty_args _ v arglist = match arglist with
  | []        -> v
  | _ -> v

let mk_prim_ty_app ctx info prim arglist =
  match Ctx.lookup_var prim ctx with
    None      -> parser_error info "Primitive %s is not in scope" prim
  | Some(v,_) -> mk_prim_app_ty_args info (TmVar(info, v)) (List.rev arglist)

let mk_lambda info bi ty term = TmAbs(info, bi, ty, term)


let rec list_to_term l body = match l with
    []                    -> body
  | (ty, n, i) :: tml -> TmAbs (i, nb_var n, ty, list_to_term tml body)

let from_args_to_term arg_list body = (list_to_term arg_list body)

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
%token <Support.FileInfo.info> AT
%token <Support.FileInfo.info> ADD
%token <Support.FileInfo.info> AMP
%token <Support.FileInfo.info> AND
%token <Support.FileInfo.info> ARROW
%token <Support.FileInfo.info> COLON
%token <Support.FileInfo.info> CONS
%token <Support.FileInfo.info> COMMA
%token <Support.FileInfo.info> DOLLAR
%token <Support.FileInfo.info> LBRACE
%token <Support.FileInfo.info> QUESTION
%token <Support.FileInfo.info> SEMI
%token <Support.FileInfo.info> SEMIM
%token <Support.FileInfo.info> RBRACE
%token <Support.FileInfo.info> EQUAL
%token <Support.FileInfo.info> HAT
%token <Support.FileInfo.info> BEQUAL
%token <Support.FileInfo.info> DBLARROW
%token <Support.FileInfo.info> SUB
%token <Support.FileInfo.info> MUL
%token <Support.FileInfo.info> DIV
%token <Support.FileInfo.info> LPAREN
%token <Support.FileInfo.info> RPAREN
%token <Support.FileInfo.info> LT
%token <Support.FileInfo.info> GT
%token <Support.FileInfo.info> LBRACK
%token <Support.FileInfo.info> RBRACK
%token <Support.FileInfo.info> PIPE
%token <Support.FileInfo.info> OR
%token <Support.FileInfo.info> BANG
%token <Support.FileInfo.info> EM
%token <Support.FileInfo.info> LOLLIPOP
%token <Support.FileInfo.info> TRUE
%token <Support.FileInfo.info> FALSE
%token <Support.FileInfo.info> INF
%token <Support.FileInfo.info> INL
%token <Support.FileInfo.info> INR
%token <Support.FileInfo.info> FUZZY
%token <Support.FileInfo.info> FUN
%token <Support.FileInfo.info> UNIONCASE
%token <Support.FileInfo.info> LISTCASE
%token <Support.FileInfo.info> NUMCASE
%token <Support.FileInfo.info> OF
%token <Support.FileInfo.info> FOLD
%token <Support.FileInfo.info> UNFOLD
%token <Support.FileInfo.info> MU
%token <Support.FileInfo.info> LET
%token <Support.FileInfo.info> TYPEDEF
%token <Support.FileInfo.info> SAMPLE
%token <Support.FileInfo.info> FUNCTION
%token <Support.FileInfo.info> PRIMITIVE
%token <Support.FileInfo.info> SET
%token <Support.FileInfo.info> BAG
%token <Support.FileInfo.info> IF
%token <Support.FileInfo.info> THEN
%token <Support.FileInfo.info> ELSE
%token <Support.FileInfo.info> PRINT
%token <Support.FileInfo.info> EOF
%token <Support.FileInfo.info> NUM
%token <Support.FileInfo.info> STRING
%token <Support.FileInfo.info> SIZE
%token <Support.FileInfo.info> SENS
%token <Support.FileInfo.info> TYPE
%token <Support.FileInfo.info> PACK
%token <Support.FileInfo.info> WITH
%token <Support.FileInfo.info> IN
%token <Support.FileInfo.info> FOR
%token <Support.FileInfo.info> UNPACK
%token <Support.FileInfo.info> FUZZ
%token <Support.FileInfo.info> FUZZB
%token <Support.FileInfo.info> PRIMITER
%token <Support.FileInfo.info> FORALL
%token <Support.FileInfo.info> EXISTS
%token <Support.FileInfo.info> LIST
%token <Support.FileInfo.info> DBLCOLON
%token <Support.FileInfo.info> NAT
%token <Support.FileInfo.info> CLIPPED
%token <Support.FileInfo.info> DBSOURCE
%token <Support.FileInfo.info> INT
%token <Support.FileInfo.info> DOT
%token <Support.FileInfo.info> SUCC
%token <Support.FileInfo.info> PROJ1
%token <Support.FileInfo.info> PROJ2
%token <Support.FileInfo.info> RND
%token <Support.FileInfo.info> OP
%token <Support.FileInfo.info> ADDOP
%token <Support.FileInfo.info> MULOP
%token <Support.FileInfo.info> DIVOP
%token <Support.FileInfo.info> SQRTOP

/* Identifier and constant value tokens */
%token <string Support.FileInfo.withinfo> ID
%token <float Support.FileInfo.withinfo> FLOATV
%token <float Support.FileInfo.withinfo> EPS
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

PrimSpec :
    STRINGV
      { $1 }

Term :
    ID MaybeType EQUAL Expr SEMI Term
      {
        fun ctx ->
          let ctx' = extend_var $1.v ctx in
          TmLet($1.i, (nb_var $1.v), $2 ctx, $4 ctx, $6 ctx')
      }
  | LET ID EQUAL Term SEMI Term
      { fun ctx ->
          let ctx' = extend_var $2.v ctx in
          TmLetBind($2.i, (nb_var $2.v), $4 ctx, $6 ctx')
      }
  | LET LPAREN ID COMMA ID RPAREN EQUAL Expr SEMI Term
      { fun ctx ->
        let ctx_x  = extend_var $3.v ctx   in
        let ctx_xy = extend_var $5.v ctx_x in
        TmTensDest($1, (nb_var $3.v), (nb_var $5.v), $8 ctx, $10 ctx_xy)
      }
  | LET LBRACK ID RBRACK EQUAL Expr SEMI Term
      { fun ctx ->
        let ctx_x  = extend_var $3.v ctx   in
        TmBoxDest($1, (nb_var $3.v), $6 ctx, $8 ctx_x)
      }
  | AExpr AExpr
      { fun ctx -> let e1 = $1 ctx in let e2 = $2 ctx in TmApp(tmInfo e1, e1, e2) }
  | FUNCTION ID Arguments MaybeType LBRACE Term RBRACE Term
      { fun ctx ->
        let (args, ctx_args) = $3 ctx                 in
        let ctx_let          = extend_var $2.v ctx    in
        let f_term           = from_args_to_term args ($6 ctx_args) in
        let f_type           = from_args_to_type args ($4 ctx_args) in
        TmLet($2.i, nb_var $2.v, f_type , f_term, $8 ctx_let)
      }
  | PROJ1 Term
      { fun ctx -> TmAmp1($1, $2 ctx)}
  | PROJ2 Term
      { fun ctx -> TmAmp1($1, $2 ctx)}
  | Expr
      { $1 }

Argument :
    LPAREN ID COLON Type RPAREN
      { fun ctx -> ([($4 ctx, $2.v, $2.i)], extend_var $2.v ctx) }

/*
   Arguments returns a tuple of (arg, ctx), where arg is the list of
   arguments .
*/
Arguments :
    Argument
      { $1 }
  | Argument Arguments
      { fun ctx ->
          let (l,  ctx')  = $1 ctx in
          let (l2, ctx'') = $2 ctx' in
          (l @ l2, ctx'')
      }

Expr :
      FTerm
      { $1 }

FTerm :
    STerm
      { $1 }

STerm :
    IF Expr THEN LBRACK Type RBRACK LBRACE Term RBRACE ELSE LBRACE Term RBRACE
      { fun ctx ->
        let if_then_spec = mk_prim_ty_app ctx $1 "if_then_else" [$5 ctx] in
        let arg_list    = [$2 ctx;
                           TmAmpersand($6,
                                       mk_lambda $7  (nb_var "thunk") (TyPrim PrimUnit) ($8  (extend_var "_" ctx)),
                                       mk_lambda $11 (nb_var "thunk") (TyPrim PrimUnit) ($12 (extend_var "_" ctx)));] in
        mk_prim_app_args $1 if_then_spec (List.rev arg_list)
      }
  | UNIONCASE Expr OF LBRACE INL LPAREN ID RPAREN DBLARROW Term PIPE INR LPAREN ID RPAREN DBLARROW Term RBRACE
      { fun ctx ->
        let ctx_l = extend_var $7.v  ctx in
        let ctx_r = extend_var $14.v ctx in
        TmUnionCase($1, $2 ctx, nb_var $7.v, $10 ctx_l, nb_var  $14.v, $17 ctx_r) }
  | FExpr
      { $1 }

FExpr :
    TFExpr
      { $1 }

/* Type application */
TFExpr:
    AExpr
      { $1 }

/* Sugar for n-ary tuples */
PairSeq:
    Term COMMA Term
      { fun ctx -> TmTens($2, $1 ctx, $3 ctx)  }
  | Term COMMA PairSeq
      { fun ctx -> TmTens($2, $1 ctx, $3 ctx)  }

AExpr:
    LPAREN RPAREN
      { fun _cx -> TmPrim ($1, PrimTUnit) }
  | ID
      { fun ctx -> TmVar($1.i, existing_var $1.i $1.v ctx) }
  | INL
      { fun ctx -> mk_prim_app ctx $1 "p_inl" []  }
  | INR
      { fun ctx -> mk_prim_app ctx $1 "p_inr" []  }
  | LPAREN Term RPAREN
      { $2 }
  | LPAREN PairSeq RPAREN
      { fun ctx -> $2 ctx }
  | LPAREN PIPE Term COMMA Term PIPE RPAREN
      { fun ctx -> TmAmpersand($1, $3 ctx, $5 ctx) }
  | LBRACK Term SensTerm RBRACK
      { fun ctx -> TmBox($1, $3 ctx, $2 ctx) }
  | RND Term
      { fun ctx -> TmRnd($1, $2 ctx) }
  | ADDOP Term
      { fun ctx -> TmOp($1, AddOp, $2 ctx) }
  | MULOP Term
      { fun ctx -> TmOp($1, MulOp, $2 ctx) }
  | DIVOP Term
      { fun ctx -> TmOp($1, DivOp, $2 ctx) }
  | SQRTOP Term
      { fun ctx -> TmOp($1, SqrtOp, $2 ctx) }
  | FUN LPAREN ID ColType RPAREN LBRACE Term RBRACE
      {
        fun ctx -> TmAbs($1, nb_var $3.v, $4 ctx, $7 (extend_var $3.v ctx ))
      }
  | STRINGV
      { fun _cx -> TmPrim($1.i, PrimTString $1.v) }
  | FLOATV
      { fun _cx -> TmPrim($1.i, PrimTNum $1.v) }


/* Sensitivities and sizes */
SensTerm :
  | LBRACE SensTerm RBRACE
      { fun ctx -> $2 ctx}
  | SensAtomicTerm
      { $1 }

SensAtomicTerm :
    ID
      { fun ctx -> let (v, _k) = existing_tyvar $1.i $1.v ctx in SiVar v
      }
  | FLOATV
      { fun _cx -> SiConst (Mlmpfr.make_from_float $1.v) }
  | EPS
      { fun _cx -> SiConst (Mlmpfr.make_from_float $1.v) }

ColSens :
  | COLON SensTerm
      { fun ctx -> ($2 ctx) }

MaybeType:
    {fun _ctx -> None}
  | COLON Type
      {fun ctx -> Some ($2 ctx)}

MaybeSens:
    {fun _ctx -> None}
  | SensTerm
      {fun ctx -> Some ($1 ctx)}

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
