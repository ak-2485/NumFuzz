From Flocq Require Import Core.
From Coq Require Import ZArith Reals Psatz.
From mathcomp Require Import all_ssreflect.
From mathcomp Require Import boolp Rstruct.

Local Open Scope R_scope.

Variable beta : radix.

Variable fexp : Z -> Z.
Context {prop_exp1 : Valid_exp fexp}.
(* prop_exp2 is satisfied by floating-point formats 
with unbounded exponents and no underflow (FLX) *)
Context {prop_exp2 : negligible_exp fexp = None}.

Variable prec :  Z.
Notation ln  := (Rpower.ln).

Fact div_neg_pos :
  forall x y, x < 0 -> y < 0 -> 0 < x/y.
Proof.
move => x y Hx Hy. 
have Hx1 : 0 < -x by apply Ropp_neg => //.
have Hx2 : 0 < -y by apply Ropp_neg => //.
have H : x/y = - x / - y by rewrite Rdiv_opp_r Rdiv_opp_l; nra.
rewrite H; apply Rdiv_lt_0_compat => //.
Qed.

Fact round_preserves_sign_neg :
  forall rnd : R -> Z, Valid_rnd rnd -> 
  forall x, x < 0 -> round beta fexp rnd x < 0.
Proof.
move => rnd Hrnd x Hx.
have Hrx : round beta fexp rnd x <> 0%R.
{ apply: round_neq_0_negligible_exp => //. apply prop_exp2. nra. } 
case: (round_le beta _ _ x 0); 
 [left => // | rewrite (round_0 beta fexp _) =>//  | rewrite round_0 => //=].
Qed.

Fact round_preserves_sign_pos : 
forall rnd : R -> Z, Valid_rnd rnd -> 
  forall x, 0 < x -> 0 < round beta fexp rnd x.
move => rnd Hrnd x Hx.
have Hrx : round beta fexp rnd x <> 0%R.
{ apply: round_neq_0_negligible_exp => //. apply prop_exp2. nra. } 
case: (round_le beta _ _ 0 x); 
 [left => // | rewrite (round_0 beta fexp _) =>//  | rewrite round_0 => //=; nra].
Qed.

Lemma exp_error_model_conversion_aux : 
forall rnd : R -> Z, Valid_rnd rnd -> 
 forall (x b : R), 
  (0 < b) -> 
  (x <> 0 -> Rabs (ln (round beta fexp rnd x /x)) < b) ->
  ( 0 < round beta fexp rnd x / x) -> 
  exists (eps : R), (Rabs eps < b) /\ (round beta fexp rnd x = x * (exp eps)).
Proof.
move => rnd Hrnd x b Hb.
set (rx := round beta fexp rnd x).
case (Req_dec x 0) => Hx0.
{ exists 0%R; rewrite /rx Hx0 Rabs_R0 expR0 round_0; split => //; nra. }
move => Hx H1.
exists (ln (rx/x)); split => //; [ apply Hx => // | rewrite exp_ln => //; 
                             field_simplify => //].
Qed. 

Lemma exp_error_model_lt_conversion : 
forall rnd : R -> Z, Valid_rnd rnd ->  
forall (x b : R), 
  (0 < b) -> let RND := round beta fexp rnd in 
  (x <> 0 -> Rabs (ln (RND x /x)) < b) ->
  exists (eps : R), (Rabs eps < b) /\ (RND x = x * (exp eps)).
Proof.
move => rnd Hrnd x b Hb.
case (Req_dec x 0) => Hx0.
{ exists 0%R; rewrite  Hx0 Rabs_R0 expR0 round_0; split => //; nra. }
simpl; move => H. apply exp_error_model_conversion_aux => //. 
case: (Rdichotomy x 0) => Hx1 => //;
[ apply div_neg_pos => //; apply round_preserves_sign_neg => // |
apply Rdiv_lt_0_compat => //; apply round_preserves_sign_pos => // ].
Qed.

Fact ln_div : 
  forall x y, 0 < x -> 0 < y -> ln (x/y) = ln x - ln y.
Proof.
move => x y Hx Hy. rewrite Rdiv_def ln_mult => //.
rewrite ln_Rinv => //. apply Rinv_0_lt_compat => //.
Qed.

Fact ln_x_lt_x :
  forall x, x > 0 -> ln x < x.
Proof.
move => x Hx.
rewrite -{2}(ln_exp x). apply ln_increasing; try nra.
apply Rlt_trans with (1 + x); try nra.
apply exp_ineq1; nra.
Qed.

Lemma rnd_DN_lt_rel_prec: 
forall x, 
~ generic_format beta fexp x ->
0 < x -> ln (round beta fexp Zfloor x / x) < 0.
Proof.
move => x Hg Hx. rewrite ln_div => //.
apply Rlt_minus. apply ln_increasing.
apply round_preserves_sign_pos => //. 
apply valid_rnd_DN.
destruct (round_DN_UP_lt beta fexp x) => //.
apply round_preserves_sign_pos => //.
apply valid_rnd_DN.
Qed.

Fact Rabs_ln_sym : 
  forall x y, 0 < x -> 0 < y -> Rabs(ln(x/y)) = Rabs(ln(y/x)).
Proof.
move => x y Hx Hy. 
rewrite !ln_div => //.
apply Rabs_minus_sym.
Qed.

Lemma relative_prec_lt_conversion : 
  forall x y b, 0 < x ->  0 < y -> 0 < b -> 
              Rabs (x - y) < b -> Rabs (ln (x/y)) < b / x.
Proof.
move => x y b Hx Hy Hb H.
case: (Req_dec (x-y) 0) => H1.
{ admit . } 
case: (Rdichotomy (x - y) 0) => // H2.
{ rewrite Rabs_minus_sym in H. 
rewrite Rabs_pos_eq in H; [| nra].
rewrite Rabs_ln_sym => //. rewrite Rabs_pos_eq.
have Ha: y/x < b/x + 1. 
apply Rplus_lt_reg_r with (-1).
field_simplify ; try nra.
apply Rmult_lt_reg_r with x => //.
field_simplify; try nra.
apply ln_increasing in Ha.
apply Rlt_trans with (ln(b/x + 1)) => //.
rewrite -{2}(ln_exp (b/x)).
apply ln_increasing; 
  [apply Rplus_lt_0_compat; [apply Rdiv_lt_0_compat => //| nra]|].
rewrite Rplus_comm; apply exp_ineq1.
admit.
Admitted.

Lemma relative_prec_lt_ulp :
  forall rnd : R -> Z, Valid_rnd rnd ->
  forall x, x <> 0 -> 
            Rabs (ln (x/round beta fexp rnd x)) < ulp beta fexp x / Rabs (x).
Proof.
move => rnd Hrnd x Hx0.
case: (generic_format_EM beta fexp x) => Hx.
(* rnd x = x *)
{ rewrite round_generic => //. rewrite Rdiv_diag => //; 
rewrite ln_1 Rabs_R0 ulp_neq_0 => //.
apply Rdiv_lt_0_compat; [apply bpow_gt_0| apply Rabs_pos_lt => // ]. } 
(* rnd x neq x *)
have Ha :  Rabs (x - round beta fexp rnd x) < ulp beta fexp x.
{ rewrite Rabs_minus_sym.
apply error_lt_ulp => //. admit. } 
apply Rabs_lt_inv in Ha; destruct Ha.
case (Rdichotomy x 0) => // H1.
{ rewrite (Rabs_left x) => //. 
rewrite -(Ropp_involutive (x /round _ _ _ x)) -Rdiv_opp_l - Rdiv_opp_r.
apply relative_prec_lt_conversion ; try nra. admit. admit. } 
rewrite (Rabs_right x); try nra.
apply relative_prec_lt_conversion => //. admit. admit.  admit.
Admitted.



