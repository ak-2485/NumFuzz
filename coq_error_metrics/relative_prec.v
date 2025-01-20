Require Import ErrorMetrics.lemmas.
Local Open Scope ring_scope.

Section RelPrec.

  Context {R : realType}.

  Definition RelPrec (a a' α : R) : Prop :=
    α >= 0 -> NonZeroSameSign a a' ->
    `|ln(a / a')| <= α.

End RelPrec.

Notation "a ~ a' ; rp( α )" := (RelPrec a a' α) (at level 99).

(* Section 3.2. *)
Section RPElementaryProperties.

  Context {R : realType}.
  Variables (a a' α : R).
  Hypothesis Halpha : 0 <= α.

  Theorem RPProp1 : (a ~ a' ; rp(α)) -> (a' ~ a ; rp(α)).
  Proof. rewrite /RelPrec /NonZeroSameSign.
         move=> H1 H2 H3.
         suff sym : ((`|ln (R:=R) (a' / a)|) = `|ln (R:=R) (a / a')|).
         rewrite sym.
         apply: H1.
         done.
         destruct H3; auto; destruct H; try split; auto.
         suff inv_neg : (ln (R:=R) (a' / a) =  - 1 * ln (R:=R) (a / a')).
         rewrite inv_neg.
         suff neg_1_swap : ( - 1 * ln (R:=R) (a / a') = - ln (R:=R) (a / a')).
         rewrite neg_1_swap.
         apply: normrN.
         apply: mulN1r.
         rewrite - (GRing.invf_div a a').
         rewrite - ln_powR.
         rewrite powRN.
         rewrite powRr1.
         reflexivity.
         case: H3.
         move=> [Ha' Ha].
         apply: divr_ge0.
         by [lra].
         by [lra].
         move=> [Ha' Ha].
         suff temp: 0 <= (- a) / (- a') by lra.
         suff neg_a: 0 <= - a'.
         suff neg_a': 0 <= - a.
         apply: divr_ge0; lra.
         by [lra].
         by [lra].
         Qed.

  Theorem RPProp2 : forall (δ : R), (a ~ a' ; rp(α)) -> 0 <= α -> α <= δ -> (a ~ a' ; rp(δ)).
  Proof. rewrite /RelPrec.
         move=> del H2 H3 H4 H5 H6.
         rewrite (@le_trans _ _ α) => //. rewrite H2 => //=. Qed.

  Theorem RPProp3 : forall (k : R), (a ~ a'; rp(α)) -> k != 0 -> (k * a ~ k * a' ; rp(α)).
  Proof. rewrite /RelPrec; move => k H1 H2 H3 H4.
         rewrite (abs_eq _ (ln (a / a'))).
         apply H1 => //.
         apply (NonZeroSameSignMul _ _ k) => //.
         rewrite -mulf_div.
         rewrite divff => //.
         f_equal.
         lra. Qed.

  Theorem RPProp4 : forall (k : R), (a ~ a' ; rp(α)) -> 0 <= α -> a `^ k ~ a' `^ k ; rp(`|k|*α).
  Proof. rewrite /RelPrec. move => k H1 H2 H3 H4.
         rewrite factor_exp. rewrite ln_powR.
         rewrite norm_mul_split.
         suff key_rel : `|ln (R:=R) (a / a')| <= α.
         apply (le_mul_pos k _ _) => //.
         apply H1 => //=.
         Check NonZeroSameSignMul.
         apply (NonZeroSameSignExp a a' k) => //.
         give_up.
  Admitted.

  Theorem RPProp5 : forall (b b' β : R),
      a ~ a' ; rp(α) ->  b ~ b' ; rp(β) -> 0 <= β -> a * b ~ a' * b' ; rp(α + β).
  Proof. Admitted.

  Theorem RPProp6 : forall (a'' δ : R ),
      a ~ a' ; rp(α) -> a' ~ a'' ; rp(δ) -> 0 <= δ -> a ~ a'' ; rp(α + δ).
  Proof. Admitted.

End RPElementaryProperties.

(* Section 3.3 *)
Section RPAddSub.
  Context {R : realType}.
  Variables (a a' b b' α β : R).

  Variables (e : R).
  (* TODO: change with canonical def in mathcomp *)
  Parameter e_is_e : ln(e) = 1.

  Hypothesis Halpha : 0 <= α.

  (* Theorem 3.1 *)
  Theorem RPAdd : a ~ a' ; rp(α) -> b ~ b' ; rp(β) ->
                  a + b ~ a' + b'; rp(ln(a' * (e `^ α) +  b * (e `^ β) / (a' + b') )).
    Admitted.

  (* Theorem 3.2 *)
  Theorem RPSub : a ~ a' ; rp(α) -> b ~ b' ; rp(β) -> `|a'| * (e `^ -α) > `|b'| * (e `^ β) ->
                  a + b ~ a' + b'; rp(ln(a' * (e `^ α) -  b * (e `^ -β) / (a' - b') )).
    Admitted.

End RPAddSub.
