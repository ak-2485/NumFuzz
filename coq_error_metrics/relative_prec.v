Require Import ErrorMetrics.lemmas.
Local Open Scope ring_scope.

Section RelPrec.

  Context {R : realType}.

  Definition RelPrec (a a' α : R) : Prop :=
    α >= 0 /\ NonZeroSameSign a a' /\
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
         move=> [H1 [H2 H3]].
         repeat split; auto.
         destruct H2. left. destruct H; split; auto.
         destruct H. right. destruct H3; split; auto.
         suff sym : ((`|ln (R:=R) (a' / a)|) = `|ln (R:=R) (a / a')|).
         rewrite sym => //.
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
         case: H2.
         move=> Ha.
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
         move=> del [H2 [H3 H4]] H5 H6. repeat split; auto.
         rewrite (@le_trans _ _ α) => //.
         rewrite (@le_trans _ _ α) => //.
         Qed.

  Theorem RPProp3 : forall (k : R), (a ~ a'; rp(α)) -> k != 0 -> (k * a ~ k * a' ; rp(α)).
  Proof. rewrite /RelPrec; move => k [H1 [H2 H3]] H4. repeat split; auto.
         apply (NonZeroSameSignMul _ _ k) => //.
         rewrite -mulf_div. rewrite divff => //. rewrite mul1r => //. Qed.

  Theorem RPProp4 : forall (k : R), (a ~ a' ; rp(α)) -> 0 <= α -> a `^ k ~ a' `^ k ; rp(`|k|*α).
  Proof. rewrite /RelPrec. move => k [H1 [H2 H3]] H4. repeat split; auto.
         rewrite mulr_ge0 => //.
         apply (NonZeroSameSignExp a a' k) => //.
         rewrite (factor_exp a a' k). rewrite ln_powR.
         rewrite norm_mul_split.
         suff key_rel : `|ln (R:=R) (a / a')| <= α.
         apply (le_mul_pos k _ _) => //. apply H3 => //.
  Qed.

  Theorem RPProp5 : forall (b b' β : R),
      a ~ a' ; rp(α) ->  b ~ b' ; rp(β) -> 0 <= β -> a * b ~ a' * b' ; rp(α + β).
  Proof. rewrite /RelPrec. move=> b b' β [H1 [H2 H3]] [H4 [H5 H6]] H7. repeat split; auto.
         rewrite addr_ge0 => //.
         apply NonZeroSameSignMulGen => //.
         suff sep_frac : (a * b / (a' * b') = (a / a') * (b / b')).
         rewrite sep_frac lnM.
         rewrite (@le_trans _ _ _ _ _ (ler_normD _ _)) => //.
         suff add_le : forall (a b c d : R), a <= b -> c <= d -> a + c <= b + d.
         rewrite add_le => //.
         move=> w x y z P1 P2. lra.
         apply NonZeroSameSignDivPos => //. apply NonZeroSameSignDivPos => //. lra. Qed.

  Lemma ln_dist_triangle :
    forall (x y z c d : R),
      0 < x -> 0 < y -> 0 < z -> 0 <= c -> 0 <= d ->
      `|ln (R:=R) (x / y)| <= c -> `|ln (R:=R) (y / z)| <= d ->
      `|ln (R:=R) (x / z)| <= c + d.
  Proof. move=> x y z c d xgt0 ygt0 zgt0 cge0 dge0 H1 H2.
         rewrite ln_div; auto. rewrite ln_div in H1; auto. rewrite ln_div in H2; auto.
         have Hceq: `|c| = c. apply ger0_norm => //. rewrite -Hceq. rewrite -Hceq in H1.
         have Hdeq: `|d| = d. apply ger0_norm => //. rewrite -Hdeq. rewrite -Hdeq in H2.
         have Hnormleq: `|(ln (R:=R) x - ln (R:=R) y) + (ln (R:=R) y - ln (R:=R) z)| <= `|ln (R:=R) x - ln (R:=R) y| + `|ln (R:=R) y - ln (R:=R) z|.
         apply ler_normD => //.
         have Haddleq: `|ln (R:=R) x - ln (R:=R) y| + `|ln (R:=R) y - ln (R:=R) z| <= `|c| + `|d|.
         apply lerD => //.
         have norm_simpl: `|ln (R:=R) x - ln (R:=R) z| =  `|(ln (R:=R) x - ln (R:=R) y) + (ln (R:=R) y - ln (R:=R) z)|.
         rewrite subrKA => //.
         rewrite -norm_simpl in Hnormleq.
         apply (@le_trans _ _ _ _ _ Hnormleq Haddleq). Qed.

  Theorem RPProp6 : forall (a'' δ : R ),
      a ~ a' ; rp(α) -> a' ~ a'' ; rp(δ) -> 0 <= δ -> a ~ a'' ; rp(α + δ).
  Proof. rewrite /RelPrec. move=> a'' δ [H1 [H2 H3]] [H4 [H5 H6]] H7. repeat split; auto.
         rewrite addr_ge0 => //.
         apply (@trans_NonZeroSameSign _ _ _ _ H2 H5).
         case: (@real_ltP _ a 0 _ _) => //= a_ltP.
         {
           have a'lt0: (a' < 0) by apply (lt0_NonZeroSameSign a).
           have a''lt0: (a'' < 0) by apply (lt0_NonZeroSameSign a').
           remember (- a) as b.
           remember (- a') as b'.
           remember (- a'') as b''.
           move: H3 H6.
           replace a with (- b) by lra.
           replace a' with (- b') by lra.
           replace a'' with (- b'') by lra.
           have blt0: (0 < b) by lra.
           have b'lt0: (0 < b') by lra.
           have b''lt0: (0 < b'') by lra.
           rewrite !divrNN.
           move=> H3 H6.
           apply (ln_dist_triangle b b' b'') => //.
         }
         rewrite /NonZeroSameSign in H2.
         have a_gt0: 0 < a by lra.
         have a'gt0: (0 < a') by apply (gt0_NonZeroSameSign a).
         have a''gt0: (0 < a'') by apply (gt0_NonZeroSameSign a').
         apply (ln_dist_triangle a a' a'') => //.
  Qed.

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
