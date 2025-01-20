From mathcomp Require Export all_ssreflect ssralg ssrnum.
From mathcomp Require Export mathcomp_extra exp reals signed.
From mathcomp Require Export boolp Rstruct.

From mathcomp.algebra_tactics Require Export ring lra.

Export Order.TTheory GRing.Theory Num.Def Num.Theory.

Local Open Scope ring_scope.

Section HelperLemmas.
  Context {R : realType}.

  Fact abs_eq : forall (a b : R), a = b -> `|a| = `|b|.
  Proof. move => a b H1; by rewrite H1. Qed.

  Definition NonZeroSameSign (a b : R) : Prop :=
    (a > 0 /\ b > 0) \/ (a < 0 /\ b < 0).

  Lemma NonZeroSameSignMul : forall (a b : R),
    forall k, k != 0 ->
              (NonZeroSameSign (k * a) (k * b) -> NonZeroSameSign a b).
  Proof. Admitted.

  Lemma NonZeroSameSignExp : forall (a b : R),
    forall k, (NonZeroSameSign (a `^ k) (b `^ k) -> NonZeroSameSign a b).
  Proof. Admitted.


  Lemma le_mul_pos : forall (k a b : R), a <= b -> (`|k| * a <= `|k| * b). Admitted.
  Lemma norm_mul_split : forall (a b : R), `| a * b | = `| a | * `| b |. Admitted.
  Lemma factor_exp : forall (a a' b k : R), (a `^ k / a' `^ k = (a / a') `^ k). Admitted.
End HelperLemmas.
