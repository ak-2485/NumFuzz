Require Import Relation_Definitions Morphisms.
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

  Instance trans_NonZeroSameSign : Transitive NonZeroSameSign.
  Proof.
    rewrite /NonZeroSameSign /Transitive => x y z H1 H2.
    case: H1 H2 => H3 H4; destruct H3; destruct H4; lra. Qed.

  Instance sym_NonZeroSameSign : Symmetric NonZeroSameSign.
  Proof.
    rewrite /NonZeroSameSign /Symmetric => x y H1.
    case: H1 => H2. lra. lra. Qed.

  (* NB: x is not reflexive *)

  Lemma lt0_NonZeroSameSign x y : x < 0 -> NonZeroSameSign x y -> y < 0.
  Proof. Admitted.

  Lemma gt0_NonZeroSameSign x y : 0 < x -> NonZeroSameSign x y -> 0 < y.
  Proof. Admitted.

  Lemma NonZeroSameSignMulGen : forall (a a' b b' : R),
    (NonZeroSameSign a a') -> (NonZeroSameSign b b') ->(NonZeroSameSign (a * b) (a' * b')).
  Proof. Admitted.

  (* TODO: prove that it is transitive + reflexive; derive the following from the above lemma *)

  Lemma NonZeroSameSignMul : forall (a b : R),
    forall k, k != 0 ->
              (NonZeroSameSign a b) -> (NonZeroSameSign (k * a) (k * b)).
  Proof. Admitted.

  Lemma NonZeroSameSignMulInv : forall (a b : R),
    forall k, k != 0 ->
              (NonZeroSameSign (k * a) (k * b) -> NonZeroSameSign a b).
  Proof. Admitted.

  Lemma NonZeroSameSignExp : forall (a b : R),
    forall k, (NonZeroSameSign a b) -> (NonZeroSameSign (a `^ k) (b `^ k)).
  Proof. Admitted.

  Lemma NonZeroSameSignExpInv : forall (a b : R),
    forall k, (NonZeroSameSign (a `^ k) (b `^ k) -> NonZeroSameSign a b).
  Proof. Admitted.

  Lemma NonZeroSameSignDivPos : forall (a a' : R),
    NonZeroSameSign (a) (a') -> 0 < a / a'.
  Proof. Admitted.

  Lemma le_mul_pos : forall (k a b : R), a <= b -> (`|k| * a <= `|k| * b). Admitted.
  Lemma norm_mul_split : forall (a b : R), `| a * b | = `| a | * `| b |. Admitted.
  Lemma factor_exp : forall (a a' k : R), (a `^ k / a' `^ k = (a / a') `^ k). Admitted.
End HelperLemmas.
