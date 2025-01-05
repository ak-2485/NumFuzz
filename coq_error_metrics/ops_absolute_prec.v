Require Import vcfloat.VCFloat.
Import absolute_prec.


From mathcomp Require Import all_ssreflect ssralg ssrnum.
From mathcomp Require Import mathcomp_extra exp reals signed.
From mathcomp Require Import boolp Rstruct.

From mathcomp.algebra_tactics Require Import ring lra. 

Import Order.TTheory GRing.Theory Num.Def Num.Theory.

Local Open Scope ring_scope.

Context {R : realType}.

Section AbsPrec.

  Definition AbsPrec (a a' α : R) : Prop := α >= 0 -> `| (a - a')%R | <= α.    

End AbsPrec.

Notation "a ~ a' ; α" := (AbsPrec a a' α) (at level 99).

Fact abs_eq : forall (a b : R), a = b -> `|a| = `|b|. 
Proof. move => a b H1; by rewrite H1. Qed.

(* Properties from Olver Section 2.2 *)
Section ElementaryProperties.

  Variables (a a' α : R). 
  Hypothesis Halpha : 0 <= α.  
  
  Theorem Prop1 : (a ~ a' ; α) -> (a' ~ a ; α).
  Proof. rewrite /AbsPrec. move => H1 H2. 
         rewrite Num.Theory.distrC H1 => //=. Qed.

  Theorem Prop2 : forall (δ : R), (a ~ a' ; α) -> 0 <= α -> α <= δ -> (a ~ a' ; δ).
  Proof. rewrite /AbsPrec; move => δ H1 H2 H3 H4.
         rewrite (@le_trans _ _ α) => //. rewrite H1 => //=. Qed.

  Theorem Prop3 : forall (k : R), (a ~ a'; α) -> (a+k ~ a'+k ; α).
  Proof. rewrite /AbsPrec; move => k H1 H2. rewrite (abs_eq _ (a-a')) => //;
         [rewrite H1 => //| ring]. Qed.

  Fact abs_mul_eq : forall (k : R), `|k * a| = `|k| * `|a|. 
  Proof. move => k; by rewrite normrM. Qed.

  Theorem Prop4 : forall (k : R), a ~ a' ; α -> 0 <= α -> a*k ~ a'*k ; `|k|*α.
  Proof. rewrite /AbsPrec; move => k H1 H2 H3. rewrite (abs_eq _ (k * (a-a'))) => //;
         [rewrite normrM ler_pM => //=; rewrite H1 => //= | ring]. Qed.

  Lemma Prop4_1 :  a ~ a' ; α -> -a ~ -a' ; α.
  Proof. rewrite /AbsPrec; move => H1 H2. rewrite -(abs_eq (a' - a)) => //;
         [rewrite Prop1 => //| ring]. Qed.
                                                              
  Theorem Prop5 : forall (b b' β : R), 
      a ~ a' ; α ->  b ~ b' ; β -> 0 <= β -> a + b ~ a' + b' ; α + β.
  Proof. rewrite /AbsPrec; move => b b' β H1 H2 H3 H4.
         rewrite (@le_trans _ _ (normr (a-a') + normr (b -b'))) => //.
         rewrite -(abs_eq ((a-a')+(b-b'))) => //; 
          [rewrite ler_normD => // | ring ]. rewrite lerD => //; 
          [rewrite H1 => //| rewrite H2 => //]. Qed.

  Theorem Prop6 : forall (a'' δ : R ), 
      a ~ a' ; α -> a' ~ a'' ; δ -> 0 <= δ -> a ~ a'' ; α + δ.
  Proof. rewrite /AbsPrec. move => a'' δ H1 H2 H3 H4.
         rewrite -(abs_eq ((a-a') + (a' -a''))) => //; [|ring].
         rewrite (@le_trans _ _ (normr (a-a') + normr (a'-a''))) => //. 
         rewrite ler_normD => //. rewrite lerD => //; [rewrite H1|rewrite H2] => //=. Qed.

End ElementaryProperties.

Fact normr_inv : forall (x : R), `|1/x| = 1/`|x|.
Proof. move => x; have Hx : (0 <= x) \/ (x < 0) by nra. destruct Hx;
         [rewrite !ger0_norm => //|]. rewrite divr_ge0 => //.
       rewrite !ltr0_norm => //; [nra|]. rewrite ltr_ndivrMr; nra. Qed.

Fact divr_le : forall (x y : R), 0< y -> y <= x  -> 1/x <= 1/y.
Proof. move => x y H1 H2. rewrite ler_pdivrMr => //; [|nra].
rewrite div1r ler_pdivlMl => //; nra. Qed.

(* Theorems from Section 2.3 *)
Section MultDiv.

  Variables (a a' b b' α β : R). 

  Hypothesis Halpha : 0 <= α.
  Hypothesis Hbeta  : 0 <= β. 

  (* Theorem 2.1 *)
  Theorem APMul : 
    a ~ a' ; α -> b ~ b' ; β -> a * b ~ a' * b' ; `|a'| * β + `|b'| * α + α * β.
    Proof. rewrite /AbsPrec. move => H1 H2 H3. 
           set (u := a - a'); set (v := b - b').
           rewrite -(abs_eq (a'*v + (b'*u + u * v))) => //.
           rewrite (@le_trans _ _ (`|a' * v| + `|b' * u + u * v|)) => //.
           rewrite ler_normD => //. rewrite -addrA lerD => //. 
           rewrite normrM /v ler_pM => //. rewrite H2 => //. 
           rewrite (@le_trans _ _ (`|b' * u| + `|u * v|)) => //.
           rewrite ler_normD => //. rewrite lerD => //. rewrite normrM /u ler_pM => //.
           rewrite H1 => //. rewrite normrM /u /v ler_pM => //; 
           [rewrite H1 => // | rewrite H2 => //]. rewrite /u /v; ring. Qed.


   (* Theorem 2.2 *)
   (* Can we prove this without |b| > β? The hypothesis isn't used in the paper. *)
   Theorem APDiv : 
     a ~ a' ; α -> b ~ b' ; β -> `|b'| > β -> `|b| > β ->  
     a/b ~ a'/b' ;  (`|a'|*β + `|b'|*α)/(`|b'|*(`|b'| - β)). 
     Proof. rewrite /AbsPrec. move => H1 H2 H3 H4 H5.
            set (u := a - a'); set (v:= b - b').
            rewrite -(abs_eq ((b'*u - a'*v) *  (1/ (b' * (b' + v))))) => //; 
                                            [ |rewrite /u /v; field]. 
            rewrite normrM. rewrite ler_pM => //. rewrite distrC. 
            rewrite (@le_trans _ _ (`|a'*v | + `|b'*u|)) => //; 
                                            [rewrite ler_normB => //|].
            repeat rewrite normrM; rewrite lerD => //; rewrite ler_pM => //;
            rewrite /u /v; try rewrite H1 => //; try rewrite H2 => //.
            rewrite normr_inv normrM -(div1r (normr b' * _)) divr_le => //.
            rewrite mulr_gt0 => //; try nra. rewrite (@le_lt_trans _ _ β) => //.
            rewrite ler_pM => //; try nra. rewrite /v lerBlDl. 
            rewrite (abs_eq (b' + _) b) => //; [|ring]. rewrite -lerBlDr. 
            rewrite (@le_trans _ _ `|b' - b|) => //. rewrite lerB_dist => //. 
            rewrite Prop1 => //. apply /andP; split; rewrite -normr_gt0 (@le_lt_trans _ _ β) => //. Qed.

End MultDiv.

Section MultDiv2.

Variable (a a' α b b' β : R).
Hypothesis Halpha : 0 <= α.
Hypothesis Hbeta  : 0 <= β.

     Corollary APMul2 :
       a ~ a'; α -> b ~ b' ; β -> a * b ~ a' * b' ; `|a| * β + `|b| * α + α * β.
       Proof. move => H1 H2. apply: Prop1; apply: APMul => //; apply Prop1 => //. Qed.

     Corollary APDiv2 : 
     a ~ a' ; α -> b ~ b' ; β -> `|b'| > β -> `|b| > β ->  
     a/b ~ a'/b' ;  (`|a|*β + `|b|*α)/(`|b|*(`|b| - β)).
       Proof. move => H1 H2 H3 H4. apply: Prop1; apply APDiv => //; apply Prop1 => //. Qed.

End MultDiv2.



