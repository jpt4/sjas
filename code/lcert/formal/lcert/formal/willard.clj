(ns lcert.formal.willard
  "A14: λᶜᵉʳᵗ₀ against Willard2016 Definition 3.4, at the concrete checker.

  Definition 3.4 calls (α, d) self-justifying when (i) one of its theorems
  states that d applied to α produces a consistent set of theorems, and
  (ii) α is in fact consistent.  The definition is informal: \"states\" is a
  reading of a sentence, not a formal relation, so it is not encoded here.
  What is formal:
  - `Hcirc_reads`: H°'s type is literally Π(r :₁ R). T(chk′ (print r) ⌜0⌝) ⊸ 0,
    where ⌜0⌝ is the literal of the code of the type 0, and chk′ is the
    calculus's own checker (the judgments below are at Check decCert);
  - `willard_def34`: clause (i)'s derivations (T2: closed budget-0
    inhabitants of H° and H₁°) and clause (ii) in two forms: no budget
    derives 0 (T1), and Check accepts no certificate of 0 (Corollary 3.7),
    which is also what H° reads as saying.
  Consistency here is relative to the metatheory, Lean's Init as the
  Ansatz kernel implements it (selfjust.clj)."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.selfjust]))

;; H°'s ⌜0⌝ (judgment.clj's cbot) is the literal term of 0's code.
(thm Hcirc_reads []
  (Eq Exp (Hcirc)
    (Exp.tPi U.u1 Exp.tR
      (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.prn (Exp.var 0))
                                     (codeTerm (encE Exp.tEmpty))))
        Exp.tEmpty)))
  (rfl))

(thm willard_def34 []
  (And
    (And (Rt (Check decCert) (List.nil Exp) (List.nil U) (Hterm) (Hcirc))
         (Rt (Check decCert) (List.nil Exp) (List.nil U) (H1term) (H1circ)))
    (And (forall [n Nat] (forall [t Exp]
           (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))))
         (forall [c Code]
           (Not (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.true)))))
  (exact (And.intro
    (And.intro (Theorem_2_H (Check decCert)) (Theorem_2_H1 (Check decCert)))
    (And.intro consistent_concrete no_refutation_code))))
