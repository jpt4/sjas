(ns lcert.formal.selfjust
  "λᶜᵉʳᵗ₀'s self-justification, with no hypotheses beyond the trust base's
  metatheory (ADR-0006): like every theorem here, T1 is a consistency result
  *relative to* Lean 4's type theory as the Ansatz kernel implements it.

  Willard's Definition 3.4 (Willard2016) asks two things of a self-justifying
  system: (i) it proves a statement of its own consistency, and (ii) it is
  consistent. For λᶜᵉʳᵗ₀ these are T2 — closed inhabitants, at budget 0, of
  H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0 and of its pair form H₁° — and T1,
  no budget derives a refutation.

  The metatheory proves T1 (and Corollary 3.7) for every checker meeting the
  trust base CheckSpec (Theorem_1_holds, Corollary_3_7_holds: convcase.clj),
  and T2 for every checker (Theorem_2_H, Theorem_2_H1: model.clj). F7
  constructs one concrete, computing checker, Check decCert, and proves that
  it meets CheckSpec (check_spec), accepts the certificates (check_complete)
  and that the certificates it accepts can be held (check_complete_lbl).

  Here the pieces are put together at that checker: the calculus whose chk′
  denotes Check decCert is consistent, its checker accepts no refutation and
  no contradictory pair, and it derives H° and H₁° — every hypothesis
  discharged. What remains assumed is Lean's Init (Ansatz's bundled kernel
  environment) and nothing else."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.convcase]
            ;; Check decCert: the canonical certificate format
            [lcert.formal.certcanon]))

;; T1 at the concrete checker: no budget n and term t give Θₙ ⊢ t :¹ 0.
(thm consistent_concrete [n :- Nat, t :- Exp]
  (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))
  ;; ‹proof elided: lines 33–37 of the full file; kernel-checked›
  )

;; Corollary 3.7 at the concrete checker: no code checks as a refutation, and
;; no two codes check as a type and its negation.
;; ‹lemma cor37_concrete: statement and proof elided, lines 41–52›

;; ‹lemma no_refutation_code: statement and proof elided, lines 54–56›

;; Self-justification (T1 and T2, Willard's Definition 3.4), at Check decCert,
;; with no hypotheses beyond the metatheory.
(thm self_justification []
  (And (forall [n Nat] (forall [t Exp] (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))))
  (And (Rt (Check decCert) (List.nil Exp) (List.nil U) (Hterm) (Hcirc))
       (Rt (Check decCert) (List.nil Exp) (List.nil U) (H1term) (H1circ))))
  ;; ‹proof elided: lines 64–65 of the full file; kernel-checked›
  )
