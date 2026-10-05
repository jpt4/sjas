(ns lcert.formal.selfjust
  "λᶜᵉʳᵗ₀'s self-justification, with no hypotheses (ADR-0006).

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
            [lcert.formal.certenc]))

;; T1 at the concrete checker: no budget n and term t give Θₙ ⊢ t :¹ 0.
(thm consistent_concrete [n :- Nat, t :- Exp]
  (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))
  (have t1 (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
             (=> (CheckSpec chkf dec encTy)
               (forall [n Nat] (forall [t Exp] (Not (Rt chkf (thetaD n) (thetaU n) t Exp.tEmpty))))))))
    Theorem_1_holds)
  (exact (t1 (Check decCert) (decOf decCert) encE (check_spec decCert) n t)))

;; Corollary 3.7 at the concrete checker: no code checks as a refutation, and
;; no two codes check as a type and its negation.
(thm cor37_concrete []
  (And (forall [c Code] (Not (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.true)))
       (forall [c1 Code] (forall [c2 Code] (forall [d Code]
         (Not (And (Eq Bool (Check decCert c1 d) Bool.true)
                   (Eq Bool (Check decCert c2 (Code.sn 25 d (Code.sl 15))) Bool.true)))))))
  (have c37 (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
              (=> (CheckSpec chkf dec encTy)
                (And (forall [c Code] (Not (Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.true)))
                     (forall [c1 Code] (forall [c2 Code] (forall [d Code]
                       (Not (And (Eq Bool (chkf c1 d) Bool.true) (Eq Bool (chkf c2 (Code.sn 25 d (Code.sl 15))) Bool.true)))))))))))
    Corollary_3_7_holds)
  (exact (c37 (Check decCert) (decOf decCert) encE (check_spec decCert))))

(thm no_refutation_code [c :- Code]
  (Not (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.true))
  (exact (And.left cor37_concrete c)))

;; Self-justification (T1 and T2, Willard's Definition 3.4), at Check decCert,
;; with no hypotheses.
(thm self_justification []
  (And (forall [n Nat] (forall [t Exp] (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))))
  (And (Rt (Check decCert) (List.nil Exp) (List.nil U) (Hterm) (Hcirc))
       (Rt (Check decCert) (List.nil Exp) (List.nil U) (H1term) (H1circ))))
  (exact (And.intro consistent_concrete
           (And.intro (Theorem_2_H (Check decCert)) (Theorem_2_H1 (Check decCert))))))
