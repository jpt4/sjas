(ns lcert.formal-test.selfjust
  "Formal suite (ADR-0006), lcert.formal.selfjust: λᶜᵉʳᵗ₀'s self-justification
  with no hypotheses — at the concrete checker Check decCert (F7), the
  calculus is consistent (T1), its checker accepts no refutation and no
  contradictory pair (Corollary 3.7), and it derives its own consistency
  propositions H° and H₁° at budget 0 (T2): Willard's Definition 3.4, both
  clauses."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.selfjust]))

(deftest self-justification
  (testing "the capstone, with no CheckSpec hypothesis"
    (doseq [nm '[self_justification consistent_concrete no_refutation_code]]
      (is (b/has? nm) (str nm))))
  (testing "T1 at the concrete checker, as a usable fact: no budget-3 refutation"
    (is (not (b/rejects? '[t :- Exp]
      '(Not (Rt (Check decCert) (thetaD 3) (thetaU 3) t Exp.tEmpty))
      '[(exact (consistent_concrete 3 t))]))))
  (testing "Corollary 3.7 at the concrete checker: the code of ⊥'s type is never accepted"
    (is (not (b/rejects? '[c :- Code]
      '(Not (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.true))
      '[(exact (no_refutation_code c))])))))
