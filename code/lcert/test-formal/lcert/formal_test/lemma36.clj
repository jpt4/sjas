(ns lcert.formal-test.lemma36
  "Formal suite (ADR-0006), lcert.formal.lemma36: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.lemma36]))
(deftest f3n-lemma36-and-main-results
  (testing "Lemma 3.6 assembled, Theorem 1, Corollary 3.7 and Theorem 3 (Conv as a hypothesis)"
    (doseq [c '[ConvCase lemma36_step outer_zero outer_succ outer_all lemma36
                theorem1 bool_false_of cor37_refutation cor37_contradiction theorem3
                ConvAll paper_lemma36 paper_theorem1 paper_cor37 Theorem_2_H1]]
      (is (b/has? c) (str c))))
  (testing "Theorem 1 is not vacuous about derivability: Θ₀ ⊢ ⋆ : 1 is derivable"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                         '(Rt chkf (thetaD 0) (thetaU 0) Exp.star Exp.tUnit)
                         '[(exact (Rt.rConst chkf (List.nil Exp) (List.nil U) Exp.star Exp.tUnit rfl rfl))]))))
  (testing "Bcons now requires its motive's formation: without hP the rule does not apply"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), h :- Exp, t :- Exp,
                      hh :- (Rt chkf (List.nil Exp) (List.nil U) h (subst1 (Exp.lbl 0) Exp.tBool)),
                      ht :- (Rt chkf (List.nil Exp) (List.nil U) t (Exp.tBrs Exp.tBool 1))]
                    '(Rt chkf (List.nil Exp) (List.nil U) (Exp.bcons h t) (Exp.tBrs Exp.tBool 0))
                    '[(exact (Rt.rBcons chkf (List.nil Exp) (List.nil U) Exp.tBool 0 h t hh ht))]))))
