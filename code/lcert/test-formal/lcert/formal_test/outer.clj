(ns lcert.formal-test.outer
  "Formal suite (ADR-0006), lcert.formal.outer: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.outer]))
(deftest f3m-reflect-and-h1
  (testing "the cases of Lemma 3.6 that use the outer induction on the budget"
    (doseq [c '[tokEnvD tok_pair tok_transfer tok_sat wf_theta base_is_base base_leaf base_den chk_true
                den_refl_some denPrev_stable OuterIH F_refl den_negT F_h1 bool_case insp_chk insp_not den_insp_val F_insp]]
      (is (b/has? c) (str c))))
  (testing "the decoded program runs one level down: at cap 0 no budget m < 0 exists"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                      m :- Nat, t :- Exp]
                    '(Eq DenBody (denPrev chkf dec encTy 0 m t) (den chkf dec encTy m t))
                    '[(rfl)]))))
