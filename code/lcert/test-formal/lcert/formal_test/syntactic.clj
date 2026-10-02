(ns lcert.formal-test.syntactic
  "Formal suite (ADR-0006), lcert.formal.syntactic: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.syntactic]))
(deftest f2d-lift-algebra
  (testing "structural lift laws needed by Lemmas 2.1 and 2.5 (by Codex)"
    (doseq [c '[lift_zero lift_comp skel_lift]]
      (is (b/has? c) (str c))))
  (testing "lifting is not the identity at a free variable"
    (is (b/rejects? '[] '(= (lift 1 0 (Exp.var 0)) (Exp.var 0)) '[(rfl)])))
  (testing "composition adds both displacements"
    (is (b/rejects? '[] '(= (lift 2 0 (lift 1 0 (Exp.var 0))) (lift 2 0 (Exp.var 0))) '[(rfl)]))))
