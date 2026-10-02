(ns lcert.formal-test.usage
  "Formal suite (ADR-0006), lcert.formal.usage: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.usage]))
(deftest f1-usages
  (testing "the laws are kernel-checked constants"
    (doseq [c '[uadd_comm uadd_assoc umul_comm umul_assoc umul_distrib uadd_eq_zero umul_eq_zero
                umul_omega_one]]
      (is (b/has? c) (str c))))
  (testing "false variants are rejected"
    (is (b/rejects? '[] '(= (uadd U.u1 U.u1) U.u1) '[(rfl)]))
    (is (b/rejects? '[x :- U, y :- U] '(= (uadd x y) x) '[(cases x) (all_goals (cases y)) (all_goals (rfl))]))))
