(ns lcert.formal-test.syntax
  "Formal suite (ADR-0006), lcert.formal.syntax: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.syntax]))
(deftest f1-syntax
  (is (b/has? 'lift_var_below)))

(deftest f1-substitution
  (is (b/has? 'subst1_under_binder))
  (testing "substitution does not reach a bound variable"
    (is (b/rejects? '[u :- Exp, A :- Exp] '(= (subst1 u (Exp.lam U.u1 A (Exp.var 0))) (Exp.lam U.u1 (subst1 u A) u)) '[(rfl)]))))
