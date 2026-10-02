(ns lcert.formal-test.judgment
  "Formal suite (ADR-0006), lcert.formal.judgment: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.judgment]
            [lcert.formal.examples]))
(deftest f2c-rule-table
  (testing "the draft's derivation of not is a derivation"
    (is (b/has? 'not_typed)))
  (testing "clean: no branch-list pseudo-type anywhere (App's argument type must be clean)"
    (is (not (b/rejects? '[] '(Eq Bool (clean (Exp.tPi U.u1 Exp.tNat (Exp.tT (Exp.ite Exp.tt Exp.tt Exp.ff)))) Bool.true) '[(rfl)])))
    (is (not (b/rejects? '[P :- Exp, k :- Nat] '(Eq Bool (clean (Exp.tPi U.u1 (Exp.tBrs P k) Exp.tNat)) Bool.false) '[(rfl)])))
    (is (b/rejects? '[P :- Exp, k :- Nat] '(Eq Bool (clean (Exp.tBrs P k)) Bool.true) '[(rfl)])))
  (testing "a usage-0 variable cannot be used at runtime"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.u0 (List.nil U)) (Exp.var 0) Exp.tBool)
                    '[(exact (Rt.rVar chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.u0 (List.nil U)) 0 Exp.tBool U.u0 rfl rfl rfl rfl))]))))
