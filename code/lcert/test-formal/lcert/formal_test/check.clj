(ns lcert.formal-test.check
  "Formal suite (ADR-0006), lcert.formal.check: decidable equality of codes
  and expressions, the first building block of the concrete checker (F7)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.check]))

(deftest f7-decidable-equality
  (testing "codeEq and expEq are sound and reflexive"
    (doseq [c '[codeEq codeEq_refl codeEq_sound expEq expEq_sound expEq_refl]]
      (is (b/has? c) (str c))))
  (testing "distinct expressions are not equal by expEq: tt vs ff"
    (is (not (b/rejects? '[] '(Eq Bool (expEq Exp.tt Exp.ff) Bool.false) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Bool (expEq Exp.tt Exp.ff) Bool.true) '[(rfl)]))))
