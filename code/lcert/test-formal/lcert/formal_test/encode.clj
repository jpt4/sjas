(ns lcert.formal-test.encode
  "Formal suite (ADR-0006), lcert.formal.encode: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.encode]))
(deftest f7-encoding
  (testing "the encoding, E5, E1 (injectivity) and CheckSpec's satisfiability"
    (doseq [c '[encE encNat E5 decT roundtrip encE_inj base_enc checkspec_sat]]
      (is (b/has? c) (str c))))
  (testing "⌜0⌝ is c⊥ and ⌜1 ⊸ 0⌝ is neg ⌜1⌝, by computation"
    (is (not (b/rejects? '[] '(Eq Code (encE Exp.tEmpty) (Code.sl 15)) '[(rfl)])))
    (is (not (b/rejects? '[] '(Eq Code (encE (Exp.tPi U.u1 Exp.tUnit Exp.tEmpty)) (Code.sn 25 (Code.sl 16) (Code.sl 15))) '[(rfl)]))))
  (testing "distinct types have distinct codes: the usage is in the label"
    (is (b/rejects? '[] '(Eq Code (encE (Exp.tPi U.u1 Exp.tUnit Exp.tEmpty)) (encE (Exp.tPi U.uw Exp.tUnit Exp.tEmpty))) '[(rfl)]))))
