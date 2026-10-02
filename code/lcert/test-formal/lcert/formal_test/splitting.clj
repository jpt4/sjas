(ns lcert.formal-test.splitting
  "Formal suite (ADR-0006), lcert.formal.splitting: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.splitting]))
(deftest f3h-lemma-3-5
  (testing "Lemma 3.5: splitting, ω-contexts, 1-contexts, raising the bound"
    (doseq [c '[entry_split EnvSat_split entry_omega EnvSat_omega EnvSat_one EnvSat_mono]]
      (is (b/has? c) (str c))))
  (testing "the 0 summand of a usage-1 entry cannot take its footprint"
    (is (b/rejects? '[j :- Nat, P :- (=> Nat Prop)] '(=> (EntryOK (uadd U.u1 U.u0) j P) (EntryOK U.u0 j P))
                    '[(intro h) (exact rfl)])))
  (testing "a usage-1 entry is not usable at footprint 0: only ω entries are"
    (is (b/rejects? '[j :- Nat, P :- (=> Nat Prop)] '(=> (EntryOK U.u1 j P) (EntryOK U.u1 0 P))
                    '[(intro h) (exact h)]))))
