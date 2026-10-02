(ns lcert.formal-test.section4
  "Formal suite (ADR-0006), lcert.formal.section4: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.section4]))
(deftest f4-section4
  (testing "Propositions 4.1 and 4.7"
    (doseq [c '[sn_not_le0 leaf_of_cnodes prop41_closed prop41_fun ConP p47term prop47]]
      (is (b/has? c) (str c))))
  (testing "a node is not a leaf: leaf_of_cnodes has content"
    (is (b/rejects? '[] '(Exists (fn [l :- Nat] (Eq Code (Code.sn 0 (Code.sl 0) (Code.sl 0)) (Code.sl l))))
                    '[(exact (leaf_of_cnodes (Code.sn 0 (Code.sl 0) (Code.sl 0)) (Nat.le_refl 0)))])))
  (testing "Con′ ⊸ H° holds but the converse direction's term does not have the swapped type"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (p47term) (Exp.tPi U.u1 (Hcirc) (ConP)))
                    '[(exact (prop47 chkf))]))))
