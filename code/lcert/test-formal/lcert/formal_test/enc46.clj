(ns lcert.formal-test.enc46
  "Formal suite (ADR-0006), lcert.formal.enc46: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.enc46]))

(deftest f4-enc46
  (testing "holed trees, Theorem 4.6's encoding hypothesis, and phantom trees as holed trees"
    (doseq [c '[HTree hfill hnodes holeIn hasHole isNodeH Enc46 toH
                toH_fill toH_nodes toH_hasHole toH_hole_lt toH_node]]
      (is (b/has? c) (str c))))
  (testing "a filled hole contributes the plugged tree, but no node of its own"
    (is (not (b/rejects? '[] '(Eq Nat (hnodes (HTree.hnode 3 (HTree.hhole 0) (HTree.hleaf 1))) 1) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Nat (hnodes (HTree.hnode 3 (HTree.hhole 0) (HTree.hleaf 1)))
                            (cnodes (hfill (fn [i :- Nat] (Code.sn 0 (Code.sl 0) (Code.sl 0))) (HTree.hnode 3 (HTree.hhole 0) (HTree.hleaf 1)))))
                    '[(rfl)])))
  (testing "a phantom ★ᵢ (i < J) is a hole; any other leaf is not"
    (is (not (b/rejects? '[] '(Eq Bool (holeIn 1 (toH 2 (Code.sn 5 (Code.sl 1) (Code.sl 7)))) Bool.true) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Bool (holeIn 7 (toH 2 (Code.sn 5 (Code.sl 1) (Code.sl 7)))) Bool.true) '[(rfl)])))
  (testing "Enc46 is satisfiable and not trivial"
    ;; enc46_sat: the checker that accepts nothing satisfies it (vacuously);
    ;; enc46_nontrivial: the checker that accepts everything does not
    (doseq [c '[enc46_sat enc46_nontrivial]] (is (b/has? c) (str c)))))
