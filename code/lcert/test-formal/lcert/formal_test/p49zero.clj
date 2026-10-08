(ns lcert.formal-test.p49zero
  "Proposition 4.9 at depth zero; kernel acceptance and scope regressions."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.p49zero]))

(deftest bounded-consistency-zero
  (doseq [nm '[p49z_delta p49z_leaf_arm p49z_node_arm prop49_0 prop49_0_concrete
               p49z_depth_agrees]]
    (is (b/has? nm) (str nm)))
  (testing "the depth predicate agrees with section4c"
    (is (not (b/rejects? '[]
              '(Eq Exp (Exp.lam U.uw Exp.tSyn (p49z_depth (Exp.var 0))) (sh4_depth0))
              '[(exact p49z_depth_agrees)]))))
  (testing "the proved bounded type cannot be replaced by unbounded Con omega"
    (is (b/rejects? '[]
          '(Rt (Check decCert) (List.nil Exp) (List.nil U) p49z_term
             (Exp.tPi U.uw Exp.tSyn
               (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)))
          '[(exact prop49_0_concrete)])))
  (testing "depth zero distinguishes leaves and nodes"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
          '(Hd chkf (p49z_depth (Exp.snode (Exp.lbl 0) (cLeaf 0) (cLeaf 0))) Exp.tt)
          '[(exact (Hd.recSN chkf Exp.tBool Exp.tt Exp.ff (Exp.lbl 0) (cLeaf 0) (cLeaf 0)))]))))
