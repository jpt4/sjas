(ns lcert.formal-test.model
  "Formal suite (ADR-0006), lcert.formal.model: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.model]))
(deftest f3e-statements-and-t2
  (doseq [c '[CheckSpec EnvSat Lemma_3_6 Theorem_1 Corollary_3_7 Theorem_2_H]]
    (is (b/has? c) (str c)))
  (testing "H° cannot be inhabited using r twice"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U))
                         (Exp.node (Exp.var 0) (Exp.lbl 0) (Exp.var 0) (Exp.leaf (Exp.lbl 0))) Exp.tR)
                    '[(exact (Rt.rNode chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) (consU U.u0 (List.nil U))
                               (consU U.u1 (List.nil U)) (consU U.u0 (List.nil U)) (Exp.var 0) (Exp.lbl 0) (Exp.var 0) (Exp.leaf (Exp.lbl 0))
                               (Rt.rVar chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) 0 Exp.tR U.u1 rfl rfl rfl rfl)
                               (Rt.rConst chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0) Exp.tLbl rfl rfl)
                               (Rt.rVar chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) 0 Exp.tR U.u1 rfl rfl rfl rfl)
                               (Rt.rLeaf chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0)
                                  (Rt.rConst chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0) Exp.tLbl rfl rfl))))]))))
