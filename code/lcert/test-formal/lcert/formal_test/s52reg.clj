(ns lcert.formal-test.s52reg
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52reg]))

(deftest s52-regularity
  (doseq [c '[regB Reg reg_formed reg_brs reg_brs_inv reg_lam reg_subst1 reg_lift2
              reg_stepTy reg_leafTy reg_nodeTy reg_gTy reg_hTy reg_chkT reg_negT reg_baseCode]]
    (is (b/has? c) (str c)))
  (testing "a branch-list type is not a formed type: only its motive's formation is regular"
    (is (b/rejects?
      '[G :- (List Sk), P :- Exp, k :- Nat]
      '(SkJ Bool.true G (Exp.tBrs P k) Sk.unit)
      '[(constructor)])))
  (testing "an arbitrary type is not regular"
    (is (b/rejects?
      '[G :- (List Sk), A :- Exp]
      '(Reg G A)
      '[(constructor)]))))
