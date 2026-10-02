(ns lcert.formal-test.vweaken
  "Formal suite (ADR-0006), lcert.formal.vweaken: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.vweaken]))
(deftest f3l-v-weakening
  (testing "weakening for V: V(lift 1 c A) at an inserted environment is V(A)"
    (doseq [c '[vw_T vw_Pi vw_Sig V_lift_gen V_lift V_lift_fam V_lift2_fam skj_subst subOK_shift subOK_sSucc envOf_shift envOf_sSucc]]
      (is (b/has? c) (str c))))
  (testing "the lift matters: T(x) is not T(x) one level up, at an environment where they differ"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                         '(Not (V chkf dec encTy 0 (Exp.tT (Exp.var 0)) (List.cons Sk Sk.bool (List.cons Sk Sk.bool (List.nil Sk)))
                                  (Prod.mk Bool.false (Prod.mk Bool.true Unit.unit)) 0 Sk.unit Unit.unit))
                         '[(intro h) (exact (Bool.noConfusion h))])))))
