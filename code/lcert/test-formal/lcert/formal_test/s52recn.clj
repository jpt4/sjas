(ns lcert.formal-test.s52recn
  "Theorem 5.2: safe natural-number recursion."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52recn]))

(deftest s52-natural-recursion
  (doseq [c '[s52_stepTy s52_iter s52_recN_step s52_recN]]
    (is (b/has? c) (str c)))
  (testing "a step which reaches abort has no safe recursion trace"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(Trace52 chkf dec encTy cap Bool.false
             (EvSrc.iter (List.nil RV) (Exp.abort Exp.tUnit Exp.star) 1 RV.star) RV.star)
          '[(apply Ok.eIterS) (constructor) (constructor)]))))
