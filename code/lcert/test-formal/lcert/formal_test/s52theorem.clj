(ns lcert.formal-test.s52theorem
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52theorem]))

(deftest s52-theorem
  (doseq [c '[theorem52 theorem52_base theorem52e theorem52_both theorem52_concrete]]
    (is (b/has? c) (str c)))
  (testing "the safe trace has no abort node: Ok has no such rule"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp))))
        encTy :- (=> Exp Code)]
      '(Ok chkf dec encTy 0 (EvSrc.tm (List.nil RV) (Exp.abort Exp.tNat Exp.zero)) (RV.nat 0))
      '[(apply Ok.eAbort)])))
  (testing "the evaluator Ev does have it, so the safe trace is a genuine restriction"
    (is (b/has? 'Ev.eAbort)))
  (testing "the same for the erasing evaluator's safe trace"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp))))
        encTy :- (=> Exp Code)]
      '(OkE chkf dec encTy 0 (EvSrc.tm (List.nil RV) (Exp.abort Exp.tNat Exp.zero)) (RV.nat 0))
      '[(apply OkE.eAbort)]))))
