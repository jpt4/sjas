(ns lcert.formal-test.s52inspect
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52inspect]))

(deftest s52-inspect
  (doseq [c '[s52_insp_true s52_insp_false s52_insp_pick_false s52_insp_pick_true s52_insp]]
    (is (b/has? c) (str c)))
  (testing "star is not evidence for the untaken true branch"
    (is (b/rejects?
      '[dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat]
      '(S52 (fn [_ :- Code, _d :- Code] Bool.false) dec encTy cap Bool.true
         (chkT (Exp.var 0) (Exp.sleaf (Exp.lbl 0)))
         (List.cons Sk Sk.cert (List.nil Sk)) (Prod.mk (Code.sl 0) Unit.unit) Sk.unit RV.star Unit.unit)
      '[(constructor) (rfl) (rfl)]))))
