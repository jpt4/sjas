(ns lcert.formal-test.s52erased
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52erased]))

(deftest s52-erasure-induction
  (testing "the induction on Er, the usage dispatcher for lam, and the budget induction"
    (doseq [c '[S52JudgE s52e_lam s52_er_step s52_closed_step_false s52_closed_step_true
                s52_closed_step s52_closed_below_succ s52_closed_below_all s52_closed_all]]
      (is (b/has? c) (str c))))
  (testing "the erased usage-0 closure clause does not demand a related argument"
    ;; at usage 0 evalE applies the closure to star for EVERY carrier value; the
    ;; non-erasing relation (flag false) is not that clause.
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
        encTy :- (=> Exp Code), cap :- Nat]
      '(Eq Prop
         (S52 chkf dec encTy cap Bool.false (Exp.tPi U.u0 Exp.tEmpty Exp.tUnit) (List.nil Sk) Unit.unit
              (Sk.arr Sk.unit Sk.unit) RV.star (fn [x :- Unit] Unit.unit))
         (S52 chkf dec encTy cap Bool.true (Exp.tPi U.u0 Exp.tEmpty Exp.tUnit) (List.nil Sk) Unit.unit
              (Sk.arr Sk.unit Sk.unit) RV.star (fn [x :- Unit] Unit.unit)))
      '[(rfl)]))))
