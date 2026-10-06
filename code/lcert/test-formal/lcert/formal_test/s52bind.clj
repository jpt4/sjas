(ns lcert.formal-test.s52bind
  "Theorem 5.2: safe function application, including erased arguments."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52bind]))

(deftest s52-function-cases
  (doseq [c '[s52_pi_normal s52_lam s52_lam0e s52_app s52_app0e]]
    (is (b/has? c) (str c)))
  (testing "erasing Pi0 demands a safe application even when its domain is empty"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(S52 chkf dec encTy cap Bool.true (Exp.tPi U.u0 Exp.tEmpty Exp.tUnit)
             (List.nil Sk) Unit.unit (Sk.arr Sk.unit Sk.unit)
             (RV.clos (List.nil RV) (Exp.abort Exp.tUnit Exp.star))
             (fn [x :- Unit] Unit.unit))
          '[(intro x) (constructor) (exact RV.star) (constructor) (constructor)]))))
