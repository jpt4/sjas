(ns lcert.formal-test.s52reflect
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52reflect]))

(deftest s52-reflect
  (doseq [c '[select52_total trace52_refl_ok s52_base_transfer s52_base_default
             tok52 s52_isH s52_refl_no s52_refl_yes s52_refl_nonH s52_refl]]
    (is (b/has? c) (str c)))
  (testing "the empty type has no related default, even at budget zero"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
        encTy :- (=> Exp Code)]
      '(S52 chkf dec encTy 0 Bool.false Exp.tEmpty (List.nil Sk) Unit.unit Sk.unit RV.star Unit.unit)
      '[(constructor)]))))
