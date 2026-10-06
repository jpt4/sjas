(ns lcert.formal-test.s52prod
  "Theorem 5.2: dependent pairs and safe elimination."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52prod]))

(deftest s52-pair-cases
  (doseq [c '[s52_pair_second s52_pair s52_pair0e]]
    (is (b/has? c) (str c)))
  (testing "the second component must satisfy its truth type, even at usage zero"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(S52 chkf dec encTy cap Bool.true
             (Exp.tSig U.u0 Exp.tUnit Exp.tEmpty)
             (List.nil Sk) Unit.unit (Sk.prod Sk.unit Sk.unit)
             (RV.pair RV.star RV.star) (Prod.mk Unit.unit Unit.unit))
          '[(constructor) (exact RV.star) (constructor) (exact RV.star)
            (constructor) (rfl) (constructor) (exact True.intro) (constructor)]))))
