(ns lcert.formal-test.s52itr
  "Theorem 5.2: certificate-fold methods must be safe."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52itr]))

(deftest s52-certificate-fold
  (doseq [c '[s52_l1s s52_lsuc s52_lift1_at s52_lift2_at s52_lift3_at s52_lift4_at
             s52_apply1_at s52_applyw_at s52_itr_leaf s52_itr_node s52_itr s52_itR]]
    (is (b/has? c) (str c)))
  (testing "applying the default function reaches abort and is not safe"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(Trace52 chkf dec encTy cap Bool.false
             (EvSrc.ap (rdflt (Sk.arr Sk.lbl Sk.unit)) (RV.lbl 0)) RV.star)
          '[(apply Ok.apClos) (constructor)]))))
