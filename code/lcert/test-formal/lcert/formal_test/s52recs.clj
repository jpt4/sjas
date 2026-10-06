(ns lcert.formal-test.s52recs
  "Theorem 5.2: every recursive call and code method is safe."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52recs]))

(deftest s52-code-recursion
  (doseq [c '[s52_recs s52_recS_leaf s52_recS_node s52_recS]]
    (is (b/has? c) (str c)))
  (testing "code recursion cannot skip an aborting leaf method"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(Trace52 chkf dec encTy cap Bool.true
             (EvSrc.recs (List.nil RV) (Exp.abort Exp.tUnit Exp.star) Exp.star (Code.sl 0)) RV.star)
          '[(apply OkE.eRecSL) (constructor)]))))
