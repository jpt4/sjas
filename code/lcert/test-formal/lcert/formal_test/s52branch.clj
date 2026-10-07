(ns lcert.formal-test.s52branch
  "Theorem 5.2: only the branch selected by the carrier is run."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52branch]))

(deftest s52-boolean-cases
  (doseq [c '[s52_den_ite s52_den_elimB s52_ite_true s52_ite_false s52_ite
             s52_elimB_true s52_elimB_false s52_elimB]]
    (is (b/has? c) (str c)))
  (testing "a true scrutinee cannot use a safe false branch to skip abort"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat]
          '(Trace52 chkf dec encTy cap Bool.false
             (EvSrc.tm (List.nil RV)
               (Exp.ite Exp.tt (Exp.abort Exp.tUnit Exp.star) Exp.star)) RV.star)
          '[(apply Ok.eIteF) (constructor) (constructor)]))))

(deftest s52-label-cases
  (doseq [c '[s52_bnil s52_bcons_head s52_bcons_tail s52_bcons s52_caseL]]
    (is (b/has? c) (str c))))
