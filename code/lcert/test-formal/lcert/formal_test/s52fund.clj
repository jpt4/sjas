(ns lcert.formal-test.s52fund
  "Theorem 5.2: elementary rules and the three impossible cases."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52fund]))

(deftest s52-elementary-cases
  (testing "the elementary cases preserve both the trace and S"
    (doseq [c '[s52_star s52_tt s52_ff s52_zero s52_lbl s52_var s52_conv
                s52_abort s52_h1 s52_H s52_chk_true]]
      (is (b/has? c) (str c))))
  (testing "a safe star cannot supply evidence of a false Boolean"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool]
          '(Result52 chkf dec encTy cap erasing (List.nil Sk) Unit.unit
             (List.nil RV) Exp.star (Exp.tT Exp.ff) Exp.star)
          '[(constructor) (exact RV.star) (constructor)
            (exact (trace52_eStar chkf dec encTy erasing cap (List.nil RV)))
            (constructor) (rfl) (rfl)]))))
