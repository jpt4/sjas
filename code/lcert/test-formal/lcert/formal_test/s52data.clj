(ns lcert.formal-test.s52data
  "Theorem 5.2: safe data construction and finite labels."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52data]))

(deftest s52-data-cases
  (doseq [c '[s52_value_bool s52_value_nat s52_value_lbl s52_value_syn s52_value_cert
              s52_valid_lbl s52_valid_syn s52_valid_cert
              s52_succ s52_sleaf s52_snode s52_leaf s52_node s52_prn s52_chk]]
    (is (b/has? c) (str c)))
  (testing "S excludes an out-of-range code label even though Code admits it"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool]
          '(S52 chkf dec encTy cap erasing Exp.tSyn (List.nil Sk) Unit.unit
             Sk.syn (RV.code (Code.sl 100)) (Code.sl 100))
          '[(constructor) (rfl) (rfl)]))))
