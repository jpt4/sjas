(ns lcert.formal-test.paclosed
  (:require [lcert.formal.paclosed]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(deftest closed-proof-combinators
  (doseq [nm '[REFL CONGS STABEQ STABBOT Q1 Q2 Q3 Q4 Q5 Q6 COE]
          :let [term (symbol (str "paC_" nm))
                formula (symbol (str "paF_" nm))
                theorem (symbol (str "paC_" nm "_rt"))]]
    (is (b/has? term) (str term))
    (is (b/has? formula) (str formula))
    (is (b/has? theorem) (str theorem))
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
          (list 'Rt 'chkf '(List.nil Exp) '(List.nil U) term (list 'trF formula 'ρ))
          [(list 'exact (list theorem 'chkf 'ρ))])) (str theorem)))
  (testing "COE requires evidence of 0 = S0; it does not inhabit Empty"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
          '(Rt chkf (List.nil Exp) (List.nil U) paC_COE Exp.tEmpty)
          '[(exact (paC_COE_rt chkf (fn [i :- Nat] i)))]))))
