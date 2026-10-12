(ns lcert.formal-test.paaxioms
  (:require [lcert.formal.paaxioms :as pa]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest arithmetic-axiom-instances
  (doseq [nm '[pa_pad_closed pa_ax_e1 pa_ax_q1 pa_ax_q2 pa_ax_q3 pa_ax_q4 pa_ax_q5 pa_ax_q6]]
    (is (b/has? nm) (str nm)))
  (testing "each instance has the exact PAx formula in every Good environment"
    (doseq [[theorem nm args formula] pa/axiom-instances
            :let [term (symbol (str "paC_" nm)), fname (symbol (str "paF_" nm))
                  e (reduce (fn [f t] (list 'Exp.app f (list 'trT t 'ρ)))
                      (list 'paPad (list 'trF fname 'ρ) term) args)
                  params (into (if (= theorem 'pa_ax_e1)
                                 '[chkf :- (=> Code Code Bool), i :- Nat]
                                 '[chkf :- (=> Code Code Bool)])
                           '[D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)])
                  proof (if (= theorem 'pa_ax_e1)
                          (list theorem 'chkf 'i 'D 'ρ 'hg)
                          (list theorem 'chkf 'D 'ρ 'hg))]]
      (is (not (b/rejects? params
        (list 'Rt 'chkf 'D '(allW D) e (list 'trF formula 'ρ))
        [(list 'exact proof)])) (str theorem))))
  (testing "padding requires the right type, even for a closed constant"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat)]
      '(Rt chkf D (allW D) (paPad Exp.tEmpty paC_REFL) Exp.tEmpty)
      '[(exact (paC_REFL_at chkf D ρ))]))))
