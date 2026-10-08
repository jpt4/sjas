(ns lcert.formal-test.paassembly
  (:require [lcert.formal.paassembly]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest translation-assembly
  (doseq [nm '[pa_lcert_of_axioms paRefute_good pa_close_refutation
              pa_refutation_of_translation]]
    (is (b/has? nm) (str nm)))
  (testing "closing needs a supplied derivation of the translated false equation"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), e :- Exp,
        he :- (Rt chkf paRefuteD (allW paRefuteD) e
                (trF (PF.peq PT.pz (PT.ps PT.pz)) paRefuteR))]
      '(Rt chkf (thetaD 0) (thetaU 0) (paRefuteTerm e) Exp.tEmpty)
      '[(exact (pa_close_refutation chkf e he))]))))
  (testing "Nat evidence cannot replace evidence of 0=S0"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), e :- Exp,
        he :- (Rt chkf paRefuteD (allW paRefuteD) e Exp.tNat)]
      '(Rt chkf (thetaD 0) (thetaU 0) (paRefuteTerm e) Exp.tEmpty)
      '[(exact (pa_close_refutation chkf e he))]))))
