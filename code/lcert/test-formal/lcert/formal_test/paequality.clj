(ns lcert.formal-test.paequality
  (:require [lcert.formal.paequality]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest closed-equality-combinators
  (doseq [c '[SYM TRANS CONGP CONGT], suffix ["_rt" "_form" "_at"]]
    (let [nm (symbol (str "paC_" c suffix))]
      (is (b/has? nm) (str nm))))
  (testing "symmetry works for an arbitrary environment and checker"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat)]
      '(Rt chkf D (allW D) (paPad (trF paF_SYM ρ) paC_SYM)
           (trF (PF.pall (PF.pall
             (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0))
                      (PF.peq (PT.pv 0) (PT.pv 1))))) ρ))
      '[(exact (paC_SYM_at chkf D ρ))]))))
  (testing "transitivity does not omit its equality premises"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
      '(Rt chkf (List.nil Exp) (List.nil U) paC_TRANS
        (trF (PF.pall (PF.pall (PF.peq (PT.pv 1) (PT.pv 0)))) ρ))
      '[(exact (paC_TRANS_rt chkf ρ))]))))
