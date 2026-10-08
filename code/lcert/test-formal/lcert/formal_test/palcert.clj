(ns lcert.formal-test.palcert
  (:require [lcert.formal.palcert]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest all-pa-proofs-translate
  (doseq [nm '[pa_axiom_translation pa_lcert pa_refutation]]
    (is (b/has? nm) (str nm)))
  (testing "the main theorem needs no axiom-translation hypothesis"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, hp :- (PPrv p),
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF p ρ))))
      '[(exact (pa_lcert chkf p hp D ρ hg))]))))
  (testing "the refutation is closed at budget zero"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), hp :- (PPrv (PF.peq PT.pz (PT.ps PT.pz)))]
      '(Exists (fn [e :- Exp] (Rt chkf (thetaD 0) (thetaU 0) e Exp.tEmpty)))
      '[(exact (pa_refutation chkf hp))]))))
  (testing "a proof of reflexivity cannot supply the refutation premise"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), hp :- (PPrv (PF.peq PT.pz PT.pz))]
      '(Exists (fn [e :- Exp] (Rt chkf (thetaD 0) (thetaU 0) e Exp.tEmpty)))
      '[(exact (pa_refutation chkf hp))]))))
