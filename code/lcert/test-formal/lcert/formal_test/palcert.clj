(ns lcert.formal-test.palcert
  (:require [lcert.formal.palcert]
            [lcert.formal.paclosure]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(deftest a05-finite-support-closure
  (testing "the PA translation has a finite-support closed-line operator"
    (doseq [nm '[paMax paSupportT paSupportF paCloseN paCloseF]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects? '[]
      '(Eq Nat (paSupportF (PF.peq (PT.pv 2) PT.pz)) 3)
      '[(rfl)])))
    (is (not (b/rejects? '[]
      '(Eq Nat (paSupportF (PF.pall (PF.peq (PT.pv 0) PT.pz))) 0)
      '[(rfl)])))))

(deftest a05-closure-introduction
  (testing "a derivation in a finite Nat context can be closed by Nat lambdas"
    (doseq [nm '[paNatD paCloseLam paClose_intro]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), e :- Exp, A :- Exp,
        he :- (Rt chkf (paNatD 1) (allW (paNatD 1)) e A)]
      '(Rt chkf (List.nil Exp) (List.nil U)
        (paCloseLam 1 e) (paCloseN 1 A))
      '[(exact (paClose_intro chkf 1 e A he))])))))

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
