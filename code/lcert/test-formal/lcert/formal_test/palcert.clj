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

(deftest a05-support-agreement
  (testing "translations depend only on PA variables below their support"
    (doseq [nm '[paMax_left paMax_right paAgree
                 trT_support trF_support]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects?
      '[p :- PF, ρ :- (=> Nat Nat), σ :- (=> Nat Nat),
        h :- (paAgree (paSupportF p) ρ σ)]
      '(Eq Exp (trF p ρ) (trF p σ))
      '[(exact (trF_support p ρ σ h))])))
    (is (b/rejects?
      '[]
      '(Eq Exp
         (trF (PF.peq (PT.pv 0) PT.pz) (fn [i :- Nat] 0))
         (trF (PF.peq (PT.pv 0) PT.pz) (fn [i :- Nat] 1)))
      '[(rfl)]))))

(deftest a05-finite-good-context
  (testing "a nonempty finite Nat context admits a total fallback assignment"
    (doseq [nm '[paNatD_nth paEnv paEnv_agree paEnv_good]]
      (is (b/has? nm) (str nm)))))

(deftest a05-positive-support-closure
  (testing "every PA proof closes when its support fits a nonempty Nat context"
    (is (b/has? 'pa_finite_lcert_pos))
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, hp :- (PPrv p),
        k :- Nat, hk :- (LT.lt 0 k), hs :- (LE.le (paSupportF p) k)]
      '(Exists (fn [e :- Exp]
        (Rt chkf (List.nil Exp) (List.nil U) e
          (paCloseN k (trF p (fn [i :- Nat] i))))))
      '[(exact (pa_finite_lcert_pos chkf p hp k hk hs))])))))

(deftest a05-substitution-support
  (testing "PA substitution reads only the free-variable support"
    (doseq [nm '[paAgreePT paSbT_support paSbF_support]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects?
      '[p :- PF, σ :- (=> Nat PT), τ :- (=> Nat PT),
        h :- (paAgreePT (paSupportF p) σ τ)]
      '(Eq PF (paSbF σ p) (paSbF τ p))
      '[(exact (paSbF_support p σ τ h))])))))

(deftest a05-closed-sentence-instantiation
  (testing "a zero-support formula is unchanged by substituting zero for an unused Nat"
    (doseq [nm '[paClosed_substPA paInst0_hyp paClosed_inst0_type]]
      (is (b/has? nm) (str nm)))))

(deftest a05-closed-sentence-translation
  (testing "a PA proof of a zero-support sentence yields a closed derivation"
    (doseq [nm '[paClosed_form1 pa_lcert_closed0 pa_lcert_closed0_F]]
      (is (b/has? nm) (str nm)))))

(deftest a05-all-proofs-close
  (testing "every PA proof has a derivation at its finite-support closure"
    (is (b/has? 'pa_lcert_closed))
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, hp :- (PPrv p)]
      '(Exists (fn [e :- Exp]
        (Rt chkf (List.nil Exp) (List.nil U) e (paCloseF p))))
      '[(exact (pa_lcert_closed chkf p hp))])))))

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
