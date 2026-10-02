(ns lcert.formal-test.erase
  "Formal suite (ADR-0006), lcert.formal.erase: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.erase]))
(deftest f5-erase
  (testing "erasure, the erasing evaluator, and the trace root"
    (doseq [c '[bad_abort bad_h1 bad_H bad_refl_unit bad_star
                isH_empty isH_unit
                usk_empty usk_unit usk_bool usk_t usk_pi0 usk_pi1 usk_sig0 usk_brs
                ebase_unit eve_star ok_star
                er_app0_shape er_pair0_shape er_total
                usk_skel e_star e_tt e_ff e_zero e_lbl ebase_dflt ebase_rel
                erdflt_arr0 erdflt_arrN erdflt_prod0 erdflt_prodN
                erdflt erdflt_ty dflt_cast erel_rv erel_car e_abort e_h1
                usks_nil usks_cons entryE_u0 entryE_u1 entryE_uw
                envE_nil envE_cons envE_cons0 envE_cons1 envE_consw
                usk_hd usk_hd_needs_wf
                nz_uadd nz_umul nz_vadd nz_vscale]]
      (is (b/has? c) (str c))))
  (testing "⋆ is not an abort, and a Π₀ type is not the runtime arrow"
    (is (b/rejects? '[] '(Eq Bool (badNode Exp.star) Bool.true) '[(rfl)]))
    (is (b/rejects? '[X :- Exp, Y :- Exp]
                    '(Eq USk (usk (Exp.tPi U.u0 X Y)) (USk.arrN (usk X) (usk Y)))
                    '[(rfl)]))
    (is (b/rejects? '[xs :- (List U)]
                    '(Eq Bool (nzAt (List.cons U U.u0 xs) 0) Bool.true)
                    '[(rfl)]))
    ;; The product default is (ff, ⋆), not (⋆, ⋆): Σ₀ of E cannot demand ⋆.
    (is (b/rejects? '[]
                    '(Eq RV (rdflt (Sk.prod Sk.bool Sk.unit)) (RV.pair RV.star RV.star))
                    '[(rfl)]))
    ;; Ill-typed β changes the usage skeleton, so usk_hd needs isTy.
    (is (b/rejects? '[]
                    '(Eq USk (usk (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star)) (usk Exp.tBool))
                    '[(rfl)]))))
