(ns lcert.formal-test.funde
  "Formal suite (ADR-0006), lcert.formal.funde: requiring the namespace kernel-checks its
  declarations; these tests check that the expected constants exist and
  that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.funde]))

(deftest f5-funde
  (testing "constants through pairs of the fundamental property of evalᴱ"
    (doseq [c '[denU AdeqE
                adeqE_star adeqE_tt adeqE_ff adeqE_zero adeqE_lbl
                nthUS some_injUS none_ne_someUS entryE_nz lookup_usk_head envE_at
                lookup_mp lookup_sk congr_uskSk denU_var var_carrier
                uskCtx_nth adeqE_var
                cast_app henv_prod usk_skel_pi0 cast_app_pi henv_ext
                denE_lam_arr denU_lam_cast denU_lam_body denU_body_env denU_lam0
                denU_lam_cast1 denU_lam_body1 denU_lam1
                denU_lam_castw denU_lam_bodyw denU_lamw
                adeqE_lam0 adeqE_lam1 adeqE_lamw
                cast_back den_at_eq cast_uskSk cast_square cast_usk_path denU_back
                denU_pi0_at denU_app_fun denU_app0 adeqE_app0
                denU_pi1_at denU_app_fun1 denU_app1 adeqE_app1
                denU_piw_at denU_app_funw denU_appw adeqE_appw
                cast_snd cast_fst denE_pair_prod cast_usk_back denU_subst_from denU_unsubst
                denU_sig0_snd denU_sig0_y denU_pair0_snd adeqE_pair0
                denU_sig1_snd denU_sig1_y denU_pair1_snd denU_sig1_fst adeqE_pair1
                denU_sigw_snd denU_sigw_y denU_pairw_snd denU_sigw_fst adeqE_pairw
                denU_cv_from denU_cv adeqE_conv]]
      (is (b/has? c) (str c))))
  (testing "tt denotes true, not false, at the carrier E reads"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), n :- Nat]
          '(Eq Bool
             (denU chkf dec encTy n (List.nil Exp) Exp.tt Exp.tBool Unit.unit)
             Bool.false)
          '[(rfl)]))
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool),
            dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
            encTy :- (=> Exp Code), n :- Nat]
          '(Eq Bool
             ((denU chkf dec encTy n (List.nil Exp)
                (Exp.lam U.u0 Exp.tUnit Exp.tt)
                (Exp.tPi U.u0 Exp.tUnit Exp.tBool) Unit.unit)
              Unit.unit)
             Bool.false)
          '[(rfl)]))))
