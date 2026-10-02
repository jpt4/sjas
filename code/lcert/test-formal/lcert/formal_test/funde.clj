(ns lcert.formal-test.funde
  "Formal suite (ADR-0006), lcert.formal.funde: requiring the namespace kernel-checks its
  declarations; these tests check that the expected constants exist and
  that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.funde]))

(deftest f5-funde
  (testing "constants through the node of recSyn in the fundamental property of evalᴱ"
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
                denU_cv_from denU_cv adeqE_conv
                evalE_cast skels_let_ctx denU_lift_from denU_lift
                denU_proj1_fst denU_proj1_snd denU_proj0_fst denU_proj0_snd
                denU_projw_fst denU_projw_snd henv_ext2 den_let_body denU_let
                adeqE_let_finish adeqE_let1 adeqE_let0 adeqE_letw
                denU_bool cast_boolrec denU_ite adeqE_ite_cases adeqE_ite
                up_skel_eq upn_skel_eq up_usk_eq upn_usk_eq
                skel_subst_eq usk_subst_eq skel_subst1_eq usk_subst1_eq
                denU_ty_from denU_ty
                denU_elim adeqE_retarget adeqE_elim_cases adeqE_elim adeqE_elimB
                usk_skel_brs cast_app_brs denU_brs_at denU_lbl
                denU_case_fun denU_case adeqE_case
                adeqE_succ adeqE_sleaf adeqE_leaf adeqE_snode adeqE_node
                denU_bnil_app adeqE_bnil
                denU_bcons_zero denU_bcons_succ adeqE_bcons_zero adeqE_bcons_succ
                adeqE_bcons_at adeqE_bconsK adeqE_bcons
                cast_nat_id skels_rec_ctx denU_nat_id
                sSucc_usk sSucc_skel stepTy_usk stepTy_skel
                natrec_cong den_recN_open cast_iter denU_rec_step denU_recN
                erel_nat_rfl adeqE_iter adeqE_step_at adeqE_recN_at adeqE_recN
                sLeaf_usk sLeaf_skel sNode_usk sNode_skel sAt_usk sAt_skel
                leafTy_usk leafTy_skel nodeTy_usk nodeTy_skel
                y1Ty_usk y1Ty_skel y2Ty_usk y2Ty_skel
                erel_lbl_rfl erel_code_rfl adeqE_recs_leaf
                cast_usk_car nodeCtx_eq castYP_eq recsDen_leaf
                nodeEta_eq nodeRho_eq nodeUs_eq
                adeqE_recs_node adeqE_recs]]
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
