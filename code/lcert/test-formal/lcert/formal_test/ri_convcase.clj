(ns lcert.formal-test.ri-convcase
  "Formal suite (ADR-0006), lcert.formal.ri-convcase: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.convcase, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-convcase]))

(deftest f4-ri-convcase
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[F_conv_ri StepPack_ri StepV_ri V_pi_at_bcst_ri V_pi_at_cast_ri V_pi_at_ri V_pi_cod_ri
                V_pi_dom_ri V_sig_at_bcst_ri V_sig_at_cast_ri V_sig_at_ri V_sig_cod_ri V_sig_dom_ri V_tTF_gen_ri
                V_tTT_gen_ri V_tT_den_ri conv_all_ri cv_V_ri den_app_f_ri den_app_u_ri den_bcons_h_ri
                den_bcons_t_ri den_caseL_a_ri den_caseL_bs_ri den_chk_c_ri den_chk_d_ri den_coe_leaf_ri
                den_coe_prn_ri den_coe_sleaf_ri den_el_b_ri den_el_e_ri den_el_t_ri den_ig_abort_A_ri
                den_ig_abort_t_ri den_ig_caseL_P_ri den_ig_elimB_P_ri den_ig_h1_c_ri den_ig_h1_e1_ri
                den_ig_h1_e2_ri den_ig_h1_r_ri den_ig_h1_s_ri den_ig_insp_X_ri den_ig_itR_X_ri den_ig_lam_A_ri
                den_ig_letp_C_ri den_ig_node_d_ri den_ig_pair_S_ri den_ig_recN_P_ri den_ig_recS_P_ri
                den_ig_refl_e_ri den_insp_c_ri den_insp_r_ri den_insp_t1_ri den_insp_t2_ri den_itR_g_ri
                den_itR_h_ri den_itR_r_ri den_ite_e_ri den_ite_t_ri den_lam_t_ri den_letp_p_ri den_letp_t_ri
                den_nd_a_ri den_nd_r1_ri den_nd_r2_ri den_pair_a_ri den_pair_b_ri den_recN_n_ri den_recN_s_ri
                den_recN_z_ri den_recS_c_ri den_recS_tl_ri den_recS_tn_ri den_refl_r_ri den_sn_a_ri den_sn_c1_ri
                den_sn_c2_ri den_succ_cong_ri step_V_abort_ri step_V_app_ri step_V_bcons_ri step_V_bnil_ri
                step_V_caseL_ri step_V_chk_ri step_V_elimB_ri step_V_ff_ri step_V_h1_ri step_V_insp_ri
                step_V_itR_ri step_V_ite_ri step_V_lam_ri step_V_lbl_ri step_V_leaf_ri step_V_letp_ri
                step_V_node_ri step_V_of_step_ri step_V_pair_ri step_V_pi_ri step_V_prn_ri step_V_recN_ri
                step_V_recS_ri step_V_refl_ri step_V_ri step_V_sig_ri step_V_sleaf_ri step_V_snode_ri
                step_V_star_ri step_V_succ_ri step_V_tBool_ri step_V_tBrs_ri step_V_tDia_ri step_V_tEmpty_ri
                step_V_tLbl_ri step_V_tNat_ri step_V_tPi_ri step_V_tR_ri step_V_tSig_ri step_V_tSyn_ri
                step_V_tT_b_ri step_V_tT_nil_ri step_V_tT_pack_ri step_V_tT_ri step_V_tUnit_ri step_V_tt_ri
                step_V_var_ri step_V_zero_ri step_abort_ri step_app_ri step_bcons_ri step_bnil_ri step_caseL_ri
                step_chk_ri step_elimB_ri step_ff_ri step_h1_ri step_insp_ri step_itR_ri step_ite_ri step_lam_ri
                step_lbl_ri step_leaf_ri step_letp_ri step_nil_fl_ri step_nil_pack_ri step_node_ri step_pack_ri
                step_pair_ri step_prn_ri step_recN_ri step_recS_ri step_refl_D_ri step_refl_ri step_sleaf_ri
                step_snode_ri step_star_ri step_succ_ri step_tBool_ri step_tBrs_ri step_tDia_ri step_tEmpty_ri
                step_tLbl_ri step_tNat_ri step_tPi_ri step_tR_ri step_tSig_ri step_tSyn_ri step_tT_ri
                step_tUnit_ri step_tt_ri step_var_ri step_zero_ri]]
      (is (b/has? c) (str c)))))
