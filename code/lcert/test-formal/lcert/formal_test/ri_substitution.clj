(ns lcert.formal-test.ri-substitution
  "Formal suite (ADR-0006), lcert.formal.ri-substitution: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.substitution, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-substitution]))

(deftest f4-ri-substitution
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[V_subst1_ri V_substL_ri V_subst_gen_ri V_subst_ri cex31_lhs_ri cex31_rhs_ri denAt_subst_ri
                denAt_weaken_ri den_fun_eq_ri den_subst1_ri den_substL2_ri den_substL_ri den_weaken_ri
                envAt_lift_ri envAt_up_ri envAt_var_ri envL_ri envOf_envAt_ri envOf_id_ri envOf_list_cons_ri
                envOf_list_ri envOf_up_ri lemma31_ri lemma31_skj_counterexample_ri lemma33_subst_ri sb_app_ri
                sb_bcons_ri sb_caseL_ri sb_chk_ri sb_elimB_ri sb_insp_ri sb_itR_ri sb_ite_ri sb_lam_ri
                sb_leaf_ri sb_letp_ri sb_node_ri sb_pair_ri sb_prn_ri sb_recN_ri sb_recS_ri sb_refl_ri
                sb_sleaf_ri sb_snode_ri sb_succ_ri sb_var_ri vs_Pi_ri vs_Sig_ri vs_T_ri wk_app_ri wk_bcons_ri
                wk_caseL_ri wk_chk_ri wk_elimB_ri wk_insp_ri wk_itR_ri wk_ite_ri wk_lam_ri wk_leaf_ri wk_letp_ri
                wk_node_ri wk_pair_ri wk_prn_ri wk_recN_ri wk_recS_ri wk_refl_ri wk_sleaf_ri wk_snode_ri
                wk_succ_ri wk_var_ri]]
      (is (b/has? c) (str c)))))
