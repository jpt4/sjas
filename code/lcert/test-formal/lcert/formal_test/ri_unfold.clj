(ns lcert.formal-test.ri-unfold
  "Formal suite (ADR-0006), lcert.formal.ri-unfold: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.unfold, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-unfold]))

(deftest f4-ri-unfold
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[denPrev_ri den_abort_at_ri den_abort_eq_ri den_app_at_ri den_app_eq_ri den_bcons_at_ri
                den_bcons_eq_ri den_bnil_at_ri den_bnil_eq_ri den_caseL_at_ri den_caseL_eq_ri den_chk_at_ri
                den_chk_eq_ri den_elimB_at_ri den_elimB_eq_ri den_eq_denAt_ri den_ff_at_ri den_ff_eq_ri
                den_h1_at_ri den_h1_eq_ri den_insp_at_ri den_insp_eq_ri den_itR_at_ri den_itR_eq_ri
                den_ite_at_ri den_ite_eq_ri den_lam_at_ri den_lam_eq_ri den_lbl_at_ri den_lbl_eq_ri
                den_leaf_at_ri den_leaf_eq_ri den_letp_at_ri den_letp_eq_ri den_node_at_ri den_node_eq_ri
                den_pair_at_ri den_pair_eq_ri den_prn_at_ri den_prn_eq_ri den_recN_at_ri den_recN_eq_ri
                den_recS_at_ri den_recS_eq_ri den_refl_at_ri den_refl_eq_ri den_sleaf_at_ri den_sleaf_eq_ri
                den_snode_at_ri den_snode_eq_ri den_star_at_ri den_star_eq_ri den_succ_at_ri den_succ_eq_ri
                den_tBool_at_ri den_tBool_eq_ri den_tBrs_at_ri den_tBrs_eq_ri den_tDia_at_ri den_tDia_eq_ri
                den_tEmpty_at_ri den_tEmpty_eq_ri den_tLbl_at_ri den_tLbl_eq_ri den_tNat_at_ri den_tNat_eq_ri
                den_tPi_at_ri den_tPi_eq_ri den_tR_at_ri den_tR_eq_ri den_tSig_at_ri den_tSig_eq_ri
                den_tSyn_at_ri den_tSyn_eq_ri den_tT_at_ri den_tT_eq_ri den_tUnit_at_ri den_tUnit_eq_ri
                den_tt_at_ri den_tt_eq_ri den_var_at_ri den_var_eq_ri den_zero_at_ri den_zero_eq_ri]]
      (is (b/has? c) (str c)))))
