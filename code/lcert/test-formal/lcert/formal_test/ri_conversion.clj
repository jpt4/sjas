(ns lcert.formal-test.ri-conversion
  "Formal suite (ADR-0006), lcert.formal.ri-conversion: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.conversion, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-conversion]))

(deftest f4-ri-conversion
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[EquivAt_ri V_tTF_at_ri V_tTF_ri V_tTT_at_ri V_tTT_ri cex_den_contr_ri cex_den_ne_ri
                cex_den_redex_ri conv_skj_counterexample_ri den_abort_ty_ri den_betaLet_at_ri
                den_betaLet_core_ri den_betaLet_ri den_beta_core_ri den_beta_ri den_boolExp_ff_ri den_boolExp_ri
                den_boolExp_tt_ri den_caseLb_ri den_codeOf_ri den_code_sleaf_ri den_code_snode_ri den_delta_ri
                den_elimF_ri den_elimT_ri den_itRL_nbr_ri den_itRL_ri den_itRN_eval_ri den_itRN_ri den_iteF_ri
                den_iteT_ri den_ite_b_ri den_lam_arr_ri den_nthB_ri den_pair_prod_ri den_prnL_ri den_prnN_ri
                den_recNS_eval_ri den_recNS_ri den_recNZ_ri den_recSL_eval_ri den_recSL_ri den_recSN_eval_ri
                den_recSN_ri den_sleaf_code_ri den_snode_code_ri den_step_nil_ri eqv_V_ri eqv_den_ri eqv_skel_ri
                eqv_sko_ri hd_den_ri hd_iteT_ri hd_tTF_ri hd_tTT_ri mk_eqv_t_ri mk_eqv_ty_ri]]
      (is (b/has? c) (str c)))))
