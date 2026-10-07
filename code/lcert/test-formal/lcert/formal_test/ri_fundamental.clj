(ns lcert.formal-test.ri-fundamental
  "Formal suite (ADR-0006), lcert.formal.ri-fundamental: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.fundamental, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-fundamental]))

(deftest f4-ri-fundamental
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[EnvSat_cons_ri EnvSat_omega_back_ri F_abort_ri F_app0_ri F_app1_ri F_app_ri F_appw_ri F_bcons_ri
                F_bnil_ri F_caseL_ri F_chk_ri F_const_ri F_elimB_ri F_itR_ri F_ite_ri F_lam0_ri F_lam1_ri
                F_lam_ri F_lamw_ri F_leaf_ri F_let0_ri F_let1_ri F_let_ri F_letw_ri F_node_ri F_pair0_ri
                F_pair1_ri F_pair_ri F_pairw_ri F_prn_ri F_recN_ri F_sleaf_ri F_snode_ri F_succ_ri F_var_ri
                Sound_ri V_l1s_ri V_lsuc_ri V_stepTy_ri bcons_succ_ri bcons_zero_ri coderec_inv_ri
                den_app_some_ri den_ff_bool_ri den_lbl_lbl_ri den_letp_some_ri den_tt_bool_ri den_zero_nat_ri
                omega_back_cons_ri var_sem_ri var_succ_ri var_zero_ri]]
      (is (b/has? c) (str c)))))
