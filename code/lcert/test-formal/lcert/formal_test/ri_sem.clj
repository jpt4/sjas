(ns lcert.formal-test.ri-sem
  "Formal suite (ADR-0006), lcert.formal.ri-sem: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.sem, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-sem]))

(deftest f4-ri-sem
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[V_ri sem_abort_ri sem_app_ri sem_bcons_ri sem_bnil_ri sem_caseL_ri sem_chk_ri sem_elimB_ri
                sem_ff_ri sem_h1_ri sem_insp_ri sem_itR_ri sem_ite_ri sem_lam_ri sem_lbl_ri sem_leaf_ri
                sem_letp_ri sem_node_ri sem_pair_ri sem_prn_ri sem_recN_ri sem_recS_ri sem_refl_ri sem_sleaf_ri
                sem_snode_ri sem_star_ri sem_succ_ri sem_tBool_ri sem_tBrs_ri sem_tDia_ri sem_tEmpty_ri
                sem_tLbl_ri sem_tNat_ri sem_tPi_ri sem_tR_ri sem_tSig_ri sem_tSyn_ri sem_tT_ri sem_tUnit_ri
                sem_tt_ri sem_var_ri sem_zero_ri]]
      (is (b/has? c) (str c))))
  (testing "V(R) bounds the nodes and reads the labels of the print: sl 200 is outside it"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G)]
                    '(V_ri chkf dec encTy stdRI n Exp.tR G en 5 Sk.cert (Code.sl 200))
                    '[(exact (And.intro (Nat.zero_le 5) (Eq.refl$1 Bool.true)))]))
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G)]
                         '(V_ri chkf dec encTy stdRI n Exp.tR G en 5 Sk.cert (Code.sl 20))
                         '[(exact (And.intro (Nat.zero_le 5) (Eq.refl$1 Bool.true)))])))))
