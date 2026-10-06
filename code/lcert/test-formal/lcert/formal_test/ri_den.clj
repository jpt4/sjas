(ns lcert.formal-test.ri-den
  "Formal suite (ADR-0006), lcert.formal.ri-den: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.den, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-den]))

(deftest f4-ri-den
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[denAt_ri denT_ri denT_stable_ri denT_step_ri denT_top_ri den_abort_ri den_app_ri den_bcons_ri
                den_bnil_ri den_caseL_ri den_chk_ri den_elimB_ri den_ff_ri den_h1_ri den_insp_ri den_itR_ri
                den_ite_ri den_lam_ri den_lbl_ri den_leaf_ri den_letp_ri den_node_ri den_not_tt_ri den_pair_ri
                den_prn_ri den_recN_double_ri den_recN_ri den_recS_ri den_refl_ri den_ri den_sleaf_ri
                den_snode_ri den_star_ri den_succ_ri den_tBool_ri den_tBrs_ri den_tDia_ri den_tEmpty_ri
                den_tLbl_ri den_tNat_ri den_tPi_ri den_tR_ri den_tSig_ri den_tSyn_ri den_tT_ri den_tUnit_ri
                den_tt_ri den_var_ri den_zero_ri]]
      (is (b/has? c) (str c))))
  (testing "leaf ℓ builds sl (riLf ri ℓ): at phRI 1 the tree of leaf 0 is sl 1, not sl 0 (the same unfolding proves one and fails on the other)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), sg :- (=> Nat Code), hsg :- (PhOk 1 sg), n :- Nat, G :- (List Sk), en :- (HEnv G)]
                         '(Eq Code (den_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.leaf (Exp.lbl 0)) G Sk.cert en) (Code.sl 1))
                         '[(rw [(den_leaf_at_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.lbl 0) G Sk.cert en)]) (rw [(den_lbl_eq_ri chkf dec encTy (phRI 1 sg hsg) n 0)])])))
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), sg :- (=> Nat Code), hsg :- (PhOk 1 sg), n :- Nat, G :- (List Sk), en :- (HEnv G)]
                    '(Eq Code (den_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.leaf (Exp.lbl 0)) G Sk.cert en) (Code.sl 0))
                    '[(rw [(den_leaf_at_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.lbl 0) G Sk.cert en)]) (rw [(den_lbl_eq_ri chkf dec encTy (phRI 1 sg hsg) n 0)])]))))
