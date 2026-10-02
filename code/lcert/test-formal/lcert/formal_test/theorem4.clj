(ns lcert.formal-test.theorem4
  "Formal suite (ADR-0006), lcert.formal.theorem4: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.theorem4]))
(deftest f5-skof-agree
  (testing "skOf agrees with simple typing off the branch-list spine"
    (doseq [c '[arrCod_arr skOf_lam_none skOf_app_none skOf_bnil_none
                brHead_bnil argsOK_bnil_true argsOK_app_eq
                skOf_brHead br_of_skSome skOf_agree
                argsOK_base argsOK_const match_sk_true argsOK_app_asm
                let_ok_prod argsOK_recs_tn tl_argsOK rt_argsOK
                sk_witness skels_theta baseSk_base closed_of_base skj_along
                eval_only app_arg_sk let_match_some let_body_of lt_le_omega
                adeq_refl_asm
                case_abort case_ite case_elim case_succ case_recN case_caseL
                case_bcons case_snode case_recS case_node case_itR
                case_sleaf case_leaf case_prn case_lam case_pair case_chk
                case_app case_let case_refl case_insp h1_oks case_h1
                adeq_skj adeq_from_below below_zero below_succ adeq_below_all
                theorem4 theorem4_spec corollary51 corollary51_conv corollary51_nodes]]
      (is (b/has? c) (str c))))
  (testing "bnil is simply typable and argsOK, but skOf is none: argsOK alone does not give skOf = some"
    (is (not (b/rejects? '[G :- (List Sk)]
                          '(Eq Bool (argsOK G Exp.bnil) Bool.true)
                          '[(rfl)])))
    (is (b/rejects? '[G :- (List Sk)]
                    '(Eq (Option Sk) (skOf G Exp.bnil) (Option.some Sk (Sk.arr Sk.lbl Sk.unit)))
                    '[(rfl)]))
    (is (b/rejects? '[G :- (List Sk)]
                    '(Eq Bool (argsOK G (Exp.app Exp.bnil Exp.bnil)) Bool.true)
                    '[(rfl)]))
    (is (b/rejects? '[]
                    '(Eq Bool (isBaseTy (Exp.tPi U.u0 Exp.tUnit Exp.tUnit)) Bool.true)
                    '[(rfl)]))
    (is (b/rejects? '[]
                    '(Eq Bool (isBaseTy Exp.tR) Bool.false)
                    '[(rfl)]))))
