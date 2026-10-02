(ns lcert.formal-test.convcase
  "Formal suite (ADR-0006), lcert.formal.convcase: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.convcase]))
(deftest f3m-convcase-subst
  (testing "nbr is preserved by substitution, and β's contractum stays skeleton-typed (Lemma 3.2)"
    (doseq [c '[nbrF_var nbrF_anyflag up_pres upn_pres nbrF_subst_var nbrF_subst
                inst1_nbr nbr_subst1 NbrAll instL_nbr instLS_nbr nbr_substL
                skj_subst1 skj_substL skj_beta_core skj_beta]]
      (is (b/has? c) (str c))))
  (testing "a branch list substituted for a variable is not nbr"
    (is (b/rejects? '[]
                    '(Eq Bool (nbr (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.var 0))) Bool.true)
                    '[(rfl)]))))

(deftest f3m-convcase-hd-skj
  (testing "every head step preserves skeleton typing and nbr (Lemma 3.2)"
    (doseq [c '[skj_iteT skj_iteF skj_elimT skj_elimF skj_recNZ skj_recNS skj_recSL
                skj_prnL skj_prnN skj_boolExp skj_delta
                skj_betaLet_at skj_betaLet_core skj_betaLet skj_recSN skj_itRL skj_itRN
                skj_bcons_pick nbr_bcons_pick skj_nthB nbr_nthB skj_caseLb hd_skj]]
      (is (b/has? c) (str c))))
  (testing "nthB of a non-branch-list is not a branch"
    (is (b/rejects? '[]
                    '(Eq (Option Exp) (nthB Exp.tt 0) (Option.some Exp Exp.tt))
                    '[(rfl)]))))

(deftest f3m-convcase-path
  (testing "a path step preserves skOf and the denotation (Lemma 3.2)"
    (doseq [c '[step_nil_pack hd_not_base nbr_hd_flag step_nil_fl
                step_tEmpty step_tUnit step_tBool step_tNat step_tLbl step_tSyn
                step_tDia step_tR step_tT step_tPi step_tSig step_tBrs
                step_var step_star step_tt step_ff step_zero step_lbl step_bnil
                den_succ_cong sk_abort_A
                den_ig_abort_A den_ig_h1_r den_ig_node_d den_coe_sleaf den_coe_leaf
                den_coe_prn den_ite_t den_ite_e
                den_sn_a den_sn_c1 den_sn_c2 den_nd_a den_nd_r1 den_nd_r2 den_chk_c den_chk_d
                step_abort step_succ step_h1 step_sleaf step_leaf step_prn step_ite
                step_snode step_node step_chk
                sk_elimB_P den_el_b den_el_t den_el_e step_elimB
                den_recN_n den_recN_z den_recN_s sk_recN_P step_recN
                den_caseL_a den_caseL_bs sk_caseL_P step_caseL
                den_bcons_h den_bcons_t step_bcons
                den_recS_c den_recS_tl den_recS_tn sk_recS_P step_recS
                den_itR_g den_itR_h den_itR_r sk_itR_X step_itR
                sk_lam_A sk_lam_t den_lam_t step_lam
                sk_app_f den_app_f den_app_u step_app
                sk_pair_S den_pair_a den_pair_b step_pair
                skof_letp sk_letp_C den_letp_p den_letp_t step_letp
                getP_base den_refl_r step_refl_D step_refl
                den_insp_r den_insp_c den_insp_t1 den_insp_t2 sk_insp_X step_insp
                step_pack
                V_tT_den V_tTT_gen V_tTF_gen step_V_tT_nil step_V_tT_b step_V_tT
                V_pi_at V_pi_at_cast V_pi_dom
                pi_no_head V_pi_at_bcst V_pi_cod step_V_pi
                V_sig_at sig_no_head V_sig_at_cast V_sig_dom
                V_sig_at_bcst V_sig_cod step_V_sig
                step_V_tEmpty step_V_tDia step_V_var step_V_tT_pack
                step_V_tPi step_V_tSig step_V_tBrs step_V
                step_V_of_step cv_V F_conv conv_all
                Lemma_3_6_holds Theorem_1_holds Corollary_3_7_holds]]
      (is (b/has? c) (str c))))
  (testing "a childless term has no child to step in"
    (is (b/rejects? '[]
                    '(Eq (Option Exp) (getP (List.cons Nat 0 (List.nil Nat)) Exp.star)
                                      (Option.some Exp Exp.star))
                    '[(rfl)]))))
