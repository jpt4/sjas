(ns lcert.formal-test.conversion
  "Formal suite (ADR-0006), lcert.formal.conversion: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.conversion]))

;; As in the substitution suite.
(def ^:private den-params
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

(deftest f3m-conversion
  (testing "conversion records nbr at both endpoints, and lifting preserves it"
    (doseq [c '[cv_nbr_left cv_nbr_right nbrF_lift_var nbrF_lift nbr_lift]]
      (is (b/has? c) (str c))))
  (testing "branch lists are admitted in caseL, but never as application arguments"
    (is (not (b/rejects? '[]
               '(Eq Bool (nbr (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil))) Bool.true)
               '[(rfl)])))
    (is (b/rejects? '[]
           '(Eq Bool (nbr (Exp.app (Exp.var 0) (Exp.bcons Exp.tt Exp.bnil))) Bool.true)
           '[(rfl)])))
  (testing "Lemma 3.2 for the head steps proved so far, and the SkJ-only counterexample"
    (doseq [c '[nbr nbrF skOf_complete skOf_ok skj_inv EquivAt mk_eqv_t mk_eqv_ty
                eqv_den eqv_sko eqv_skel eqv_V
                hd_iteT skof_ite den_iteT den_iteF den_elimT den_elimF den_recNZ
                den_prnL den_prnN den_boolExp den_boolExp_ff den_boolExp_tt
                code_none_ne_some den_sleaf_code codeOf_sleaf_inv
                arr_inj exSk exU exExpC den_lam_arr den_beta_core den_beta
                prod_inj skof_pair den_pair_prod den_betaLet_at den_betaLet_core den_betaLet
                den_itRL den_itRL_nbr V_tTT V_tTF V_tTT_at V_tTF_at hd_tTT hd_tTF
                cex_red_eq cex_den_redex cex_den_contr cex_nbr_ff cex_redex_typed
                cex_den_ne conv_skj_counterexample
                nbr_recN_n nbr_recS_c nbr_snode_a nbr_snode_c1 nbr_snode_c2
                nbr_itR_r nbr_node_d nbr_node_a nbr_caseL_bs nbr_bcons_t nbr_bcons_h
                den_recNS_eval den_recNS den_nthB den_caseLb
                den_recSL_eval den_recSL den_recSN_eval den_recSN
                canonicalNode canonicalNode_some canonicalNode_den
                den_code_sleaf den_snode_code den_code_snode den_codeOf den_delta
                den_dia_unique den_itRN_eval den_itRN cex_no_cv
                skj_not_tT skj_not_tUnit hd_den setP_nil getP_nil den_step_nil
                setKid_ite_0 setKid_ite_1 setKid_ite_2 den_abort_ty den_ite_b]]
      (is (b/has? c) (str c))))
  (testing "the β redex and its contractum are not denoted equally: the application defaults"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0
                             (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool)
                                               (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                                      (Exp.bcons Exp.tt Exp.bnil))
                             (List.nil Sk) Sk.bool Unit.unit)
                        (den chkf dec encTy 0
                             (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil))
                             (List.nil Sk) Sk.bool Unit.unit))
                    '[(rfl)]))))
