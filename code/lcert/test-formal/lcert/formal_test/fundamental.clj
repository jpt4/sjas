(ns lcert.formal-test.fundamental
  "Formal suite (ADR-0006), lcert.formal.fundamental: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.fundamental]))
(deftest f3j-fundamental-lemma-cases
  (testing "Lemma 3.6, the cases proved so far"
    (doseq [c '[Sound F_const F_succ F_sleaf F_snode F_prn F_chk F_lam0 F_lam1 F_lamw F_lam F_abort F_ite F_leaf F_node F_bnil
                den_app_some F_app1 F_appw F_app sk_transport bool_rec_dep F_elimB
                WFCtx var_wf entry_nz var_succ var_zero var_sem F_var le_sub_add F_pair1 F_pairw F_pair
                den_letp_some EnvSat_cons le_let le_let0 F_let1 F_letw F_let0 F_let F_app0 F_pair0
                entry_omega_back EnvSat_omega_back V_stepTy den_zero_nat natrec_inv F_recN
                F_caseL bcons_zero bcons_succ sub_eq_zero sub_succ den_lbl_lbl F_bcons
                lblOk_sn cnodes_sn lbl_lt coderec_inv V_l1s skj_lsuc V_lsuc F_itR]]
      (is (b/has? c) (str c))))
  (testing "a label constant must be below NL: lbl 100 is not in V(Lbl)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tLbl (List.nil Sk) Unit.unit 0 Sk.lbl 100))
                         '[(intro h) (have h2 (LT.lt 100 100) h) (omega)]))))
  (testing "certificate labels are below NL: a leaf labelled NL is not in V(R)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tR (List.nil Sk) Unit.unit 5 Sk.cert (Code.sl 100)))
                         '[(intro h) (exact (Bool.noConfusion (And.right h)))]))))
  (testing "a node costs a token: node ⋆ … at footprint 0 is not in V₀(R)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tR (List.nil Sk) Unit.unit 0 Sk.cert (Code.sn 0 (Code.sl 0) (Code.sl 0))))
                         '[(intro h) (have h2 (LE.le 1 0) (And.left h)) (omega)])))))
