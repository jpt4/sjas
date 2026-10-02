(ns lcert.formal-test.eval
  "Formal suite (ADR-0006), lcert.formal.eval: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.eval]))
(deftest f5-evaluator
  (testing "Theorem 4's evaluator: a program evaluates, and the constant cases are adequate"
    (doseq [c '[Eval AppV rel envRel argsOK Adeq Theorem_4
                eval_tt eval_not_tt rlookup_head rlookup_zero rlookup_succ eval_cast
                some_injSk rel_bool_rfl rel_unit_rfl skel_reify rdflt_rel env_at
                adeq_star adeq_tt adeq_ff adeq_zero adeq_lbl adeq_abort adeq_h1
                adeq_succ adeq_var envRel_cons arrCase_arr adeq_lam adeq_app
                adeq_ite adeq_ite_den prodCase_prod adeq_pair adeq_chk
                adeq_bnil adeq_elim adeq_elim_den adeq_prn
                adeq_sleaf adeq_leaf adeq_snode adeq_node adeq_caseL
                bcons_apply_zero bcons_apply_succ adeq_bcons
                adeq_insp_pick adeq_insp adeq_let adeq_iter adeq_recN
                adeq_recs_leaf adeq_recs_node adeq_recs adeq_recS
                adeq_itr_leaf adeq_itr_node adeq_itr adeq_itR
                baseSk rel_budget tokens_rel den_refl_false den_refl_none den_refl_ok_eq
                adeq_refl_no adeq_refl_none adeq_refl_ok]]
      (is (b/has? c) (str c))))
  (testing "tt does not evaluate to ff: the true-constructor is the wrong value"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      n :- Nat, rho :- (List RV)]
                    '(Eval chkf dec encTy n rho Exp.tt (RV.bool Bool.false))
                    '[(exact (Ev.eTT chkf dec encTy n rho))]))))
