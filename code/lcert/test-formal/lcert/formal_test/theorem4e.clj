(ns lcert.formal-test.theorem4e
  "Formal suite (ADR-0006), lcert.formal.theorem4e: requiring the namespace
  kernel-checks its declarations (the assembly of the fundamental property
  of evalᴱ and Theorem 4′); these tests check that the expected constants
  exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.theorem4e]))

(deftest f5-theorem4e
  (testing "restriction, the remaining cases, the induction on Er, and Theorem 4′"
    (doseq [c '[SubU subU_refl subU_trans len_vadd_cc len_vadd len_vscale vadd_comm_cc vadd_comm
                subU_vadd_l subU_vadd_r subU_vscale subU_cons
                entryE_mono envE_sub_cons envE_sub_nil envE_sub len_vadd2 len_vscale2
                er_rt er_len
                adeqE_const denU_abort adeqE_abort adeqE_h1
                adeqE_insp_pick adeqE_retarget_eq adeqE_insp
                tokU tokE psig_cast tokU_pair tokU_transfer
                isBase_of_code erel_base_budget erel_rel_base
                adeqE_refl_no adeqE_refl_yes adeqE_refl
                cE_lam cE_app cE_pair cE_let
                adeqE_step adeqE_from_below adeqE_below_succ adeqE_below_all adeqE_all
                theorem4e theorem4e_agree theorem4e_nodes]]
      (is (b/has? c) (str c))))
  (testing "restriction goes one way: a premise may not use an entry the conclusion has at usage 0"
    (is (b/rejects? '[]
                    '(SubU (List.cons U U.u1 (List.nil U)) (List.cons U U.u0 (List.nil U)))
                    '[(constructor) (rfl) (intro i h) (exact h)]))
    (is (not (b/rejects? '[]
                         '(SubU (List.cons U U.u0 (List.nil U)) (List.cons U U.u1 (List.nil U)))
                         '[(constructor) (rfl) (intro i) (cases i) (intro h) (rfl) (intro h) (exact h)]))))
  (testing "the tokens are related to ◇, but ⋆ is not"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code), n :- Nat]
                    '(envE chkf dec encTy n (uskCtx (thetaD 1)) (thetaU 1)
                       (List.cons RV RV.star (List.nil RV)) (tokU 1))
                    '[(exact (tokE chkf dec encTy n 1))]))))
