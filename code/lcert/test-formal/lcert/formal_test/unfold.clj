(ns lcert.formal-test.unfold
  "Formal suite (ADR-0006), lcert.formal.unfold: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.unfold]))
(deftest f3i-unfolding
  (testing "one step of ⟦·⟧ⁿ at symbolic n, for every constructor"
    (doseq [c '[den_eq_denAt den_succ_eq den_lam_at den_h1_at den_tBrs_at den_refl_at]]
      (is (b/has? c) (str c))))
  (testing "den at a symbolic cap does not compute by itself"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                    '(= (den chkf dec encTy n Exp.tt (List.nil Sk) Sk.bool Unit.unit) Bool.true) '[(rfl)])))
  (testing "but it does after unfolding"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(= (den chkf dec encTy n Exp.tt (List.nil Sk) Sk.bool Unit.unit) Bool.true)
                         '[(rw [(den_tt_at chkf dec encTy n (List.nil Sk) Sk.bool Unit.unit)])])))))
