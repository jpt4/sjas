(ns lcert.formal-test.cor46
  "Formal suite (ADR-0006), lcert.formal.cor46: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.cor46]))

(deftest f4-cor46
  (testing "Corollary 4.6′ (D2) and Corollary 4.6″ (D3, exactly)"
    (doseq [c '[pi_ne_cod46 closed_code46 closed_box46 two_cases46 one_case46 LargeCert thm46_one
                d2As d2_lower d2_lower1 d2_upper cor46_prime d3_lower d3_gap cor46_dprime]]
      (is (b/has? c) (str c))))
  (testing "exactly: the upper bounds at every budget k ≥ ‖v‖, the two iffs, and no uniform budget for D2"
    (doseq [c '[lit_fixed_n46 lit_inner_n46 lit_left_n46 cv_inner_n46 cv_left_n46 tl_lit_left_n46 tl_ev_left_n46
                len_front_n46 rt_star_left_n46 len_prefix_n46 vadd_blocks_n46 us_sum_n46 cert_front_n46 add_sub_n46
                d2_upper_n46 d2_upper_k46 cor46_prime_iff d3_upper_n46 d3_upper_k46 cor46_dprime_iff
                negN_closedF46 unit_ne_negN46 arith_nu46 d2_no_uniform]]
      (is (b/has? c) (str c))))
  (testing "at Θ_{‖v‖+p} ⋆ carries the binder and the p extra tokens: the usages add up to Θ, and nothing is dropped"
    ;; ‖v‖ = 1, p = 1: lit's (0; 1, 0) plus ⋆'s (1; 0, 1) is (1; 1, 1)
    (is (not (b/rejects? '[] '(Eq (List U) (vadd (vscale U.u1 (List.cons U U.u0 (prefixU 1 U.u1 (vzero 1))))
                                                 (List.cons U U.u1 (prefixU 1 U.u0 (thetaU 1))))
                                           (List.cons U U.u1 (thetaU 2)))
                         '[(rfl)])))
    ;; the binder's usage is ⋆'s, not 0
    (is (b/rejects? '[] '(Eq (List U) (vadd (vscale U.u1 (List.cons U U.u0 (prefixU 1 U.u1 (vzero 1))))
                                            (List.cons U U.u1 (prefixU 1 U.u0 (thetaU 1))))
                                      (List.cons U U.u0 (thetaU 2)))
                    '[(rfl)]))
    ;; the extra token is used once, not left at 0
    (is (b/rejects? '[] '(Eq (List U) (vadd (vscale U.u1 (List.cons U U.u0 (prefixU 1 U.u1 (vzero 1))))
                                            (List.cons U U.u1 (prefixU 1 U.u0 (thetaU 1))))
                                      (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.u0 (List.nil U)))))
                    '[(rfl)])))
  (testing "no uniform budget uses ¬ᵏ⁺¹1, not ¬ᵏ1: at depth 0 the family meets A = 1, which B must differ from"
    (is (not (b/rejects? '[] '(Eq Exp (negN 0) Exp.tUnit) '[(rfl)])))
    (is (b/rejects? '[] '(=> (Eq Exp (negN 0) Exp.tUnit) False) '[(intro e) (exact (Exp.noConfusion e))])))
  (testing "the two inputs of D2 are A ⊸ B (index 0) and A (index 1)"
    (is (not (b/rejects? '[A :- Exp, B :- Exp] '(Eq Exp (d2As A B 0) (Exp.tPi U.u1 A B)) '[(rfl)])))
    (is (not (b/rejects? '[A :- Exp, B :- Exp] '(Eq Exp (d2As A B 1) A) '[(rfl)])))
    (is (b/rejects? '[A :- Exp, B :- Exp] '(Eq Exp (d2As A B 1) (Exp.tPi U.u1 A B)) '[(rfl)])))
  (testing "B ≠ A ⊸ B holds outright, but A ⊸ B = A ⊸ B: pi_ne_cod46 is about the proper subterm"
    (is (b/rejects? '[A :- Exp, B :- Exp] '(=> (Eq Exp (Exp.tPi U.u1 A B) (Exp.tPi U.u1 A B)) False)
                    '[(intro h) (exact (pi_ne_cod46 A (Exp.tPi U.u1 A B) h))])))
  (testing "LargeCert is not trivial: the checker that accepts nothing has no large certificates"
    (is (not (b/rejects? '[dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), sz :- (=> Exp Nat), d :- Code]
                         '(=> (LargeCert (fn [a :- Code, b :- Code] Bool.false) dec sz d) False)
                         '[(intro h) (refine' (exT Code _ _ (h 0) _)) (intro c hc)
                           (exact (False.elim (Bool.noConfusion (And.left hc))))])))))
