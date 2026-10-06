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
