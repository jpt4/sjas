(ns lcert.formal-test.prop410
  "Formal suite (ADR-0006), lcert.formal.prop410: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop410]))
(deftest f4-prop410-prime
  (testing "Proposition 4.10′ and its parts: the χ-model's Lemma 3.6, the checker transport, the separation"
    (doseq [c '[noH1 noh_app refl0_val den_refl_dec0 refl0_empty F_refl0 lemma36_noH1
                Agree agree_mono hd_tr trI step_tr cv_tr tl_tr rt_tr
                chiF chi_agree padC pad_nodes pad_ok chi_big ev4 ev5 p410_core prop410_prime]]
      (is (b/has? c) (str c))))
  (testing "H₁ itself is not H₁-free, so the theorem does not cover the term of Theorem 2"
    (is (b/rejects? '[] '(Eq Bool (noH1 (H1term)) Bool.true) '[(rfl)])))
  (testing "χ accepts a code larger than its bound, and agrees with the checker below it"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)] '(Eq Bool (chiF chkf 0 (Code.sl 0) (padC 0)) Bool.true)
                         '[(exact (chi_big chkf 0 (Code.sl 0) (padC 0) (big1 0)))])))))
