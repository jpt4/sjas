(ns lcert.formal-test.strengthen
  "Formal suite (ADR-0006), lcert.formal.strengthen: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.strengthen]))
(deftest f2-strengthening
  (testing "Lemma 2.3 and its infrastructure"
    (doseq [c '[freshF fresh_app fresh_lam fresh_insp zeroUF zero_vadd zero_vscale zero_len zero_nth rt_ucast rt_strengthen]]
      (is (b/has? c) (str c))))
  (let [ps '[chkf :- (=> Code Code Bool)]
        D '(List.cons Exp Exp.tDia (List.cons Exp Exp.tDia (List.nil Exp)))
        us '(List.cons U U.u1 (List.cons U U.u1 (List.nil U)))
        der (list 'Rt.rVar 'chkf D us 0 'Exp.tDia 'U.u1 'rfl 'rfl 'rfl 'rfl)]
    (testing "a token not free in the term is lowered to usage 0"
      (is (not (b/rejects? ps (list 'Rt 'chkf D '(List.cons U U.u1 (List.cons U U.u0 (List.nil U))) '(Exp.var 0) '(lift 1 0 Exp.tDia))
                           [(list 'exact (list 'rt_strengthen 'chkf D us '(Exp.var 0) '(lift 1 0 Exp.tDia) der 1 'rfl))]))))
    (testing "the variable that occurs cannot be lowered: its freshness is false"
      (is (b/rejects? ps (list 'Rt 'chkf D '(List.cons U U.u0 (List.cons U U.u1 (List.nil U))) '(Exp.var 0) '(lift 1 0 Exp.tDia))
                      [(list 'exact (list 'rt_strengthen 'chkf D us '(Exp.var 0) '(lift 1 0 Exp.tDia) der 0 'rfl))])))))

(deftest f3-theorem3-tokens
  (testing "Lemma 2.3 for a mask, and Theorem 3's refinement to the free tokens"
    (doseq [c '[maskUF gsh mask_cons mask_vadd mask_vscale mask_len mask_nth sh_ok mask_var_false rt_mask
                ucnt cntU tok_entry tok_sat_mask theorem3_tokens]]
      (is (b/has? c) (str c))))
  (testing "the count is of the tokens actually free: var 0 in Θ₂ leaves one token"
    (is (not (b/rejects? '[] '(Eq Nat (cntU (maskUF (thetaU 2) (freshF (Exp.var 0)))) 1) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Nat (cntU (maskUF (thetaU 2) (freshF (Exp.var 0)))) 2) '[(rfl)]))))
