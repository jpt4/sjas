(ns lcert.formal-test.enc46w
  "Formal suite (ADR-0006), lcert.formal.enc46w: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.enc46w]))

(deftest f4-enc46w
  (testing "a computing checker meets CheckSpec, Enc46 and LargeCert at once"
    (doseq [c '[isU46 isW46 chkW46 num46 pad46 decW46 tl_num46 rt_pad46 chkW_node46 chkW_spec46
                isU_hole46 isU_notW46 hasHole_ex46 enc46W_node enc46_W uN46 uN_U46 uN_lbl46 uN_nodes46
                num_size46 pad_size46 chkW_wrap46 lbl_wrap46 large_dec46 large46 thm46_hyps_sat thm46_W_noterm]]
      (is (b/has? c) (str c))))
  (testing "the checker computes: it accepts the wrapper at ⌜1⌝, and rejects it at ⌜0⌝ and a U-tree anywhere"
    (is (not (b/rejects? '[] '(Eq Bool (chkW46 (Code.sn 0 (Code.sn 1 (Code.sl 0) (Code.sl 0)) (Code.sl 0)) (encE Exp.tUnit)) Bool.true) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Bool (chkW46 (Code.sn 0 (Code.sn 1 (Code.sl 0) (Code.sl 0)) (Code.sl 0)) (encE Exp.tEmpty)) Bool.true) '[(rfl)]))
    (is (b/rejects? '[] '(Eq Bool (chkW46 (Code.sn 1 (Code.sl 0) (Code.sl 0)) (encE Exp.tUnit)) Bool.true) '[(rfl)])))
  (testing "a wrapper may not embed a wrapper: the accepted code with a wrapper inside is rejected"
    (is (b/rejects? '[] '(Eq Bool (chkW46 (Code.sn 0 (Code.sn 0 (Code.sl 0) (Code.sl 0)) (Code.sl 0)) (encE Exp.tUnit)) Bool.true) '[(rfl)])))
  (testing "the decoded term grows with the certificate: N = 3 for a three-node chain"
    (is (not (b/rejects? '[] '(Eq (Option (Prod Nat (Prod Exp Exp))) (decW46 (Code.sn 0 (uN46 3) (Code.sl 0)))
                                  (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk (pad46 3) Exp.tUnit)))) '[(rfl)])))
    (is (b/rejects? '[] '(Eq (Option (Prod Nat (Prod Exp Exp))) (decW46 (Code.sn 0 (uN46 3) (Code.sl 0)))
                            (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk (pad46 2) Exp.tUnit)))) '[(rfl)]))))
