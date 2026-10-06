(ns lcert.formal-test.thm46f7
  "Formal suite (ADR-0006), lcert.formal.thm46f7: Theorem 4.6 and
  Corollaries 4.6′ and 4.6″ at F7's concrete checker Check decCert, with no
  encoding hypothesis.  Requiring the namespace kernel-checks its
  declarations; these tests check that the expected constants exist and that
  false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.thm46f7]))

(def ^:private nilE '(List.nil Exp))
(def ^:private rc (list 'DT.rConst nilE '(List.nil U) 'Exp.star 'Exp.tUnit))
(def ^:private fb (list 'DT.fBase nilE 'Exp.tUnit))
(def ^:private cert (list 'encCert 0 'Exp.star 'Exp.tUnit rc fb))
(def ^:private X (list 'certX 0 'Exp.star 'Exp.tUnit rc fb))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest f4-thm46f7-nesting
  (testing "no accepted code nests inside another, so Enc46 holds at Check decCert for every measure"
    (doseq [c '[nlb_sub nest_node nest_K acc_root_ff check_nest_free hasHole_ex enc46_canon]]
      (is (b/has? c) (str c))))
  (testing "the nesting the padded format allowed: a node over an accepted certificate, accepted by the padded checker, not by the canonical one"
    (is (holds? (list 'Eq 'Bool (list 'Check 'decCert cert '(encE Exp.tUnit)) 'Bool.true)))
    (is (holds? (list 'Eq 'Bool (list 'Check 'decCertPad (list 'Code.sn 0 X cert) '(encE Exp.tUnit)) 'Bool.true)))
    (is (holds? (list 'Eq 'Bool (list 'Check 'decCert (list 'Code.sn 96 X cert) '(encE Exp.tUnit)) 'Bool.false))))
  (testing "enc46_canon is about the canonical checker: it does not transfer to the padded one"
    (is (b/rejects? '[sz :- (=> Exp Nat)] '(Enc46 (Check decCertPad) (decOf decCertPad) sz) '[(exact (enc46_canon sz))]))))

(deftest f4-thm46f7-theorems
  (testing "Theorem 4.6 and Corollaries 4.6′ and 4.6″ at Check decCert, with no CheckSpec, Enc46 or LargeCert hypothesis"
    (doseq [c '[thm46_F7 cert_closed thm46_types_F7 cert_type_lbl box_nodes_gt box_code_ne
                cor46_prime_F7 cor46_prime_iff_F7 cor46_dprime_F7 cor46_dprime_iff_F7 d3_gap_F7]]
      (is (b/has? c) (str c))))
  (testing "the hypotheses can be met: a certified type exists (⋆ : Unit), and certified types are closed"
    (is (not (b/rejects? '[] '(Eq Bool (closedTy Exp.tUnit) Bool.true)
                         [(list 'exact (list 'cert_closed cert 'Exp.tUnit '(Eq.refl Bool.true)))]))))
  (testing "⌜□A⌝ ≠ ⌜A⌝ is derived, but only between a type and its box: a type's code equals itself"
    (is (b/rejects? '[A :- Exp] '(=> (Eq Code (encE A) (encE A)) False) '[(intro e) (exact (box_code_ne A e))]))))
