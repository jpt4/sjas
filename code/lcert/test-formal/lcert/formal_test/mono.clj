(ns lcert.formal-test.mono
  "Formal suite (ADR-0006), lcert.formal.mono: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.mono]))
(deftest f3g-monotonicity
  (is (b/has? 'V_mono))
  (testing "footprints cannot be lowered: Vₖ(R) is not contained in V₀(R)"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                    '(V chkf dec encTy 1 Exp.tR (List.nil Sk) Unit.unit 0 Sk.cert (Code.sn 0 (Code.sl 0) (Code.sl 0)))
                    '[(decide)])))
  (testing "Lemma 3.4: base data types across budgets"
    (is (b/has? 'Lemma_3_4)))
  (testing "Lemma 3.4 needs m ≤ k: a one-node certificate is in V₁(R) and provably not in V₀(R)"
    (let [ps '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
          vR (fn [k] (list 'V 'chkf 'dec 'encTy 5 'Exp.tR '(List.nil Sk) 'Unit.unit k 'Sk.cert '(Code.sn 0 (Code.sl 0) (Code.sl 0))))]
      (is (not (b/rejects? ps (vR 1) '[(exact (And.intro (Nat.le_refl 1) rfl))])))
      (is (not (b/rejects? ps (list 'Not (vR 0)) '[(intro h) (have h2 (LE.le 1 0) (And.left h)) (omega)])))))
  (testing "and T(b) is not a base type: its set depends on the cap"
    (is (b/rejects? '[] '(Eq Bool (isBaseTy (Exp.tT Exp.tt)) Bool.true) '[(rfl)]))))
