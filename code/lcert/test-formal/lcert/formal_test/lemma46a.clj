(ns lcert.formal-test.lemma46a
  "Formal suite (ADR-0006), lcert.formal.lemma46a: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.lemma46a]))

(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

(deftest f4-lemma46a
  (testing "Lemma 4.6a, the generic Lemma 3.6 from CheckSpec alone, and the standard model as its instance"
    (doseq [c '[phcons_of_spec lemma36_gen lemma46a lemma36_std den_ri_std V_ri_std sound_ri_std lemma36_via_ri]]
      (is (b/has? c) (str c))))
  (testing "the phantom model's denotation is not the standard one: leaf ℓ builds sl (ℓ + J), so no term builds a phantom"
    ;; one unfolding script: it proves the value at phRI is sl 1 and fails on sl 0
    (is (not (b/rejects? (into P3 '[sg :- (=> Nat Code), hsg :- (PhOk 1 sg), n :- Nat, G :- (List Sk), en :- (HEnv G)])
                         '(Eq Code (den_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.leaf (Exp.lbl 0)) G Sk.cert en) (Code.sl 1))
                         '[(rw [(den_leaf_at_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.lbl 0) G Sk.cert en)]) (rw [(den_lbl_eq_ri chkf dec encTy (phRI 1 sg hsg) n 0)])])))
    (is (b/rejects? (into P3 '[sg :- (=> Nat Code), hsg :- (PhOk 1 sg), n :- Nat, G :- (List Sk), en :- (HEnv G)])
                    '(Eq Code (den_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.leaf (Exp.lbl 0)) G Sk.cert en) (Code.sl 0))
                    '[(rw [(den_leaf_at_ri chkf dec encTy (phRI 1 sg hsg) n (Exp.lbl 0) G Sk.cert en)]) (rw [(den_lbl_eq_ri chkf dec encTy (phRI 1 sg hsg) n 0)])]))
    ;; at the standard interpretation the same tree is sl 0, the standard den's (den_ri_std: rfl)
    (is (not (b/rejects? (into P3 '[n :- Nat, G :- (List Sk), en :- (HEnv G)])
                         '(Eq Code (den_ri chkf dec encTy stdRI n (Exp.leaf (Exp.lbl 0)) G Sk.cert en) (Code.sl 0))
                         '[(rw [(den_leaf_at_ri chkf dec encTy stdRI n (Exp.lbl 0) G Sk.cert en)]) (rw [(den_lbl_eq_ri chkf dec encTy stdRI n 0)])])))
    (is (not (b/rejects? (into P3 '[n :- Nat, G :- (List Sk), en :- (HEnv G)])
                         '(Eq Code (den_ri chkf dec encTy stdRI n (Exp.leaf (Exp.lbl 0)) G Sk.cert en)
                                   (den chkf dec encTy n (Exp.leaf (Exp.lbl 0)) G Sk.cert en))
                         '[(rfl)]))))
  (testing "print at phRI sends a phantom to its certificate: ⟦print r⟧* ≠ ⟦r⟧*"
    (is (not (b/rejects? (into P3 '[n :- Nat, G :- (List Sk), en :- (HEnv G), r :- Exp,
                                    hsg :- (PhOk 1 (fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))))])
                         '(=> (Eq Code (den_ri chkf dec encTy (phRI 1 (fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))) hsg) n r G Sk.cert en) (Code.sl 0))
                              (Eq Code (den_ri chkf dec encTy (phRI 1 (fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))) hsg) n (Exp.prn r) G Sk.syn en)
                                       (Code.sn 5 (Code.sl 3) (Code.sl 4))))
                         '[(intro h)
                           (rw [(den_prn_val_ri chkf dec encTy (phRI 1 (fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))) hsg) n r G en)])
                           ;; riPr of ★₀ computes to σ 0: rw closes the goal by rfl
                           (rw [h])])))
    ;; and the standard model's print is the identity: the same claim is refuted there
    (is (b/rejects? (into P3 '[n :- Nat, G :- (List Sk), en :- (HEnv G), r :- Exp])
                    '(=> (Eq Code (den_ri chkf dec encTy stdRI n r G Sk.cert en) (Code.sl 0))
                         (Eq Code (den_ri chkf dec encTy stdRI n (Exp.prn r) G Sk.syn en) (Code.sn 5 (Code.sl 3) (Code.sl 4))))
                    '[(intro h)
                      (rw [(den_prn_val_ri chkf dec encTy stdRI n r G en)])
                      (rw [h])])))
  (testing "PhCons needs CheckSpec: phcons_of_spec has no proof without it (the accept-all checker fails PhCons, rint suite)"
    (is (b/rejects? (into P3 '[sg :- (=> Nat Code), hsg :- (PhOk 1 sg)])
                    '(PhCons chkf encTy (phRI 1 sg hsg))
                    '[(exact (phcons_of_spec chkf dec encTy _ (phRI 1 sg hsg)))]))))
