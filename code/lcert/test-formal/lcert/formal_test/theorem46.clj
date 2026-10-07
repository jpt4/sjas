(ns lcert.formal-test.theorem46
  "Formal suite (ADR-0006), lcert.formal.theorem46: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.theorem46]))

(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])
(def ^:private sg2 '(fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))))

(deftest f4-theorem46
  (testing "Theorem 4.6: the input tensor, the phantom input in V₀, the three cases, and the theorem"
    (doseq [c '[tensF tensB phIn den_boxev_ri phbox_V ph_leaf_V ph_leaf_chk phin_V
                acc_same out_pf46 out_leaf46 out_node46 out_ph46 thm46_out thm46 thm46_types
                id_box46 cert_pos46 thm46_ne_needed]]
      (is (b/has? c) (str c))))
  (testing "B = A₁ must be excluded: λz. z : □c ⊸ □c at Θ₀ (it costs 0), and no certificate has 0 nodes"
    (doseq [c '[id_box46 cert_pos46 thm46_ne_needed]] (is (b/has? c) (str c)))
    ;; the identity's derivation is real: Θ₀ ⊢ λz. z :¹ □⌜1⌝ ⊸ □⌜1⌝ for the code sl 16 of 1
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                         '(Rt chkf (thetaD 0) (thetaU 0) (Exp.lam U.u1 (boxTy (codeTerm (Code.sl 16))) (Exp.var 0))
                              (Exp.tPi U.u1 (boxTy (codeTerm (Code.sl 16))) (boxTy (codeTerm (Code.sl 16)))))
                         '[(exact (id_box46 chkf (Code.sl 16) (Eq.refl$1 Bool.true)))]))))
  (testing "the phantom ★₀ is in V₀(□c) at phRI when σ 0 is accepted at c, though it has no node"
    ;; chkf accepts exactly the one-node codes; σ 0 = sn 5 (sl 3) (sl 4) has one
    (is (not (b/rejects? '[dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G),
                           hsg :- (PhOk 1 (fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))))]
                         (list 'V_ri '(fn [a :- Code, b :- Code] (Nat.beq (cnodes a) 1)) 'dec 'encTy (list 'phRI 1 sg2 'hsg) 'n
                               '(boxTy (codeTerm (Code.sl 9))) 'G 'en 0 '(skel (boxTy (codeTerm (Code.sl 9)))) '(Prod.mk (Code.sl 0) Unit.unit))
                         [(list 'exact (list 'phbox_V '(fn [a :- Code, b :- Code] (Nat.beq (cnodes a) 1)) 'dec 'encTy (list 'phRI 1 sg2 'hsg) 'n 'G 'en
                                             '(Code.sl 9) '(Code.sl 0)
                                             (list 'ph_leaf_V '(fn [a :- Code, b :- Code] (Nat.beq (cnodes a) 1)) 'dec 'encTy 1 sg2 'hsg 'n 'G 'en 0 '(Nat.zero_lt_succ 0))
                                             '(Eq.refl$1 Bool.true)))])))
    ;; in the standard model the same pair is not in V₀(□c): the leaf itself is checked, and it has no node
    (is (b/rejects? '[dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), en :- (HEnv G)]
                    '(V_ri (fn [a :- Code, b :- Code] (Nat.beq (cnodes a) 1)) dec encTy stdRI n
                           (boxTy (codeTerm (Code.sl 9))) G en 0 (skel (boxTy (codeTerm (Code.sl 9)))) (Prod.mk (Code.sl 0) Unit.unit))
                    '[(exact (phbox_V (fn [a :- Code, b :- Code] (Nat.beq (cnodes a) 1)) dec encTy stdRI n G en (Code.sl 9) (Code.sl 0)
                                      (And.intro (Nat.le_refl 0) (Eq.refl$1 Bool.true)) (Eq.refl$1 Bool.true)))]))))
