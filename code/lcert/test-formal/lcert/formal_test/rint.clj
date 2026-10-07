(ns lcert.formal-test.rint
  "Formal suite (ADR-0006), lcert.formal.rint: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.rint]))

(def ^:private sg2 '(fn [i :- Nat] (Code.sn 5 (Code.sl 3) (Code.sl 4))))

(deftest f4-rint
  (testing "R-interpretations, their laws, and the two instances (Theorem 4.6 design note §2)"
    (doseq [c '[RIntD mkRID dLf dPr dPf RLawsD RInt riLf riPr riPf leafLbl riIt RLaws ri_laws
                rl_leaf rl_node rl_nodes rl_dflt ri_leaf ri_node riIt_leaf ri_itleaf leafLbl_lt riIt_lt PhCons
                stdRID stdRI_laws stdRI stdRI_pr stdRI_lf stdRI_pf stdRI_it stdRI_phcons
                phPr phPf phRID PhOk phPr_sl phPr_sn phPf_sl phPf_sn phPr_std phPr_ph phPf_ph
                phRI_leaf phRI_nodes phRI_dflt phRI_laws phRI phRI_pr phRI_lf phRI_pf]]
      (is (b/has? c) (str c))))
  (testing "the phantom ★₀ prints as σ 0, not as the leaf it is"
    (is (not (b/rejects? '[sg :- (=> Nat Code), hsg :- (PhOk 2 sg)]
                         '(Eq Code (riPr (phRI 2 sg hsg) (Code.sl 0)) (sg 0)) '[(rfl)])))
    (is (b/rejects? [(symbol "hsg") :- (list 'PhOk 2 sg2)]
                    (list 'Eq 'Code (list 'riPr (list 'phRI 2 sg2 'hsg) '(Code.sl 0)) '(Code.sl 0))
                    '[(rfl)])))
  (testing "leaf a builds no phantom: its tree is phantom-free, and itR passes a"
    (is (not (b/rejects? '[sg :- (=> Nat Code), hsg :- (PhOk 2 sg)]
                         '(Eq Bool (riPf (phRI 2 sg hsg) (Code.sl (riLf (phRI 2 sg hsg) 0))) Bool.true) '[(rfl)])))
    (is (b/rejects? '[sg :- (=> Nat Code), hsg :- (PhOk 2 sg)] '(Eq Bool (riPf (phRI 2 sg hsg) (Code.sl 1)) Bool.true) '[(rfl)]))
    (is (not (b/rejects? '[sg :- (=> Nat Code), hsg :- (PhOk 2 sg)] '(Eq Nat (riIt (phRI 2 sg hsg) 9) 7) '[(rfl)]))))
  (testing "itR sees a phantom whose certificate is a node as a leaf labelled 0"
    (is (not (b/rejects? [(symbol "hsg") :- (list 'PhOk 2 sg2)] (list 'Eq 'Nat (list 'riIt (list 'phRI 2 sg2 'hsg) 1) 0) '[(rfl)]))))
  (testing "print is not the identity at phRI, so the phantom model differs from the standard one"
    (is (not (b/rejects? [(symbol "hsg") :- (list 'PhOk 1 sg2)]
                         (list '=> (list 'forall '[c Code] (list 'Eq 'Code (list 'riPr (list 'phRI 1 sg2 'hsg) 'c) 'c)) 'False)
                         '[(intro h) (exact (False.elim (Code.noConfusion (h (Code.sl 0)))))]))))
  (testing "L3 needs phantom-freeness: a phantom has no node but prints to a node"
    (is (b/rejects? [(symbol "hsg") :- (list 'PhOk 1 sg2)]
                    (list 'Eq 'Nat (list 'cnodes (list 'riPr (list 'phRI 1 sg2 'hsg) '(Code.sl 0))) '(cnodes (Code.sl 0)))
                    '[(rfl)])))
  (testing "PhCons is not trivial at phRI: it fails for the checker that accepts everything"
    (is (not (b/rejects? '[hsg :- (PhOk 1 (fn [i :- Nat] (Code.sl 0)))]
                         '(=> (PhCons (fn [a :- Code, b :- Code] Bool.true) (fn [e :- Exp] (Code.sl 0)) (phRI 1 (fn [i :- Nat] (Code.sl 0)) hsg)) False)
                         '[(intro h) (exact ((And.left h) (Code.sl 0) (Eq.refl$1 Bool.false) (Eq.refl$1 Bool.true)))])))))
