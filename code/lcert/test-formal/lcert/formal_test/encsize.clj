(ns lcert.formal-test.encsize
  "Formal suite (ADR-0006), lcert.formal.encsize: size and label facts about
  the expression encoding, from which the unpadded certificate format
  (certcanon.clj) derives its size facts (E3, E4) and F7's form of E6.
  Requiring the namespace kernel-checks its declarations; these tests check
  that the expected constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.encsize]))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest f7-encsize-tokens
  (testing "token counting, E3 (tokens ≤ budget) and E4 (tokens ≤ the term's internal nodes)"
    (doseq [c '[tokc tokc_succ tok_true tok_congr tok_and tok_var
                tokE_var tokE_lam tokE_app tokE_recS tok_encE_cut tok_encE]]
      (is (b/has? c) (str c))))
  (testing "a variable uses one token; a closed term none"
    (is (holds? '(Eq Nat (tokc 3 (freshF (Exp.var 1))) 1)))
    (is (not (holds? '(Eq Nat (tokc 3 (freshF (Exp.var 1))) 0))))
    (is (holds? '(Eq Nat (tokc 3 (freshF Exp.star)) 0))))
  (testing "E4 is tight: var 0 uses one token and its code has one internal node, so tok_encE cannot be strict"
    (is (holds? '(Eq Nat (cnodes (encE (Exp.var 0))) 1)))
    (is (holds? '(Eq Bool (Nat.blt (tokc 1 (freshF (Exp.var 0))) (cnodes (encE (Exp.var 0)))) Bool.false))))
  (testing "the count reaches the budget when no token is fresh (so E3's bound m is attained)"
    (is (holds? '(Eq Nat (tokc 2 (fn [j :- Nat] Bool.false)) 2)))
    (is (holds? '(Eq Bool (Nat.ble (tokc 3 (fn [j :- Nat] Bool.false)) 2) Bool.false)))))

(deftest f7-encsize-codes
  (testing "unary numbers, and raw codes written as literal terms"
    (doseq [c '[cnodes_encNat code_enc_gt lbl_codeTerm]] (is (b/has? c) (str c)))
    (is (holds? '(Eq Nat (cnodes (encNat 5)) 5)))
    ;; a leaf sl 5 becomes sleaf (lbl 5): two internal nodes where the code had none
    (is (holds? '(Eq Nat (cnodes (encE (codeTerm (Code.sl 5)))) 2)))
    (is (not (holds? '(Eq Nat (cnodes (encE (codeTerm (Code.sl 5)))) 0)))))
  (testing "nlb bounds internal labels only: leaves carry data and are free"
    (doseq [c '[nlb uLbl_lt96 nlb_encNat nlbE_lam nlbE_snode nlb_encE]] (is (b/has? c) (str c)))
    (is (holds? '(Eq Bool (nlb 96 (Code.sn 3 (Code.sl 150) (Code.sl 99))) Bool.true)))
    (is (holds? '(Eq Bool (nlb 96 (Code.sn 96 (Code.sl 0) (Code.sl 0))) Bool.false)))
    ;; a raw code with label 96 inside becomes a term whose labels are leaves
    (is (holds? '(Eq Bool (nlb 96 (encE (codeTerm (Code.sn 96 (Code.sl 0) (Code.sl 0))))) Bool.true)))))
