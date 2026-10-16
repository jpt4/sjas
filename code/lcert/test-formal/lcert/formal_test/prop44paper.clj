(ns lcert.formal-test.prop44paper
  "Formal suite (ADR-0006), lcert.formal.prop44paper: Proposition 4.4 for the
  paper's family A_j = 1 ⊸ ⋯ ⊸ 1, at Check, for every budget k.  prop434's
  versions at ¬ᵏ⁺¹1 are vacuous at even k; these are not."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop44paper]))

(deftest prop44-general-family
  (testing "the proofs hold for any closed A whose code has more than k nodes"
    (doseq [nm '[prop43_g box_out_g prop44g_1 prop44g_2 prop44g_3]]
      (is (b/has? nm) (str nm)))))

(defn- box [A] (list 'Exp.tSig 'U.u1 'Exp.tR (list 'chkT '(Exp.var 0) (list 'cTerm (list 'encE A)))))

(deftest prop44-paper-form
  (testing "no uniform D3 and no uniform boxed contraction, at every k"
    (is (not (b/rejects? '[k :- Nat]
          (list 'Not (list 'forall '[A Exp]
            (list '=> '(Eq Bool (closedTy A) Bool.true) '(Certifiable A)
              (list 'Exists (list 'fn '[q :- Exp]
                (list 'Rt '(Check decCert) '(thetaD k) '(thetaU k) 'q
                      (list 'Exp.tPi 'U.u1 (box 'A) (box (box 'A)))))))))
          '[(exact (prop44_paper_2 k))])))
    (is (b/has? 'prop44_paper_3))))

(deftest prop44-paper-form-1-and-cor46
  (testing "Proposition 4.4 (1), and Corollary 4.6′'s instance A = 1, B = A_{k+1}, at every k"
    (doseq [nm '[prop44_paper_1 cor46_prime_no_uniform]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects? '[k :- Nat]
          '(Not (Exists (fn [tt :- Exp]
                  (Rt (Check decCert) (thetaD k) (thetaU k) tt
                    (Exp.tPi U.u1
                      (Exp.tSig U.u1 (boxTy (codeTerm (encE (Exp.tPi U.u1 Exp.tUnit (arrN (+ k 1))))))
                                     (boxTy (codeTerm (encE Exp.tUnit))))
                      (boxTy (codeTerm (encE (arrN (+ k 1))))))))))
          '[(exact (And.right (And.right (And.right (And.right (cor46_prime_no_uniform k))))))])))))
