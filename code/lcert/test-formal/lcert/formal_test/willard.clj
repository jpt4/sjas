(ns lcert.formal-test.willard
  "Formal suite (ADR-0006), lcert.formal.willard: A14, Theorems 1 and 2
  against Willard2016 Definition 3.4.  These tests check the exact
  statements, and that H°'s type is not read with a different code."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.willard]))

(deftest a14-hcirc-reads
  (testing "H° is Π(r :₁ R). T(chk′ (print r) ⌜0⌝) ⊸ 0, with ⌜0⌝ the code of 0"
    (is (not (b/rejects? '[]
          '(Eq Exp (Hcirc)
             (Exp.tPi U.u1 Exp.tR
               (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.prn (Exp.var 0))
                                              (codeTerm (encE Exp.tEmpty))))
                 Exp.tEmpty)))
          '[(exact Hcirc_reads)])))
    (is (b/rejects? '[]
          '(Eq Exp (Hcirc)
             (Exp.tPi U.u1 Exp.tR
               (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.prn (Exp.var 0))
                                              (codeTerm (encE Exp.tUnit))))
                 Exp.tEmpty)))
          '[(rfl)]))))

(deftest a14-definition-34
  (testing "clause (i) by T2, clause (ii) by T1 and Corollary 3.7, at Check decCert"
    (is (not (b/rejects? '[]
          '(And
             (And (Rt (Check decCert) (List.nil Exp) (List.nil U) (Hterm) (Hcirc))
                  (Rt (Check decCert) (List.nil Exp) (List.nil U) (H1term) (H1circ)))
             (And (forall [n Nat] (forall [t Exp]
                    (Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty))))
                  (forall [c Code]
                    (Not (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.true)))))
          '[(exact willard_def34)])))))
