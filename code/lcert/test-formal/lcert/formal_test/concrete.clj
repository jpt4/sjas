(ns lcert.formal-test.concrete
  "Formal suite (ADR-0006), lcert.formal.concrete: the fidelity pass's
  specializations of generic results to the concrete checker Check decCert.
  Each statement is checked in its exact form, with no checker premise."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.concrete]))

(deftest fidelity-concrete-specializations
  (testing "every generic result named in the audit has a Check decCert form"
    (doseq [nm '[lemma36_F7 theorem3_F7 theorem3_tokens_F7 prop41_closed_F7
                 prop41_fun_F7 lemma46a_F7 theorem4_F7 corollary51_nodes_F7
                 theorem4e_nodes_F7 theorem4e_agree_F7 prop45_contraction_mu prop45_d3_mu
                 thm46_mu_F7 prop44_2_F7 prop44_3_F7]]
      (is (b/has? nm) (str nm))))
  (testing "Lemma 3.6 at Check needs only a derivation and a well-formed context"
    (is (not (b/rejects? '[n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                           der :- (Rt (Check decCert) D us t A),
                           hw :- (WFCtx (Check decCert) D)]
          '(Sound (Check decCert) (decOf decCert) encE n D us t A)
          '[(exact (lemma36_F7 n D us t A der hw))]))))
  (testing "T4 at Check holds at every budget, unconditionally"
    (is (not (b/rejects? '[n :- Nat]
          '(Adeq (Check decCert) (decOf decCert) encE n)
          '[(exact (theorem4_F7 n))]))))
  (testing "Corollary 5.1 at Check bounds an R program's value by its budget"
    (is (not (b/rejects? '[n :- Nat, t :- Exp,
                           hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tR)]
          '(LE.le (cnodes (den (Check decCert) (decOf decCert) encE n t (skels (thetaD n))
                            Sk.cert (tokEnvD n)))
                  n)
          '[(exact (corollary51_nodes_F7 n t hd))])))))
