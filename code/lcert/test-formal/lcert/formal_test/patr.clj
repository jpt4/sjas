(ns lcert.formal-test.patr
  "Formal suite (ADR-0006, F6.3a), lcert.formal.patr: the translation of PA
  terms and formulas into λᶜᵉʳᵗ₀ (R4 §6.2), relative to a variable
  environment, and its substitution and lifting lemmas."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.patr]))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest f6-translation
  (testing "the closed definitions, the translation, and its lemmas"
    (doseq [nm '[trISZERO trPRED trEQ trPLUS trTIMES trT trF trSucR trUpR trLiftR
                 trT_subst trF_subst trF_inst trF_suc trT_liftc trF_liftc trF_ext]]
      (is (b/has? nm) (str nm))))
  (testing "the closed definitions are fixed by substitution and lifting"
    (is (not (b/rejects? '[τ :- (=> Nat Exp)] '(Eq Exp (subst τ trEQ) trEQ) '[(rfl)]))))
  (testing "x₀ = 0 translates to T(EQ v₀ zero); its ∀ to a Π over Nat"
    (is (holds? '(Eq Exp (trF (PF.peq (PT.pv 0) PT.pz) (fn [i :- Nat] i))
                         (Exp.tT (Exp.app (Exp.app trEQ (Exp.var 0)) Exp.zero)))))
    (is (holds? '(Eq Exp (trF (PF.pall (PF.peq (PT.pv 0) PT.pz)) (fn [i :- Nat] i))
                         (Exp.tPi U.uw Exp.tNat (Exp.tT (Exp.app (Exp.app trEQ (Exp.var 0)) Exp.zero)))))))
  (testing "an implication's consequent skips the hypothesis binder"
    (is (holds? '(Eq Exp (trF (PF.pimp PF.pbot (PF.peq (PT.pv 0) (PT.pv 0))) (fn [i :- Nat] i))
                         (Exp.tPi U.uw Exp.tEmpty (Exp.tT (Exp.app (Exp.app trEQ (Exp.var 1)) (Exp.var 1)))))))
    (is (b/rejects? [] '(Eq Exp (trF (PF.pimp PF.pbot (PF.peq (PT.pv 0) (PT.pv 0))) (fn [i :- Nat] i))
                                (Exp.tPi U.uw Exp.tEmpty (Exp.tT (Exp.app (Exp.app trEQ (Exp.var 0)) (Exp.var 0)))))
                    '[(rfl)])))
  (testing "A4's instance: substituting S0 into ∀x₀ (x₀ = x₀) gives S0 = S0, as syntax"
    (is (not (b/rejects? []
      '(Eq Exp (subst1 (trT (PT.ps PT.pz) (fn [i :- Nat] i)) (trF (PF.peq (PT.pv 0) (PT.pv 0)) (trUpR (fn [i :- Nat] i))))
               (trF (paSbF (paInst (PT.ps PT.pz)) (PF.peq (PT.pv 0) (PT.pv 0))) (fn [i :- Nat] i)))
      '[(exact (trF_inst (PF.peq (PT.pv 0) (PT.pv 0)) (PT.ps PT.pz) (fn [i :- Nat] i)))])))))
