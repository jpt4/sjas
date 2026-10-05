(ns lcert.formal-test.pa
  "Formal suite (ADR-0006, F6.1), lcert.formal.pa: Peano arithmetic H_PA
  (R4-metatheory.md §6.2) as a deep embedding, its semantics in ℕ, soundness,
  and consistency relative to the metatheory — a theorem, so P5 need not
  assume it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.pa]))

(deftest f6-pa-semantics
  (testing "syntax, substitution, truth, and their lemmas"
    (doseq [nm '[PT PF paSbT paSbF paEval paHolds paEval_sbT paHolds_sbF paHolds_ext paHolds_stab]]
      (is (b/has? nm) (str nm))))
  (testing "truth computes: 1 + 1 = 2 holds, 0 = 1 does not"
    (is (not (b/rejects? [] '(paHolds (fn [i :- Nat] 0) (PF.peq (PT.padd (PT.ps PT.pz) (PT.ps PT.pz)) (PT.ps (PT.ps PT.pz)))) '[(rfl)])))
    (is (b/rejects? [] '(paHolds (fn [i :- Nat] 0) (PF.peq PT.pz (PT.ps PT.pz))) '[(rfl)])))
  (testing "A4's instance substitutes and lowers: (x₁ = x₀)[S0/x₀] is x₀ = S0"
    (is (not (b/rejects? [] '(Eq PF (paSbF (paInst (PT.ps PT.pz)) (PF.peq (PT.pv 1) (PT.pv 0))) (PF.peq (PT.pv 0) (PT.ps PT.pz))) '[(rfl)])))))

(deftest f6-pa-proofs
  (testing "the axiom schemes, derivability, soundness, consistency"
    (doseq [nm '[PAx PPrv paAx_sound paPrv_sound pa_consistent]]
      (is (b/has? nm) (str nm))))
  (testing "a derivation: ∀x (x + 0 = x), by Q3 and Gen"
    (is (not (b/rejects? [] '(PPrv (PF.pall (PF.peq (PT.padd (PT.pv 0) PT.pz) (PT.pv 0))))
                         '[(exact (PPrv.gen _ (PPrv.ax _ PAx.q3)))]))))
  (testing "soundness refutes S0 = 0 as a theorem"
    (is (not (b/rejects? [] '(Not (PPrv (PF.peq (PT.ps PT.pz) PT.pz)))
                         '[(intro hd) (exact (Nat.succ_ne_zero 0 (paPrv_sound (PF.peq (PT.ps PT.pz) PT.pz) hd (fn [i :- Nat] 0))))]))))
  (testing "0 = S0 is not an axiom instance"
    (is (b/rejects? [] '(PAx (PF.peq PT.pz (PT.ps PT.pz))) '[(constructor)]))))
