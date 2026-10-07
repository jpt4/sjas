(ns lcert.formal-test.painst
  "F6.3: syntactic substitution and the typed rules used by A4 and MP."
  (:require [lcert.formal.painst]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(deftest substitution-and-instantiation
  (doseq [nm '[paSbT_idlike paUp_idlike paSbF_idlike paSbT_id paSbF_id
              paSbT_inst_shift paSbT_up_shift trF_lower paTrLift0 trF_lift1 trF_lift2
              pa_instantiate pa_mp pa_generalize]]
    (is (b/has? nm) (str nm)))
  (testing "an implication binder is lowered without changing the PA formula"
    (is (not (b/rejects? '[p :- PF, ρ :- (=> Nat Nat), u :- Exp]
               '(Eq Exp (subst1 u (trF p (trSucR ρ))) (trF p ρ))
               '[(exact (trF_lower p u ρ))]))))
  (testing "universal elimination returns exactly the translated PA instance"
    (is (not (b/rejects?
               '[chkf :- (=> Code Code Bool), p :- PF, t :- PT, D :- (List Exp),
                 ρ :- (=> Nat Nat), hg :- (Good D ρ), f :- Exp,
                 hf :- (Rt chkf D (allW D) f (trF (PF.pall p) ρ))]
               '(Rt chkf D (allW D) (Exp.app f (trT t ρ))
                  (trF (paSbF (paInst t) p) ρ))
               '[(exact (pa_instantiate chkf p t D ρ hg f hf))]))))
  (testing "MP lowers the proof binder and preserves the all-omega vector"
    (is (not (b/rejects?
               '[chkf :- (=> Code Code Bool), p :- PF, q :- PF, D :- (List Exp),
                 ρ :- (=> Nat Nat), hg :- (Good D ρ), f :- Exp, u :- Exp,
                 hf :- (Rt chkf D (allW D) f (trF (PF.pimp p q) ρ)),
                 hu :- (Rt chkf D (allW D) u (trF p ρ))]
               '(Rt chkf D (allW D) (Exp.app f u) (trF q ρ))
               '[(exact (pa_mp chkf p q D ρ hg f u hf hu))]))))
  (testing "Gen binds a Nat variable at omega"
    (is (not (b/rejects?
               '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
                 ρ :- (=> Nat Nat), e :- Exp,
                 he :- (Rt chkf (List.cons Exp Exp.tNat D)
                           (allW (List.cons Exp Exp.tNat D)) e (trF p (trUpR ρ)))]
               '(Rt chkf D (allW D) (Exp.lam U.uw Exp.tNat e) (trF (PF.pall p) ρ))
               '[(exact (pa_generalize chkf p D ρ e he))]))))
  (testing "instantiation removes the binder rather than leaving variable zero"
    (is (b/rejects? []
          '(Eq Exp (subst1 (Exp.succ Exp.zero)
                     (trF (PF.peq (PT.pv 0) PT.pz) (trUpR (fn [i :- Nat] i))))
                   (trF (PF.peq (PT.pv 0) PT.pz) (fn [i :- Nat] i)))
          '[(rfl)]))))
