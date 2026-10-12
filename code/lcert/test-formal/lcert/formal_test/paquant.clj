(ns lcert.formal-test.paquant
  (:require [lcert.formal.paquant]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest quantifier-distribution
  (doseq [nm '[trF_identity_subst trF_shift trF_open_var pa_open pa_ax_a5]]
    (is (b/has? nm) (str nm)))
  (testing "opening a universal formula keeps the new Nat variable"
    (is (not (b/rejects? '[p :- PF, ρ :- (=> Nat Nat)]
      '(Eq Exp (subst1 (Exp.var 0) (trF p (trUpR (trSucR ρ))))
               (trF p (trUpR ρ)))
      '[(exact (trF_open_var p ρ))]))))
  (testing "A5 uses the new Nat and the shifted proof hypothesis"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, q :- PF,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Rt chkf D (allW D)
         (Exp.lam U.uw (trF (PF.pall (PF.pimp (paSbF paShT p) q)) ρ)
           (Exp.lam U.uw (trF p (trSucR ρ))
             (Exp.lam U.uw Exp.tNat
               (Exp.app (Exp.app (Exp.var 2) (Exp.var 0)) (Exp.var 1)))))
         (trF (PF.pimp (PF.pall (PF.pimp (paSbF paShT p) q))
                      (PF.pimp p (PF.pall q))) ρ))
      '[(exact (pa_ax_a5 chkf p q D ρ hg))]))))
  (testing "PA shift is needed when moving the antecedent under forall"
    (is (b/rejects? []
      '(Eq Exp
         (trF (PF.peq (PT.pv 0) PT.pz) (trUpR (fn [i :- Nat] i)))
         (trF (PF.peq (PT.pv 0) PT.pz) (trSucR (fn [i :- Nat] i))))
      '[(rfl)]))))
