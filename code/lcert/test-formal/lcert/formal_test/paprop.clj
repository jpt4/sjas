(ns lcert.formal-test.paprop
  (:require [lcert.formal.paprop]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(deftest schematic-axiom-templates
  (doseq [nm '[paHyp_allW trF_lift3 pa_ax_a1 pa_ax_a2 pa_ax_a4]]
    (is (b/has? nm) (str nm)))
  (testing "A1 inhabits its translation for arbitrary formulas and Good environments"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), a :- PF, b :- PF,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Rt chkf D (allW D)
         (Exp.lam U.uw (trF a ρ)
           (Exp.lam U.uw (trF b (trSucR ρ)) (Exp.var 1)))
         (trF (PF.pimp a (PF.pimp b a)) ρ))
      '[(exact (pa_ax_a1 chkf a b D ρ hg))]))))
  (testing "A2 shares its argument at omega"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), a :- PF, b :- PF, c :- PF,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Rt chkf D (allW D)
         (Exp.lam U.uw (trF (PF.pimp a (PF.pimp b c)) ρ)
           (Exp.lam U.uw (trF (PF.pimp a b) (trSucR ρ))
             (Exp.lam U.uw (trF a (trSucR (trSucR ρ)))
               (Exp.app (Exp.app (Exp.var 2) (Exp.var 0))
                        (Exp.app (Exp.var 1) (Exp.var 0))))))
         (trF (PF.pimp (PF.pimp a (PF.pimp b c))
                      (PF.pimp (PF.pimp a b) (PF.pimp a c))) ρ))
      '[(exact (pa_ax_a2 chkf a b c D ρ hg))]))))
  (testing "A4 binds its function before translating the argument"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, t :- PT,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Rt chkf D (allW D)
         (Exp.lam U.uw (trF (PF.pall p) ρ)
           (Exp.app (Exp.var 0) (trT t (trSucR ρ))))
         (trF (PF.pimp (PF.pall p) (paSbF (paInst t) p)) ρ))
      '[(exact (pa_ax_a4 chkf p t D ρ hg))]))))
  (testing "A1 cannot be replaced by the second projection at arbitrary formulas"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), a :- PF, b :- PF,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Rt chkf D (allW D)
         (Exp.lam U.uw (trF a ρ)
           (Exp.lam U.uw (trF b (trSucR ρ)) (Exp.var 0)))
         (trF (PF.pimp a (PF.pimp b a)) ρ))
      '[(exact (pa_ax_a1 chkf a b D ρ hg))]))))
