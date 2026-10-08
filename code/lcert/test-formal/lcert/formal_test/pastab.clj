(ns lcert.formal-test.pastab
  (:require [lcert.formal.pastab]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest schematic-stability
  (doseq [nm '[pa_stab_eq pa_stab_bot pa_stab_imp pa_stab_all pa_ax_dn]]
    (is (b/has? nm) (str nm)))
  (testing "DN is uniform in arbitrary formulas, including quantifiers"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
        ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e
        (trF (PF.pimp (PF.pimp (PF.pimp p PF.pbot) PF.pbot) p) ρ))))
      '[(exact (pa_ax_dn chkf p D ρ hg))]))))
  (testing "stability of bottom does not inhabit bottom"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e Exp.tEmpty)))
      '[(exact (pa_ax_dn chkf PF.pbot D ρ hg))]))))
