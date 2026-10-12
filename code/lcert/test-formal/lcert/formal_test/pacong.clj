(ns lcert.formal-test.pacong
  (:require [lcert.formal.pacong]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest schematic-equality-substitution
  (doseq [nm '[pa_eq_refl pa_eq_sym pa_eq_trans pa_eq_congs pa_eq_congp
              pa_eq_congt pa_cong pa_ax_e2]]
    (is (b/has? nm) (str nm)))
  (testing "E2 translates arbitrary terms and arbitrary renaming indices"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), i :- Nat, j :- Nat, z :- Nat, s :- PT, t :- PT,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e
        (trF (PF.pimp (PF.peq (PT.pv i) (PT.pv j))
          (PF.pimp (PF.peq (paSbT (paRn z i) s) (paSbT (paRn z i) t))
                   (PF.peq (paSbT (paRn z j) s) (paSbT (paRn z j) t)))) ρ))))
      '[(exact (pa_ax_e2 chkf i j z s t D ρ hg))]))))
  (testing "congruence cannot equate arbitrary terms without an equality premise"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), s :- PT, t :- PT,
        D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF (PF.peq s t) ρ))))
      '[(exact (pa_eq_refl chkf s D ρ hg))]))))
