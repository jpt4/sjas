(ns lcert.formal-test.paind
  (:require [lcert.formal.paind]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest schematic-packaged-induction
  (doseq [nm '[paBang_form paBang_intro paBang_project paBang_lifts
              paBang_map_rt paInd_open_agree paInd_open_eq pa_ax_ind]]
    (is (b/has? nm) (str nm)))
  (testing "IND is uniform in formulas, not a check of concrete instances"
    (is (not (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
        ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e
        (trF (PF.pimp (paSbF (paInst PT.pz) p)
          (PF.pimp (PF.pall (PF.pimp p (paSbF paSuc p))) (PF.pall p))) ρ))))
      '[(exact (pa_ax_ind chkf p D ρ hg))]))))
  (testing "induction requires its base and step premises"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
        ρ :- (=> Nat Nat), hg :- (Good D ρ)]
      '(Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF (PF.pall p) ρ))))
      '[(exact (pa_ax_ind chkf p D ρ hg))]))))

(deftest package-consumption-is-affine
  (testing "the map theorem consumes the package at one, not omega"
    (is (b/rejects?
      '[chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, B :- Exp, e :- Exp,
        hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true D B Exp.tUnit),
        he :- (Rt chkf (consE A D) (allW (consE A D)) e (lift 1 0 B))]
      '(Rt chkf (consE (paBang A) D) (consU U.uw (allW D))
         (paBang_map A B e) (paBang (lift 1 0 B)))
      '[(exact (paBang_map_rt chkf D A B e hA hB he))]))))
