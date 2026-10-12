(ns lcert.formal-test.paform
  "R4 §6.2: formation and both typings under an arbitrary Good environment.
  The positive example aliases every PA variable to one Nat entry; this
  deliberately checks that Good does not secretly require an injection."
  (:require [lcert.formal.paform]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))

(deftest formation-theorems
  (doseq [nm '[Good allW allZ paGood_suc paGood_up paAllW_len paAllW_nth
              paW_mul paAllW_add paAllW_scale paZeroW_add paAllW_app paZeroW_app
              paClosed_tl paClosed_rt trT_tl trT_rt trF_form]]
    (is (b/has? nm) (str nm)))
  (doseq [term '[trEQ trPLUS trTIMES], suffix ["_rt_at" "_tl_at" "_rt_app2" "_tl_app2"]
          :let [nm (symbol (str term suffix))]]
    (is (b/has? nm) (str nm)))
  (testing "every translated term has both typings at every Good environment"
    (doseq [[goal theorem]
            [['(Tl chkf Bool.false D (trT t ρ) Exp.tNat) 'trT_tl]
             ['(Rt chkf D (allW D) (trT t ρ) Exp.tNat) 'trT_rt]]]
      (is (not (b/rejects?
                 '[chkf :- (=> Code Code Bool), t :- PT, D :- (List Exp),
                   ρ :- (=> Nat Nat), hg :- (Good D ρ)] goal
                 [(list 'exact (list theorem 'chkf 't 'D 'ρ 'hg))])))))
  (testing "every translated formula is formed, including under quantifiers"
    (is (not (b/rejects?
               '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
                 ρ :- (=> Nat Nat), hg :- (Good D ρ)]
               '(Tl chkf Bool.true D (trF p ρ) Exp.tUnit)
               '[(exact (trF_form chkf p D ρ hg))]))))
  (testing "Good permits repeated variables but rejects the wrong entry type"
    (is (not (b/rejects? []
               '(Good (List.cons Exp Exp.tNat (List.nil Exp)) (fn [i :- Nat] 0))
               '[(intro i) (exact (nthE.eq_2 Exp.tNat (List.nil Exp)))])))
    (is (b/rejects? []
          '(Good (List.cons Exp Exp.tBool (List.nil Exp)) (fn [i :- Nat] 0))
          '[(intro i) (exact (nthE.eq_2 Exp.tBool (List.nil Exp)))])))
  (testing "term translation does not have an arbitrary result type"
    (is (b/rejects?
          '[chkf :- (=> Code Code Bool), t :- PT, D :- (List Exp),
            ρ :- (=> Nat Nat), hg :- (Good D ρ)]
          '(Rt chkf D (allW D) (trT t ρ) Exp.tBool)
          '[(exact (trT_rt chkf t D ρ hg))]))))
