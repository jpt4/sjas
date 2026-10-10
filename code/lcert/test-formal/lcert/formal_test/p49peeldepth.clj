(ns lcert.formal-test.p49peeldepth
  (:require [clojure.test :refer [deftest is]]
            [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.p49peeldepth]))

(deftest depth-peel-obligations
  (is (b/has? 'p49pd_dl_rt))
  (is (b/has? 'p49pd_dl_lift))
  (is (b/has? 'p49pd_hself))
  (doseq [nm '[p49pd_hM p49pd_hY1 p49pd_hY2 p49pd_hLA p49pd_hNA p49pd_hNB p49pd_hX p49pd_hY]]
    (is (b/has? nm)))
  (is (b/has? 'p49pd_split_context))
  (is (b/has? 'p49pd_hq))
  (is (b/has? 'p49pd_peel)))

;; Discharge every uniform family premise with proofs, then instantiate
;; the assembled peel for an arbitrary predecessor j (not a finite sample).
(kdef p49pd_test_F (=> Exp Exp) (fn [c :- Exp] Exp.tUnit))
(thm p49pd_test_form [chkf :- (=> Code Code Bool)]
  (p49pd_Form chkf p49pd_test_F)
  (intro D c hc) (exact (Tl.fBase chkf D Exp.tUnit rfl)))
(thm p49pd_test_lift [] (p49pd_Lift p49pd_test_F) (intro n cut c) (rfl))
(thm p49pd_test_subst [] (p49pd_Subst p49pd_test_F) (intro sg c) (rfl))

(defn- ls [ty xs]
  (reduce #(list 'List.cons ty %2 %1) (list 'List.nil ty) (reverse xs)))
(let [M '(p49peel_motive (p49pd_A j) Exp.tUnit)
      leafD (ls 'Exp '[(p49peel_leafA (p49pd_A j)) Exp.tLbl Exp.tSyn])
      nodeD (ls 'Exp ['(p49pd_X j) '(p49pd_X j) '(p49peel_nodeA (p49pd_A j))
                      (list 'y2Ty M) (list 'y1Ty M) 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn])
      leafU (ls 'U '[U.u1 U.uw U.u0])
      nodeU (ls 'U '[U.u1 U.u1 U.u0 U.u1 U.u1 U.uw U.uw U.uw U.u0])]
  (a/prove-theorem 'p49pd_test_unit '[chkf :- (=> Code Code Bool), j :- Nat]
    '(Rt chkf (List.nil Exp) (List.nil U) (p49pd_term j Exp.tUnit Exp.star Exp.star)
       (Exp.tPi U.uw Exp.tSyn (p49peel_body (p49pd_A j) Exp.tUnit)))
    [(list 'exact
       (list 'p49pd_peel 'chkf 'j '(List.nil Exp) 'p49pd_test_F '(p49pd_test_form chkf)
         '(p49pd_test_lift) '(p49pd_test_subst) '(List.nil U) 'rfl 'Exp.star 'Exp.star
         (list 'Rt.rConst 'chkf leafD leafU 'Exp.star 'Exp.tUnit 'rfl 'rfl)
         (list 'Rt.rConst 'chkf nodeD nodeU 'Exp.star 'Exp.tUnit 'rfl 'rfl)))]))

;; A nonconstant conclusion family validates formation of the rebuilt
;; code. Its formation is not a proof that the checker returns true.
(kdef p49pd_test_C (=> Exp Exp) (fn [c :- Exp] (Exp.tT (Exp.chk c (cbot)))))
(thm p49pd_test_Cform [chkf :- (=> Code Code Bool)]
  (p49pd_Form chkf p49pd_test_C)
  (intro D c hc)
  (exact (Tl.fT chkf D (Exp.chk c (cbot))
    (Tl.zChk chkf D c (cbot) hc
      (Tl.zSleaf chkf D (Exp.lbl 15) (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl rfl))))))
(thm p49pd_test_Clift [] (p49pd_Lift p49pd_test_C) (intro n cut c) (rfl))
(thm p49pd_test_Csubst [] (p49pd_Subst p49pd_test_C) (intro sg c) (rfl))
(thm p49pd_test_Cnode [chkf :- (=> Code Code Bool), j :- Nat, D :- (List Exp)]
  (Tl chkf Bool.true (p49pd_De j (p49pd_test_C (Exp.var 0)) D)
    (p49peel_nodeB (p49pd_test_C (Exp.var 0))) Exp.tUnit)
  (exact (p49pd_hNB chkf j D p49pd_test_C
    (p49pd_test_Cform chkf) (p49pd_test_Clift) (p49pd_test_Csubst))))

(deftest assembled-successor-peel
  (is (b/has? 'p49pd_test_unit))
  (is (b/has? 'p49pd_test_Cnode))
  ;; The unit instance does not establish the all-depth empty conclusion.
  (is (b/rejects? '[chkf :- (=> Code Code Bool), j :- Nat]
        '(Rt chkf (List.nil Exp) (List.nil U) (p49pd_term j Exp.tUnit Exp.star Exp.star)
           (Exp.tPi U.uw Exp.tSyn (p49peel_body (p49pd_A j) Exp.tEmpty)))
        '[(exact (p49pd_test_unit chkf j))]))
  ;; The pair has child1 at index4 and child2 at index4 under its binder.
  ;; Reusing index3 in the domain is the wrong child and is rejected.
  (is (b/rejects? '[chkf :- (=> Code Code Bool), j :- Nat, W :- Exp]
        '(Rt chkf (p49pd_De j W (List.nil Exp)) (p49peel_Uq (List.nil U)) (p49pd_q j)
           (Exp.tSig U.u1 (Exp.tT (p49dl j (Exp.var 3))) (p49pd_X j)))
        '[(exact (p49pd_hq chkf j W (List.nil Exp) (List.nil U) rfl))]))
  ;; Runtime computation cannot erase an arbitrary input-code resource.
  (is (b/rejects? '[chkf :- (=> Code Code Bool), k :- Nat, c :- Exp,
                   hc :- (Rt chkf (consE Exp.tSyn (List.nil Exp)) (consU U.uw (List.nil U)) c Exp.tSyn)]
        '(Rt chkf (consE Exp.tSyn (List.nil Exp)) (consU U.u0 (List.nil U)) (p49dl k c) Exp.tBool)
        '[(exact (p49pd_dl_rt chkf k (consE Exp.tSyn (List.nil Exp))
                   (consU U.uw (List.nil U)) c rfl hc))])))
