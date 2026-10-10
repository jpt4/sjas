(ns lcert.formal-test.p49peelzero
  (:require [clojure.test :refer [deftest is]]
            [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.p49peelzero]))

(deftest depth-zero-peel-proofs
  (doseq [nm '[p49pz_hself p49pz_hM p49pz_hY1 p49pz_hY2
               p49pz_hLA p49pz_hNA p49pz_hNB p49pz_NA_lift
               p49pz_node_absurd p49pz_peel]]
    (is (b/has? nm))))

(kdef p49pz_test_F (=> Exp Exp) (fn [c :- Exp] Exp.tUnit))
(thm p49pz_test_form [chkf :- (=> Code Code Bool)]
  (p49pd_Form chkf p49pz_test_F)
  (intro D c hc) (exact (Tl.fBase chkf D Exp.tUnit rfl)))
(thm p49pz_test_lift [] (p49pd_Lift p49pz_test_F)
  (intro n cut c) (rfl))
(thm p49pz_test_subst [] (p49pd_Subst p49pz_test_F)
  (intro sg c) (rfl))

(a/prove-theorem 'p49pz_test_unit
  '[chkf :- (=> Code Code Bool)]
  '(Rt chkf (List.nil Exp) (List.nil U)
     (p49pz_term Exp.tUnit Exp.star)
     (Exp.tPi U.uw Exp.tSyn (p49peel_body p49pz_A Exp.tUnit)))
  ['(exact
     (p49pz_peel chkf (List.nil Exp) p49pz_test_F
       (p49pz_test_form chkf) (p49pz_test_lift) (p49pz_test_subst)
       (List.nil U) rfl Exp.star
       (Rt.rConst chkf
         (consE (p49peel_leafA p49pz_A)
           (consE Exp.tLbl (consE Exp.tSyn (List.nil Exp))))
         (consU U.u1 (consU U.uw (p49peel_Uz (List.nil U))))
         Exp.star Exp.tUnit rfl rfl)))])

(deftest concrete-leaf-instance
  (is (b/has? 'p49pz_test_unit))
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (List.nil Exp) (List.nil U)
           (p49pz_term Exp.tUnit Exp.star)
           (Exp.tPi U.uw Exp.tSyn (p49peel_body p49pz_A Exp.tEmpty)))
        '[(exact (p49pz_test_unit chkf))])))
