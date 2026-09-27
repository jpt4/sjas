(ns lcert.formal.examples
  "F2 checks of the rule table against the draft's worked example
  (R4-metatheory.md §1.4, 'the draft's derivation of not')."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]))

;; ⊢ λ(x :ω Bool). if x then ff else tt :¹ Bool → Bool, at budget 0.
(thm not_typed [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.nil Exp) (List.nil U) (Exp.lam U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt))
      (Exp.tPi U.uw Exp.tBool Exp.tBool))
  (exact (Rt.rLam chkf (List.nil Exp) (List.nil U) U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt) Exp.tBool
           (Tl.fBase chkf (List.nil Exp) Exp.tBool rfl)
           (Rt.rIte chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.uw (List.nil U))
                    (List.cons U U.uw (List.nil U)) (Exp.var 0) Exp.ff Exp.tt Exp.tBool
             (Rt.rVar chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.uw (List.nil U)) 0 Exp.tBool U.uw
                      rfl rfl rfl rfl)
             (Rt.rConst chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.uw (List.nil U)) Exp.ff Exp.tBool rfl rfl)
             (Rt.rConst chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.uw (List.nil U)) Exp.tt Exp.tBool rfl rfl)))))
