(ns lcert.formal.prop410witness
  "Proposition 4.10's concrete closed identity certificate and fixed budget."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.dtgen :as g]
            [lcert.formal.certcanon]
            [lcert.formal.section4b]))

;; The literal term and type in Proposition 4.10.  The untrusted Clojure
;; generator proposes derivation trees; dtCheck_sound and the kernel check
;; every premise before either tree contributes to a theorem.
(kdef id00Term Exp (Exp.lam U.u1 Exp.tEmpty (Exp.var 0)))
(kdef id00Type Exp (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))
(lcert.formal.base/kdef! 'id00Tree 'DT
  (g/rt [] [] [:lam :u1 [:tEmpty] [:var 0]]
    [:tPi :u1 [:tEmpty] [:tEmpty]]))
(lcert.formal.base/kdef! 'id00FormTree 'DT
  (g/tl-form [] [:tPi :u1 [:tEmpty] [:tEmpty]]))

(thm id00_closed []
  (Rt (Check decCert) (List.nil Exp) (List.nil U) id00Term id00Type)
  (exact (dtCheck_sound (Check decCert) id00Tree
           (DTJ.rt (List.nil Exp) (List.nil U) id00Term id00Type) rfl)))

;; check_complete_lbl supplies a label-safe accepted concrete certificate.
;; prop410 specializes its fixed resource budget to that certificate's exact
;; node count.  The existential is therefore the paper's fixed K witness.
(thm prop410_witness []
  (Exists (fn [v :- Code]
    (And (Eq Bool (lblOk v) Bool.true)
      (And (Eq Bool (Check decCert v (encE id00Type)) Bool.true)
        (Rt (Check decCert) (thetaD (cnodes v)) (thetaU (cnodes v))
          (Exp.lam U.u1 Exp.tR
            (Exp.lam U.u1 (chkT (Exp.var 0) (cbot))
              (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star)))
          (Hcirc))))))
  (have hc (Exists (fn [v :- Code]
             (And (Eq Bool (lblOk v) Bool.true)
                  (Eq Bool (Check decCert v (encE id00Type)) Bool.true))))
    (check_complete_lbl 0 id00Term id00Type id00Tree id00FormTree
      rfl rfl rfl rfl rfl rfl rfl))
  (cases hc)
  (constructor)
  (exact w)
  (constructor)
  (exact (And.left h))
  (constructor)
  (exact (And.right h))
  (exact (prop410 (Check decCert) (decOf decCert) encE
           (check_spec decCert) w (And.left h) (And.right h))))
