(ns lcert.formal.prop411cert
  "A10: a concrete non-normal certificate for Proposition 4.11."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.section4c]
            [lcert.formal.certcanon]))

;; Generated once by dtgen/rt for (λx.x) (sleaf 0), then stored literally so
;; editing the shared generator does not reload the PA translation family.
;; F7's dtCheck validates every premise of the proposed tree in the kernel.
(lcert.formal.base/kdef! 'sh4_c0_tree 'DT
  '(DT.rApp (List.nil Exp) (List.nil U) (List.nil U) U.uw
     (Exp.lam U.uw Exp.tSyn (Exp.var 0)) (Exp.sleaf (Exp.lbl 0))
     Exp.tSyn Exp.tSyn
     (DT.rLam (List.nil Exp) (List.nil U) U.uw Exp.tSyn (Exp.var 0)
       Exp.tSyn (DT.fBase (List.nil Exp) Exp.tSyn)
       (DT.rVar (List.cons Exp Exp.tSyn (List.nil Exp))
         (List.cons U U.uw (List.nil U)) 0 Exp.tSyn U.uw))
     (DT.rSleaf (List.nil Exp) (List.nil U) (Exp.lbl 0)
       (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl 0) Exp.tLbl))
     (DT.fBase (List.nil Exp) Exp.tSyn)
     (DT.fBase (List.cons Exp Exp.tSyn (List.nil Exp)) Exp.tSyn)))
(lcert.formal.base/kdef! 'sh4_c0_form 'DT
  '(DT.fBase (List.nil Exp) Exp.tSyn))

(thm prop411_redex_typed []
  (Rt (Check decCert) (List.nil Exp) (List.nil U) (sh4_cn 0) Exp.tSyn)
  (exact (dtCheck_sound (Check decCert) sh4_c0_tree
           (DTJ.rt (List.nil Exp) (List.nil U) (sh4_cn 0) Exp.tSyn) rfl)))

;; The accepted tree records an application of a lambda at its root.  This
;; is a beta redex in a checked derivation, hence certificates need not be
;; normal derivations.
(thm prop411_redex_cert []
  (Exists (fn [c :- Code]
    (Eq Bool (Check decCert c (encE Exp.tSyn)) Bool.true)))
  (exact (check_complete 0 (sh4_cn 0) Exp.tSyn sh4_c0_tree sh4_c0_form
           rfl rfl rfl)))
