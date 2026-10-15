(ns lcert.formal.prop411cert
  "A10: a concrete non-normal certificate for Proposition 4.11."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm lv]]
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

;; F7 records the complete term encoding strictly below the certificate
;; root.  Reuse certcanon's exact generated node equation; the arithmetic
;; proof is checked by the kernel, while the Clojure expression generator
;; contributes no trusted inference.
(def ^:private cert-nodes-form
  ((deref #'lcert.formal.certcanon/nodes-of)
   (deref #'lcert.formal.certcanon/cert-expr)))
(a/prove-theorem 'cert_term_size
  (lv '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT])
  (lv '(LT.lt (cnodes (encE t)) (cnodes (encCert m t A T1 T2))))
  (lv [(list 'have 'q
         (list 'Eq 'Nat '(cnodes (encCert m t A T1 T2)) cert-nodes-form)
         '(cert_nodes m t A T1 T2))
       '(omega)]))

;; A literal for the normal code has at least one encoding node per code
;; node (E4), and its term encoding is a proper part of any F7 certificate.
(thm prop411_literal_lower [n :- Nat, m :- Nat, T1 :- DT, T2 :- DT]
  (LE.le (Nat.sub (sh4_pow n) 1)
         (cnodes (encCert m (codeTerm (sh4_bush n)) Exp.tSyn T1 T2)))
  (have hb (Eq Nat (cnodes (sh4_bush n)) (Nat.sub (sh4_pow n) 1))
    (prop411_nodes n))
  (have he (LT.lt (cnodes (sh4_bush n))
                  (cnodes (encE (codeTerm (sh4_bush n)))))
    (code_enc_gt (sh4_bush n)))
  (have hc (LT.lt (cnodes (encE (codeTerm (sh4_bush n))))
                  (cnodes (encCert m (codeTerm (sh4_bush n)) Exp.tSyn T1 T2)))
    (cert_term_size m (codeTerm (sh4_bush n)) Exp.tSyn T1 T2))
  (omega))
