(ns lcert.formal.paconsistency
  "F6.3 — consistency of PA relative to the metatheory via λᶜᵉʳᵗ₀.

  Compose the syntactic translation with selfjust's consistent_concrete.
  This is a theorem of the Ansatz metatheory, not a consistency theorem
  proved inside PA. The proof does not use pa_consistent or paPrv_sound."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.palcert]
            [lcert.formal.selfjust]))

;; Specialize the translation to the concrete checker, extract its closed
;; budget-zero witness, and contradict T1 at that same checker and budget.
;; No semantic interpretation of PA is used in this composition.
(thm pa_consistent_via_lcert []
  (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))
  ;; ‹proof elided: lines 16–19 of the full file; kernel-checked›
  )
