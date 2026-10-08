(ns lcert.formal.palcert
  "F6.3 — meta-level translation of every H_PA proof into λᶜᵉʳᵗ₀.

  All axiom templates are discharged by object-calculus derivations.
  The result is uniform in the checker and in every Good environment.
  This is not Proposition 6.1's internal PA proof-code theorem. Combined
  with consistent_concrete, pa_refutation yields consistency of PA
  relative to the metatheory, not an internal PA consistency proof."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.paassembly]
            [lcert.formal.pacong]
            [lcert.formal.paind]
            [lcert.formal.pastab]))
;; Every H_PA axiom, by its constructor. No semantic soundness or
;; consistency theorem is used to produce these object derivations.
(thm pa_axiom_translation [chkf :- (=> Code Code Bool), f0 :- PF, hx :- (PAx f0)]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
    (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF f0 ρ)))))))
  (cases hx)
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_a1 chkf a b D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_a2 chkf a b c D ρ hg)))
  (intro D ρ hg) (exact (pa_ax_dn chkf a D ρ hg))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_a4 chkf p t D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_a5 chkf p q D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_e1 chkf i D ρ hg)))
  (intro D ρ hg) (exact (pa_ax_e2 chkf i j z s t D ρ hg))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q1 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q2 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q3 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q4 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q5 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_exists_given chkf _ D ρ _ (pa_ax_q6 chkf D ρ hg)))
  (intro D ρ hg) (exact (pa_ax_ind chkf p D ρ hg)))

;; R4 section 6.2 / F6.3: every H_PA proof translates in every Good
;; environment, with all-omega usage. This is a meta-level construction;
;; it is not the internalized proof-code theorem inside PA.
(thm pa_lcert [chkf :- (=> Code Code Bool), p :- PF, hp :- (PPrv p)]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
    (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF p ρ)))))))
  (exact (pa_lcert_of_axioms chkf (pa_axiom_translation chkf) p hp)))

;; A PA proof of 0=S0 would give a closed, budget-zero refutation.
;; Together with consistent_concrete this gives consistency of PA
;; relative to the metatheory, not a PA-internal consistency proof.
(thm pa_refutation [chkf :- (=> Code Code Bool), hp :- (PPrv (PF.peq PT.pz (PT.ps PT.pz)))]
  (Exists (fn [t :- Exp] (Rt chkf (thetaD 0) (thetaU 0) t Exp.tEmpty)))
  (exact (pa_refutation_of_translation chkf (pa_lcert chkf) hp)))
