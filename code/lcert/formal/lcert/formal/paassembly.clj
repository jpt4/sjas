(ns lcert.formal.paassembly
  "F6.3 — assembly rules for the PA translation (R4 §6.2).

  The main proof translation still needs DN, E2 and IND. The theorem
  pa_lcert_of_axioms explicitly assumes typed axiom instances; it is not
  pa_lcert. pa_close_refutation closes a supplied translation of 0=S0.
  These are meta-level derivation constructions, not internal PA proofs.
  Any resulting consistency corollary is relative to the metatheory."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.paaxioms]))

;; This is the PPrv induction for the desired main theorem. Its axiom
;; premise is explicit until DN, E2 and IND have been supplied. In
;; particular this theorem does not claim that those templates are proved.
(thm pa_lcert_of_axioms [chkf :- (=> Code Code Bool),
  hax :- (forall [p PF] (=> (PAx p)
    (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
      (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF p ρ))))))))),
  p :- PF, hd0 :- (PPrv p)]
  (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
    (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF p ρ)))))))
  (induction hd0)
  (intro D ρ hg) (exact (hax g hx D ρ hg))
  (intro D ρ hg)
  (refine' (exists_elimE _ _ (ih_hp D ρ hg) _)) (intro e he)
  (refine' (exists_elimE _ _ (ih_hi D ρ hg) _)) (intro f hf)
  (constructor) (exact (Exp.app f e)) (exact (pa_mp chkf g k D ρ hg f e hf he))
  (intro D ρ hg)
  (refine' (exists_elimE _ _ (ih_hp (List.cons Exp Exp.tNat D) (trUpR ρ) (paGood_up D ρ hg)) _))
  (intro e he) (constructor) (exact (Exp.lam U.uw Exp.tNat e))
  (exact (pa_generalize chkf g D ρ e he)))

;; All free PA variables may alias this one Nat entry. The formula 0=S0
;; is closed, but Good's total assignment cannot live in an empty context.
(kdef paRefuteR (=> Nat Nat) (fn [i :- Nat] 0))
(kdef paRefuteD (List Exp) (List.cons Exp Exp.tNat (List.nil Exp)))
(thm paRefute_good [] (Good paRefuteD paRefuteR)
  (intro i) (exact (nthE.eq_2 Exp.tNat (List.nil Exp))))
(kdef paRefuteTerm (=> Exp Exp)
  (fn [e :- Exp]
    (Exp.app (Exp.lam U.uw Exp.tNat
      (Exp.app (paPad (trF paF_COE paRefuteR) paC_COE) e)) Exp.zero)))

;; The final coercion and closing application are unconditional typing
;; rules about a supplied translation of 0=S0, not an inhabitant of it.
(thm pa_close_refutation [chkf :- (=> Code Code Bool), e :- Exp,
  he :- (Rt chkf paRefuteD (allW paRefuteD) e
    (trF (PF.peq PT.pz (PT.ps PT.pz)) paRefuteR))]
  (Rt chkf (thetaD 0) (thetaU 0) (paRefuteTerm e) Exp.tEmpty)
  (exact (Rt.rApp chkf (List.nil Exp) (List.nil U) (List.nil U) U.uw
    (Exp.lam U.uw Exp.tNat (Exp.app (paPad (trF paF_COE paRefuteR) paC_COE) e))
    Exp.zero Exp.tNat Exp.tEmpty (Eq.refl Bool.true)
    (Rt.rLam chkf (List.nil Exp) (List.nil U) U.uw Exp.tNat
      (Exp.app (paPad (trF paF_COE paRefuteR) paC_COE) e) Exp.tEmpty
      (Tl.fBase chkf (List.nil Exp) Exp.tNat (Eq.refl Bool.true))
      (pa_mp chkf (PF.peq PT.pz (PT.ps PT.pz)) PF.pbot paRefuteD paRefuteR paRefute_good
        (paPad (trF paF_COE paRefuteR) paC_COE) e (paC_COE_at chkf paRefuteD paRefuteR) he))
    (Rt.rConst chkf (List.nil Exp) (List.nil U) Exp.zero Exp.tNat rfl rfl)
    (Tl.fBase chkf (List.nil Exp) Exp.tNat rfl)
    (Tl.fBase chkf paRefuteD Exp.tEmpty rfl))))

;; Once pa_lcert is available, this implication is the desired
;; pa_refutation. No appeal to pa_consistent or paPrv_sound occurs.
(thm pa_refutation_of_translation [chkf :- (=> Code Code Bool),
  htr :- (forall [p PF] (=> (PPrv p)
    (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
      (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF p ρ))))))))),
  hp :- (PPrv (PF.peq PT.pz (PT.ps PT.pz)))]
  (Exists (fn [t :- Exp] (Rt chkf (thetaD 0) (thetaU 0) t Exp.tEmpty)))
  (refine' (exists_elimE _ _ (htr (PF.peq PT.pz (PT.ps PT.pz)) hp paRefuteD paRefuteR paRefute_good) _))
  (intro e he) (constructor) (exact (paRefuteTerm e))
  (exact (pa_close_refutation chkf e he)))
