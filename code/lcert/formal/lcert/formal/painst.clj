(ns lcert.formal.painst
  "F6.3d — typed instantiation, modus ponens and generalization (R4 §6.2).

  pa_instantiate is A4's elimination step: application of a translated
  universal formula to a translated PA term has exactly the translated
  PA-substituted formula as its type. pa_mp lowers an implication's proof
  binder using trF_lower; pa_generalize adds an omega Nat binder. These
  are rules about supplied derivations, not yet the translation of all PA
  axiom schemes or a consistency result.

  The PA substitution identities below are syntactic inductions. In
  particular, neither semantic soundness of H_PA nor pa_consistent is used."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.paform]))

;; Pointwise identity hypotheses avoid equating functions extensionally.
;; Under a PA quantifier the hypothesis is lifted by paUp_idlike.
(thm paSbT_idlike [σ :- (=> Nat PT), h :- (forall [i Nat] (Eq PT (σ i) (PT.pv i))), t :- PT]
  (Eq PT (paSbT σ t) t)
  (induction t) (exact (h i)) (rfl)
  (have e (Eq PT (PT.ps (paSbT σ t)) (PT.ps t)) (congrArg PT.ps ih_t)) (exact e)
  (have e (Eq PT (PT.padd (paSbT σ t) (paSbT σ u)) (PT.padd t u))
    (congr (congrArg PT.padd ih_t) ih_u)) (exact e)
  (have e (Eq PT (PT.pmul (paSbT σ t) (paSbT σ u)) (PT.pmul t u))
    (congr (congrArg PT.pmul ih_t) ih_u)) (exact e))
(thm paUp_idlike [σ :- (=> Nat PT), h :- (forall [i Nat] (Eq PT (σ i) (PT.pv i))), i :- Nat]
  (Eq PT (paUp σ i) (PT.pv i))
  (cases i) (rfl) (exact (congrArg (paSbT paShT) (h n))))
(thm paSbF_idlike [φ :- PF]
  (forall [σ (=> Nat PT)]
    (=> (forall [i Nat] (Eq PT (σ i) (PT.pv i))) (Eq PF (paSbF σ φ) φ)))
  (induction φ)
  (intro σ h)
  (have e (Eq PF (PF.peq (paSbT σ t) (paSbT σ u)) (PF.peq t u))
    (congr (congrArg PF.peq (paSbT_idlike σ h t)) (paSbT_idlike σ h u))) (exact e)
  (intro σ h) (rfl)
  (intro σ h)
  (have e (Eq PF (PF.pimp (paSbF σ p) (paSbF σ q)) (PF.pimp p q))
    (congr (congrArg PF.pimp (ih_p σ h)) (ih_q σ h))) (exact e)
  (intro σ h)
  (have e (Eq PF (PF.pall (paSbF (paUp σ) p)) (PF.pall p))
    (congrArg PF.pall (ih_p (paUp σ) (paUp_idlike σ h)))) (exact e))
(thm paSbT_id [t :- PT] (Eq PT (paSbT PT.pv t) t)
  (exact (paSbT_idlike PT.pv (fn [i :- Nat] (Eq.refl (PT.pv i))) t)))
(thm paSbF_id [φ :- PF] (Eq PF (paSbF PT.pv φ) φ)
  (exact (paSbF_idlike φ PT.pv (fn [i :- Nat] (Eq.refl (PT.pv i))))))

;; Instantiating a new binder cancels a shift; a lifted parallel
;; substitution commutes with that shift. These are the term identities
;; needed when closed n-ary PA combinators are instantiated successively.
(thm paSbT_inst_shift [a :- PT, b :- PT]
  (Eq PT (paSbT (paInst b) (paSbT paShT a)) a)
  (induction a) (rfl) (rfl)
  (have e (Eq PT (PT.ps (paSbT (paInst b) (paSbT paShT t))) (PT.ps t))
    (congrArg PT.ps ih_t)) (exact e)
  (have e (Eq PT (PT.padd (paSbT (paInst b) (paSbT paShT t)) (paSbT (paInst b) (paSbT paShT u))) (PT.padd t u))
    (congr (congrArg PT.padd ih_t) ih_u)) (exact e)
  (have e (Eq PT (PT.pmul (paSbT (paInst b) (paSbT paShT t)) (paSbT (paInst b) (paSbT paShT u))) (PT.pmul t u))
    (congr (congrArg PT.pmul ih_t) ih_u)) (exact e))
(thm paSbT_up_shift [σ :- (=> Nat PT), a :- PT]
  (Eq PT (paSbT (paUp σ) (paSbT paShT a)) (paSbT paShT (paSbT σ a)))
  (induction a) (rfl) (rfl)
  (have e (Eq PT (PT.ps (paSbT (paUp σ) (paSbT paShT t))) (PT.ps (paSbT paShT (paSbT σ t))))
    (congrArg PT.ps ih_t)) (exact e)
  (have e (Eq PT
    (PT.padd (paSbT (paUp σ) (paSbT paShT t)) (paSbT (paUp σ) (paSbT paShT u)))
    (PT.padd (paSbT paShT (paSbT σ t)) (paSbT paShT (paSbT σ u))))
    (congr (congrArg PT.padd ih_t) ih_u)) (exact e)
  (have e (Eq PT
    (PT.pmul (paSbT (paUp σ) (paSbT paShT t)) (paSbT (paUp σ) (paSbT paShT u)))
    (PT.pmul (paSbT paShT (paSbT σ t)) (paSbT paShT (paSbT σ u))))
    (congr (congrArg PT.pmul ih_t) ih_u)) (exact e))

;; An implication binder is absent from the image of trSucR, so any term
;; substituted for that binder merely lowers the translated formula.
(thm trF_lower [φ :- PF, u :- Exp, ρ :- (=> Nat Nat)]
  (Eq Exp (subst1 u (trF φ (trSucR ρ))) (trF φ ρ))
  (exact (Eq.trans
    (trF_subst φ (fn [j :- Nat] (inst1 u j)) (trSucR ρ) PT.pv ρ
      (fn [i :- Nat] (Eq.refl (Exp.var (ρ i)))))
    (congrArg (fn [p :- PF] (trF p ρ)) (paSbF_id φ)))))

(thm paTrLift0 [ρ :- (=> Nat Nat), i :- Nat]
  (Eq Nat (trLiftR 0 ρ i) (trSucR ρ i))
  (exact (congrArg
    (fn [bb :- Bool] (Bool.rec$1 (fn [_ :- Bool] Nat) (ρ i) (Nat.succ (ρ i)) bb))
    (Nat.ble_eq_true_of_le (Nat.zero_le (ρ i))))))
(thm trF_lift1 [φ :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (lift 1 0 (trF φ ρ)) (trF φ (trSucR ρ)))
  (exact (Eq.trans (trF_liftc φ 0 ρ)
           (trF_ext φ (trLiftR 0 ρ) (trSucR ρ) (paTrLift0 ρ)))))
(thm trF_lift2 [φ :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (lift 2 0 (trF φ ρ)) (trF φ (trSucR (trSucR ρ))))
  (exact (Eq.trans (Eq.symm (lift_comp (trF φ ρ) 1 1 0))
    (Eq.trans (congrArg (lift 1 0) (trF_lift1 φ ρ)) (trF_lift1 φ (trSucR ρ))))))

;; A4's core. All premises are available from formation and Good; the
;; conclusion's usage equation and substitution equation are explicit.
(thm pa_instantiate [chkf :- (=> Code Code Bool), φ :- PF, t :- PT,
                     D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ), f :- Exp,
                     hf :- (Rt chkf D (allW D) f (trF (PF.pall φ) ρ))]
  (Rt chkf D (allW D) (Exp.app f (trT t ρ)) (trF (paSbF (paInst t) φ) ρ))
  (exact (rt_cast chkf D (vadd (allW D) (vscale U.uw (allW D))) (allW D)
    (Exp.app f (trT t ρ)) (subst1 (trT t ρ) (trF φ (trUpR ρ))) (trF (paSbF (paInst t) φ) ρ)
    (Rt.rApp chkf D (allW D) (allW D) U.uw f (trT t ρ) Exp.tNat (trF φ (trUpR ρ))
      (Eq.refl Bool.true) hf (trT_rt chkf t D ρ hg)
      (Tl.fBase chkf D Exp.tNat (Eq.refl Bool.true))
      (trF_form chkf φ (List.cons Exp Exp.tNat D) (trUpR ρ) (paGood_up D ρ hg)))
    (paAllW_app D U.uw (Eq.refl Bool.true)) (trF_inst φ t ρ))))

(thm pa_mp [chkf :- (=> Code Code Bool), φ :- PF, ψ :- PF,
            D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ), f :- Exp, u :- Exp,
            hf :- (Rt chkf D (allW D) f (trF (PF.pimp φ ψ) ρ)),
            hu :- (Rt chkf D (allW D) u (trF φ ρ))]
  (Rt chkf D (allW D) (Exp.app f u) (trF ψ ρ))
  (exact (rt_cast chkf D (vadd (allW D) (vscale U.uw (allW D))) (allW D)
    (Exp.app f u) (subst1 u (trF ψ (trSucR ρ))) (trF ψ ρ)
    (Rt.rApp chkf D (allW D) (allW D) U.uw f u (trF φ ρ) (trF ψ (trSucR ρ))
      (Eq.refl Bool.true) hf hu (trF_form chkf φ D ρ hg)
      (trF_form chkf ψ (List.cons Exp (trF φ ρ) D) (trSucR ρ) (paGood_suc D ρ hg (trF φ ρ))))
    (paAllW_app D U.uw (Eq.refl Bool.true)) (trF_lower ψ u ρ))))

(thm pa_generalize [chkf :- (=> Code Code Bool), φ :- PF,
                    D :- (List Exp), ρ :- (=> Nat Nat), e :- Exp,
                    he :- (Rt chkf (List.cons Exp Exp.tNat D)
                              (allW (List.cons Exp Exp.tNat D)) e (trF φ (trUpR ρ)))]
  (Rt chkf D (allW D) (Exp.lam U.uw Exp.tNat e) (trF (PF.pall φ) ρ))
  (exact (Rt.rLam chkf D (allW D) U.uw Exp.tNat e (trF φ (trUpR ρ))
           (Tl.fBase chkf D Exp.tNat (Eq.refl Bool.true)) he)))
