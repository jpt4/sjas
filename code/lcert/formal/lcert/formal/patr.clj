(ns lcert.formal.patr
  "F6.3a — the translation of PA into λᶜᵉʳᵗ₀ (R4-metatheory.md §6.2), at the
  meta level (ADR-0006, F6 plan), and its substitution and lifting lemmas.

  The closed definitions ISZERO, PRED, EQ, PLUS and TIMES are R4's, in de
  Bruijn form. They are fixed by every substitution and lifting
  (trEQ_subst, trEQ_lift, …, by computation).

  The translation is relative to an environment ρ : Nat → Nat, which sends
  a PA variable index to a λᶜᵉʳᵗ₀ variable index. That is needed because the
  translation of an implication φ → ψ binds a hypothesis variable, and ψ's
  variables must skip it (trSucR), while a ∀ binds a Nat variable (trUpR):
  - trT: pv i ↦ var (ρ i), 0, S, + and · by trPLUS and trTIMES;
  - trF: t = u ↦ T(EQ ⟦t⟧ ⟦u⟧), ⊥ ↦ 0, φ → ψ ↦ Π(_ :ω ⟦φ⟧). ⟦ψ⟧,
    ∀φ ↦ Π(_ :ω Nat). ⟦φ⟧.

  The substitution lemma (trF_subst): if τ agrees with the translation on
  ρ's image (τ (ρ i) = ⟦θ i⟧ρ′ for every i), then τ applied to ⟦φ⟧ρ is
  ⟦φ[θ]⟧ρ′. Its instances (trF_inst, for A4 and IND's base; trF_suc, for
  IND's step) identify translated instances with substituted types as
  syntax: R4's 'substitution fact'. trF_liftc relates lifting (as variable
  lookups and stepTy perform it) to an environment shift; trF_ext,
  extensionality in the environment. All by induction on PA syntax."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.pa]))

(defn- prove! [nm params goal tactics] (a/prove-theorem nm (lv params) (lv goal) (lv tactics)))

(kdef trISZERO Exp (Exp.lam U.uw Exp.tNat (Exp.recN Exp.tBool Exp.tt Exp.ff (Exp.var 0))))

(kdef trPRED Exp (Exp.lam U.uw Exp.tNat (Exp.recN Exp.tNat Exp.zero (Exp.var 1) (Exp.var 0))))

(kdef trEQ Exp (Exp.lam U.uw Exp.tNat (Exp.recN (Exp.tPi U.uw Exp.tNat Exp.tBool) trISZERO
   (Exp.lam U.uw Exp.tNat (Exp.ite (Exp.app trISZERO (Exp.var 0)) Exp.ff (Exp.app (Exp.var 1) (Exp.app trPRED (Exp.var 0))))) (Exp.var 0))))

(kdef trPLUS Exp (Exp.lam U.u1 Exp.tNat (Exp.lam U.uw Exp.tNat (Exp.recN Exp.tNat (Exp.var 1) (Exp.succ (Exp.var 0)) (Exp.var 0)))))

(kdef trTIMES Exp (Exp.lam U.uw Exp.tNat (Exp.lam U.uw Exp.tNat (Exp.recN Exp.tNat Exp.zero (Exp.app (Exp.app trPLUS (Exp.var 0)) (Exp.var 3)) (Exp.var 0)))))

(doseq [c '[trISZERO trPRED trEQ trPLUS trTIMES]]
  (prove! (symbol (str c "_subst")) '[τ :- (=> Nat Exp)] (list 'Eq 'Exp (list 'subst 'τ c) c) '[(rfl)])
  (prove! (symbol (str c "_lift")) '[k :- Nat, cc :- Nat] (list 'Eq 'Exp (list 'lift 'k 'cc c) c) '[(rfl)]))

(kdef trSucR (=> (=> Nat Nat) (=> Nat Nat)) (fn [ρ :- (=> Nat Nat)] (fn [i :- Nat] (Nat.succ (ρ i)))))

(kdef trUpR (=> (=> Nat Nat) (=> Nat Nat))
  (fn [ρ :- (=> Nat Nat)] (fn [i :- Nat] (Nat.rec$1 (fn [_ :- Nat] Nat) 0 (fn [k :- Nat, _ :- Nat] (Nat.succ (ρ k))) i))))

(kdef trT (=> PT (=> Nat Nat) Exp)
  (fn [t :- PT, ρ :- (=> Nat Nat)]
    (PT.rec$1 (fn [_ :- PT] Exp)
      (fn [i :- Nat] (Exp.var (ρ i))) Exp.zero
      (fn [t :- PT, it :- Exp] (Exp.succ it))
      (fn [t :- PT, u :- PT, it :- Exp, iu :- Exp] (Exp.app (Exp.app trPLUS it) iu))
      (fn [t :- PT, u :- PT, it :- Exp, iu :- Exp] (Exp.app (Exp.app trTIMES it) iu)) t)))

(kdef trFF (=> PF (=> (=> Nat Nat) Exp))
  (fn [φ :- PF]
    (PF.rec$1 (fn [_ :- PF] (=> (=> Nat Nat) Exp))
      (fn [t :- PT, u :- PT] (fn [ρ :- (=> Nat Nat)] (Exp.tT (Exp.app (Exp.app trEQ (trT t ρ)) (trT u ρ)))))
      (fn [ρ :- (=> Nat Nat)] Exp.tEmpty)
      (fn [p :- PF, q :- PF, ip :- (=> (=> Nat Nat) Exp), iq :- (=> (=> Nat Nat) Exp)] (fn [ρ :- (=> Nat Nat)] (Exp.tPi U.uw (ip ρ) (iq (trSucR ρ)))))
      (fn [p :- PF, ip :- (=> (=> Nat Nat) Exp)] (fn [ρ :- (=> Nat Nat)] (Exp.tPi U.uw Exp.tNat (ip (trUpR ρ)))))
      φ)))

(kdef trF (=> PF (=> Nat Nat) Exp) (fn [φ :- PF, ρ :- (=> Nat Nat)] (trFF φ ρ)))

(prove! 'trT_lift '[t :- PT, ρ :- (=> Nat Nat)] '(Eq Exp (lift 1 0 (trT t ρ)) (trT t (trSucR ρ)))
  '[(induction t)
    (exact (lift_var_above 1 0 (ρ i) (Nat.zero_le (ρ i))))
    (rfl)
    (have q (Eq Exp (Exp.succ (lift 1 0 (trT t ρ))) (Exp.succ (trT t (trSucR ρ)))) (congrArg Exp.succ ih_t)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trPLUS (lift 1 0 (trT t ρ))) (lift 1 0 (trT u ρ))) (Exp.app (Exp.app trPLUS (trT t (trSucR ρ))) (trT u (trSucR ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) ih_t) ih_u)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trTIMES (lift 1 0 (trT t ρ))) (lift 1 0 (trT u ρ))) (Exp.app (Exp.app trTIMES (trT t (trSucR ρ))) (trT u (trSucR ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) ih_t) ih_u)) (exact q)])

(def HYP '(forall [i Nat] (Eq Exp (τ (ρ i)) (trT (θ i) ρ2))))

(def PS '[τ :- (=> Nat Exp), ρ :- (=> Nat Nat), θ :- (=> Nat PT), ρ2 :- (=> Nat Nat)])

;; Substitution into a translated term: if τ agrees with the translation on
;; ρ's image, τ applied to ⟦t⟧ρ is ⟦t[θ]⟧ρ′ (by induction on t).
(prove! 'trT_subst (into PS ['h :- HYP 't :- 'PT]) '(Eq Exp (subst τ (trT t ρ)) (trT (paSbT θ t) ρ2))
  '[(induction t)
    (exact (Eq.trans (subst_var τ (ρ i)) (h i)))
    (rfl)
    (have q (Eq Exp (Exp.succ (subst τ (trT t ρ))) (Exp.succ (trT (paSbT θ t) ρ2))) (congrArg Exp.succ ih_t)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trPLUS (subst τ (trT t ρ))) (subst τ (trT u ρ))) (Exp.app (Exp.app trPLUS (trT (paSbT θ t) ρ2)) (trT (paSbT θ u) ρ2)))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) ih_t) ih_u)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trTIMES (subst τ (trT t ρ))) (subst τ (trT u ρ))) (Exp.app (Exp.app trTIMES (trT (paSbT θ t) ρ2)) (trT (paSbT θ u) ρ2)))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) ih_t) ih_u)) (exact q)])

(prove! 'trT_shift '[t :- PT, ρ :- (=> Nat Nat)] '(Eq Exp (trT (paSbT paShT t) (trUpR ρ)) (trT t (trSucR ρ)))
  '[(induction t) (rfl) (rfl)
    (have q (Eq Exp (Exp.succ (trT (paSbT paShT t) (trUpR ρ))) (Exp.succ (trT t (trSucR ρ)))) (congrArg Exp.succ ih_t)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trPLUS (trT (paSbT paShT t) (trUpR ρ))) (trT (paSbT paShT u) (trUpR ρ))) (Exp.app (Exp.app trPLUS (trT t (trSucR ρ))) (trT u (trSucR ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) ih_t) ih_u)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trTIMES (trT (paSbT paShT t) (trUpR ρ))) (trT (paSbT paShT u) (trUpR ρ))) (Exp.app (Exp.app trTIMES (trT t (trSucR ρ))) (trT u (trSucR ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) ih_t) ih_u)) (exact q)])

;; The agreement condition survives the two kinds of binder the translation
;; introduces: a hypothesis (trSucR) and a Nat variable (trUpR).
(prove! 'trHyp_suc (into PS ['h :- HYP]) '(forall [i Nat] (Eq Exp (up τ (trSucR ρ i)) (trT (θ i) (trSucR ρ2))))
  '[(intro i) (exact (Eq.trans (congrArg (lift 1 0) (h i)) (trT_lift (θ i) ρ2)))])

(prove! 'trHyp_up (into PS ['h :- HYP]) '(forall [i Nat] (Eq Exp (up τ (trUpR ρ i)) (trT (paUp θ i) (trUpR ρ2))))
  '[(intro i) (cases i) (rfl)
    (exact (Eq.trans (congrArg (lift 1 0) (h n)) (Eq.trans (trT_lift (θ n) ρ2) (Eq.symm (trT_shift (θ n) ρ2)))))])

(def MOT '(forall [τ (=> Nat Exp)] (forall [ρ (=> Nat Nat)] (forall [θ (=> Nat PT)] (forall [ρ2 (=> Nat Nat)]
            (=> (forall [i Nat] (Eq Exp (τ (ρ i)) (trT (θ i) ρ2))) (Eq Exp (subst τ (trF φ ρ)) (trF (paSbF θ φ) ρ2))))))))

(def PSH (into PS ['h :- HYP]))

(def IHT (fn [x env] (list 'forall '[τ (=> Nat Exp)] (list 'forall '[ρ (=> Nat Nat)] (list 'forall '[θ (=> Nat PT)] (list 'forall '[ρ2 (=> Nat Nat)]
            (list '=> HYP (list 'Eq 'Exp (list 'subst 'τ (list 'trF x 'ρ)) (list 'trF (list 'paSbF 'θ x) 'ρ2)))))))))

(prove! 'trF_subst_imp (into PSH ['p :- 'PF 'q :- 'PF 'ihp :- (IHT 'p nil) 'ihq :- (IHT 'q nil)])
  '(Eq Exp (subst τ (trF (PF.pimp p q) ρ)) (trF (paSbF θ (PF.pimp p q)) ρ2))
  '[(have r (Eq Exp (Exp.tPi U.uw (subst τ (trF p ρ)) (subst (upn 1 τ) (trF q (trSucR ρ))))
                    (Exp.tPi U.uw (trF (paSbF θ p) ρ2) (trF (paSbF θ q) (trSucR ρ2))))
      (congr (congrArg (Exp.tPi U.uw) (ihp τ ρ θ ρ2 h)) (ihq (upn 1 τ) (trSucR ρ) θ (trSucR ρ2) (trHyp_suc τ ρ θ ρ2 h))))
    (exact r)])

(prove! 'trF_subst_all (into PSH ['p :- 'PF 'ihp :- (IHT 'p nil)])
  '(Eq Exp (subst τ (trF (PF.pall p) ρ)) (trF (paSbF θ (PF.pall p)) ρ2))
  '[(have r (Eq Exp (Exp.tPi U.uw Exp.tNat (subst (upn 1 τ) (trF p (trUpR ρ))))
                    (Exp.tPi U.uw Exp.tNat (trF (paSbF (paUp θ) p) (trUpR ρ2))))
      (congrArg (Exp.tPi U.uw Exp.tNat) (ihp (upn 1 τ) (trUpR ρ) (paUp θ) (trUpR ρ2) (trHyp_up τ ρ θ ρ2 h))))
    (exact r)])

(prove! 'trT_appEQ (into PSH '[t :- PT])
  '(Eq Exp (Exp.app (subst τ trEQ) (subst τ (trT t ρ))) (Exp.app trEQ (trT (paSbT θ t) ρ2)))
  '[(exact (congr (congrArg Exp.app (trEQ_subst τ)) (trT_subst τ ρ θ ρ2 h t)))])

(prove! 'trT_subst_eq (into PSH '[t :- PT])
  '(Eq Exp (subst τ (trT t ρ)) (trT (paSbT θ t) ρ2))
  '[(exact (trT_subst τ ρ θ ρ2 h t))])

;; Substitution into a translated formula, case by case (each case its own
;; lemma: the Ansatz elaborator mis-handles the composite congruence inline).
(prove! 'trF_subst_eq (into PSH '[t :- PT, u :- PT])
  '(Eq Exp (subst τ (trF (PF.peq t u) ρ)) (trF (paSbF θ (PF.peq t u)) ρ2))
  '[(change (Eq Exp (Exp.tT (Exp.app (Exp.app (subst τ trEQ) (subst τ (trT t ρ))) (subst τ (trT u ρ))))
                    (Exp.tT (Exp.app (Exp.app trEQ (trT (paSbT θ t) ρ2)) (trT (paSbT θ u) ρ2)))))
    (exact (congrArg Exp.tT (congr (congrArg Exp.app (trT_appEQ τ ρ θ ρ2 h t)) (trT_subst_eq τ ρ θ ρ2 h u))))])

(prove! 'trF_subst '[φ :- PF] MOT
  '[(induction φ)
    (intro τ ρ θ ρ2 h) (exact (trF_subst_eq τ ρ θ ρ2 h t u))
    (intro τ ρ θ ρ2 h) (rfl)
    (intro τ ρ θ ρ2 h) (exact (trF_subst_imp τ ρ θ ρ2 h p q ih_p ih_q))
    (intro τ ρ θ ρ2 h) (exact (trF_subst_all τ ρ θ ρ2 h p ih_p))])

;; The single-variable instance: A4's ∀φ → φ[t/0] and IND's base φ[0/0].
(prove! 'trInst_hyp '[t :- PT, ρ :- (=> Nat Nat)]
  '(forall [i Nat] (Eq Exp ((fn [j :- Nat] (inst1 (trT t ρ) j)) (trUpR ρ i)) (trT (paInst t i) ρ)))
  '[(intro i) (cases i) (rfl) (rfl)])

(prove! 'trF_inst '[φ :- PF, t :- PT, ρ :- (=> Nat Nat)]
  '(Eq Exp (subst1 (trT t ρ) (trF φ (trUpR ρ))) (trF (paSbF (paInst t) φ) ρ))
  '[(exact (trF_subst φ (fn [j :- Nat] (inst1 (trT t ρ) j)) (trUpR ρ) (paInst t) ρ (trInst_hyp t ρ)))])

(def BR (fn [x] (list 'Bool.rec$1 '(fn [_ :- Bool] Nat) x (list 'Nat.succ x))))

(kdef trLiftR (=> Nat (=> Nat Nat) (=> Nat Nat))
  (fn [c :- Nat, ρ :- (=> Nat Nat)] (fn [i :- Nat] (Bool.rec$1 (fn [_ :- Bool] Nat) (ρ i) (Nat.succ (ρ i)) (Nat.ble c (ρ i))))))

;; Lifting at any cutoff c, as variable lookups (lift (i+1) 0) and stepTy
;; perform it: lifting a translated formula shifts the environment's values
;; at or above c (trLiftR), with extensionality in the environment.
(prove! 'trVarLift '[c :- Nat, j :- Nat, b :- Bool]
  '(=> (Eq Bool (Nat.ble c j) b) (Eq Exp (lift 1 c (Exp.var j)) (Exp.var (Bool.rec$1 (fn [_ :- Bool] Nat) j (Nat.succ j) b))))
  '[(cases b)
    (intro h) (exact (lift_var_below 1 c j (Nat.lt_of_not_le (fn [hle :- (LE.le c j)] (Bool.noConfusion (Eq.trans (Eq.symm (Nat.ble_eq_true_of_le hle)) h))))))
    (intro h) (exact (lift_var_above 1 c j (Nat.le_of_ble_eq_true h)))])

(prove! 'trT_ext '[ρ :- (=> Nat Nat), ρ2 :- (=> Nat Nat), h :- (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))), t :- PT]
  '(Eq Exp (trT t ρ) (trT t ρ2))
  '[(induction t)
    (exact (congrArg Exp.var (h i))) (rfl)
    (have q (Eq Exp (Exp.succ (trT t ρ)) (Exp.succ (trT t ρ2))) (congrArg Exp.succ ih_t)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trPLUS (trT t ρ)) (trT u ρ)) (Exp.app (Exp.app trPLUS (trT t ρ2)) (trT u ρ2)))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) ih_t) ih_u)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trTIMES (trT t ρ)) (trT u ρ)) (Exp.app (Exp.app trTIMES (trT t ρ2)) (trT u ρ2)))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) ih_t) ih_u)) (exact q)])

(prove! 'trSucR_ext '[ρ :- (=> Nat Nat), ρ2 :- (=> Nat Nat), h :- (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))), i :- Nat]
  '(Eq Nat (trSucR ρ i) (trSucR ρ2 i)) '[(exact (congrArg Nat.succ (h i)))])

(prove! 'trUpR_ext '[ρ :- (=> Nat Nat), ρ2 :- (=> Nat Nat), h :- (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))), i :- Nat]
  '(Eq Nat (trUpR ρ i) (trUpR ρ2 i)) '[(cases i) (rfl) (exact (congrArg Nat.succ (h n)))])

(prove! 'trF_ext '[φ :- PF]
  '(forall [ρ (=> Nat Nat)] (forall [ρ2 (=> Nat Nat)] (=> (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))) (Eq Exp (trF φ ρ) (trF φ ρ2)))))
  '[(induction φ)
    (intro ρ ρ2 h)
    (have q (Eq Exp (Exp.tT (Exp.app (Exp.app trEQ (trT t ρ)) (trT u ρ))) (Exp.tT (Exp.app (Exp.app trEQ (trT t ρ2)) (trT u ρ2))))
      (congrArg Exp.tT (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trEQ x))) (trT_ext ρ ρ2 h t)) (trT_ext ρ ρ2 h u)))) (exact q)
    (intro ρ ρ2 h) (rfl)
    (intro ρ ρ2 h)
    (have q (Eq Exp (Exp.tPi U.uw (trF p ρ) (trF q (trSucR ρ))) (Exp.tPi U.uw (trF p ρ2) (trF q (trSucR ρ2))))
      (congr (congrArg (Exp.tPi U.uw) (ih_p ρ ρ2 h)) (ih_q (trSucR ρ) (trSucR ρ2) (trSucR_ext ρ ρ2 h)))) (exact q)
    (intro ρ ρ2 h)
    (have q (Eq Exp (Exp.tPi U.uw Exp.tNat (trF p (trUpR ρ))) (Exp.tPi U.uw Exp.tNat (trF p (trUpR ρ2))))
      (congrArg (Exp.tPi U.uw Exp.tNat) (ih_p (trUpR ρ) (trUpR ρ2) (trUpR_ext ρ ρ2 h)))) (exact q)])

(prove! 'trBrSucc '[x :- Nat, b :- Bool]
  '(Eq Nat (Bool.rec$1 (fn [_ :- Bool] Nat) (Nat.succ x) (Nat.succ (Nat.succ x)) b) (Nat.succ (Bool.rec$1 (fn [_ :- Bool] Nat) x (Nat.succ x) b)))
  '[(cases b) (rfl) (rfl)])

(prove! 'trT_liftc '[c :- Nat, ρ :- (=> Nat Nat), t :- PT] '(Eq Exp (lift 1 c (trT t ρ)) (trT t (trLiftR c ρ)))
  '[(induction t)
    (exact (trVarLift c (ρ i) (Nat.ble c (ρ i)) (Eq.refl (Nat.ble c (ρ i)))))
    (rfl)
    (have q (Eq Exp (Exp.succ (lift 1 c (trT t ρ))) (Exp.succ (trT t (trLiftR c ρ)))) (congrArg Exp.succ ih_t)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trPLUS (lift 1 c (trT t ρ))) (lift 1 c (trT u ρ))) (Exp.app (Exp.app trPLUS (trT t (trLiftR c ρ))) (trT u (trLiftR c ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) ih_t) ih_u)) (exact q)
    (have q (Eq Exp (Exp.app (Exp.app trTIMES (lift 1 c (trT t ρ))) (lift 1 c (trT u ρ))) (Exp.app (Exp.app trTIMES (trT t (trLiftR c ρ))) (trT u (trLiftR c ρ))))
      (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) ih_t) ih_u)) (exact q)])

(prove! 'trLiftSuc '[c :- Nat, ρ :- (=> Nat Nat), i :- Nat] '(Eq Nat (trLiftR (Nat.succ c) (trSucR ρ) i) (trSucR (trLiftR c ρ) i))
  '[(exact (trBrSucc (ρ i) (Nat.ble c (ρ i))))])

(prove! 'trLiftUp '[c :- Nat, ρ :- (=> Nat Nat), i :- Nat] '(Eq Nat (trLiftR (Nat.succ c) (trUpR ρ) i) (trUpR (trLiftR c ρ) i))
  '[(cases i) (rfl) (exact (trBrSucc (ρ n) (Nat.ble c (ρ n))))])

(prove! 'trF_liftc '[φ :- PF] '(forall [c Nat] (forall [ρ (=> Nat Nat)] (Eq Exp (lift 1 c (trF φ ρ)) (trF φ (trLiftR c ρ)))))
  '[(induction φ)
    (intro c ρ)
    (have q (Eq Exp (Exp.tT (Exp.app (Exp.app trEQ (lift 1 c (trT t ρ))) (lift 1 c (trT u ρ)))) (Exp.tT (Exp.app (Exp.app trEQ (trT t (trLiftR c ρ))) (trT u (trLiftR c ρ)))))
      (congrArg Exp.tT (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trEQ x))) (trT_liftc c ρ t)) (trT_liftc c ρ u)))) (exact q)
    (intro c ρ) (rfl)
    (intro c ρ)
    (have q (Eq Exp (Exp.tPi U.uw (lift 1 c (trF p ρ)) (lift 1 (+ c 1) (trF q (trSucR ρ)))) (Exp.tPi U.uw (trF p (trLiftR c ρ)) (trF q (trSucR (trLiftR c ρ)))))
      (congr (congrArg (Exp.tPi U.uw) (ih_p c ρ))
             (Eq.trans (ih_q (Nat.succ c) (trSucR ρ)) (trF_ext q (trLiftR (Nat.succ c) (trSucR ρ)) (trSucR (trLiftR c ρ)) (trLiftSuc c ρ)))))
    (exact q)
    (intro c ρ)
    (have q (Eq Exp (Exp.tPi U.uw Exp.tNat (lift 1 (+ c 1) (trF p (trUpR ρ)))) (Exp.tPi U.uw Exp.tNat (trF p (trUpR (trLiftR c ρ)))))
      (congrArg (Exp.tPi U.uw Exp.tNat)
        (Eq.trans (ih_p (Nat.succ c) (trUpR ρ)) (trF_ext p (trLiftR (Nat.succ c) (trUpR ρ)) (trUpR (trLiftR c ρ)) (trLiftUp c ρ)))))
    (exact q)])

;; IND's step type: the successor substitution, as an environment fact.
(prove! 'trSuc_hyp '[ρ :- (=> Nat Nat)]
  '(forall [i Nat] (Eq Exp (sSucc (trUpR ρ i)) (trT (paSuc i) (trUpR ρ))))
  '[(intro i) (cases i) (rfl) (rfl)])
(prove! 'trF_suc '[φ :- PF, ρ :- (=> Nat Nat)]
  '(Eq Exp (subst (fn [i :- Nat] (sSucc i)) (trF φ (trUpR ρ))) (trF (paSbF paSuc φ) (trUpR ρ)))
  '[(exact (trF_subst φ (fn [i :- Nat] (sSucc i)) (trUpR ρ) paSuc (trUpR ρ) (trSuc_hyp ρ)))])

