(ns lcert.formal.pa
  "F6.1 — Peano arithmetic H_PA (R4-metatheory.md §6.2) as a deep embedding,
  its semantics in ℕ, soundness, and consistency (ADR-0006, F6 plan).

  P5's conclusion (§6.4, step 4) uses that PA is consistent, \"being true in ℕ\".
  Here that is a theorem of the metatheory, not an assumption: every H_PA
  theorem is true in ℕ under every assignment (paPrv_sound), so 0 = S0 is not
  a theorem (pa_consistent). It is a *relative* consistency result: it holds
  if the metatheory — Lean 4's type theory, as the Ansatz kernel implements
  it — is consistent. The argument is the standard semantic one (a truth
  predicate, and induction on derivations for statements that mention it),
  which needs more strength than PA has: roughly ACA, or PA with a
  compositional truth predicate and full induction. By G2 no such proof can
  be carried out in PA itself. Only Lean's Init is used — no classical axiom: the axiom
  DN is sound because every formula of this language is *stable*
  (paHolds_stab), its atoms being decidable equations and its connectives →,
  ⊥, ∀ (the reason R4 chose them).

  The language is R4's: terms over 0, S, +, ·; formulas in =, ⊥, →, ∀.
  Deviation (recorded in ADR-0006): variables are de Bruijn indices — ∀
  binds index 0 — so the schemes need no \"free for\" side conditions:
  - A4 ∀φ → φ[t/0] is paSbF (paInst t), which substitutes t and lowers the
    other indices;
  - A5 ∀(φ↑ → ψ) → (φ → ∀ψ), where φ↑ = paSbF paShT φ mentions no index 0
    (the paper's \"x not free in φ\");
  - E1 is i = i for each variable i; E2 is x = y → (α[x/z] → α[y/z]) for an
    atom α, where α[x/z] renames the variable z to x (paRn);
  - Q1–Q6 use the variables 0 and 1; Gen closes them;
  - IND is φ[0/0] → ∀(φ → φ[S0/0]) → ∀φ (paSuc replaces index 0 by its
    successor and keeps the others).
  Gen: from φ infer ∀φ (it generalizes index 0). MP as usual."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]))

;; ---------------------------------------------------------------------------
;; Syntax.

(a/inductive PT [] (pv [i Nat]) (pz) (ps [t PT]) (padd [t PT] [u PT]) (pmul [t PT] [u PT]))
(a/inductive PF [] (peq [t PT] [u PT]) (pbot) (pimp [p PF] [q PF]) (pall [p PF]))

;; Parallel substitution σ : Nat → PT on terms.
(kdef paSbT (=> (=> Nat PT) PT PT)
  (fn [σ :- (=> Nat PT), t :- PT]
    (PT.rec$1 (fn [_ :- PT] PT)
      (fn [i :- Nat] (σ i)) PT.pz
      (fn [t :- PT, it :- PT] (PT.ps it))
      (fn [t :- PT, u :- PT, it :- PT, iu :- PT] (PT.padd it iu))
      (fn [t :- PT, u :- PT, it :- PT, iu :- PT] (PT.pmul it iu)) t)))

;; The shift (every index up one), and σ lifted under a binder.
(kdef paShT (=> Nat PT) (fn [j :- Nat] (PT.pv (Nat.succ j))))
(kdef paUp (=> (=> Nat PT) (=> Nat PT))
  (fn [σ :- (=> Nat PT)]
    (fn [i :- Nat] (Nat.rec$1 (fn [_ :- Nat] PT) (PT.pv 0) (fn [k :- Nat, _ :- PT] (paSbT paShT (σ k))) i))))

;; Parallel substitution on formulas (recursion on the formula, returning a
;; function of σ, which changes under ∀).
(kdef paSbFF (=> PF (=> (=> Nat PT) PF))
  (fn [φ :- PF]
    (PF.rec$1 (fn [_ :- PF] (=> (=> Nat PT) PF))
      (fn [t :- PT, u :- PT] (fn [σ :- (=> Nat PT)] (PF.peq (paSbT σ t) (paSbT σ u))))
      (fn [σ :- (=> Nat PT)] PF.pbot)
      (fn [p :- PF, q :- PF, ip :- (=> (=> Nat PT) PF), iq :- (=> (=> Nat PT) PF)] (fn [σ :- (=> Nat PT)] (PF.pimp (ip σ) (iq σ))))
      (fn [p :- PF, ip :- (=> (=> Nat PT) PF)] (fn [σ :- (=> Nat PT)] (PF.pall (ip (paUp σ)))))
      φ)))
(kdef paSbF (=> (=> Nat PT) PF PF) (fn [σ :- (=> Nat PT), φ :- PF] (paSbFF φ σ)))

;; The substitutions the schemes use: A4's instance (t for 0, the rest
;; lowered), IND's successor (S0 for 0, the rest kept), E2's renaming of z to x.
(kdef paInst (=> PT (=> Nat PT))
  (fn [t :- PT] (fn [i :- Nat] (Nat.rec$1 (fn [_ :- Nat] PT) t (fn [k :- Nat, _ :- PT] (PT.pv k)) i))))
(kdef paSuc (=> Nat PT)
  (fn [i :- Nat] (Nat.rec$1 (fn [_ :- Nat] PT) (PT.ps (PT.pv 0)) (fn [k :- Nat, _ :- PT] (PT.pv (Nat.succ k))) i)))
(kdef paRn (=> Nat Nat (=> Nat PT))
  (fn [z :- Nat, x :- Nat] (fn [i :- Nat] (Bool.rec$1 (fn [_ :- Bool] PT) (PT.pv i) (PT.pv x) (Nat.beq i z)))))

;; ---------------------------------------------------------------------------
;; Semantics in ℕ: assignments ρ : Nat → Nat.

(kdef paCons (=> Nat (=> Nat Nat) (=> Nat Nat))
  (fn [a :- Nat, ρ :- (=> Nat Nat)] (fn [i :- Nat] (Nat.rec$1 (fn [_ :- Nat] Nat) a (fn [k :- Nat, _ :- Nat] (ρ k)) i))))
(kdef paEval (=> (=> Nat Nat) PT Nat)
  (fn [ρ :- (=> Nat Nat), t :- PT]
    (PT.rec$1 (fn [_ :- PT] Nat)
      (fn [i :- Nat] (ρ i)) 0
      (fn [t :- PT, it :- Nat] (Nat.succ it))
      (fn [t :- PT, u :- PT, it :- Nat, iu :- Nat] (Nat.add it iu))
      (fn [t :- PT, u :- PT, it :- Nat, iu :- Nat] (Nat.mul it iu)) t)))
(kdef paHoldsF (=> PF (=> (=> Nat Nat) Prop))
  (fn [φ :- PF]
    (PF.rec$1 (fn [_ :- PF] (=> (=> Nat Nat) Prop))
      (fn [t :- PT, u :- PT] (fn [ρ :- (=> Nat Nat)] (Eq Nat (paEval ρ t) (paEval ρ u))))
      (fn [ρ :- (=> Nat Nat)] False)
      (fn [p :- PF, q :- PF, ip :- (=> (=> Nat Nat) Prop), iq :- (=> (=> Nat Nat) Prop)] (fn [ρ :- (=> Nat Nat)] (=> (ip ρ) (iq ρ))))
      (fn [p :- PF, ip :- (=> (=> Nat Nat) Prop)] (fn [ρ :- (=> Nat Nat)] (forall [a Nat] (ip (paCons a ρ)))))
      φ)))
(kdef paHolds (=> (=> Nat Nat) PF Prop) (fn [ρ :- (=> Nat Nat), φ :- PF] (paHoldsF φ ρ)))

;; Evaluation commutes with substitution, and depends on ρ pointwise.
;; (Each congruence is stated unfolded with have: exact against the folded
;; goal mis-unifies congr's f a.)
(def ^:private EVσ '(fn [i :- Nat] (paEval ρ (σ i))))
(a/prove-theorem 'paEval_sbT (lv '[σ :- (=> Nat PT), ρ :- (=> Nat Nat), t :- PT])
  (lv (list 'Eq 'Nat '(paEval ρ (paSbT σ t)) (list 'paEval EVσ 't)))
  (lv [(list 'induction 't) '(rfl) '(rfl)
       (list 'have 'q (list 'Eq 'Nat '(Nat.succ (paEval ρ (paSbT σ t))) (list 'Nat.succ (list 'paEval EVσ 't))) '(congrArg Nat.succ ih_t)) '(exact q)
       (list 'have 'q (list 'Eq 'Nat '(Nat.add (paEval ρ (paSbT σ t)) (paEval ρ (paSbT σ u))) (list 'Nat.add (list 'paEval EVσ 't) (list 'paEval EVσ 'u))) '(congr (congrArg Nat.add ih_t) ih_u)) '(exact q)
       (list 'have 'q (list 'Eq 'Nat '(Nat.mul (paEval ρ (paSbT σ t)) (paEval ρ (paSbT σ u))) (list 'Nat.mul (list 'paEval EVσ 't) (list 'paEval EVσ 'u))) '(congr (congrArg Nat.mul ih_t) ih_u)) '(exact q)]))

(thm paEval_ext [ρ :- (=> Nat Nat), ρ2 :- (=> Nat Nat), h :- (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))), t :- PT]
  (Eq Nat (paEval ρ t) (paEval ρ2 t))
  (induction t) (exact (h i)) (rfl)
  (have q (Eq Nat (Nat.succ (paEval ρ t)) (Nat.succ (paEval ρ2 t))) (congrArg Nat.succ ih_t)) (exact q)
  (have q (Eq Nat (Nat.add (paEval ρ t) (paEval ρ u)) (Nat.add (paEval ρ2 t) (paEval ρ2 u))) (congr (congrArg Nat.add ih_t) ih_u)) (exact q)
  (have q (Eq Nat (Nat.mul (paEval ρ t) (paEval ρ u)) (Nat.mul (paEval ρ2 t) (paEval ρ2 u))) (congr (congrArg Nat.mul ih_t) ih_u)) (exact q))

;; Pointwise facts about the substitutions (by cases on the index).
(thm paUp_eval [a :- Nat, σ :- (=> Nat PT), ρ :- (=> Nat Nat), i :- Nat]
  (Eq Nat (paEval (paCons a ρ) (paUp σ i)) (paCons a (fn [j :- Nat] (paEval ρ (σ j))) i))
  (cases i) (rfl) (exact (paEval_sbT paShT (paCons a ρ) (σ n))))
(thm paCons_ext [a :- Nat, ρ :- (=> Nat Nat), ρ2 :- (=> Nat Nat), h :- (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))), i :- Nat]
  (Eq Nat (paCons a ρ i) (paCons a ρ2 i))
  (cases i) (rfl) (exact (h n)))
(thm paInst_eval [t :- PT, i :- Nat, ρ :- (=> Nat Nat)]
  (Eq Nat (paEval ρ (paInst t i)) (paCons (paEval ρ t) ρ i))
  (cases i) (rfl) (rfl))
(thm paSuc_eval [a :- Nat, i :- Nat, ρ :- (=> Nat Nat)]
  (Eq Nat (paEval (paCons a ρ) (paSuc i)) (paCons (Nat.succ a) ρ i))
  (cases i) (rfl) (rfl))

;; Truth depends on the assignment pointwise.
(thm paHolds_ext [φ :- PF]
  (forall [ρ (=> Nat Nat)] (forall [ρ2 (=> Nat Nat)] (=> (forall [i Nat] (Eq Nat (ρ i) (ρ2 i))) (Iff (paHolds ρ φ) (paHolds ρ2 φ)))))
  (induction φ)
  (intro ρ ρ2 h)
  (have q (Iff (Eq Nat (paEval ρ t) (paEval ρ u)) (Eq Nat (paEval ρ2 t) (paEval ρ2 u)))
    (Iff.intro (fn [e :- (Eq Nat (paEval ρ t) (paEval ρ u))] (Eq.trans (Eq.symm (paEval_ext ρ ρ2 h t)) (Eq.trans e (paEval_ext ρ ρ2 h u))))
               (fn [e :- (Eq Nat (paEval ρ2 t) (paEval ρ2 u))] (Eq.trans (paEval_ext ρ ρ2 h t) (Eq.trans e (Eq.symm (paEval_ext ρ ρ2 h u)))))))
  (exact q)
  (intro ρ ρ2 h) (exact (Iff.intro (fn [x :- False] x) (fn [x :- False] x)))
  (intro ρ ρ2 h)
  (have q (Iff (=> (paHolds ρ p) (paHolds ρ q)) (=> (paHolds ρ2 p) (paHolds ρ2 q)))
    (Iff.intro (fn [f :- (=> (paHolds ρ p) (paHolds ρ q)), x :- (paHolds ρ2 p)] (Iff.mp (ih_q ρ ρ2 h) (f (Iff.mpr (ih_p ρ ρ2 h) x))))
               (fn [f :- (=> (paHolds ρ2 p) (paHolds ρ2 q)), x :- (paHolds ρ p)] (Iff.mpr (ih_q ρ ρ2 h) (f (Iff.mp (ih_p ρ ρ2 h) x))))))
  (exact q)
  (intro ρ ρ2 h)
  (have q (Iff (forall [a Nat] (paHolds (paCons a ρ) p)) (forall [a Nat] (paHolds (paCons a ρ2) p)))
    (Iff.intro (fn [f :- (forall [a Nat] (paHolds (paCons a ρ) p)), a :- Nat] (Iff.mp (ih_p (paCons a ρ) (paCons a ρ2) (paCons_ext a ρ ρ2 h)) (f a)))
               (fn [f :- (forall [a Nat] (paHolds (paCons a ρ2) p)), a :- Nat] (Iff.mpr (ih_p (paCons a ρ) (paCons a ρ2) (paCons_ext a ρ ρ2 h)) (f a)))))
  (exact q))

;; The substitution lemma: φ[σ] holds at ρ iff φ holds at ρ∘σ (evaluated).
(a/prove-theorem 'paHolds_sbF (lv '[φ :- PF])
  (lv (list 'forall '[σ (=> Nat PT)] (list 'forall '[ρ (=> Nat Nat)] (list 'Iff '(paHolds ρ (paSbF σ φ)) (list 'paHolds EVσ 'φ)))))
  (lv [(list 'induction 'φ)
       '(intro σ ρ)
       (list 'have 'q (list 'Iff '(Eq Nat (paEval ρ (paSbT σ t)) (paEval ρ (paSbT σ u))) (list 'Eq 'Nat (list 'paEval EVσ 't) (list 'paEval EVσ 'u)))
         (list 'Iff.intro (list 'fn ['e :- '(Eq Nat (paEval ρ (paSbT σ t)) (paEval ρ (paSbT σ u)))] '(Eq.trans (Eq.symm (paEval_sbT σ ρ t)) (Eq.trans e (paEval_sbT σ ρ u))))
                          (list 'fn ['e :- (list 'Eq 'Nat (list 'paEval EVσ 't) (list 'paEval EVσ 'u))] '(Eq.trans (paEval_sbT σ ρ t) (Eq.trans e (Eq.symm (paEval_sbT σ ρ u)))))))
       '(exact q)
       '(intro σ ρ) '(exact (Iff.intro (fn [x :- False] x) (fn [x :- False] x)))
       '(intro σ ρ)
       (list 'have 'q (list 'Iff '(=> (paHolds ρ (paSbF σ p)) (paHolds ρ (paSbF σ q))) (list '=> (list 'paHolds EVσ 'p) (list 'paHolds EVσ 'q)))
         (list 'Iff.intro (list 'fn ['f :- '(=> (paHolds ρ (paSbF σ p)) (paHolds ρ (paSbF σ q))) 'x :- (list 'paHolds EVσ 'p)] '(Iff.mp (ih_q σ ρ) (f (Iff.mpr (ih_p σ ρ) x))))
                          (list 'fn ['f :- (list '=> (list 'paHolds EVσ 'p) (list 'paHolds EVσ 'q)) 'x :- '(paHolds ρ (paSbF σ p))] '(Iff.mpr (ih_q σ ρ) (f (Iff.mp (ih_p σ ρ) x))))))
       '(exact q)
       '(intro σ ρ)
       (list 'have 'q (list 'Iff '(forall [a Nat] (paHolds (paCons a ρ) (paSbF (paUp σ) p))) (list 'forall '[a Nat] (list 'paHolds (list 'paCons 'a EVσ) 'p)))
         (list 'Iff.intro
           (list 'fn ['f :- '(forall [a Nat] (paHolds (paCons a ρ) (paSbF (paUp σ) p))) 'a :- 'Nat]
             (list 'Iff.mp (list 'paHolds_ext 'p '(fn [i :- Nat] (paEval (paCons a ρ) (paUp σ i))) (list 'paCons 'a EVσ) '(paUp_eval a σ ρ))
                   '(Iff.mp (ih_p (paUp σ) (paCons a ρ)) (f a))))
           (list 'fn ['f :- (list 'forall '[a Nat] (list 'paHolds (list 'paCons 'a EVσ) 'p)) 'a :- 'Nat]
             (list 'Iff.mpr (list 'ih_p '(paUp σ) '(paCons a ρ))
                   (list 'Iff.mpr (list 'paHolds_ext 'p '(fn [i :- Nat] (paEval (paCons a ρ) (paUp σ i))) (list 'paCons 'a EVσ) '(paUp_eval a σ ρ)) '(f a))))))
       '(exact q)]))

;; Stability: ¬¬φ → φ for every formula (atoms are decidable; → and ∀
;; preserve it). This is DN's soundness, with no classical axiom.
(thm paHolds_stab [φ :- PF] (forall [ρ (=> Nat Nat)] (=> (Not (Not (paHolds ρ φ))) (paHolds ρ φ)))
  (induction φ)
  (intro ρ nn)
  (exact (AT_Decidable.byContradiction (Eq Nat (paEval ρ t) (paEval ρ u)) (Nat.decEq (paEval ρ t) (paEval ρ u)) nn))
  (intro ρ nn) (exact (nn (fn [x :- False] x)))
  (intro ρ nn)
  (have r (=> (paHolds ρ p) (paHolds ρ q))
    (fn [x :- (paHolds ρ p)] (ih_q ρ (fn [nb :- (Not (paHolds ρ q))] (nn (fn [f :- (=> (paHolds ρ p) (paHolds ρ q))] (nb (f x))))))))
  (exact r)
  (intro ρ nn)
  (have r (forall [a Nat] (paHolds (paCons a ρ) p))
    (fn [a :- Nat] (ih_p (paCons a ρ) (fn [np :- (Not (paHolds (paCons a ρ) p))] (nn (fn [f :- (forall [b Nat] (paHolds (paCons b ρ) p))] (np (f a))))))))
  (exact r))

;; ---------------------------------------------------------------------------
;; H_PA: the axiom schemes and derivability.

(a/inductive PAx [] :in Prop :indices [f PF]
  (a1 [a PF] [b PF] :where [(PF.pimp a (PF.pimp b a))])
  (a2 [a PF] [b PF] [c PF] :where [(PF.pimp (PF.pimp a (PF.pimp b c)) (PF.pimp (PF.pimp a b) (PF.pimp a c)))])
  (dn [a PF] :where [(PF.pimp (PF.pimp (PF.pimp a PF.pbot) PF.pbot) a)])
  (a4 [p PF] [t PT] :where [(PF.pimp (PF.pall p) (paSbF (paInst t) p))])
  (a5 [p PF] [q PF] :where [(PF.pimp (PF.pall (PF.pimp (paSbF paShT p) q)) (PF.pimp p (PF.pall q)))])
  (e1 [i Nat] :where [(PF.peq (PT.pv i) (PT.pv i))])
  (e2 [i Nat] [j Nat] [z Nat] [s PT] [t PT]
      :where [(PF.pimp (PF.peq (PT.pv i) (PT.pv j))
                       (PF.pimp (PF.peq (paSbT (paRn z i) s) (paSbT (paRn z i) t)) (PF.peq (paSbT (paRn z j) s) (paSbT (paRn z j) t))))])
  (q1 :where [(PF.pimp (PF.peq (PT.ps (PT.pv 0)) PT.pz) PF.pbot)])
  (q2 :where [(PF.pimp (PF.peq (PT.ps (PT.pv 0)) (PT.ps (PT.pv 1))) (PF.peq (PT.pv 0) (PT.pv 1)))])
  (q3 :where [(PF.peq (PT.padd (PT.pv 0) PT.pz) (PT.pv 0))])
  (q4 :where [(PF.peq (PT.padd (PT.pv 0) (PT.ps (PT.pv 1))) (PT.ps (PT.padd (PT.pv 0) (PT.pv 1))))])
  (q5 :where [(PF.peq (PT.pmul (PT.pv 0) PT.pz) PT.pz)])
  (q6 :where [(PF.peq (PT.pmul (PT.pv 0) (PT.ps (PT.pv 1))) (PT.padd (PT.pmul (PT.pv 0) (PT.pv 1)) (PT.pv 0)))])
  (ind [p PF] :where [(PF.pimp (paSbF (paInst PT.pz) p) (PF.pimp (PF.pall (PF.pimp p (paSbF paSuc p))) (PF.pall p)))]))

(a/inductive PPrv [] :in Prop :indices [f PF]
  (ax [g PF] [hx (PAx g)] :where [g])
  (mp [g PF] [k PF] [hp (PPrv g)] [hi (PPrv (PF.pimp g k))] :where [k])
  (gen [g PF] [hp (PPrv g)] :where [(PF.pall g)]))

;; ---------------------------------------------------------------------------
;; Soundness: one lemma per scheme, then induction on derivations.

(thm paS_a1 [a :- PF, b :- PF, ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp a (PF.pimp b a))) (intro x y) (exact x))
(thm paS_a2 [a :- PF, b :- PF, c :- PF, ρ :- (=> Nat Nat)]
  (paHolds ρ (PF.pimp (PF.pimp a (PF.pimp b c)) (PF.pimp (PF.pimp a b) (PF.pimp a c))))
  (intro f g x) (exact (f x (g x))))
(thm paS_dn [a :- PF, ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp (PF.pimp (PF.pimp a PF.pbot) PF.pbot) a))
  (intro nn) (exact (paHolds_stab a ρ nn)))
(thm paS_a4 [p :- PF, t :- PT, ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp (PF.pall p) (paSbF (paInst t) p)))
  (intro f)
  (exact (Iff.mpr (paHolds_sbF p (paInst t) ρ)
           (Iff.mpr (paHolds_ext p (fn [i :- Nat] (paEval ρ (paInst t i))) (paCons (paEval ρ t) ρ) (fn [i :- Nat] (paInst_eval t i ρ)))
                    (f (paEval ρ t))))))
(thm paS_a5 [p :- PF, q :- PF, ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp (PF.pall (PF.pimp (paSbF paShT p) q)) (PF.pimp p (PF.pall q))))
  (intro f hp a) (exact (f a (Iff.mpr (paHolds_sbF p paShT (paCons a ρ)) hp))))
(thm paS_e1 [i :- Nat, ρ :- (=> Nat Nat)] (paHolds ρ (PF.peq (PT.pv i) (PT.pv i))) (rfl))
(thm paRn_bool [ρ :- (=> Nat Nat), x :- Nat, i :- Nat, j :- Nat, e :- (Eq Nat (ρ i) (ρ j)), bb :- Bool]
  (Eq Nat (paEval ρ (Bool.rec$1 (fn [_ :- Bool] PT) (PT.pv x) (PT.pv i) bb)) (paEval ρ (Bool.rec$1 (fn [_ :- Bool] PT) (PT.pv x) (PT.pv j) bb)))
  (cases bb) (rfl) (exact e))
(thm paSb_agree [ρ :- (=> Nat Nat), σ :- (=> Nat PT), τ :- (=> Nat PT), h :- (forall [x Nat] (Eq Nat (paEval ρ (σ x)) (paEval ρ (τ x)))), s :- PT]
  (Eq Nat (paEval ρ (paSbT σ s)) (paEval ρ (paSbT τ s)))
  (exact (Eq.trans (paEval_sbT σ ρ s)
           (Eq.trans (paEval_ext (fn [x :- Nat] (paEval ρ (σ x))) (fn [x :- Nat] (paEval ρ (τ x))) h s) (Eq.symm (paEval_sbT τ ρ s))))))
(thm paS_e2 [i :- Nat, j :- Nat, z :- Nat, s :- PT, t :- PT, ρ :- (=> Nat Nat)]
  (paHolds ρ (PF.pimp (PF.peq (PT.pv i) (PT.pv j))
                      (PF.pimp (PF.peq (paSbT (paRn z i) s) (paSbT (paRn z i) t)) (PF.peq (paSbT (paRn z j) s) (paSbT (paRn z j) t)))))
  (intro e h)
  (have ag (forall [x Nat] (Eq Nat (paEval ρ (paRn z i x)) (paEval ρ (paRn z j x))))
    (fn [x :- Nat] (paRn_bool ρ x i j e (Nat.beq x z))))
  (exact (Eq.trans (Eq.symm (paSb_agree ρ (paRn z i) (paRn z j) ag s)) (Eq.trans h (paSb_agree ρ (paRn z i) (paRn z j) ag t)))))
(thm paS_q1 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp (PF.peq (PT.ps (PT.pv 0)) PT.pz) PF.pbot))
  (intro h) (exact (Nat.succ_ne_zero (ρ 0) h)))
(thm paS_q2 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.pimp (PF.peq (PT.ps (PT.pv 0)) (PT.ps (PT.pv 1))) (PF.peq (PT.pv 0) (PT.pv 1))))
  (intro h) (exact (Nat.succ.inj h)))
;; Q3–Q6 hold by computation: Nat.add and Nat.mul recurse on their second argument.
(thm paS_q3 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.peq (PT.padd (PT.pv 0) PT.pz) (PT.pv 0))) (rfl))
(thm paS_q4 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.peq (PT.padd (PT.pv 0) (PT.ps (PT.pv 1))) (PT.ps (PT.padd (PT.pv 0) (PT.pv 1))))) (rfl))
(thm paS_q5 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.peq (PT.pmul (PT.pv 0) PT.pz) PT.pz)) (rfl))
(thm paS_q6 [ρ :- (=> Nat Nat)] (paHolds ρ (PF.peq (PT.pmul (PT.pv 0) (PT.ps (PT.pv 1))) (PT.padd (PT.pmul (PT.pv 0) (PT.pv 1)) (PT.pv 0)))) (rfl))
;; IND: induction in ℕ.
(thm paS_ind [p :- PF, ρ :- (=> Nat Nat)]
  (paHolds ρ (PF.pimp (paSbF (paInst PT.pz) p) (PF.pimp (PF.pall (PF.pimp p (paSbF paSuc p))) (PF.pall p))))
  (intro h0 hs a)
  (induction a)
  (exact (Iff.mp (paHolds_ext p (fn [i :- Nat] (paEval ρ (paInst PT.pz i))) (paCons 0 ρ) (fn [i :- Nat] (paInst_eval PT.pz i ρ)))
                 (Iff.mp (paHolds_sbF p (paInst PT.pz) ρ) h0)))
  (exact (Iff.mp (paHolds_ext p (fn [i :- Nat] (paEval (paCons n ρ) (paSuc i))) (paCons (Nat.succ n) ρ) (fn [i :- Nat] (paSuc_eval n i ρ)))
                 (Iff.mp (paHolds_sbF p paSuc (paCons n ρ)) (hs n ih_n)))))

(thm paAx_sound [f0 :- PF, hx :- (PAx f0)] (forall [ρ (=> Nat Nat)] (paHolds ρ f0))
  (cases hx)
  (intro ρ) (exact (paS_a1 a b ρ))
  (intro ρ) (exact (paS_a2 a b c ρ))
  (intro ρ) (exact (paS_dn a ρ))
  (intro ρ) (exact (paS_a4 p t ρ))
  (intro ρ) (exact (paS_a5 p q ρ))
  (intro ρ) (exact (paS_e1 i ρ))
  (intro ρ) (exact (paS_e2 i j z s t ρ))
  (intro ρ) (exact (paS_q1 ρ))
  (intro ρ) (exact (paS_q2 ρ))
  (intro ρ) (exact (paS_q3 ρ))
  (intro ρ) (exact (paS_q4 ρ))
  (intro ρ) (exact (paS_q5 ρ))
  (intro ρ) (exact (paS_q6 ρ))
  (intro ρ) (exact (paS_ind p ρ)))

;; Soundness: every H_PA theorem is true in ℕ under every assignment.
(thm paPrv_sound [f0 :- PF, hd :- (PPrv f0)] (forall [ρ (=> Nat Nat)] (paHolds ρ f0))
  (induction hd)
  (intro ρ) (exact (paAx_sound g hx ρ))
  (intro ρ) (exact (ih_hi ρ (ih_hp ρ)))
  (intro ρ a) (exact (ih_hp (paCons a ρ))))

;; PA is consistent, relative to the metatheory: 0 = S0 is not a theorem
;; (R4 §6.4, step 4).
(thm pa_consistent [] (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))
  (intro hd) (exact (Nat.zero_ne_one (paPrv_sound (PF.peq PT.pz (PT.ps PT.pz)) hd (fn [i :- Nat] 0)))))
