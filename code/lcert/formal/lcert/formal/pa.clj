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
;; ‹lemma paEval_sbT: statement and proof elided, lines 103–108›

;; ‹lemma paEval_ext: statement and proof elided, lines 110–115›

;; Pointwise facts about the substitutions (by cases on the index).
;; ‹lemma paUp_eval: statement and proof elided, lines 118–120›
;; ‹lemma paCons_ext: statement and proof elided, lines 121–123›
;; ‹lemma paInst_eval: statement and proof elided, lines 124–126›
;; ‹lemma paSuc_eval: statement and proof elided, lines 127–129›

;; Truth depends on the assignment pointwise.
;; ‹lemma paHolds_ext: statement and proof elided, lines 132–150›

;; The substitution lemma: φ[σ] holds at ρ iff φ holds at ρ∘σ (evaluated).
;; ‹lemma paHolds_sbF: statement and proof elided, lines 153–176›

;; Stability: ¬¬φ → φ for every formula (atoms are decidable; → and ∀
;; preserve it). This is DN's soundness, with no classical axiom.
(thm paHolds_stab [φ :- PF] (forall [ρ (=> Nat Nat)] (=> (Not (Not (paHolds ρ φ))) (paHolds ρ φ)))
  ;; ‹proof elided: lines 181–192 of the full file; kernel-checked›
  )

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

;; ‹lemma paS_a1: statement and proof elided, lines 223–223›
;; ‹lemma paS_a2: statement and proof elided, lines 224–226›
;; ‹lemma paS_dn: statement and proof elided, lines 227–228›
;; ‹lemma paS_a4: statement and proof elided, lines 229–233›
;; ‹lemma paS_a5: statement and proof elided, lines 234–235›
;; ‹lemma paS_e1: statement and proof elided, lines 236–236›
;; ‹lemma paRn_bool: statement and proof elided, lines 237–239›
;; ‹lemma paSb_agree: statement and proof elided, lines 240–243›
;; ‹lemma paS_e2: statement and proof elided, lines 244–250›
;; ‹lemma paS_q1: statement and proof elided, lines 251–252›
;; ‹lemma paS_q2: statement and proof elided, lines 253–254›
;; Q3–Q6 hold by computation: Nat.add and Nat.mul recurse on their second argument.
;; ‹lemma paS_q3: statement and proof elided, lines 256–256›
;; ‹lemma paS_q4: statement and proof elided, lines 257–257›
;; ‹lemma paS_q5: statement and proof elided, lines 258–258›
;; ‹lemma paS_q6: statement and proof elided, lines 259–259›
;; IND: induction in ℕ.
;; ‹lemma paS_ind: statement and proof elided, lines 261–268›

;; ‹lemma paAx_sound: statement and proof elided, lines 270–285›

;; Soundness: every H_PA theorem is true in ℕ under every assignment.
(thm paPrv_sound [f0 :- PF, hd :- (PPrv f0)] (forall [ρ (=> Nat Nat)] (paHolds ρ f0))
  ;; ‹proof elided: lines 289–292 of the full file; kernel-checked›
  )

;; PA is consistent, relative to the metatheory: 0 = S0 is not a theorem
;; (R4 §6.4, step 4).
(thm pa_consistent [] (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))
  ;; ‹proof elided: lines 297–297 of the full file; kernel-checked›
  )
