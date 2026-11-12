/-
  Guarded trust: the self-justification counterpart of Gross, Gallagher and
  Fallenstein, "Löb's Theorem: A functional pearl of dependently typed
  quining" (ICFP 2016 draft; https://jasongross.github.io/lob-paper/nightly/lob.pdf),
  sections 6 to 8.

  The paper adds a constructor for Löb's theorem to a syntax and proves the
  result sound relative to Agda with a *local* interpretation,
      ⟦ Löb f ⟧ = ⟦ f ⟧ (Löb f),
  which hands `f` its own syntax without looking inside it. It then shows that
  such a syntax cannot inhabit ‘¬’ (‘□’ ‘⊥’) (`incompleteness = löb`).

  This file adds the opposite constructor. `con` inhabits ‘¬’ (‘□’ ‘⊥’): it is
  the syntax's own consistency, an interpreter at ‘⊥’. Its explanation cannot
  be local, because it has to run the syntax it is handed. It is sound relative
  to Lean through a *guarded* interpretation: a type is read at a level n,
  ‘□’ A at level n means syntax of A smaller than n, and `con` at level n runs
  its argument at that argument's own, smaller, level. The clause for `con` is
  the induction hypothesis of a strong induction on n (`below` in `interpAt`).

  The guarded syntax keeps the naming fixed point (`godel`, the paper's ‘H’ at
  ‘X’ = ‘⊥’), lambda, application, and quotation of closed terms (D1). The
  unguarded constructors live in a second syntax, `TmU`: the paper's `Löb`,
  and internal quotation and application of a variable (the paper's
  ‘⌜‘VAR₀’⌝ᵗ’ and _w‘‘’’ₐ_, i.e. D3 and D2 at a variable). With them the same
  `con` derives ‘⊥’ (`viaLob`, `viaQuine`).

  Check with:  lean 2026-10-29-guarded-trust.lean   (Lean 4 core, no Mathlib;
  checked 2026-10-29 with v4.32.1). `soundness` uses propext and Quot.sound;
  `no_sound_reading` uses no axioms.

  Negative checks, run 2026-10-29 on edited copies and not kept here:
  1. Reading ‘□’ A at level n as all syntax of A (bound `True`): the `con`
     clause fails ("hs has type True but is expected to have type size s < n").
  2. Adding internal quotation of a variable to `Tm`, with clause
     ⟨s, _⟩ ↦ ⟨quo s, _⟩: omega cannot prove `size s + 1 < n`.
  3. Adding the paper's `Löb` to `Tm`, with the paper's clause: omega cannot
     prove `size f + 1 < n` from `size f + 1 ≤ n`.
-/

/-- Syntactic types. -/
inductive Ty : Type where
  | arr   : Ty → Ty → Ty
  /-- ‘□’ A: closed syntax of type A. -/
  | box   : Ty → Ty
  | top   : Ty
  | bot   : Ty
  /-- G ≅ (‘□’ G ‘→’ ‘⊥’), "I am not provable": a naming fixed point. -/
  | godel : Ty

/-- De Bruijn variables. -/
inductive Var : List Ty → Ty → Type where
  | zero {Γ : List Ty} {A : Ty} : Var (A :: Γ) A
  | succ {Γ : List Ty} {A B : Ty} : Var Γ A → Var (B :: Γ) A

/-- The guarded syntax. -/
inductive Tm : List Ty → Ty → Type where
  | var {Γ A} : Var Γ A → Tm Γ A
  | lam {Γ A B} : Tm (A :: Γ) B → Tm Γ (.arr A B)
  | app {Γ A B} : Tm Γ (.arr A B) → Tm Γ A → Tm Γ B
  | tt {Γ} : Tm Γ .top
  /-- ⌜_⌝ᵗ: quotation of a closed term (necessitation, D1). -/
  | quo {Γ A} : Tm [] A → Tm Γ (.box A)
  /-- quine←, at the Gödel sentence. -/
  | fold {Γ} : Tm Γ (.arr (.arr (.box .godel) .bot) .godel)
  /-- quine→, at the Gödel sentence. -/
  | unfold {Γ} : Tm Γ (.arr .godel (.arr (.box .godel) .bot))
  /-- Trust: ‘¬’ (‘□’ ‘⊥’). -/
  | con {Γ} : Tm Γ (.arr (.box .bot) .bot)

/-- Syntax size: the well-founded order that trust descends along. -/
def size {Γ A} : Tm Γ A → Nat
  | .var _ => 1
  | .lam b => size b + 1
  | .app f x => size f + size x + 1
  | .tt => 1
  | .quo t => size t + 1
  | .fold => 1
  | .unfold => 1
  | .con => 1

/-- Types read at level `n`. ‘□’ A is the syntax of A smaller than `n`;
    `godel` reads as its own unfolding. -/
def TyI (n : Nat) : Ty → Type
  | .arr A B => TyI n A → TyI n B
  | .box A => { t : Tm [] A // size t < n }
  | .top => Unit
  | .bot => Empty
  | .godel => { t : Tm [] .godel // size t < n } → Empty

/-- Environments read at level `n`. -/
def EnvI (n : Nat) : List Ty → Type
  | [] => Unit
  | A :: Γ => TyI n A × EnvI n Γ

def lookup {n Γ A} : Var Γ A → EnvI n Γ → TyI n A
  | .zero, (a, _) => a
  | .succ v, (_, ρ) => lookup v ρ

/-- The interpreter at one level, given soundness at every lower level.
    Every clause except `con` is local in the paper's sense. The `con` clause
    is the guard: it runs its argument only because that argument is
    smaller than the level, and it runs it by calling `below`. -/
def interpAt (n : Nat)
    (below : ∀ m, m < n → ∀ s : Tm [] .bot, size s ≤ m → Empty) :
    {Γ : List Ty} → {A : Ty} → (t : Tm Γ A) → size t ≤ n → EnvI n Γ → TyI n A
  | _, _, .var v, _, ρ => lookup v ρ
  | _, _, .lam b, h, ρ => fun a =>
      interpAt n below b (by simp only [size] at h; omega) (a, ρ)
  | _, _, .app f x, h, ρ =>
      interpAt n below f (by simp only [size] at h; omega) ρ
        (interpAt n below x (by simp only [size] at h; omega) ρ)
  | _, _, .tt, _, _ => ()
  | _, _, .quo s, h, _ => ⟨s, by simp only [size] at h; omega⟩
  | _, _, .fold, _, _ => fun g => g
  | _, _, .unfold, _, _ => fun g => g
  | _, _, .con, _, _ => fun ⟨s, hs⟩ => below (size s) hs s (Nat.le_refl _)
  -- The paper's `Löb` would need, at level n = size (Löb f),
  --     ⟦ Löb f ⟧ₙ = ⟦ f ⟧ₙ ⟨Löb f, size (Löb f) < n⟩,
  -- an inequality that fails at its own size. Internal quotation of a
  -- variable needs `size s + 1 < n` from `size s < n`, and internal
  -- application needs `size f + size x + 1 < n` from `size f < n` and
  -- `size x < n`. Each fails by the syntax it adds.

/-- Soundness at every level, by strong induction on the level. -/
def soundAt (n : Nat) (s : Tm [] .bot) (h : size s ≤ n) : Empty :=
  interpAt n (fun m _hm s' h' => soundAt m s' h') s h ()
termination_by n

/-- There is no syntactic proof of absurdity. -/
theorem soundness (s : Tm [] .bot) : False :=
  (soundAt (size s) s (Nat.le_refl _)).elim

/-- The guarded syntax inhabits ‘¬’ (‘□’ ‘⊥’): it proves its own consistency.
    The paper's `incompleteness` empties this type in every syntax that
    admits Löb. -/
def selfJustification : Tm [] (.arr (.box .bot) .bot) := .con

/-- The same syntax with the unguarded constructors added. -/
inductive TmU : List Ty → Ty → Type where
  | var {Γ A} : Var Γ A → TmU Γ A
  | lam {Γ A B} : TmU (A :: Γ) B → TmU Γ (.arr A B)
  | app {Γ A B} : TmU Γ (.arr A B) → TmU Γ A → TmU Γ B
  | tt {Γ} : TmU Γ .top
  | quo {Γ A} : TmU [] A → TmU Γ (.box A)
  | fold {Γ} : TmU Γ (.arr (.arr (.box .godel) .bot) .godel)
  | unfold {Γ} : TmU Γ (.arr .godel (.arr (.box .godel) .bot))
  | con {Γ} : TmU Γ (.arr (.box .bot) .bot)
  /-- The paper's sections 6 and 7: Löb's theorem as a constructor. -/
  | lob {Γ A} : TmU [] (.arr (.box A) A) → TmU Γ A
  /-- Internal quotation of a variable (D3): the paper's ‘⌜‘VAR₀’⌝ᵗ’. -/
  | iquo {Γ A} : TmU Γ (.arr (.box A) (.box (.box A)))
  /-- Internal application (D2): the paper's _w‘‘’’ₐ_. -/
  | iapp {Γ A B} : TmU Γ (.arr (.box (.arr A B)) (.arr (.box A) (.box B)))

/-- Löb as a constructor refutes trust in one step. -/
def viaLob : TmU [] .bot := .lob .con

/-- Löb derived from the naming fixed point plus D2 and D3 at a variable. This
    is the paper's section 8 (`‘□‘H’→□‘X’’`, `‘h’`, `Löb`) with ‘f’ := `con`
    and ‘X’ := ‘⊥’. The variable is used twice, once quoted. -/
def viaQuine : TmU [] .bot :=
  let c : TmU [.box .godel] (.box .godel) := .var .zero
  -- From a proof c of G: internally unfold it, quote it, and apply.
  let unfolded : TmU [.box .godel] (.box (.arr (.box .godel) .bot)) :=
    .app (.app .iapp (.quo .unfold)) c
  let quoted : TmU [.box .godel] (.box (.box .godel)) := .app .iquo c
  let refuted : TmU [.box .godel] (.box .bot) := .app (.app .iapp unfolded) quoted
  let h : TmU [] .godel := .app .fold (.lam (.app .con refuted))
  .app (.app .unfold h) (.quo h)

/-- So no reading of `TmU` that sends ‘⊥’ to `Empty` can be total. -/
theorem no_sound_reading (sound : TmU [] .bot → False) : False :=
  sound viaQuine

#print axioms soundness
#print axioms no_sound_reading
