/-
  Trust horizon: a false contract added to the guarded syntax of
  2026-10-29-guarded-trust.lean, which this file does not change.

  Question: is a certified term safe when an unchecked assumption it uses is
  false, so long as the first violation lies beyond the term's reach?

  The contract is `ax : ‘□’ C ‘→’ ‘⊥’`, "no certificate of C exists", for a
  parameter C. Its horizon J is the size of the smallest closed term of C. The
  contract is true below J and false at J. `abort` is added too, so that one
  refutation can certify anything, as in λᶜᵉʳᵗ₀.

  Method: read ‘⊥’ at level n as *blame*, a closed term of C smaller than n,
  instead of `Empty` (Friedman's A-translation, with the contract's
  counterexample as the answer). The `ax` clause returns its argument and the
  `con` clause runs its argument at a smaller level. Neither needs the
  contract to be true, so `blame` holds with no hypothesis on C:

      every closed term of ‘⊥’ runs to a counterexample smaller than itself.

  Tightness and the two stability analogues are corollaries. Choosing C := ‘⊥’
  makes the contract the consistency axiom, and the same theorem then gives
  the guarded syntax's consistency by descent.

  Check with:  lean 2026-11-05-trust-horizon.lean   (Lean 4 core, no Mathlib;
  checked 2026-11-05 with v4.32.1). Every theorem uses propext and Quot.sound.
  The results are relative to Lean.

  Negative checks, run 2026-11-05 on edited copies and not kept here:
  1. Free quotation, `size (.quo _) = 1`: the `quo` clause fails, because omega
     cannot prove `size s < n`.
  2. Reading ‘□’ A at level n as all syntax (bound `True`): the `con` and `ax`
     clauses fail.
  3. Adding internal quotation of a variable (D3), with clause
     ⟨s, _⟩ ↦ ⟨quo s, _⟩: omega cannot prove `size s + 1 < n`.
  4. `tightness` without its hypothesis `size s ≤ J`: omega cannot prove
     `size c < J`.

  What this does not test: the contract has the one shape "no certificate of C",
  not an arbitrary decidable predicate; the guard is syntax size, not λᶜᵉʳᵗ₀'s
  tokens; there is no data sort, recursion or linearity; and no erasing
  evaluator is formalized, only the monitored one.
-/

/-- Syntactic types. -/
inductive Ty : Type where
  | arr   : Ty → Ty → Ty
  /-- ‘□’ A: closed syntax of type A. -/
  | box   : Ty → Ty
  | top   : Ty
  | bot   : Ty
  /-- G ≅ (‘□’ G ‘→’ ‘⊥’): the naming fixed point. -/
  | godel : Ty

/-- De Bruijn variables. -/
inductive Var : List Ty → Ty → Type where
  | zero {Γ : List Ty} {A : Ty} : Var (A :: Γ) A
  | succ {Γ : List Ty} {A B : Ty} : Var Γ A → Var (B :: Γ) A

/-- The guarded syntax, with the contract on C and `abort`. -/
inductive Tm (C : Ty) : List Ty → Ty → Type where
  | var {Γ A} : Var Γ A → Tm C Γ A
  | lam {Γ A B} : Tm C (A :: Γ) B → Tm C Γ (.arr A B)
  | app {Γ A B} : Tm C Γ (.arr A B) → Tm C Γ A → Tm C Γ B
  | tt {Γ} : Tm C Γ .top
  /-- Quotation of a closed term. It costs the size of what it quotes. -/
  | quo {Γ A} : Tm C [] A → Tm C Γ (.box A)
  | fold {Γ} : Tm C Γ (.arr (.arr (.box .godel) .bot) .godel)
  | unfold {Γ} : Tm C Γ (.arr .godel (.arr (.box .godel) .bot))
  /-- Trust: ‘¬’ (‘□’ ‘⊥’). -/
  | con {Γ} : Tm C Γ (.arr (.box .bot) .bot)
  /-- The contract, unchecked: no certificate of C exists. May be false. -/
  | ax {Γ} : Tm C Γ (.arr (.box C) .bot)
  /-- One refutation certifies anything. -/
  | abort {Γ A} : Tm C Γ (.arr .bot A)

/-- Syntax size: the measure a certificate's reach is bounded by. -/
def size {C Γ A} : Tm C Γ A → Nat
  | .var _ => 1
  | .lam b => size b + 1
  | .app f x => size f + size x + 1
  | .tt => 1
  | .quo t => size t + 1
  | .fold => 1
  | .unfold => 1
  | .con => 1
  | .ax => 1
  | .abort => 1

/-- A counterexample to the contract, smaller than `n`: what ‘⊥’ means here. -/
abbrev Blame (C : Ty) (n : Nat) : Type := { c : Tm C [] C // size c < n }

/-- Types read at level `n`, with blame as a possible result at every type.
    ‘□’ A is the syntax of A smaller than `n`, as in the guarded-trust file. -/
def TyM (C : Ty) (n : Nat) : Ty → Type
  | .arr A B => TyM C n A → TyM C n B
  | .box A => Sum (Blame C n) { t : Tm C [] A // size t < n }
  | .top => Sum (Blame C n) Unit
  | .bot => Blame C n
  | .godel => Sum (Blame C n) { t : Tm C [] .godel // size t < n } → Blame C n

/-- Blame propagates through every type: the `abort` clause. -/
def err {C n} (b : Blame C n) : (A : Ty) → TyM C n A
  | .arr _ B => fun _ => err b B
  | .box _ => Sum.inl b
  | .top => Sum.inl b
  | .bot => b
  | .godel => fun _ => b

/-- Environments read at level `n`. -/
def EnvM (C : Ty) (n : Nat) : List Ty → Type
  | [] => Unit
  | A :: Γ => TyM C n A × EnvM C n Γ

def lookup {C n Γ A} : Var Γ A → EnvM C n Γ → TyM C n A
  | .zero, (a, _) => a
  | .succ v, (_, ρ) => lookup v ρ

/-- The monitored interpreter at one level, given its result at lower levels.
    The `ax` clause is the monitor: it returns the certificate it was handed,
    which the level bounds. The `con` clause runs its argument, smaller than
    the level, at that argument's own level. -/
def interp {C : Ty} (n : Nat)
    (below : ∀ m, m < n → ∀ s : Tm C [] .bot, size s ≤ m → Blame C m) :
    {Γ : List Ty} → {A : Ty} → (t : Tm C Γ A) → size t ≤ n → EnvM C n Γ → TyM C n A
  | _, _, .var v, _, ρ => lookup v ρ
  | _, _, .lam b, h, ρ => fun a =>
      interp n below b (by simp only [size] at h; omega) (a, ρ)
  | _, _, .app f x, h, ρ =>
      interp n below f (by simp only [size] at h; omega) ρ
        (interp n below x (by simp only [size] at h; omega) ρ)
  | _, _, .tt, _, _ => Sum.inr ()
  | _, _, .quo s, h, _ => Sum.inr ⟨s, by simp only [size] at h; omega⟩
  | _, _, .fold, _, _ => fun g => g
  | _, _, .unfold, _, _ => fun g => g
  | _, _, .con, _, _ => fun x =>
      match x with
      | Sum.inl b => b
      | Sum.inr ⟨s, hs⟩ =>
          let b := below (size s) hs s (Nat.le_refl _)
          ⟨b.1, Nat.lt_trans b.2 hs⟩
  | _, _, .ax, _, _ => fun x =>
      match x with
      | Sum.inl b => b
      | Sum.inr c => c
  | _, _, .abort, _, _ => fun b => err b _

/-- A refutation run at level `n`, by strong induction on the level. -/
def blameAt {C : Ty} (n : Nat) (s : Tm C [] .bot) (h : size s ≤ n) : Blame C n :=
  interp n (fun m _hm s' h' => blameAt m s' h') s h ()
termination_by n

/-- **No silent failure.** With no hypothesis on C: every closed term of ‘⊥’
    runs to a real counterexample to the contract, smaller than itself. -/
def blame {C : Ty} (s : Tm C [] .bot) : { c : Tm C [] C // size c < size s } :=
  blameAt (size s) s (Nat.le_refl _)

/-- The contract holds on everything smaller than J. J is then a horizon. -/
def TrueBelow (C : Ty) (J : Nat) : Prop := ∀ c : Tm C [] C, size c < J → False

/-- **Tightness.** No refutation fits under the horizon, though the contract
    may be false at J. -/
theorem tightness {C J} (hθ : TrueBelow C J) (s : Tm C [] .bot) (h : size s ≤ J) : False :=
  hθ (blame s).1 (Nat.lt_of_lt_of_le (blame s).2 h)

/-- A certificate of ‘□’ A runs to blame or to an output smaller than itself. -/
def run {C A} (t : Tm C [] (.box A)) :
    Sum (Blame C (size t)) { r : Tm C [] A // size r < size t } :=
  interp (size t) (fun m _ s h => blameAt m s h) t (Nat.le_refl _) ()

/-- **E-analogue.** Under the horizon the run returns a real output, bounded by
    the certificate alone. Nothing about the bound depends on J. -/
theorem output_below {C A J} (hθ : TrueBelow C J) (t : Tm C [] (.box A)) (h : size t ≤ J) :
    ∃ r : Tm C [] A, size r < size t :=
  match run t with
  | .inl b => (hθ b.1 (Nat.lt_of_lt_of_le b.2 h)).elim
  | .inr r => ⟨r.1, r.2⟩

/-- Under the horizon there is no fake certificate of a refutation either. -/
theorem no_fake_below {C J} (hθ : TrueBelow C J) (t : Tm C [] (.box .bot)) (h : size t ≤ J) :
    False :=
  match output_below hθ t h with
  | ⟨r, hr⟩ => tightness hθ r (Nat.le_trans (Nat.le_of_lt hr) h)

/-- **A-analogue.** A certified universal claim, "no certificate of A exists",
    has no counterexample under the horizon. Program and input each need only
    fit under J: the level is one cap, not a budget they share. -/
theorem universal_below {C A J} (hθ : TrueBelow C J)
    (f : Tm C [] (.arr (.box A) .bot)) (hf : size f ≤ J)
    (x : Tm C [] A) (hx : size x < J) : False :=
  let b : Blame C J :=
    interp J (fun m _ s h => blameAt m s h) f hf () (Sum.inr ⟨x, hx⟩)
  hθ b.1 b.2

/-- **Self-justification is the special case C := ‘⊥’.** The contract is then
    the consistency axiom, and a refutation's counterexample is a smaller
    refutation. -/
theorem consistent_when_contract_is_consistency (s : Tm .bot [] .bot) : False :=
  have _h : size (blame s).1 < size s := (blame s).2
  consistent_when_contract_is_consistency (blame s).1
termination_by size s

/-! ### The theorems are not vacuous: a false contract with a tunable horizon -/

/-- ‘□’ applied k times to ‘⊤’. -/
def boxes : Nat → Ty
  | 0 => .top
  | k + 1 => .box (boxes k)

/-- The smallest certificate of `boxes k`, of size k + 1. -/
def quos (C : Ty) : (k : Nat) → Tm C [] (boxes k)
  | 0 => .tt
  | k + 1 => .quo (quos C k)

theorem size_quos (C : Ty) (k : Nat) : size (quos C k) = k + 1 := by
  induction k with
  | zero => rfl
  | succ k ih => simp only [quos, size, ih]

theorem size_pos {C Γ A} (t : Tm C Γ A) : 1 ≤ size t := by
  cases t <;> simp only [size] <;> omega

/-- `run`, as a statement about sizes. -/
theorem step {C A} (t : Tm C [] (.box A)) :
    (∃ c : Tm C [] C, size c < size t) ∨ (∃ r : Tm C [] A, size r < size t) :=
  match run t with
  | .inl b => Or.inl ⟨b.1, b.2⟩
  | .inr r => Or.inr ⟨r.1, r.2⟩

/-- Every closed term of `boxes j` with size at most k has size at least j + 1,
    in the syntax whose contract is on `boxes k`. The interpreter proves it: a
    term of ‘□’ A yields a smaller term of A, or a smaller term of the contract
    type, and each is impossible by induction on size. -/
theorem minCert (k n : Nat) :
    ∀ (j : Nat) (t : Tm (boxes k) [] (boxes j)), size t = n → n ≤ k → j + 1 ≤ n := by
  induction n using Nat.strongRecOn with
  | ind n ih =>
    intro j t ht hn
    cases j with
    | zero =>
      have := size_pos t
      omega
    | succ j =>
      have ht' : size (A := .box (boxes j)) t = n := ht
      rcases step (A := boxes j) t with ⟨c, hc⟩ | ⟨r, hr⟩
      · have := ih (size c) (by omega) k c rfl (by omega)
        omega
      · have := ih (size r) (by omega) j r rfl (by omega)
        omega

/-- The contract on `boxes k` is true below k + 1 ... -/
theorem contract_true_below (k : Nat) : TrueBelow (boxes k) (k + 1) := fun c h => by
  have := minCert k (size c) k c rfl (by omega)
  omega

/-- ... false at k + 1, where `quos` is its counterexample ... -/
theorem contract_false_at (k : Nat) :
    ∃ c : Tm (boxes k) [] (boxes k), size c = k + 1 :=
  ⟨quos _ k, size_quos _ k⟩

/-- ... and refuted a constant above that. -/
def refute (k : Nat) : Tm (boxes k) [] .bot :=
  .app .ax (.quo (quos (boxes k) k))

theorem size_refute (k : Nat) : size (refute k) = k + 4 := by
  simp only [refute, size, size_quos]
  omega

/-- **The horizon is sharp to within 3.** With J = k + 1: no refutation of size
    at most J, and one of size J + 3. -/
theorem horizon_sharp (k : Nat) :
    (∀ s : Tm (boxes k) [] .bot, size s ≤ k + 1 → False) ∧
    (∃ s : Tm (boxes k) [] .bot, size s = k + 4) :=
  ⟨fun s h => tightness (contract_true_below k) s h, ⟨refute k, size_refute k⟩⟩

/-- Blame names the actual counterexample. -/
theorem blame_refute (k : Nat) : (blame (refute k)).1 = quos (boxes k) k := by
  unfold blame
  rw [blameAt]
  rfl

/-- Above the horizon `abort` certifies a false claim: a certificate of ‘⊥’. -/
def fake (k : Nat) : Tm (boxes k) [] (.box .bot) := .app .abort (refute k)

/-- The monitor catches it: the run blames instead of returning an output. -/
theorem fake_is_blamed (k : Nat) : ∃ b, run (fake k) = Sum.inl b := ⟨_, rfl⟩

/-! ### The contrast: a contract on a sort whose values cost nothing to name -/

def tower : Nat → Nat
  | 0 => 1
  | k + 1 => 2 ^ tower k

/-- In plain Lean a false contract on `Nat` falls to a constant-size term,
    however far out its first violation is, and nothing computes `tower 5`.
    Plain Lean, not λᶜᵉʳᵗ₀, which has no postulates to test this with. -/
theorem data_contract_refuted : (∀ n, n ≠ tower 5) → False := fun ax => ax _ rfl

#print axioms blame
#print axioms tightness
#print axioms output_below
#print axioms universal_below
#print axioms consistent_when_contract_is_consistency
#print axioms horizon_sharp
#print axioms blame_refute
#print axioms fake_is_blamed
