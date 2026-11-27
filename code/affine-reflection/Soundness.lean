/-
The complete declarative typing relation for ADR-0011, including the
evaluator E, quotation D, code application M, and the negative recursive
type G with explicit fold/unfold. No theorem assumes the old fragment's
soundness. Consistency is a recursion on the size of a putative Bot term.

Only the declarative relation is verified here. The external Python
unification algorithm and its correspondence to Typed remain unverified.
-/
import Decrease
namespace AffineReflection
open Tm

inductive Ty where
  | bot | unit | g
  | arr : Ty → Ty → Ty
  | box : Ty → Ty
  deriving DecidableEq, Repr
open Ty

def AtomTy : Prim → Ty → Prop
  | .i, t => ∃ a, t = arr a a
  | .k, t => ∃ a b, t = arr a (arr b a)
  | .b, t => ∃ a b c, t = arr (arr b c) (arr (arr a b) (arr a c))
  | .c, t => ∃ a b c, t = arr (arr a (arr b c)) (arr b (arr a c))
  | .e, t => ∃ a, t = arr (box a) a
  | .d, t => ∃ a, t = arr (box a) (box (box a))
  | .m, t => ∃ a b, t = arr (box (arr a b)) (arr (box a) (box b))
  | .f, t => t = arr (arr (box g) bot) g
  | .u, t => t = arr g (arr (box g) bot)
  | .star, t => t = unit
  | .w, _ => False

/-- A finite, syntax-directed declarative type assignment, without W. -/
def Typed : Tm → Ty → Prop
  | atom p, a => AtomTy p a
  | ap f x, b => ∃ a, Typed f (arr a b) ∧ Typed x a
  | qt t, b => ∃ a, b = box a ∧ Typed t a

@[simp] theorem typed_atom : Typed (atom p) a ↔ AtomTy p a := Iff.rfl
@[simp] theorem typed_ap : Typed (ap f x) b ↔ ∃ a, Typed f (arr a b) ∧ Typed x a := Iff.rfl
@[simp] theorem typed_qt : Typed (qt t) b ↔ ∃ a, b = box a ∧ Typed t a := Iff.rfl

theorem preservation (r : Red t s) (ht : Typed t a) : Typed s a := by
  induction r generalizing a with
  | i => simpa [AtomTy] using ht
  | k =>
    simp [AtomTy] at ht
    exact ht.left
  | b =>
    simp [AtomTy] at ht ⊢
    obtain ⟨_, ⟨_, ⟨_, ⟨a0, b0, c0, rfl, rfl, rfl, rfl⟩, hf⟩, hg⟩, hx⟩ := ht
    exact ⟨_, hf, _, hg, hx⟩
  | c =>
    simp [AtomTy] at ht ⊢
    obtain ⟨_, ⟨_, ⟨_, ⟨a0, b0, c0, rfl, rfl, rfl, rfl⟩, hf⟩, hx⟩, hy⟩ := ht
    exact ⟨_, ⟨_, hf, hy⟩, hx⟩
  | e => simpa [AtomTy] using ht
  | d =>
    simp [AtomTy] at ht ⊢
    obtain ⟨_, ⟨b, rfl, rfl⟩, ⟨_, heq, hb⟩⟩ := ht
    cases heq
    exact ⟨_, rfl, _, rfl, hb⟩
  | m =>
    simp [AtomTy] at ht ⊢
    obtain ⟨_, ⟨_, ⟨a0, b0, rfl, rfl, rfl⟩, ⟨_, heq, hf⟩⟩, ⟨_, heq2, hx⟩⟩ := ht
    cases heq
    cases heq2
    exact ⟨_, rfl, _, hf, hx⟩
  | u =>
    simp [AtomTy] at ht
    obtain ⟨_, ⟨rfl, rfl⟩, ⟨_, ⟨rfl, _⟩, hb⟩⟩ := ht
    exact hb
  | left r ih =>
    obtain ⟨b, hf, hx⟩ := ht
    exact ⟨b, ih hf, hx⟩
  | right r ih =>
    obtain ⟨b, hf, hx⟩ := ht
    exact ⟨b, hf, ih hx⟩
  | quote r ih =>
    obtain ⟨b, rfl, hb⟩ := ht
    exact ⟨b, rfl, ih hb⟩

/-- Weak values include the unsaturated affine combinators. -/
inductive Value : Tm → Prop where
  | atom (p) : Value (atom p)
  | quote (t) : Value (qt t)
  | k1 : Value x → Value (ap (atom .k) x)
  | b1 : Value x → Value (ap (atom .b) x)
  | b2 : Value x → Value y → Value (ap (ap (atom .b) x) y)
  | c1 : Value x → Value (ap (atom .c) x)
  | c2 : Value x → Value y → Value (ap (ap (atom .c) x) y)
  | m1 : Value x → Value (ap (atom .m) x)
  | f1 : Value x → Value (ap (atom .f) x)

theorem value_not_bot (v : Value t) : ¬ Typed t bot := by
  cases v with
  | atom p => cases p <;> simp [AtomTy]
  | _ => simp [AtomTy]

theorem value_box (v : Value t) (ht : Typed t (box a)) :
    ∃ s, t = qt s ∧ Typed s a := by
  cases v with
  | atom p => cases p <;> simp [AtomTy] at ht
  | _ => simp_all [AtomTy]

theorem value_g (v : Value t) (ht : Typed t g) :
    ∃ s, t = ap (atom .f) s ∧ Typed s (arr (box g) bot) := by
  cases v with
  | atom p => cases p <;> simp [AtomTy] at ht
  | _ => simp_all [AtomTy]

theorem value_application (vf : Value f) (vx : Value x)
    (ht : Typed (ap f x) a) : Value (ap f x) ∨ ∃ s, Red (ap f x) s := by
  cases vf with
  | atom p =>
    cases p with
    | i => exact .inr ⟨x, .i⟩
    | k => exact .inl (.k1 vx)
    | b => exact .inl (.b1 vx)
    | c => exact .inl (.c1 vx)
    | m => exact .inl (.m1 vx)
    | f => exact .inl (.f1 vx)
    | e =>
      have hx : Typed x (box a) := by simpa [AtomTy] using ht
      obtain ⟨s, rfl, _⟩ := value_box vx hx
      exact .inr ⟨s, .e⟩
    | d =>
      simp [AtomTy] at ht
      obtain ⟨_, ⟨b, rfl, rfl⟩, hx⟩ := ht
      obtain ⟨s, rfl, _⟩ := value_box vx hx
      exact .inr ⟨qt (qt s), .d⟩
    | u =>
      have hx : Typed x g := by
        simp [AtomTy] at ht
        obtain ⟨_, ⟨rfl, _⟩, hx⟩ := ht
        exact hx
      obtain ⟨s, rfl, _⟩ := value_g vx hx
      exact .inr ⟨s, .u⟩
    | star => simp [AtomTy] at ht
    | w => simp [AtomTy] at ht
  | quote => simp at ht
  | k1 => exact .inr ⟨_, .k⟩
  | b1 vy => exact .inl (.b2 vy vx)
  | b2 => exact .inr ⟨_, .b⟩
  | c1 vy => exact .inl (.c2 vy vx)
  | c2 => exact .inr ⟨_, .c⟩
  | m1 vy =>
    simp [AtomTy] at ht
    obtain ⟨_, ⟨_, ⟨b, c, rfl, rfl, rfl⟩, hf⟩, hx⟩ := ht
    obtain ⟨s, rfl, _⟩ := value_box vy hf
    obtain ⟨t, rfl, _⟩ := value_box vx hx
    exact .inr ⟨qt (ap s t), .m⟩
  | f1 => simp [AtomTy] at ht

theorem progress (ht : Typed t a) : Value t ∨ ∃ s, Red t s := by
  induction t generalizing a with
  | atom p => exact .inl (.atom p)
  | qt t => exact .inl (.quote t)
  | ap f x ihf ihx =>
    obtain ⟨b, hf, hx⟩ := ht
    rcases ihf hf with vf | ⟨f', rf⟩
    · rcases ihx hx with vx | ⟨x', rx⟩
      · exact value_application vf vx ⟨b, hf, hx⟩
      · exact .inr ⟨ap f x', .right rx⟩
    · exact .inr ⟨ap f' x, .left rf⟩

/-- Constructive consistency of the whole declaratively typed language. -/
theorem consistency (t : Tm) (ht : Typed t bot) : False := by
  rcases progress ht with hv | ⟨s, hr⟩
  · exact value_not_bot hv ht
  · exact consistency s (preservation hr ht)
termination_by size t
decreasing_by exact decrease hr

theorem h0 : Typed (atom .e) (arr (box bot) bot) := by simp [AtomTy]

/-- A single term, with a type-indexed schema, implements pair refutation. -/
def h1term : Tm := ap (ap (atom .b) (ap (atom .c) (atom .e))) (atom .e)

theorem h1 (a : Ty) : Typed h1term (arr (box a) (arr (box (arr a bot)) bot)) := by
  let z := arr (box (arr a bot)) bot
  refine ⟨arr (box a) a, ?_, ?_⟩
  · refine ⟨arr a z, ?_, ?_⟩
    · exact ⟨box a, a, z, rfl⟩
    · refine ⟨arr (box (arr a bot)) (arr a bot), ?_, ?_⟩
      · exact ⟨box (arr a bot), a, bot, rfl⟩
      · exact ⟨arr a bot, rfl⟩
  · exact ⟨a, rfl⟩

/-- No two closed terms inhabit a type and its affine negation. -/
theorem pair_consistency (hp : Typed p a) (hq : Typed q (arr a bot)) : False :=
  consistency (ap q p) ⟨a, hq, hp⟩

#print axioms preservation
#print axioms consistency
#print axioms h0
#print axioms h1
#print axioms pair_consistency
end AffineReflection
