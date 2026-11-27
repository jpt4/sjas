/-
ADR-0011: the full untyped rewrite relation, its strict descent, and the
four-step counterexample after adding W. Lean 4.32.1 core only.

This checks strong normalization, including reduction under quotation.
Soundness.lean supplies type preservation and the canonical-forms argument.
Neither file verifies the Python type-inference implementation.
-/
namespace AffineReflection

inductive Prim where
  | i | k | b | c | e | d | m | f | u | star | w
  deriving DecidableEq, Repr

inductive Tm where
  | atom : Prim → Tm
  | ap : Tm → Tm → Tm
  | qt : Tm → Tm
  deriving DecidableEq, Repr

open Tm

def size : Tm → Nat
  | atom _ => 1
  | ap f x => size f + size x + 1
  | qt x => size x + 1

theorem size_pos (t : Tm) : 0 < size t := by
  cases t <;> simp [size] <;> omega

/-- Contextual reduction. W deliberately has no rule here. -/
inductive Red : Tm → Tm → Prop where
  | i : Red (ap (atom .i) x) x
  | k : Red (ap (ap (atom .k) x) y) x
  | b : Red (ap (ap (ap (atom .b) f) g) x) (ap f (ap g x))
  | c : Red (ap (ap (ap (atom .c) f) x) y) (ap (ap f y) x)
  | e : Red (ap (atom .e) (qt t)) t
  | d : Red (ap (atom .d) (qt t)) (qt (qt t))
  | m : Red (ap (ap (atom .m) (qt f)) (qt x)) (qt (ap f x))
  | u : Red (ap (atom .u) (ap (atom .f) t)) t
  | left : Red f g → Red (ap f x) (ap g x)
  | right : Red x y → Red (ap f x) (ap f y)
  | quote : Red x y → Red (qt x) (qt y)

/-- No root rule copies a metavariable; all context rules preserve the drop. -/
theorem decrease (r : Red t s) : size s < size t := by
  induction r <;> simp_all [size] <;> omega

/-- Constructive strong normalization, with the reduction direction explicit. -/
theorem stronglyNormalizing (t : Tm) : Acc (fun s t => Red t s) t := by
  apply Acc.intro
  intro s r
  exact stronglyNormalizing s
termination_by size t
decreasing_by exact decrease r

/-- The W extension contains all the safe reductions and contextual W. -/
inductive UnsafeRed : Tm → Tm → Prop where
  | safe : Red t s → UnsafeRed t s
  | w : UnsafeRed (ap (ap (atom .w) f) x) (ap (ap f x) x)
  | left : UnsafeRed f g → UnsafeRed (ap f x) (ap g x)
  | right : UnsafeRed x y → UnsafeRed (ap f x) (ap f y)
  | quote : UnsafeRed x y → UnsafeRed (qt x) (qt y)

def h : Tm := ap (atom .f) (ap (atom .w) (ap (ap (atom .b) (atom .u)) (atom .e)))
def loop0 : Tm := ap (ap (atom .u) h) (qt h)
def loop1 : Tm := ap (ap (atom .w) (ap (ap (atom .b) (atom .u)) (atom .e))) (qt h)
def loop2 : Tm := ap (ap (ap (ap (atom .b) (atom .u)) (atom .e)) (qt h)) (qt h)
def loop3 : Tm := ap (ap (atom .u) (ap (atom .e) (qt h))) (qt h)

theorem cycle01 : UnsafeRed loop0 loop1 := .safe (.left .u)
theorem cycle12 : UnsafeRed loop1 loop2 := .w
theorem cycle23 : UnsafeRed loop2 loop3 := .safe (.left .b)
theorem cycle30 : UnsafeRed loop3 loop0 := .safe (.left (.right .e))

theorem exact_sizes :
    [size loop0, size loop1, size loop2, size loop3, size loop0] =
    [22, 18, 27, 25, 22] := rfl

def cycle (n : Nat) : Tm :=
  match n % 4 with
  | 0 => loop0
  | 1 => loop1
  | 2 => loop2
  | _ => loop3

theorem infinite_unsafe_run (n : Nat) : UnsafeRed (cycle n) (cycle (n+1)) := by
  have hlt := Nat.mod_lt n (by decide : 0 < 4)
  have hnext : (n+1)%4 = (n%4 + 1)%4 := by omega
  unfold cycle
  rw [hnext]
  have cases : n%4 = 0 ∨ n%4 = 1 ∨ n%4 = 2 ∨ n%4 = 3 := by omega
  rcases cases with h | h | h | h
  · simpa [h] using cycle01
  · simpa [h] using cycle12
  · simpa [h] using cycle23
  · simpa [h] using cycle30

#print axioms decrease
#print axioms stronglyNormalizing
#print axioms infinite_unsafe_run
end AffineReflection
