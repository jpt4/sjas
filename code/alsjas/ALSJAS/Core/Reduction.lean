import ALSJAS.Core.Syntax

/-!
# Deterministic affine reduction

The executable strategy is left-to-right call by value. The logical
normalization theorem will later cover the compatible reduction relation; this
module supplies an inspectable deterministic evaluator and the first
determinism theorem. It never descends into a `quote` payload.
-/

namespace ALSJAS

namespace Term

/-- Lift a renaming through one binder. -/
private def up (mapping : Nat → Nat) : Nat → Nat
  | 0 => 0
  | index + 1 => mapping index + 1

/-- Lift a renaming through the two binders introduced by tensor elimination. -/
private def upTwo (mapping : Nat → Nat) : Nat → Nat
  | 0 => 0
  | 1 => 1
  | index + 2 => mapping index + 2

/-- Capture-avoiding renaming. Serialized quote payloads are opaque syntax and
are not rewritten when surrounding binders move. -/
def rename (mapping : Nat → Nat) : Term → Term
  | .var index => .var (mapping index)
  | .triv => .triv
  | .pair left right => .pair (left.rename mapping) (right.rename mapping)
  | .letTensor scrutinee body =>
      .letTensor (scrutinee.rename mapping) (body.rename (upTwo mapping))
  | .lam domain body => .lam domain (body.rename (up mapping))
  | .app function argument =>
      .app (function.rename mapping) (argument.rename mapping)
  | .abort resultType contradiction =>
      .abort resultType (contradiction.rename mapping)
  | .quote system conclusion proof => .quote system conclusion proof
  | .boxComp function argument =>
      .boxComp (function.rename mapping) (argument.rename mapping)
  | .boxIntrosp proof => .boxIntrosp (proof.rename mapping)
  | .godelFold system proof => .godelFold system (proof.rename mapping)
  | .godelUnfold system proof => .godelUnfold system (proof.rename mapping)
  | .self0 system => .self0 system
  | .collapse1 system => .collapse1 system
  | .copyBox system copiedType => .copyBox system copiedType

/-- Lift a simultaneous substitution through one binder. -/
private def upSubstitution (substitution : Nat → Term) : Nat → Term
  | 0 => .var 0
  | index + 1 => (substitution index).rename Nat.succ

/-- Lift a simultaneous substitution through two binders. -/
private def upTwoSubstitution (substitution : Nat → Term) : Nat → Term
  | 0 => .var 0
  | 1 => .var 1
  | index + 2 => (substitution index).rename (fun variableIndex => variableIndex + 2)

/-- Capture-avoiding simultaneous substitution over program syntax. -/
def substitute (substitution : Nat → Term) : Term → Term
  | .var index => substitution index
  | .triv => .triv
  | .pair left right =>
      .pair (left.substitute substitution) (right.substitute substitution)
  | .letTensor scrutinee body =>
      .letTensor (scrutinee.substitute substitution)
        (body.substitute (upTwoSubstitution substitution))
  | .lam domain body => .lam domain (body.substitute (upSubstitution substitution))
  | .app function argument =>
      .app (function.substitute substitution) (argument.substitute substitution)
  | .abort resultType contradiction =>
      .abort resultType (contradiction.substitute substitution)
  | .quote system conclusion proof => .quote system conclusion proof
  | .boxComp function argument =>
      .boxComp (function.substitute substitution) (argument.substitute substitution)
  | .boxIntrosp proof => .boxIntrosp (proof.substitute substitution)
  | .godelFold system proof => .godelFold system (proof.substitute substitution)
  | .godelUnfold system proof => .godelUnfold system (proof.substitute substitution)
  | .self0 system => .self0 system
  | .collapse1 system => .collapse1 system
  | .copyBox system copiedType => .copyBox system copiedType

private def betaSubstitution (argument : Term) : Nat → Term
  | 0 => argument
  | index + 1 => .var index

/-- Contract a beta redex and remove its binder. -/
def beta (body argument : Term) : Term :=
  body.substitute (betaSubstitution argument)

private def tensorSubstitution (left right : Term) : Nat → Term
  | 0 => right
  | 1 => left
  | index + 2 => .var index

/-- Contract tensor elimination. The body binds the right component at index
zero and the left component at index one. -/
def openTensor (body left right : Term) : Term :=
  body.substitute (tensorSubstitution left right)

end Term

namespace Reduction

/-- Values do not inspect proof payloads. `copyBox` is a function constant;
its application is the one reduction rule that duplicates a boxed term. -/
def isValue : Term → Bool
  | .triv | .lam _ _ | .quote _ _ _ | .self0 _ | .collapse1 _ |
      .copyBox _ _ => true
  | .pair left right => isValue left && isValue right
  | .boxComp function argument => isValue function && isValue argument
  | .boxIntrosp proof => isValue proof
  | .godelFold _ proof => isValue proof
  | _ => false

/-- One deterministic left-to-right reduction step. -/
def step? : Term → Option Term
  | .app (.lam domain body) argument =>
      if isValue argument then some (body.beta argument)
      else (.app (.lam domain body) ·) <$> step? argument
  | .app (.copyBox system copiedType) argument =>
      if isValue argument then some (.pair argument argument)
      else (.app (.copyBox system copiedType) ·) <$> step? argument
  | .app function argument =>
      match step? function with
      | some next => some (.app next argument)
      | none => (.app function ·) <$> step? argument
  | .pair left right =>
      match step? left with
      | some next => some (.pair next right)
      | none => (.pair left ·) <$> step? right
  | .letTensor (.pair left right) body =>
      if isValue left && isValue right then some (body.openTensor left right)
      else
        (.letTensor · body) <$> step? (.pair left right)
  | .letTensor scrutinee body => (.letTensor · body) <$> step? scrutinee
  | .abort resultType contradiction =>
      (.abort resultType ·) <$> step? contradiction
  | .boxComp function argument =>
      match step? function with
      | some next => some (.boxComp next argument)
      | none => (.boxComp function ·) <$> step? argument
  | .boxIntrosp proof => .boxIntrosp <$> step? proof
  | .godelFold system proof => .godelFold system <$> step? proof
  | .godelUnfold system (.godelFold foldedSystem proof) =>
      if system == foldedSystem && isValue proof then some proof
      else .godelUnfold system <$> step? (.godelFold foldedSystem proof)
  | .godelUnfold system proof => .godelUnfold system <$> step? proof
  | _ => none

/-- The graph of the executable step function. -/
def Step (source target : Term) : Prop :=
  step? source = some target

/-- Determinism follows by construction, but is recorded as a theorem for the
metatheory dependency graph. -/
theorem step_deterministic {source left right : Term} :
    Step source left → Step source right → left = right := by
  intro leftStep rightStep
  unfold Step at leftStep rightStep
  rw [leftStep] at rightStep
  exact Option.some.inj rightStep

/-- Fuel-bounded iteration is total and returns the current normal/stuck term
when no step remains. Fuel is a driver bound, never a proof of normalization. -/
def normalize : Nat → Term → Term
  | 0, term => term
  | fuel + 1, term =>
      match step? term with
      | none => term
      | some next => normalize fuel next

end Reduction

end ALSJAS
