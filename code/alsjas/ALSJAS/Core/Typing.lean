import ALSJAS.Core.Syntax

/-!
# Executable affine typing

`infer` returns both the inferred type and a bit for every input assumption.
Sibling premises are merged only when their bit-vectors are disjoint. This is
the executable source of affinity: weakening is represented by `false`, while
using one assumption twice is rejected at the merge that would contract it.

Raw quotation is intentionally deferred. The later proof checker validates the
payload and then invokes the same affine rules; this module never trusts an
opaque host callback.
-/

namespace ALSJAS

/-- Structured failures are part of the public checker interface and support
precise negative controls. -/
inductive TypeError where
  | variableOutOfScope : Nat → TypeError
  | resourceDuplicated : Nat → TypeError
  | typeMismatch : Ty → Ty → TypeError
  | expectedFunction : Ty → TypeError
  | expectedTensor : Ty → TypeError
  | expectedBox : Ty → TypeError
  | usageLengthMismatch
  | quoteRequiresProofCheck
  | capabilityForbidden : String → TypeError
deriving BEq, Repr

/-- Successful inference records exactly which assumptions were consumed. -/
structure Inferred where
  type : Ty
  usage : List Bool
deriving BEq, Repr

namespace Typing

/-- A constant or closed constructor weakens every ambient assumption. -/
private def unused (context : List Ty) : List Bool :=
  List.replicate context.length false

/-- Look up a variable while constructing its one-hot usage vector. -/
private def inferVariable : Nat → List Ty → Except TypeError Inferred
  | index, [] => .error (.variableOutOfScope index)
  | 0, proposition :: tail =>
      .ok { type := proposition, usage := true :: List.replicate tail.length false }
  | index + 1, _ :: tail => do
      let inferred ← inferVariable index tail
      pure { inferred with usage := false :: inferred.usage }

/-- Merge affine usage at a known context position. Two `true` bits are the
exact executable witness of a forbidden contraction. -/
private def mergeAt : Nat → List Bool → List Bool →
    Except TypeError (List Bool)
  | _, [], [] => .ok []
  | index, left :: leftTail, right :: rightTail =>
      if left && right then
        .error (.resourceDuplicated index)
      else do
        let tail ← mergeAt (index + 1) leftTail rightTail
        pure ((left || right) :: tail)
  | _, _, _ => .error .usageLengthMismatch

/-- Public form of affine usage union. -/
def mergeUsage (left right : List Bool) : Except TypeError (List Bool) :=
  mergeAt 0 left right

/-- Infer a raw term in an affine context. The result has one usage bit per
context entry. Modal quotation is the sole constructor intentionally handed to
the proof-checking layer. -/
def infer (profile : RuleProfile) (context : List Ty) : Term →
    Except TypeError Inferred
  | .var index => inferVariable index context
  | .triv => .ok { type := .one, usage := unused context }
  | .pair left right => do
      let leftResult ← infer profile context left
      let rightResult ← infer profile context right
      let usage ← mergeUsage leftResult.usage rightResult.usage
      pure { type := .tensor leftResult.type rightResult.type, usage }
  | .letTensor scrutinee body => do
      let scrutineeResult ← infer profile context scrutinee
      match scrutineeResult.type with
      | .tensor leftType rightType =>
          let bodyResult ← infer profile (rightType :: leftType :: context) body
          match bodyResult.usage with
          | _ :: _ :: outerUsage => do
              let usage ← mergeUsage scrutineeResult.usage outerUsage
              pure { type := bodyResult.type, usage }
          | _ => .error .usageLengthMismatch
      | actual => .error (.expectedTensor actual)
  | .lam domain body => do
      let bodyResult ← infer profile (domain :: context) body
      match bodyResult.usage with
      | _ :: outerUsage =>
          pure { type := .lolli domain bodyResult.type, usage := outerUsage }
      | [] => .error .usageLengthMismatch
  | .app function argument => do
      let functionResult ← infer profile context function
      let argumentResult ← infer profile context argument
      match functionResult.type with
      | .lolli domain codomain =>
          if domain == argumentResult.type then
            let usage ← mergeUsage functionResult.usage argumentResult.usage
            pure { type := codomain, usage }
          else
            .error (.typeMismatch domain argumentResult.type)
      | actual => .error (.expectedFunction actual)
  | .abort resultType contradiction => do
      let contradictionResult ← infer profile context contradiction
      if contradictionResult.type == .zero then
        pure { type := resultType, usage := contradictionResult.usage }
      else
        .error (.typeMismatch .zero contradictionResult.type)
  | .quote _ _ _ => .error .quoteRequiresProofCheck
  | .boxComp function argument => do
      let functionResult ← infer profile context function
      let argumentResult ← infer profile context argument
      match functionResult.type with
      | .box system (.lolli domain codomain) =>
          let expectedArgument := Ty.box system domain
          if argumentResult.type == expectedArgument then
            let usage ← mergeUsage functionResult.usage argumentResult.usage
            pure { type := .box system codomain, usage }
          else
            .error (.typeMismatch expectedArgument argumentResult.type)
      | actual => .error (.expectedBox actual)
  | .boxIntrosp proof => do
      let proofResult ← infer profile context proof
      match proofResult.type with
      | .box system boxedType =>
          let resultType := Ty.box system (Ty.box system boxedType)
          pure ({ type := resultType, usage := proofResult.usage } : Inferred)
      | actual => .error (.expectedBox actual)
  | .godelFold system proof => do
      let proofResult ← infer profile context proof
      let expected := Ty.box system (.lolli (.godel system) .zero)
      if proofResult.type == expected then
        pure { type := .godel system, usage := proofResult.usage }
      else
        .error (.typeMismatch expected proofResult.type)
  | .godelUnfold system proof => do
      let proofResult ← infer profile context proof
      let expected := Ty.godel system
      if proofResult.type == expected then
        let resultType := Ty.box system (Ty.lolli (Ty.godel system) Ty.zero)
        pure ({ type := resultType, usage := proofResult.usage } : Inferred)
      else
        .error (.typeMismatch expected proofResult.type)
  | .self0 system =>
      let resultType := Ty.lolli (Ty.box system Ty.zero) Ty.zero
      .ok { type := resultType, usage := unused context }
  | .collapse1 system =>
      let resultType := Ty.lolli (Ty.clash1 system) (Ty.box system Ty.zero)
      .ok { type := resultType, usage := unused context }
  | .copyBox system copiedType =>
      match profile with
      | .baseline => .error (.capabilityForbidden "copy-box")
      | .withCopy =>
          let boxed := Ty.box system copiedType
          let resultType := Ty.lolli boxed (Ty.tensor boxed boxed)
          .ok { type := resultType, usage := unused context }

/-- Check a term against an expected type while preserving the inferred usage
certificate for subsequent affine composition. -/
def check (profile : RuleProfile) (context : List Ty) (term : Term)
    (expected : Ty) : Except TypeError (List Bool) := do
  let inferred ← infer profile context term
  if inferred.type == expected then pure inferred.usage
  else .error (.typeMismatch expected inferred.type)

end Typing

end ALSJAS
