import ALSJAS.Core.Reduction
import ALSJAS.Core.Typing

/-!
# First core metatheory slice

These declarations lift properties previously tested on examples to universal
theorems over complete syntax types. They are intentionally modest: strong
normalization, subject reduction, and checker adequacy remain separate gates.
-/

namespace ALSJAS

namespace SystemRef

/-- Canonical serialization followed by decoding preserves every system
reference structurally. -/
@[simp] theorem from_toSExpr (system : SystemRef) :
    fromSExpr system.toSExpr = some system := by
  cases system <;> rfl

end SystemRef

namespace Ty

/-- Canonical serialization followed by decoding preserves every ALSJAS type. -/
@[simp] theorem from_toSExpr (proposition : Ty) :
    fromSExpr proposition.toSExpr = some proposition := by
  induction proposition with
  | zero => rfl
  | one => rfl
  | tensor left right leftIH rightIH =>
      simp [toSExpr, fromSExpr, leftIH, rightIH]
  | lolli domain codomain domainIH codomainIH =>
      simp [toSExpr, fromSExpr, domainIH, codomainIH]
  | box system boxedType boxedIH =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr, boxedIH]
  | godel system =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr]
  | clash1 system =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr]
  | atom name => rfl

end Ty

namespace Term

/-- Canonical serialization followed by decoding preserves every raw term,
including the complete untouched payload of every quotation. -/
@[simp] theorem from_toSExpr (term : Term) :
    fromSExpr term.toSExpr = some term := by
  induction term with
  | var index =>
      simp [toSExpr, fromSExpr]
  | triv => rfl
  | pair left right leftIH rightIH =>
      simp [toSExpr, fromSExpr, leftIH, rightIH]
  | letTensor scrutinee body scrutineeIH bodyIH =>
      simp [toSExpr, fromSExpr, scrutineeIH, bodyIH]
  | lam domain body bodyIH =>
      simp [toSExpr, fromSExpr, Ty.from_toSExpr, bodyIH]
  | app function argument functionIH argumentIH =>
      simp [toSExpr, fromSExpr, functionIH, argumentIH]
  | abort resultType contradiction contradictionIH =>
      simp [toSExpr, fromSExpr, Ty.from_toSExpr, contradictionIH]
  | quote system conclusion proof =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr, Ty.from_toSExpr]
  | boxComp function argument functionIH argumentIH =>
      simp [toSExpr, fromSExpr, functionIH, argumentIH]
  | boxIntrosp proof proofIH =>
      simp [toSExpr, fromSExpr, proofIH]
  | godelFold system proof proofIH =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr, proofIH]
  | godelUnfold system proof proofIH =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr, proofIH]
  | self0 system =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr]
  | collapse1 system =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr]
  | copyBox system copiedType =>
      simp [toSExpr, fromSExpr, SystemRef.from_toSExpr, Ty.from_toSExpr]

end Term

namespace Core

/-- A quote is a value and core reduction never enters its serialized payload. -/
theorem quote_is_reduction_normal (system : SystemRef) (conclusion : Ty)
    (payload : SExpr) :
    Reduction.step? (.quote system conclusion payload) = none := by
  rfl

/-- Baseline typing rejects the sole matched-control constructor for every
system and proposition. -/
theorem baseline_rejects_copyBox (system : SystemRef) (copiedType : Ty) :
    Typing.infer .baseline [] (.copyBox system copiedType) =
      .error (.capabilityForbidden "copy-box") := by
  rfl

/-- The control's operational rule makes the resource change explicit by
placing two complete occurrences of a boxed value in the result tree. -/
theorem copyBox_duplicates_value (system : SystemRef) (copiedType : Ty)
    (payload : SExpr) :
    Reduction.step?
      (.app (.copyBox system copiedType)
        (.quote system copiedType payload)) =
      some
        (.pair
          (.quote system copiedType payload)
          (.quote system copiedType payload)) := by
  rfl

end Core

end ALSJAS
