import ALSJAS.SExpr

/-!
# Core ALSJAS syntax

This file contains data only.  Raw terms are untrusted syntax; in particular,
a `quote` stores a raw S-expression certificate and does not become accepted
evidence until the proof checker validates it under the exact system identity.
-/

namespace ALSJAS

/-- The two matched rule profiles. `withCopy` differs from `baseline` only by
admitting the `copyBox` term constructor during checking. -/
inductive RuleProfile where
  | baseline
  | withCopy
deriving DecidableEq, BEq, Repr

/-- A modal system reference is either the specially scoped recursive marker
or a complete structural identity. A digest is never substituted for the
identity tree. -/
inductive SystemRef where
  | self
  | sealed : SExpr → SystemRef
deriving BEq, Repr

namespace SystemRef

/-- Canonical surface representation of a system reference. -/
def toSExpr : SystemRef → SExpr
  | .self => .atom "self"
  | .sealed identity => .list [.atom "system", identity]

/-- Decode only canonical system-reference forms. Validation of where `self`
may occur belongs to the sealing layer. -/
def fromSExpr : SExpr → Option SystemRef
  | .atom "self" => some .self
  | .list [.atom "system", identity] => some (.sealed identity)
  | _ => none

end SystemRef

/-- Propositions/types of the first ALSJAS core. `godel` names the one selected
fixed point for a system; it is not a general recursive-type constructor.
`clash1` is the native protected-contract conflict package. -/
inductive Ty where
  | zero
  | one
  | tensor : Ty → Ty → Ty
  | lolli : Ty → Ty → Ty
  | box : SystemRef → Ty → Ty
  | godel : SystemRef → Ty
  | clash1 : SystemRef → Ty
  | atom : String → Ty
deriving BEq, Repr

namespace Ty

/-- Canonical S-expression form used by proofs, identities, and the CLI. -/
def toSExpr : Ty → SExpr
  | .zero => .atom "0"
  | .one => .atom "1"
  | .tensor left right => .list [.atom "tensor", left.toSExpr, right.toSExpr]
  | .lolli domain codomain =>
      .list [.atom "lolli", domain.toSExpr, codomain.toSExpr]
  | .box system proposition =>
      .list [.atom "box", system.toSExpr, proposition.toSExpr]
  | .godel system => .list [.atom "godel", system.toSExpr]
  | .clash1 system => .list [.atom "clash1", system.toSExpr]
  | .atom name => .list [.atom "atom", .atom name]

/-- Decode the canonical type grammar. Ill-formed arities and unknown heads
are rejected with `none`; they are not reinterpreted as atomic types. -/
def fromSExpr : SExpr → Option Ty
  | .atom "0" => some .zero
  | .atom "1" => some .one
  | .list [.atom "tensor", left, right] => do
      let leftType ← fromSExpr left
      let rightType ← fromSExpr right
      pure (.tensor leftType rightType)
  | .list [.atom "lolli", domain, codomain] => do
      let domainType ← fromSExpr domain
      let codomainType ← fromSExpr codomain
      pure (.lolli domainType codomainType)
  | .list [.atom "box", system, proposition] => do
      let systemRef ← SystemRef.fromSExpr system
      let propositionType ← fromSExpr proposition
      pure (.box systemRef propositionType)
  | .list [.atom "godel", system] => do
      pure (.godel (← SystemRef.fromSExpr system))
  | .list [.atom "clash1", system] => do
      pure (.clash1 (← SystemRef.fromSExpr system))
  | .list [.atom "atom", .atom name] => some (.atom name)
  | _ => none

end Ty

/-- Raw proof-program syntax. De Bruijn variables make scoping executable and
remove alpha-equivalence from the trusted checker. The payload of `quote` is
untrusted serialized proof syntax and is deliberately opaque here. -/
inductive Term where
  | var : Nat → Term
  | triv
  | pair : Term → Term → Term
  | letTensor : Term → Term → Term
  | lam : Ty → Term → Term
  | app : Term → Term → Term
  | abort : Ty → Term → Term
  | quote : SystemRef → Ty → SExpr → Term
  | boxComp : Term → Term → Term
  | boxIntrosp : Term → Term
  | godelFold : SystemRef → Term → Term
  | godelUnfold : SystemRef → Term → Term
  | self0 : SystemRef → Term
  | collapse1 : SystemRef → Term
  | copyBox : SystemRef → Ty → Term
deriving BEq, Repr

end ALSJAS
