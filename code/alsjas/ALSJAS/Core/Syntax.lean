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

namespace Term

/-- Canonical serialization of raw terms. Proof payloads remain complete
S-expressions and are inserted directly rather than replaced by references. -/
def toSExpr : Term → SExpr
  | .var index => .list [.atom "var", .atom (toString index)]
  | .triv => .list [.atom "triv"]
  | .pair left right => .list [.atom "pair", left.toSExpr, right.toSExpr]
  | .letTensor scrutinee body =>
      .list [.atom "let-tensor", scrutinee.toSExpr, body.toSExpr]
  | .lam domain body => .list [.atom "lam", domain.toSExpr, body.toSExpr]
  | .app function argument =>
      .list [.atom "app", function.toSExpr, argument.toSExpr]
  | .abort resultType contradiction =>
      .list [.atom "abort", resultType.toSExpr, contradiction.toSExpr]
  | .quote system conclusion proof =>
      .list [.atom "quote", system.toSExpr, conclusion.toSExpr, proof]
  | .boxComp function argument =>
      .list [.atom "box-comp", function.toSExpr, argument.toSExpr]
  | .boxIntrosp proof => .list [.atom "box-introsp", proof.toSExpr]
  | .godelFold system proof =>
      .list [.atom "godel-fold", system.toSExpr, proof.toSExpr]
  | .godelUnfold system proof =>
      .list [.atom "godel-unfold", system.toSExpr, proof.toSExpr]
  | .self0 system => .list [.atom "self0", system.toSExpr]
  | .collapse1 system => .list [.atom "collapse1", system.toSExpr]
  | .copyBox system copiedType =>
      .list [.atom "copy-box", system.toSExpr, copiedType.toSExpr]

/-- Decode exactly the raw-term grammar. There are no node IDs, references,
or sharing instructions, so a successfully decoded proof program is a tree. -/
def fromSExpr : SExpr → Option Term
  | .list [.atom "var", .atom index] => do
      pure (.var (← index.toNat?))
  | .list [.atom "triv"] => some .triv
  | .list [.atom "pair", left, right] => do
      pure (.pair (← fromSExpr left) (← fromSExpr right))
  | .list [.atom "let-tensor", scrutinee, body] => do
      pure (.letTensor (← fromSExpr scrutinee) (← fromSExpr body))
  | .list [.atom "lam", domain, body] => do
      pure (.lam (← Ty.fromSExpr domain) (← fromSExpr body))
  | .list [.atom "app", function, argument] => do
      pure (.app (← fromSExpr function) (← fromSExpr argument))
  | .list [.atom "abort", resultType, contradiction] => do
      pure (.abort (← Ty.fromSExpr resultType) (← fromSExpr contradiction))
  | .list [.atom "quote", system, conclusion, proof] => do
      pure (.quote (← SystemRef.fromSExpr system) (← Ty.fromSExpr conclusion) proof)
  | .list [.atom "box-comp", function, argument] => do
      pure (.boxComp (← fromSExpr function) (← fromSExpr argument))
  | .list [.atom "box-introsp", proof] => do
      pure (.boxIntrosp (← fromSExpr proof))
  | .list [.atom "godel-fold", system, proof] => do
      pure (.godelFold (← SystemRef.fromSExpr system) (← fromSExpr proof))
  | .list [.atom "godel-unfold", system, proof] => do
      pure (.godelUnfold (← SystemRef.fromSExpr system) (← fromSExpr proof))
  | .list [.atom "self0", system] => do
      pure (.self0 (← SystemRef.fromSExpr system))
  | .list [.atom "collapse1", system] => do
      pure (.collapse1 (← SystemRef.fromSExpr system))
  | .list [.atom "copy-box", system, copiedType] => do
      pure (.copyBox (← SystemRef.fromSExpr system) (← Ty.fromSExpr copiedType))
  | _ => none

end Term

end ALSJAS
