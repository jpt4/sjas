import ALSJAS.Core.Typing

/-!
# Finite structural system identity

ALSJAS identities are canonical S-expression trees with one restricted
`(mu self ...)` binder.  They are compared structurally; hashes may be useful
for display in a future CLI, but are never the identity accepted by the proof
checker.
-/

namespace ALSJAS.System

open ALSJAS

/-- Failures produced while validating a proposed sealed identity. -/
inductive SealError where
  | expectedMuBinder
  | invalidSelfPlacement : SExpr → SealError
  | invalidSystemReference : SExpr → SealError
  | malformedSystemBody
  | missingProfile
  | duplicateProfile
  | unknownProfile : String → SealError
  | validationFuelExhausted
deriving BEq, Repr

/-- A validated finite source tree together with the rule profile recovered
from that same tree. Public consumers still compare the complete `source`. -/
structure Sealed where
  source : SExpr
  profile : RuleProfile
deriving BEq, Repr

/-- The complete baseline capability list. -/
private def baselineCapabilities : List String :=
  [ "closed-quote"
  , "box-compose"
  , "positive-introspection"
  , "selected-fixed-point"
  , "self0"
  , "collapse1"
  ]

/-- The control is defined by appending one entry, making the matched
difference executable and inspectable. -/
def capabilities : RuleProfile → List String
  | .baseline => baselineCapabilities
  | .withCopy => baselineCapabilities ++ ["copy-box"]

private def profileName : RuleProfile → String
  | .baseline => "baseline"
  | .withCopy => "with-copy"

private def parseProfileName : String → Except SealError RuleProfile
  | "baseline" => .ok .baseline
  | "with-copy" => .ok .withCopy
  | other => .error (.unknownProfile other)

/- Validate all ordinary children with the recursive marker disabled. Fuel
decreases across the list and at every nested node, so malformed trees cannot
make validation partial. -/
mutual
  private def validateNode : Nat → Bool → SExpr → Except SealError Unit
    | 0, _, _ => .error .validationFuelExhausted
    | _ + 1, allowSelf, .atom "self" =>
        if allowSelf then .ok ()
        else .error (.invalidSelfPlacement (.atom "self"))
    | _ + 1, _, .atom _ => .ok ()
    | fuel + 1, _, .list [.atom "box", system, body] => do
        validateSystemReference fuel system
        validateNode fuel false body
    | fuel + 1, _, .list [.atom "godel", system] =>
        validateSystemReference fuel system
    | fuel + 1, _, .list [.atom "clash1", system] =>
        validateSystemReference fuel system
    | fuel + 1, _, .list elements =>
        validateList fuel elements

  private def validateList : Nat → List SExpr → Except SealError Unit
    | 0, [] => .ok ()
    | 0, _ :: _ => .error .validationFuelExhausted
    | _ + 1, [] => .ok ()
    | fuel + 1, head :: tail => do
        validateNode fuel false head
        validateList fuel tail

  private def validateSystemReference : Nat → SExpr → Except SealError Unit
    | 0, _ => .error .validationFuelExhausted
    | _ + 1, .atom "self" => .ok ()
    | fuel + 1, .list [.atom "system", identity] =>
        validateNode fuel false identity
    | _ + 1, other => .error (.invalidSystemReference other)
end

/-- Recover the unique immediate `(profile NAME)` field from the canonical
system body. Profiles hidden in arbitrary nested data are not authoritative. -/
private def profileFields : List SExpr → List String
  | [] => []
  | .list [.atom "profile", .atom name] :: tail => name :: profileFields tail
  | _ :: tail => profileFields tail

private def extractProfile : SExpr → Except SealError RuleProfile
  | .list (.atom "system" :: fields) =>
      match profileFields fields with
      | [] => .error .missingProfile
      | [name] => parseProfileName name
      | _ => .error .duplicateProfile
  | _ => .error .malformedSystemBody

/-- Validate and seal a finite recursive system source. The binder itself is
handled here; every occurrence inside the body is checked by `validateNode`. -/
def sealIdentity (source : SExpr) : Except SealError Sealed :=
  match source with
  | .list [.atom "mu", .atom "self", body] => do
      validateNode (source.render.length + 1) false body
      let profile ← extractProfile body
      pure { source, profile }
  | _ => .error .expectedMuBinder

private def capabilityTree (profile : RuleProfile) : SExpr :=
  .list (.atom "capabilities" :: (capabilities profile).map SExpr.atom)

/-- The normative source generator. All references to the recursive identity
occur in a system argument position checked by `seal`. -/
def canonicalSource (profile : RuleProfile) (contracts : List SExpr) : SExpr :=
  .list
    [ .atom "mu"
    , .atom "self"
    , .list
        [ .atom "system"
        , .list [.atom "format", .atom "alsjas-1"]
        , .list [.atom "checker", .atom "alsjas-checker-1"]
        , .list [.atom "profile", .atom (profileName profile)]
        , capabilityTree profile
        , .list
            [ .atom "fixed-point"
            , .list [.atom "godel", .atom "self"]
            , .list
                [ .atom "box"
                , .atom "self"
                , .list
                    [ .atom "lolli"
                    , .list [.atom "godel", .atom "self"]
                    , .atom "0"
                    ]
                ]
            ]
        , .list
            [ .atom "self0"
            , .list
                [ .atom "lolli"
                , .list [.atom "box", .atom "self", .atom "0"]
                , .atom "0"
                ]
            ]
        , .list
            [ .atom "collapse1"
            , .list
                [ .atom "lolli"
                , .list [.atom "clash1", .atom "self"]
                , .list [.atom "box", .atom "self", .atom "0"]
                ]
            ]
        , .list (.atom "contracts" :: contracts)
        ]
    ]

/-- Generate and validate a canonical identity. Returning `Except` keeps the
validation step visible even for sources produced by this module. -/
def canonical (profile : RuleProfile) (contracts : List SExpr) :
    Except SealError Sealed :=
  sealIdentity (canonicalSource profile contracts)

namespace Sealed

/-- The modal reference embedded in proof terms and box types. -/
def reference (system : Sealed) : SystemRef :=
  .sealed system.source

/-- Regenerate the distinguished consistency program for this exact identity. -/
def self0 (system : Sealed) : Term :=
  .self0 system.reference

/-- Regenerate the Level-1 collapse program for this exact identity. -/
def collapse1 (system : Sealed) : Term :=
  .collapse1 system.reference

end Sealed

/-- Exact identity comparison. The recovered profile alone is insufficient. -/
def sameIdentity (left right : Sealed) : Bool :=
  left.source == right.source

end ALSJAS.System
