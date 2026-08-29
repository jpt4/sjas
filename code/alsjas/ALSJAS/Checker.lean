import ALSJAS.System

/-!
# Native proof-certificate checker

The checker consumes only structural S-expressions. It reparses the complete
term tree, validates the complete system identity, checks every modal reference,
and recursively validates every quoted proof payload. No Lean tactic or theorem
search participates in the decision.
-/

namespace ALSJAS.Checker

open ALSJAS

/-- Public, structured checker failures. -/
inductive CheckError where
  | malformedCertificate
  | malformedSystem : System.SealError → CheckError
  | malformedType
  | malformedTerm
  | identityMismatch
  | staleModalReference
  | typeError : TypeError → CheckError
  | conclusionMismatch : Ty → Ty → CheckError
  | openTerm
  | quoteDepthExhausted
deriving BEq, Repr

/-- Successful checking returns the parsed tree, not a boolean or opaque host
token. Later adequacy theorems relate this record to intrinsic derivability. -/
structure Checked where
  system : System.Sealed
  conclusion : Ty
  term : Term
  source : SExpr
deriving BEq, Repr

/-- Canonical complete certificate used by the CLI and tests. -/
def certificateSExpr (system : System.Sealed) (conclusion : Ty)
    (term : Term) : SExpr :=
  .list [.atom "proof", system.source, conclusion.toSExpr, term.toSExpr]

private structure Parsed where
  system : System.Sealed
  conclusion : Ty
  term : Term

private def parseCertificate : SExpr → Except CheckError Parsed
  | .list [.atom "proof", systemSource, conclusionSource, termSource] => do
      let system ←
        (System.sealIdentity systemSource).mapError CheckError.malformedSystem
      let conclusion ←
        match Ty.fromSExpr conclusionSource with
        | some value => .ok value
        | none => .error .malformedType
      let term ←
        match Term.fromSExpr termSource with
        | some value => .ok value
        | none => .error .malformedTerm
      pure { system, conclusion, term }
  | _ => .error .malformedCertificate

private def referenceMatches (system : System.Sealed) : SystemRef → Bool
  | .self => false
  | .sealed identity => identity == system.source

/-- Check every system reference inside a type annotation. -/
private def typeReferencesMatch (system : System.Sealed) : Ty → Bool
  | .zero | .one | .atom _ => true
  | .tensor left right | .lolli left right =>
      typeReferencesMatch system left && typeReferencesMatch system right
  | .box reference boxedType =>
      referenceMatches system reference && typeReferencesMatch system boxedType
  | .godel reference | .clash1 reference => referenceMatches system reference

/-- Check references throughout raw term syntax without entering serialized
quoted proof payloads; each payload is independently parsed by `checkFuel`. -/
private def termReferencesMatch (system : System.Sealed) : Term → Bool
  | .var _ | .triv => true
  | .pair left right | .letTensor left right | .app left right |
      .boxComp left right =>
      termReferencesMatch system left && termReferencesMatch system right
  | .lam domain body =>
      typeReferencesMatch system domain && termReferencesMatch system body
  | .abort resultType contradiction =>
      typeReferencesMatch system resultType &&
        termReferencesMatch system contradiction
  | .quote reference conclusion _ =>
      referenceMatches system reference && typeReferencesMatch system conclusion
  | .boxIntrosp proof => termReferencesMatch system proof
  | .godelFold reference proof | .godelUnfold reference proof =>
      referenceMatches system reference && termReferencesMatch system proof
  | .self0 reference | .collapse1 reference => referenceMatches system reference
  | .copyBox reference copiedType =>
      referenceMatches system reference && typeReferencesMatch system copiedType

/-- Recursive checker. Fuel decreases only when crossing a quote boundary;
the initial source length exceeds the nesting depth of every finite tree. -/
def checkFuel : Nat → System.Sealed → SExpr → Except CheckError Checked
  | 0, _, _ => .error .quoteDepthExhausted
  | fuel + 1, expectedSystem, source => do
      let parsed ← parseCertificate source
      unless System.sameIdentity expectedSystem parsed.system do
        throw .identityMismatch
      unless typeReferencesMatch expectedSystem parsed.conclusion do
        throw .staleModalReference
      unless termReferencesMatch expectedSystem parsed.term do
        throw .staleModalReference
      let validateQuote : Typing.QuoteValidator := fun reference conclusion proof =>
        if !referenceMatches expectedSystem reference then
          .error .invalidQuote
        else
          match checkFuel fuel expectedSystem proof with
          | .error _ => .error .invalidQuote
          | .ok checked =>
              if checked.conclusion == conclusion then .ok ()
              else .error .invalidQuote
      let inferred ←
        (Typing.inferWith validateQuote parsed.system.profile [] parsed.term).mapError
          CheckError.typeError
      unless inferred.type == parsed.conclusion do
        throw (.conclusionMismatch parsed.conclusion inferred.type)
      unless inferred.usage == [] do
        throw .openTerm
      pure
        { system := parsed.system
        , conclusion := parsed.conclusion
        , term := parsed.term
        , source
        }

/-- Check a complete certificate against an externally selected exact system. -/
def check (system : System.Sealed) (source : SExpr) :
    Except CheckError Checked :=
  checkFuel (source.render.length + 1) system source

end ALSJAS.Checker
