import ALSJAS.Checker

/-!
Native proof-checker tests. Every positive quote contains a complete serialized
closed proof. Negative cases alter conclusions, identities, capabilities, or
tree syntax rather than relying on timeouts.
-/

namespace ALSJASTest.Checker

open ALSJAS

private def withSystems
    (test : System.Sealed → System.Sealed → Bool) : Bool :=
  match System.canonical .baseline [], System.canonical .withCopy [] with
  | .ok baseline, .ok copied => test baseline copied
  | _, _ => false

private def accepts (system : System.Sealed) (conclusion : Ty)
    (term : Term) : Bool :=
  match Checker.check system (Checker.certificateSExpr system conclusion term) with
  | .ok checked => checked.conclusion == conclusion
  | .error _ => false

private def rejects (system : System.Sealed) (raw : SExpr) : Bool :=
  match Checker.check system raw with
  | .error _ => true
  | .ok _ => false

private def cases : List (String × Bool) :=
  [ ("closed trivial proof accepted", withSystems fun baseline _ =>
      accepts baseline .one .triv)
  , ("self0 accepted under exact identity", withSystems fun baseline _ =>
      accepts baseline (.lolli (.box baseline.reference .zero) .zero)
        baseline.self0)
  , ("complete quote accepted", withSystems fun baseline _ =>
      let inner := Checker.certificateSExpr baseline .one .triv
      accepts baseline (.box baseline.reference .one)
        (.quote baseline.reference .one inner))
  , ("forged quote conclusion rejected", withSystems fun baseline _ =>
      let forged := Checker.certificateSExpr baseline .zero .triv
      rejects baseline <| Checker.certificateSExpr baseline
        (.box baseline.reference .zero)
        (.quote baseline.reference .zero forged))
  , ("boxed composition accepted", withSystems fun baseline _ =>
      let functionType := Ty.lolli .one .one
      let functionProof := Checker.certificateSExpr baseline functionType
        (.lam .one (.var 0))
      let argumentProof := Checker.certificateSExpr baseline .one .triv
      accepts baseline (.box baseline.reference .one)
        (.boxComp
          (.quote baseline.reference functionType functionProof)
          (.quote baseline.reference .one argumentProof)))
  , ("positive introspection accepted", withSystems fun baseline _ =>
      let inner := Checker.certificateSExpr baseline .one .triv
      accepts baseline (.box baseline.reference (.box baseline.reference .one))
        (.boxIntrosp (.quote baseline.reference .one inner)))
  , ("declared conclusion alteration rejected", withSystems fun baseline _ =>
      rejects baseline (Checker.certificateSExpr baseline .zero .triv))
  , ("malformed certificate rejected", withSystems fun baseline _ =>
      rejects baseline (.atom "not-a-proof"))
  , ("sharing instruction rejected", withSystems fun baseline _ =>
      rejects baseline
        (.list [.atom "proof", baseline.source, .atom "1",
          .list [.atom "share", .atom "0"]]))
  , ("copy certificate rejected by baseline", withSystems fun baseline _ =>
      let copiedType := Ty.lolli (.box baseline.reference .one)
        (.tensor (.box baseline.reference .one) (.box baseline.reference .one))
      rejects baseline <| Checker.certificateSExpr baseline copiedType
        (.copyBox baseline.reference .one))
  , ("copy certificate accepted by control", withSystems fun _ copied =>
      let copiedType := Ty.lolli (.box copied.reference .one)
        (.tensor (.box copied.reference .one) (.box copied.reference .one))
      accepts copied copiedType (.copyBox copied.reference .one))
  , ("copy identity stale for baseline", withSystems fun baseline copied =>
      let copiedType := Ty.lolli (.box copied.reference .one)
        (.tensor (.box copied.reference .one) (.box copied.reference .one))
      rejects baseline <| Checker.certificateSExpr copied copiedType
        (.copyBox copied.reference .one))
  , ("stale modal reference rejected", withSystems fun baseline copied =>
      rejects baseline <| Checker.certificateSExpr baseline
        (.lolli (.box copied.reference .zero) .zero) copied.self0)
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"proof checker test failed: {name}"
  IO.println s!"proof checker tests passed: {cases.length}"

end ALSJASTest.Checker
