import ALSJAS.Core.Typing

/-!
Behavior-level tests for the affine type checker.  They are written before the
core syntax and checker modules and focus on resource separation rather than
the mere presence of constructors.
-/

namespace ALSJASTest.Typing

open ALSJAS

private def base : SystemRef :=
  .sealed (.list [.atom "mu", .atom "self", .atom "baseline"])

private def other : SystemRef :=
  .sealed (.list [.atom "mu", .atom "self", .atom "other"])

private def inferredAs (profile : RuleProfile) (context : List Ty)
    (term : Term) (expected : Ty) (usage : List Bool) : Bool :=
  match Typing.infer profile context term with
  | .ok actual => actual.type == expected && actual.usage == usage
  | .error _ => false

private def rejectedAs (profile : RuleProfile) (context : List Ty)
    (term : Term) (expected : TypeError → Bool) : Bool :=
  match Typing.infer profile context term with
  | .error error => expected error
  | .ok _ => false

private def duplicated : TypeError → Bool
  | .resourceDuplicated _ => true
  | _ => false

private def copyForbidden : TypeError → Bool
  | .capabilityForbidden "copy-box" => true
  | _ => false

private def quoteDeferred : TypeError → Bool
  | .quoteRequiresProofCheck => true
  | _ => false

private def mismatch : TypeError → Bool
  | .typeMismatch _ _ => true
  | _ => false

private def outOfScope : TypeError → Bool
  | .variableOutOfScope _ => true
  | _ => false

private def cases : List (String × Bool) :=
  [ ("type S-expression round trip",
      Ty.fromSExpr
        (Ty.toSExpr (.lolli (.box base (.atom "A"))
          (.tensor .one (.godel base)))) ==
        some (.lolli (.box base (.atom "A"))
          (.tensor .one (.godel base))))
  , ("variable consumes once",
      inferredAs .baseline [.one] (.var 0) .one [true])
  , ("weakening is allowed",
      inferredAs .baseline [] (.lam .one .triv) (.lolli .one .one) [])
  , ("identity is affine",
      inferredAs .baseline [] (.lam .one (.var 0)) (.lolli .one .one) [])
  , ("tensor separates resources",
      inferredAs .baseline [.one, .one]
        (.pair (.var 0) (.var 1)) (.tensor .one .one) [true, true])
  , ("tensor duplication rejected",
      rejectedAs .baseline [.one] (.pair (.var 0) (.var 0)) duplicated)
  , ("lambda duplication rejected",
      rejectedAs .baseline []
        (.lam .one (.pair (.var 0) (.var 0))) duplicated)
  , ("application separates resources",
      inferredAs .baseline [.one]
        (.app (.lam .one (.var 0)) (.var 0)) .one [true])
  , ("application mismatch rejected",
      rejectedAs .baseline [] (.app (.lam .one (.var 0)) (.lam .one .triv)) mismatch)
  , ("out of scope rejected",
      rejectedAs .baseline [] (.var 0) outOfScope)
  , ("self0 exact type",
      inferredAs .baseline [] (.self0 base)
        (.lolli (.box base .zero) .zero) [])
  , ("collapse1 exact type",
      inferredAs .baseline [] (.collapse1 base)
        (.lolli (.clash1 base) (.box base .zero)) [])
  , ("boxed composition",
      inferredAs .baseline
        [.box base (.lolli (.atom "A") (.atom "B")),
         .box base (.atom "A")]
        (.boxComp (.var 0) (.var 1))
        (.box base (.atom "B")) [true, true])
  , ("boxed system mismatch rejected",
      rejectedAs .baseline
        [.box base (.lolli (.atom "A") (.atom "B")),
         .box other (.atom "A")]
        (.boxComp (.var 0) (.var 1)) mismatch)
  , ("positive introspection",
      inferredAs .baseline [.box base (.atom "A")]
        (.boxIntrosp (.var 0)) (.box base (.box base (.atom "A"))) [true])
  , ("selected fixed point fold",
      inferredAs .baseline
        [.box base (.lolli (.godel base) .zero)]
        (.godelFold base (.var 0)) (.godel base) [true])
  , ("selected fixed point unfold",
      inferredAs .baseline [.godel base]
        (.godelUnfold base (.var 0))
        (.box base (.lolli (.godel base) .zero)) [true])
  , ("quote deferred to proof checker",
      rejectedAs .baseline []
        (.quote base .one (.list [.atom "proof", .atom "triv"])) quoteDeferred)
  , ("copy forbidden in baseline",
      rejectedAs .baseline [] (.copyBox base (.atom "A")) copyForbidden)
  , ("copy admitted only by control profile",
      inferredAs .withCopy [] (.copyBox base (.atom "A"))
        (.lolli (.box base (.atom "A"))
          (.tensor (.box base (.atom "A")) (.box base (.atom "A")))) [])
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"typing test failed: {name}"
  IO.println s!"affine typing tests passed: {cases.length}"

end ALSJASTest.Typing
