import ALSJAS.Core.Reduction

/-!
Behavioral tests for deterministic affine reduction. Quoted certificate syntax
must remain opaque: core reduction never enters or rewrites its payload.
-/

namespace ALSJASTest.Reduction

open ALSJAS

private def system : SystemRef :=
  .sealed (.list [.atom "mu", .atom "self", .atom "reduction-test"])

private def payload : SExpr :=
  .list [.atom "proof", .atom "payload", .list [.atom "app", .atom "inside"]]

private def cases : List (String × Bool) :=
  [ ("beta identity",
      Reduction.step? (.app (.lam .one (.var 0)) .triv) == some .triv)
  , ("beta weakening",
      Reduction.step? (.app (.lam .one .triv) .triv) == some .triv)
  , ("left-to-right application",
      Reduction.step?
        (.app (.app (.lam .one (.lam .one (.var 0))) .triv) .triv) ==
        some (.app (.lam .one (.var 0)) .triv))
  , ("tensor elimination",
      Reduction.step?
        (.letTensor (.pair .triv .triv) (.pair (.var 1) (.var 0))) ==
        some (.pair .triv .triv))
  , ("pair evaluates left first",
      Reduction.step?
        (.pair (.app (.lam .one (.var 0)) .triv)
          (.app (.lam .one (.var 0)) .triv)) ==
        some (.pair .triv (.app (.lam .one (.var 0)) .triv)))
  , ("godel fold/unfold",
      Reduction.step?
        (.godelUnfold system (.godelFold system
          (.quote system (.lolli (.godel system) .zero) payload))) ==
        some (.quote system (.lolli (.godel system) .zero) payload))
  , ("quote is opaque",
      Reduction.step? (.quote system .one payload) == none)
  , ("quote payload unchanged by normalization",
      Reduction.normalize 20 (.quote system .one payload) ==
        .quote system .one payload)
  , ("normalization reaches beta normal form",
      Reduction.normalize 20
        (.app (.lam .one (.var 0))
          (.app (.lam .one (.var 0)) .triv)) == .triv)
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

example {source left right : Term} :
    Reduction.Step source left → Reduction.Step source right → left = right := by
  exact Reduction.step_deterministic

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"reduction test failed: {name}"
  IO.println s!"reduction tests passed: {cases.length}"

end ALSJASTest.Reduction
