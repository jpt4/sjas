import ALSJAS.Reflection.Programs

/-!
End-to-end matched-control tests. The positive case is not a marker constant:
it submits the complete nested proof certificate for `boom` to the same native
checker used by every other proof.
-/

namespace ALSJASTest.CopyControl

open ALSJAS

private def accepts (system : System.Sealed) (source : SExpr)
    (conclusion : Ty) : Bool :=
  match Checker.check system source with
  | .ok checked => checked.conclusion == conclusion
  | .error _ => false

private def rejects (system : System.Sealed) (source : SExpr) : Bool :=
  match Checker.check system source with
  | .error _ => true
  | .ok _ => false

private def withSystems
    (test : System.Sealed → System.Sealed → Bool) : Bool :=
  match System.canonical .baseline [], System.canonical .withCopy [] with
  | .ok baseline, .ok copied => test baseline copied
  | _, _ => false

private def cases : List (String × Bool) :=
  [ ("g-to-box-zero proof accepted", withSystems fun _ copied =>
      accepts copied (Reflection.Programs.gToBoxZeroCertificate copied)
        (Reflection.Programs.gToBoxZeroType copied.reference))
  , ("formalized G2 proof accepted", withSystems fun _ copied =>
      accepts copied (Reflection.Programs.formalizedG2Certificate copied)
        (Reflection.Programs.formalizedG2Type copied.reference))
  , ("explicit copy boom accepted", withSystems fun _ copied =>
      accepts copied (Reflection.Programs.boomCertificate copied) .zero)
  , ("baseline analogue rejected", withSystems fun baseline _ =>
      rejects baseline (Reflection.Programs.boomCertificate baseline))
  , ("copy boom rejected under baseline identity", withSystems fun baseline copied =>
      rejects baseline (Reflection.Programs.boomCertificate copied))
  , ("boom tree contains copy-box", withSystems fun _ copied =>
      Reflection.Programs.containsCopyBox
        (Reflection.Programs.boom copied))
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"matched copy control failed: {name}"
  IO.println s!"matched copy control tests passed: {cases.length}"

end ALSJASTest.CopyControl
