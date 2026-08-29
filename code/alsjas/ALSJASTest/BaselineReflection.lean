import ALSJAS.Reflection.BaselinePrograms

/-!
Executable baseline reflection artifacts. Their acceptance demonstrates exact
native construction, not consistency; the preservation theorem remains a
separate gate.
-/

namespace ALSJASTest.BaselineReflection

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
  [ ("selected fold accepted", withSystems fun baseline _ =>
      accepts baseline (Reflection.BaselinePrograms.foldCertificate baseline)
        (Reflection.BaselinePrograms.foldType baseline.reference))
  , ("selected unfold accepted", withSystems fun baseline _ =>
      accepts baseline (Reflection.BaselinePrograms.unfoldCertificate baseline)
        (Reflection.BaselinePrograms.unfoldType baseline.reference))
  , ("exact self0 accepted", withSystems fun baseline _ =>
      accepts baseline (Reflection.BaselinePrograms.self0Certificate baseline)
        (Reflection.Programs.self0Type baseline.reference))
  , ("exact collapse accepted", withSystems fun baseline _ =>
      accepts baseline (Reflection.BaselinePrograms.collapseCertificate baseline)
        (Reflection.BaselinePrograms.collapseType baseline.reference))
  , ("derived self1 accepted", withSystems fun baseline _ =>
      accepts baseline (Reflection.BaselinePrograms.self1Certificate baseline)
        (Reflection.BaselinePrograms.self1Type baseline.reference))
  , ("baseline self1 stale for copy identity", withSystems fun _ copied =>
      let baselineSource :=
        match System.canonical .baseline [] with
        | .ok baseline => Reflection.BaselinePrograms.self1Certificate baseline
        | .error _ => .atom "generation-failed"
      rejects copied baselineSource)
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"baseline reflection test failed: {name}"
  IO.println s!"baseline reflection tests passed: {cases.length}"

end ALSJASTest.BaselineReflection
