import ALSJAS.System

/-!
Red-green tests for finite structural identity. These tests distinguish a
validated recursive binder from arbitrary occurrences of the atom `self` and
require the matched control to regenerate its own identity.
-/

namespace ALSJASTest.System

open ALSJAS

private def getSystem : Except System.SealError System.Sealed →
    Option System.Sealed
  | .ok system => some system
  | .error _ => none

private def baseline : Option System.Sealed :=
  getSystem (System.canonical .baseline [])

private def copied : Option System.Sealed :=
  getSystem (System.canonical .withCopy [])

private def rejectsPlacement (source : SExpr) : Bool :=
  match System.sealIdentity source with
  | .error (.invalidSelfPlacement _) => true
  | _ => false

private def rejectsBinder (source : SExpr) : Bool :=
  match System.sealIdentity source with
  | .error .expectedMuBinder => true
  | _ => false

private def rejectsAny (source : SExpr) : Bool :=
  match System.sealIdentity source with
  | .error _ => true
  | .ok _ => false

private def cases : List (String × Bool) :=
  [ ("canonical baseline seals", baseline.isSome)
  , ("canonical copy seals", copied.isSome)
  , ("baseline profile recovered",
      baseline.any (fun system => system.profile == .baseline))
  , ("copy profile recovered",
      copied.any (fun system => system.profile == .withCopy))
  , ("matched identities differ",
      match baseline, copied with
      | some baseSystem, some copySystem =>
          !System.sameIdentity baseSystem copySystem
      | _, _ => false)
  , ("copy adds exactly one capability",
      System.capabilities .withCopy ==
        System.capabilities .baseline ++ ["copy-box"])
  , ("self accepted in box system position",
      (getSystem <| System.sealIdentity
        (.list [.atom "mu", .atom "self",
          .list [.atom "system",
            .list [.atom "profile", .atom "baseline"],
            .list [.atom "claim", .list [.atom "box", .atom "self", .atom "0"]]]])).isSome)
  , ("self rejected as ordinary data",
      rejectsPlacement
        (.list [.atom "mu", .atom "self",
          .list [.atom "system",
            .list [.atom "profile", .atom "baseline"],
            .list [.atom "claim", .atom "self"]]]))
  , ("self rejected in atom name",
      rejectsPlacement
        (.list [.atom "mu", .atom "self",
          .list [.atom "system",
            .list [.atom "profile", .atom "baseline"],
            .list [.atom "atom", .atom "self"]]]))
  , ("non-reference box system rejected",
      rejectsAny
        (.list [.atom "mu", .atom "self",
          .list [.atom "system",
            .list [.atom "profile", .atom "baseline"],
            .list [.atom "claim",
              .list [.atom "box", .atom "not-a-system", .atom "0"]]]]))
  , ("wrong binder rejected",
      rejectsBinder (.list [.atom "system", .atom "baseline"]))
  , ("self0 regenerated for baseline identity",
      baseline.any (fun system =>
        system.self0 == .self0 system.reference))
  , ("self0 regenerated for copy identity",
      copied.any (fun system =>
        system.self0 == .self0 system.reference))
  , ("regenerated self0 terms differ",
      match baseline, copied with
      | some baseSystem, some copySystem =>
          !(baseSystem.self0 == copySystem.self0)
      | _, _ => false)
  ]

example : cases.all (fun test => test.2) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"system identity test failed: {name}"
  IO.println s!"system identity tests passed: {cases.length}"

end ALSJASTest.System
