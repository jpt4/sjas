import ALSJAS.SExpr

/-!
Executable and theorem-level acceptance tests for the shared native syntax
carrier.  These tests intentionally precede `ALSJAS.SExpr`.
-/

namespace ALSJASTest.SExpr

open ALSJAS

private def failed (source : String) : Bool :=
  match SExpr.parse source with
  | .error _ => true
  | .ok _ => false

private def parsesAs (source : String) (expected : SExpr) : Bool :=
  match SExpr.parse source with
  | .ok actual => actual == expected
  | .error _ => false

private def cases : List (String × Bool) :=
  [ ("atom", parsesAs "alpha" (.atom "alpha"))
  , ("empty list", parsesAs "()" (.list []))
  , ("nested list",
      parsesAs "(alpha (beta gamma))"
        (.list [.atom "alpha", .list [.atom "beta", .atom "gamma"]]))
  , ("whitespace",
      parsesAs "  (alpha\n\t beta)  "
        (.list [.atom "alpha", .atom "beta"]))
  , ("quoted atom",
      parsesAs "(label \"two words\")"
        (.list [.atom "label", .atom "two words"]))
  , ("quoted escapes",
      parsesAs "\"line\\nquote\\\"slash\\\\\""
        (.atom "line\nquote\"slash\\"))
  , ("canonical atom render", (SExpr.atom "alpha").render == "alpha")
  , ("quoted atom render", (SExpr.atom "two words").render == "\"two words\"")
  , ("nested render",
      (SExpr.list [.atom "alpha", .list [.atom "beta"]]).render ==
        "(alpha (beta))")
  , ("missing close rejected", failed "(alpha")
  , ("extra close rejected", failed "alpha)")
  , ("trailing expression rejected", failed "alpha beta")
  , ("bad escape rejected", failed "\"bad\\q\"")
  , ("unterminated quote rejected", failed "\"alpha")
  ]

private def roundTrips : List SExpr :=
  [ .atom "alpha"
  , .atom "two words"
  , .atom "line\nquote\"slash\\"
  , .list []
  , .list [.atom "alpha", .list [.atom "beta", .atom "two words"]]
  ]

example : parsesAs "alpha" (.atom "alpha") = true := by
  native_decide

example : parsesAs "(alpha (beta gamma))"
    (.list [.atom "alpha", .list [.atom "beta", .atom "gamma"]]) = true := by
  native_decide

example : roundTrips.all (fun expression =>
    parsesAs expression.render expression) = true := by
  native_decide

def run : IO Unit := do
  for (name, passed) in cases do
    unless passed do
      throw <| IO.userError s!"S-expression test failed: {name}"
  for expression in roundTrips do
    unless parsesAs expression.render expression do
      throw <| IO.userError s!"S-expression round trip failed: {expression.render}"
  IO.println s!"S-expression tests passed: {cases.length + roundTrips.length}"

end ALSJASTest.SExpr
