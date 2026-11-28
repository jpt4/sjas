/- A runnable demonstration of the same definitions checked by RawChecks. -/
import RawChecks
open RawReflection

private def report (name : String) (term : Term) (expected : Ty) : IO Unit := do
  IO.println name
  IO.println s!"  accepted input: {check term expected}"
  IO.println s!"  result: {repr (run term)}"
  IO.println s!"  syntax nodes: {nodes term} -> {nodes (run term)}"

def main : IO Unit := do
  report "Check the checker's own instruction"
    (.app (.app (.atom .check) (.raw (.atom .check))) (.type checkTy)) .bool
  report "Seal the uniform H1 instruction" sealedH1 (.option (.box h1Ty))
  report "Check, seal, then execute the checker on H1" checkedChecker .bool
  report "Use checked code in an object-language option branch" useSeal .unit
  report "Reject a purported Bot certificate"
    (.app (.atom (.seal .bot)) (.raw (.junk 19))) (.option (.box .bot))
