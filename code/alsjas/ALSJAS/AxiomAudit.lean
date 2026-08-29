import ALSJAS.Core.Metatheory
import ALSJAS.Reflection.AbstractG2

/-!
Build-time trust report. Every headline theorem is added here when introduced;
`lake build` therefore asks Lean to print the axioms on which it depends.
-/

#print axioms ALSJAS.SystemRef.from_toSExpr
#print axioms ALSJAS.Ty.from_toSExpr
#print axioms ALSJAS.Term.from_toSExpr
#print axioms ALSJAS.Reduction.step_deterministic
#print axioms ALSJAS.Core.quote_is_reduction_normal
#print axioms ALSJAS.Core.baseline_rejects_copyBox
#print axioms ALSJAS.Core.copyBox_duplicates_value
#print axioms ALSJAS.Reflection.AbstractG2.contradictionFromSelf0
#print axioms ALSJAS.Reflection.AbstractG2.self0Excluded
