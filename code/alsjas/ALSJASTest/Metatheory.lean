import ALSJAS.Core.Metatheory

/-!
Compile-time probes for universal core theorems. These are not finite runtime
samples: each example quantifies over the complete corresponding syntax type.
-/

namespace ALSJASTest.Metatheory

open ALSJAS

example (system : SystemRef) :
    SystemRef.fromSExpr system.toSExpr = some system := by
  exact SystemRef.from_toSExpr system

example (proposition : Ty) :
    Ty.fromSExpr proposition.toSExpr = some proposition := by
  exact Ty.from_toSExpr proposition

example (term : Term) :
    Term.fromSExpr term.toSExpr = some term := by
  exact Term.from_toSExpr term

example (system : SystemRef) (conclusion : Ty) (payload : SExpr) :
    Reduction.step? (.quote system conclusion payload) = none := by
  exact Core.quote_is_reduction_normal system conclusion payload

example (system : SystemRef) (copiedType : Ty) :
    Typing.infer .baseline [] (.copyBox system copiedType) =
      .error (.capabilityForbidden "copy-box") := by
  exact Core.baseline_rejects_copyBox system copiedType

example (system : SystemRef) (copiedType : Ty) :
    Reduction.step?
      (.app (.copyBox system copiedType)
        (.quote system copiedType (.atom "payload"))) =
      some
        (.pair
          (.quote system copiedType (.atom "payload"))
          (.quote system copiedType (.atom "payload"))) := by
  exact Core.copyBox_duplicates_value system copiedType (.atom "payload")

def run : IO Unit :=
  IO.println "core metatheory probes passed: 6 universal declarations"

end ALSJASTest.Metatheory
