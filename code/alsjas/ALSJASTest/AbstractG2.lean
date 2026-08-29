import ALSJAS.Reflection.AbstractG2

/-!
Type-level probes for the coding-independent computational G2 theorem. No
concrete syntax, number code, checker result, or bounded execution occurs here.
-/

namespace ALSJASTest.AbstractG2

open ALSJAS.Reflection

example (calculus : AbstractG2.Calculus)
    (self0 : calculus.Program
      (calculus.lolli (calculus.box calculus.zero) calculus.zero)) :
    calculus.Program calculus.zero :=
  AbstractG2.contradictionFromSelf0 calculus self0

example (calculus : AbstractG2.Calculus)
    (consistent : AbstractG2.Consistent calculus) :
    calculus.Program
      (calculus.lolli (calculus.box calculus.zero) calculus.zero) → False :=
  AbstractG2.self0Excluded calculus consistent

def run : IO Unit :=
  IO.println "abstract computational G2 probes passed: 2 universal declarations"

end ALSJASTest.AbstractG2
