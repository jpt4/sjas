import ALSJAS.SExpr
import ALSJAS.Core.Typing
import ALSJAS.Core.Reduction
import ALSJAS.Core.Metatheory
import ALSJAS.System
import ALSJAS.Checker
import ALSJAS.AxiomAudit
import ALSJAS.Reflection.Programs
import ALSJAS.Reflection.AbstractG2
import ALSJAS.Reflection.BaselinePrograms

/-!
The public root of the ALSJAS library.  Imports are added here only after their
own red-green slice is complete, keeping the default library target honest.
-/
