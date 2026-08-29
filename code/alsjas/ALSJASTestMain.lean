import ALSJASTest.SExpr
import ALSJASTest.Typing
import ALSJASTest.System
import ALSJASTest.Reduction
import ALSJASTest.Checker
import ALSJASTest.Metatheory
import ALSJASTest.CopyControl
import ALSJASTest.AbstractG2

def main : IO Unit := do
  ALSJASTest.SExpr.run
  ALSJASTest.Typing.run
  ALSJASTest.System.run
  ALSJASTest.Reduction.run
  ALSJASTest.Checker.run
  ALSJASTest.Metatheory.run
  ALSJASTest.CopyControl.run
  ALSJASTest.AbstractG2.run
