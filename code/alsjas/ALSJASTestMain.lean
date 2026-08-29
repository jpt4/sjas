import ALSJASTest.SExpr
import ALSJASTest.Typing
import ALSJASTest.System
import ALSJASTest.Reduction
import ALSJASTest.Checker

def main : IO Unit := do
  ALSJASTest.SExpr.run
  ALSJASTest.Typing.run
  ALSJASTest.System.run
  ALSJASTest.Reduction.run
  ALSJASTest.Checker.run
