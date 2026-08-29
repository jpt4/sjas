import ALSJASTest.SExpr
import ALSJASTest.Typing
import ALSJASTest.System

def main : IO Unit := do
  ALSJASTest.SExpr.run
  ALSJASTest.Typing.run
  ALSJASTest.System.run
