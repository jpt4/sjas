# Phase 04 green: deterministic reduction and native proof checker

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [20/26] Built ALSJAS.Core.Reduction (1.2s)
✔ [21/26] Built ALSJAS.Core.Reduction:c.o (1.1s)
✔ [22/26] Built ALSJASTest.Reduction (1.4s)
✔ [23/26] Built ALSJASTest.Reduction:c.o (433ms)
✔ [24/26] Built ALSJASTestMain (999ms)
✔ [25/26] Built ALSJASTestMain:c.o (271ms)
✔ [26/26] Built alsjas_tests:exe (2.8s)
Build completed successfully (26 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
reduction tests passed: 9
proof checker tests passed: 13
```

Reduction cases cover beta contraction, weakening, left-to-right evaluation,
tensor elimination, fixed-point fold/unfold, quote opacity, and fuel-bounded
normalization examples. The theorem `Reduction.step_deterministic` proves the
graph of `step?` is deterministic.

Checker cases cover closed proof acceptance, exact `self0`, complete quotation,
forged quote rejection, boxed composition, positive introspection, altered
conclusions, malformed certificates, sharing syntax rejection, baseline copy
rejection, copy-profile acceptance, copy identity mismatch, and stale modal
references. Checker adequacy and normalization remain theorem gates; these
executable results do not discharge them.
