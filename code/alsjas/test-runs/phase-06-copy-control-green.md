# Phase 06 green: matched copy contradiction

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [28/34] Built ALSJAS.Reflection.Programs (820ms)
✔ [29/34] Built ALSJAS.Reflection.Programs:c.o (471ms)
✔ [30/34] Built ALSJASTest.CopyControl (1.4s)
✔ [31/34] Built ALSJASTest.CopyControl:c.o (530ms)
✔ [32/34] Built ALSJASTestMain (941ms)
✔ [33/34] Built ALSJASTestMain:c.o (187ms)
✔ [34/34] Built alsjas_tests:exe (2.7s)
Build completed successfully (34 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
reduction tests passed: 9
proof checker tests passed: 13
core metatheory probes passed: 6 universal declarations
matched copy control tests passed: 6
```

The six controls check the complete nested certificates for `G -> box 0`,
formalized G2, and `boom : 0`; reject the structurally regenerated baseline
analogue; reject the copy identity under the baseline expected identity; and
confirm that the executable boom tree contains the `copyBox` constructor.

These checker results are the required executable witness. Checker adequacy
and the baseline semantic non-definability theorem remain independent gates.
