# Phase 02 green: affine core typing

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [8/14] Built ALSJAS.Core.Typing (1.3s)
✔ [9/14] Built ALSJASTest.Typing (1.2s)
✔ [10/14] Built ALSJAS.Core.Typing:c.o (2.4s)
✔ [11/14] Built ALSJASTest.Typing:c.o (1.2s)
✔ [12/14] Built ALSJASTestMain (1.3s)
✔ [13/14] Built ALSJASTestMain:c.o (163ms)
✔ [14/14] Built alsjas_tests:exe (3.4s)
Build completed successfully (14 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
```

The 20 typing cases cover canonical type serialization, one-shot variable use,
weakening, lambda/tensor duplication rejection, affine application, mismatch
and scope errors, native `self0`/`collapse1` types, boxed composition and
system separation, positive introspection, both selected fixed-point
directions, deferred proof checking for raw quotation, baseline rejection of
`copy-box`, and admission of that exact capability by the matched profile.
