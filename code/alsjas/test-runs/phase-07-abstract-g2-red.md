# Phase 07 red: coding-independent computational G2 absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [15/21] Running ALSJAS.Reflection.AbstractG2
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Reflection/AbstractG2.lean
✖ [16/21] Running ALSJASTest.AbstractG2
error: ALSJASTest/AbstractG2.lean: bad import 'ALSJAS.Reflection.AbstractG2'
✖ [17/21] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.AbstractG2'
✖ [19/21] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
error: build failed
Some required targets logged failures:
- ALSJAS.Reflection.AbstractG2
- ALSJASTest.AbstractG2
- ALSJASTestMain
- alsjas_tests:exe
```

Interpretation: the universal probes require a contradiction constructor and
the resulting exclusion theorem for every reflective proof-program calculus,
but the abstract theorem module is absent. This is the intended red state.
