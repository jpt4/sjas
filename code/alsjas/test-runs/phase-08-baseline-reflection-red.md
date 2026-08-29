# Phase 08 red: baseline reflective artifacts absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [19/23] Running ALSJAS.Reflection.BaselinePrograms
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Reflection/BaselinePrograms.lean
✖ [20/23] Running ALSJASTest.BaselineReflection
error: ALSJASTest/BaselineReflection.lean: bad import 'ALSJAS.Reflection.BaselinePrograms'
✖ [21/23] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.BaselineReflection'
✖ [23/23] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
Some required targets logged failures:
- ALSJAS.Reflection.BaselinePrograms
- ALSJASTest.BaselineReflection
- ALSJASTestMain
- alsjas_tests:exe
error: build failed
```

Interpretation: tests require complete selected fold/unfold, `self0`, collapse,
and derived `self1` certificates, but their builder module is absent. This is
the intended red state.
