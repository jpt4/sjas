# Phase 03 red: structural system identity absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [7/11] Running ALSJAS.System
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/System.lean
✖ [8/11] Running ALSJASTest.System
error: ALSJASTest/System.lean: bad import 'ALSJAS.System'
✖ [9/11] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.System'
✖ [11/11] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
Some required targets logged failures:
- ALSJAS.System
- ALSJASTest.System
- ALSJASTestMain
- alsjas_tests:exe
error: build failed
```

Interpretation: the tests require recursive-binder validation, baseline/copy
identity separation, and regenerated reflective artifacts, but the structural
identity module is absent. This is the intended red state.
