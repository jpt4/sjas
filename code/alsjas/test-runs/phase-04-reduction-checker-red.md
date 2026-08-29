# Phase 04 red: reduction and native proof checker absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [9/15] Running ALSJAS.Core.Reduction
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Core/Reduction.lean
✖ [10/15] Running ALSJASTest.Reduction
error: ALSJASTest/Reduction.lean: bad import 'ALSJAS.Core.Reduction'
✖ [11/15] Running ALSJAS.Checker
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Checker.lean
✖ [12/15] Running ALSJASTest.Checker
error: ALSJASTest/Checker.lean: bad import 'ALSJAS.Checker'
✖ [13/15] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.Reduction'
error: ALSJASTestMain.lean: bad import 'ALSJASTest.Checker'
✖ [15/15] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
Some required targets logged failures:
- ALSJAS.Core.Reduction
- ALSJASTest.Reduction
- ALSJAS.Checker
- ALSJASTest.Checker
- ALSJASTestMain
- alsjas_tests:exe
error: build failed
```

Interpretation: the configured tests require deterministic beta/tensor
reduction, opaque quotes, complete nested quote checking, and exact identity
controls. Both production modules are intentionally absent in this red state.
