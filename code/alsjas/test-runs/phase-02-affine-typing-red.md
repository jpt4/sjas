# Phase 02 red: affine core absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [4/8] Running ALSJAS.Core.Typing
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Core/Typing.lean
✖ [5/8] Running ALSJASTest.Typing
error: ALSJASTest/Typing.lean: bad import 'ALSJAS.Core.Typing'
✖ [6/8] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.Typing'
✖ [8/8] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
Some required targets logged failures:
- ALSJAS.Core.Typing
- ALSJASTest.Typing
- ALSJASTestMain
- alsjas_tests:exe
error: build failed
```

Interpretation: the configured tests exercise affine usage separation and the
modal/control profiles, but `ALSJAS.Core.Typing` does not yet exist. This is
the intended red state before implementing the core syntax and checker.
