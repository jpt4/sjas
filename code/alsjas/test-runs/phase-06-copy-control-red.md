# Phase 06 red: matched copy contradiction absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [15/19] Running ALSJAS.Reflection.Programs
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Reflection/Programs.lean
✖ [16/19] Running ALSJASTest.CopyControl
error: ALSJASTest/CopyControl.lean: bad import 'ALSJAS.Reflection.Programs'
✖ [17/19] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.CopyControl'
✖ [19/19] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
error: build failed
Some required targets logged failures:
- ALSJAS.Reflection.Programs
- ALSJASTest.CopyControl
- ALSJASTestMain
- alsjas_tests:exe
```

Interpretation: end-to-end tests require complete `g -> box 0`, formalized G2,
and `boom : 0` certificates plus matched baseline rejection, but the proof-term
builder is absent. This is the intended red state.
