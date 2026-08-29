# Phase 05 red: universal core metatheory absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [13/17] Running ALSJAS.Core.Metatheory
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/Core/Metatheory.lean
✖ [14/17] Running ALSJASTest.Metatheory
error: ALSJASTest/Metatheory.lean: bad import 'ALSJAS.Core.Metatheory'
✖ [15/17] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.Metatheory'
✖ [17/17] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
error: build failed
Some required targets logged failures:
- ALSJAS.Core.Metatheory
- ALSJASTest.Metatheory
- ALSJASTestMain
- alsjas_tests:exe
```

Interpretation: six universally quantified theorem probes are configured, but
their metatheory module is absent. This is the intended red state.
