# Phase 01 red: native S-expression carrier absent

Date: 2026-08-28

Command, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [2/6] Running ALSJAS.SExpr
error: no such file or directory (error code: 4294967294)
  file: /home/jpt4/code/proflog/sjas/code/alsjas/ALSJAS/SExpr.lean
✖ [3/6] Running ALSJASTest.SExpr
error: ALSJASTest/SExpr.lean: bad import 'ALSJAS.SExpr'
✖ [4/6] Running ALSJASTestMain
error: ALSJASTestMain.lean: bad import 'ALSJASTest.SExpr'
✖ [6/6] Running alsjas_tests:exe
error: bad imports (see the 'ALSJASTestMain' job for details)
error: build failed
Some required targets logged failures:
- ALSJAS.SExpr
- ALSJASTest.SExpr
- ALSJASTestMain
- alsjas_tests:exe
```

Interpretation: the behavior-level test module and executable are configured,
but the production module `ALSJAS.SExpr` does not exist. This is the intended
red state before implementing the native syntax carrier.
