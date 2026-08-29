# Phase 03b red: malformed modal system reference accepted

Date: 2026-08-28

Command, run from `code/alsjas` after adding the behavioral regression:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
```

Expected exit status: `1`

Observed output:

```text
✖ [14/18] Building ALSJASTest.System (3.7s)
error: ALSJASTest/System.lean:95:2: Tactic `native_decide` evaluated that the proposition
  (cases.all fun test => test.snd) = true
is false
error: Lean exited with code 1
Some required targets logged failures:
- ALSJASTest.System
error: build failed
```

Interpretation: the first validator correctly restricted occurrences of
`self`, but accepted an arbitrary atom such as `not-a-system` in the system
argument of `box`. The new regression requires either the bound `self` marker
or a complete `(system IDENTITY)` reference.
