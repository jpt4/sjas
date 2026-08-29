# Phase 03 green: structural system identity

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [12/18] Built ALSJAS.System (1.4s)
✔ [13/18] Built ALSJASTest.System (1.1s)
✔ [14/18] Built ALSJASTest.System:c.o (679ms)
✔ [15/18] Built ALSJASTestMain (962ms)
✔ [17/18] Built ALSJAS.System:c.o (2.0s)
✔ [18/18] Built alsjas_tests:exe (1.1s)
Build completed successfully (18 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
```

The 14 identity cases validate both canonical profiles, recover their declared
profiles, distinguish their complete identity trees, prove executably that the
control appends only `copy-box`, accept `self` in a modal system position,
reject it as ordinary data, reject malformed modal system references, reject
an absent binder, and regenerate distinct `self0` terms for the two exact
identities.
