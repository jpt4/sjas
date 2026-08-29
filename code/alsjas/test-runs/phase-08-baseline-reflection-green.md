# Phase 08 green: baseline reflective artifacts accepted

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed status: both commands exited `0`.

Relevant output:

```text
Build completed successfully (42 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
reduction tests passed: 9
proof checker tests passed: 13
core metatheory probes passed: 6 universal declarations
matched copy control tests passed: 6
abstract computational G2 probes passed: 2 universal declarations
baseline reflection tests passed: 6
```

The new suite checks the selected fold and unfold directions, the exact native
`self0`, the native `collapse1`, and the derived affine `self1`. It also checks
that the baseline `self1` certificate is stale under the copy-system identity.
These are executable acceptance results, not consistency evidence.
