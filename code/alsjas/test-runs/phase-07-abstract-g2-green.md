# Phase 07 green: coding-independent computational G2

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [32/38] Built ALSJAS.Reflection.AbstractG2 (1.7s)
✔ [34/38] Built ALSJASTest.AbstractG2 (576ms)
✔ [36/38] Built ALSJASTestMain (1.7s)
Build completed successfully (38 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
reduction tests passed: 9
proof checker tests passed: 13
core metatheory probes passed: 6 universal declarations
matched copy control tests passed: 6
abstract computational G2 probes passed: 2 universal declarations
```

`AbstractG2.contradictionFromSelf0` is a universe-polymorphic, coding-free
proof program. It constructs `Program zero` from a native `self0` for any
calculus providing the selected fixed point, lambda/application, tensor
elimination, necessitation, boxed composition, positive introspection, and
box-restricted contraction. `AbstractG2.self0Excluded` then proves that
external consistency excludes such a `self0`. Neither declaration imports
ALSJAS syntax, the native checker, machine code, or numeric program codes.
