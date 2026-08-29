# Phase 05 green: universal core metatheory

Date: 2026-08-28

Commands, run from `code/alsjas`:

```text
/home/jpt4/.elan/bin/lake build alsjas_tests
/home/jpt4/.elan/bin/lake exe alsjas_tests
```

Observed exit status: `0`

Observed output:

```text
✔ [24/30] Built ALSJAS.Core.Metatheory (18s)
✔ [25/30] Built ALSJASTest.Metatheory (985ms)
✔ [26/30] Built ALSJASTest.Metatheory:c.o (247ms)
✔ [27/30] Built ALSJASTestMain (1.0s)
✔ [28/30] Built ALSJASTestMain:c.o (260ms)
✔ [29/30] Built ALSJAS.Core.Metatheory:c.o (4.9s)
✔ [30/30] Built alsjas_tests:exe (2.5s)
Build completed successfully (30 jobs).
S-expression tests passed: 19
affine typing tests passed: 20
system identity tests passed: 14
reduction tests passed: 9
proof checker tests passed: 13
core metatheory probes passed: 6 universal declarations
```

The universal declarations prove structural serialization round trips for all
system references, types, and terms; reduction opacity for every quote;
baseline rejection of every `copyBox`; explicit operational duplication for
the control; and deterministic one-step reduction. `ALSJAS.AxiomAudit` prints
the dependencies of these declarations during the default library build.

This slice does not yet prove the surface-string parser round trip, subject
reduction, normalization, canonical forms, or checker adequacy.

The subsequent default build printed this initial axiom report:

```text
SystemRef.from_toSExpr: [propext]
Ty.from_toSExpr: [propext, Quot.sound]
Term.from_toSExpr: [propext, Classical.choice, Quot.sound]
Reduction.step_deterministic: [propext]
Core.quote_is_reduction_normal: [propext]
Core.baseline_rejects_copyBox: [propext]
Core.copyBox_duplicates_value: [propext]
```

These are Lean/standard-library foundational dependencies, not project-local
axiom declarations. The final trust audit must revisit the complete headline
set rather than treating this initial report as final.
