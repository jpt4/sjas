# tabt — Willard Tab / Xtab in Prolog

Prolog inhabitant checker for the inductive families defined in
[`docs/log/2026-09-05-tab-xtab-dependent-types.md`](../../docs/log/2026-09-05-tab-xtab-dependent-types.md).

See [ADR-0001](ADR-0001-tab-xtab-prolog.md).

## Run tests

Requires SWI-Prolog (`swipl`). From this directory:

```bash
swipl -q -g 'consult(tabt_test), run_tests, halt(0)' -t 'halt(1)' tabt_test.pl
```

Or via Docker if `swipl` is not installed locally:

```bash
docker run --rm -v "$PWD":/src -w /src swipl:stable \
  swipl -q -g 'consult(tabt_test), run_tests, halt(0)' -t 'halt(1)' tabt_test.pl
```

## API

| Predicate | Meaning |
| --- | --- |
| `tab(+Axioms, +Branch, +Proof)` | `Proof` inhabits `Tab_α(Branch)` |
| `tab_proof(+Axioms, +Psi, +Proof)` | Tab-proof of `Psi` |
| `xtab(+Axioms, +Branch, +Proof)` | `Proof` inhabits `Xtab_α(Branch)` |
| `xtab_proof(+Axioms, +Psi, +Proof)` | Xtab-proof of `Psi` |
| `tab_search/3`, `xtab_search/3` | shallow iterative-deepening builders |

Proof terms are constructor trees (`close/1`, `and1/1`, `or/2`, `lem/2`, …)
matching the paper note.
