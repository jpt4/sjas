# ADR-0001 — Prolog implementation of Tab / Xtab inductive families

**Status.** Accepted for implementation.
**Date.** 2026-09-05.
**Location.** `code/tabt/`
**Depends on.** Paper construction
[`docs/log/2026-09-05-tab-xtab-dependent-types.md`](../../docs/log/2026-09-05-tab-xtab-dependent-types.md)
(Willard 2020 Appendix six-rule profile + Xtab LEM constructor).

## Context

The dependent-type note defines Willard **Tab** as an inductive family
`Tab_α(Γ)` over branch contexts, and **Xtab** as that family plus a LEM/cut
constructor. The next computational step is a Prolog encoding that treats
proof terms as explicit constructor trees and checks them against those
rules — a direct inhabitant checker, not the arithmeticized
`tableau-proof/3` predicate in Proflog.

## Decision

Implement in `code/tabt/`:

1. **Formula syntax** as Prolog terms (`and/2`, `or/2`, `imp/2`, `neg/1`,
   `all/2`, `ex/2`, `ball/3`, `bex/3`, `leq/2`, `var/1`, `param/1`).
2. **`tab/3`** — `tab(Axioms, Branch, Proof)` succeeds iff `Proof` is a
   well-formed inhabitant of `Tab_α(Branch)`.
3. **`tab_proof/3`** — `tab_proof(Axioms, Psi, Proof)` ≡
   `tab(Axioms, [neg(Psi)], Proof)`.
4. **`xtab/3` / `xtab_proof/3`** — same, with an additional `lem/2`
   constructor writing `or(Mho, neg(Mho))`.
5. **Optional shallow search** that builds small proof terms by iterative
   deepening, for tests and demos only; not claimed complete for FOL.
6. **plunit tests** covering: complementary closure; ∧-split; ∨-branch;
   axiom use; fresh ∃-parameter; Tab failure where only LEM enables Xtab
   closure on a minimal example; rejection of ill-formed proof terms.

Profile: Willard 2020 Appendix (Rules 1–6 + bounded hybrids). Prenex\* root
normalisation is **not** required. Eight-rule 2001 numbering is the same
constructors under different names, not a second engine.

## Success criteria

- `swipl -g run_tests -t halt code/tabt/tabt_test.pl` exits 0.
- Every constructor in the paper note has a corresponding proof-term
  functor checked by `tab/3` or `xtab/3`.
- Ill-typed / ill-justified proof terms fail.
- Comments sufficient for a developer unfamiliar with Willard tableaux.

## Failure / out of scope

- No Gödel encoding / `SemPrf` arithmeticization.
- No claim of completeness of the shallow searcher.
- No modification of Proflog SJAS predicates or Codification registries.

## After Action Report

**2026-09-05.** Implemented `code/tabt/tabt.pl` with plunit suite
`tabt_test.pl` (21 tests). Red–green cycle completed via Docker image
`swipl:stable` (host has no local `swipl`; apt install requires sudo).

All success criteria met: every Tab constructor from the 2020 Appendix
profile is checked; Xtab adds `of_tab/1` and `lem/2`; ill-formed terms and
non-fresh existential parameters are rejected; shallow search finds a Tab
proof of `or(p, neg(p))`. No Proflog or Codification registry changes.
