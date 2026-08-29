# ALSJAS Acceptance Matrix

Status: Suspended implementation checklist; paper-first gates active under
ADR-0003

Date: 2026-08-28

Paper-first revision: 2026-08-29

No row is complete merely because its test name exists. Evidence columns must
name a checked theorem, executable test, or retained red/green log.

Rows completed before ADR-0003 are prototype evidence only until P00–P03 pass.
They establish behavior of the exploratory implementation, not conformance to
an independently specified calculus.

| ID | Requirement | Red test or probe | Green evidence | Status |
| --- | --- | --- | --- | --- |
| P00 | Theory paper accepted | Review questions in paper Section 19 | [`docs/theory/alsjas-calculus.md`](../theory/alsjas-calculus.md) draft | Review pending |
| P01 | Minimal grammar and `B/J/C` profiles accepted | Existing design has one mixed constructor list | Paper Sections 3–7 | Review pending |
| P02 | Natural-language metatheory accepted | Existing theorem names lack anchoring proofs | Paper Sections 9–14 | Review pending |
| P03 | Prototype conformance audited | Rule-by-rule paper/code comparison not run | Classification of every ALSJAS module | Blocked by P00 |
| C01 | Canonical S-expression round trip | `test-runs/phase-01-sexpr-red.md` | 19 executable cases green; universal `sexpr_roundtrip` theorem pending | In progress |
| C02 | Decidable affine typing | `test-runs/phase-02-affine-typing-red.md` | total `Typing.infer`; 20 executable cases including contraction negatives | Prototype green; P03 pending |
| C03 | Deterministic reduction | `test-runs/phase-04-reduction-checker-red.md` | `Reduction.step_deterministic` and 9 executable cases | Prototype green; P03 pending |
| C04 | Subject reduction | Typed step loses its type | `subject_reduction` | Pending |
| C05 | Normalization | Well-typed core term lacks a normal form | `normalization` | Pending |
| C06 | Canonical forms/base consistency | Closed normal `zero` accepted | `base_consistent` | Pending |
| C07 | Checker adequacy | `test-runs/phase-04-reduction-checker-red.md` | 13 native checker cases green; inductive soundness/completeness pair pending | In progress |
| C08 | Explicit tree boxes | Sharing instruction submitted in phase 04 | raw grammar rejects references; universal serialization and `quote_is_reduction_normal` preserve complete payloads; typed no-duplication theorem pending | In progress |
| C09 | Structural identity | phase 03 red logs plus stale controls in phase 04 | binder/profile layer and checker-level stale rejection green; `system_seal_sound` pending | In progress |
| R01 | Selected fixed point | `test-runs/phase-08-baseline-reflection-red.md` | standalone checker-accepted fold/unfold artifacts and six-case green transcript; derivation theorem pending | In progress |
| R02 | Necessitation/composition/introspection | Missing native checker paths in phase 04 red | quotation, boxed composition, and positive introspection checker cases green; derivation theorems pending | In progress |
| G01 | Coding-independent G2 | `test-runs/phase-07-abstract-g2-red.md` | `contradictionFromSelf0`, `self0Excluded`, import separation, and axiom audit | Prototype green; affine-interface audit pending |
| S01 | Structural `self0` preservation | Hypothetical self0 contradiction not eliminable | `self0_preservation` | Pending |
| S02 | Concrete consistency | Exact sealed system not instantiated | `alsjas_consistent`; axiom audit | Pending |
| S03 | Native `self0` | `test-runs/phase-08-baseline-reflection-red.md` | exact checked artifact, executable demo, and stale-identity control | Prototype green; P03 pending |
| S04 | Native `self1` | `test-runs/phase-08-baseline-reflection-red.md` | native `collapse1` and derived affine `self1` accepted; stale-identity control green; malformed-clash control pending | In progress |
| X01 | Copy realizes box contraction | Copy extension cannot discharge repeated box use | operational duplication plus abstract box-contraction interface green; named derivation theorem pending | In progress |
| X02 | Copy contradiction | `test-runs/phase-06-copy-control-red.md` | complete nested `boom : 0` certificate accepted; checker-adequacy theorem pending | In progress |
| X03 | Matched baseline rejection | Phase 06 baseline analogue and identity controls | regenerated baseline certificate and copy-identity certificate both rejected; theorem-level rejection pending | In progress |
| X04 | Copy non-definability | Baseline polymorphic copy candidate typechecks | free-model/logical-relation theorem | Pending |
| X05 | Identity regenerated | Baseline and copy identities compare equal | identity-distinction theorem and CLI | Pending |
| T01 | Trace checker sound | Accepted adjacent pair is not a `Step` | `trace_checker_sound` | Pending |
| T02 | Trace checker complete | Valid finite trace rejected | `trace_checker_complete` | Pending |
| T03 | Turing correspondence | Translation fails to preserve/reflect a step | `tm_translation_step_iff` | Pending |
| T04 | Partial computation representation | Standard TM lacks native representation | finite-run correspondence theorem | Pending |
| T05 | No total-run claim | Core imports total universal evaluator | dependency/source audit | Pending |
| Q01 | Forbidden declarations absent | Seeded forbidden fixture not detected | source-audit self-test | Pending |
| Q02 | Axiom dependencies reviewed | Headline theorem omitted from audit | `ALSJAS.AxiomAudit` now covers first seven universal declarations; final headline set pending | In progress |
| Q03 | Reproducible CLI | Pinned clean build or demo suite fails | release script and transcript | Pending |

## Required negative corpus

- malformed lists, strings, and escapes;
- out-of-scope variables and duplicated affine variables;
- type mismatches at application and tensor elimination;
- forged quotes and proof conclusions;
- stale, edited, baseline/copy-swapped system identities;
- DAG references, cycles, aliases, and hidden sharing encodings;
- altered proof-tree children and altered computation-trace steps;
- the copy contradiction submitted to baseline ALSJAS; and
- a nonterminating machine prefix handled only by explicit fuel/partial APIs.

## Test tiers

The default suite contains deterministic unit tests and theorem compilation.
Property enumeration over larger terms, long normalization examples, and
machine traces live in an explicit extended target. Both tiers are required at
the final gate, but the extended tier does not make the edit/test loop opaque.
