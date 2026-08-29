# ALSJAS Acceptance Matrix

Status: Active checklist for ADR-0002

Date: 2026-08-28

No row is complete merely because its test name exists. Evidence columns must
name a checked theorem, executable test, or retained red/green log.

| ID | Requirement | Red test or probe | Green evidence | Status |
| --- | --- | --- | --- | --- |
| C01 | Canonical S-expression round trip | `test-runs/phase-01-sexpr-red.md` | 19 executable cases green; universal `sexpr_roundtrip` theorem pending | In progress |
| C02 | Decidable affine typing | `test-runs/phase-02-affine-typing-red.md` | total `Typing.infer`; 20 executable cases including contraction negatives | Complete |
| C03 | Deterministic reduction | `test-runs/phase-04-reduction-checker-red.md` | `Reduction.step_deterministic` and 9 executable cases | Complete |
| C04 | Subject reduction | Typed step loses its type | `subject_reduction` | Pending |
| C05 | Normalization | Well-typed core term lacks a normal form | `normalization` | Pending |
| C06 | Canonical forms/base consistency | Closed normal `zero` accepted | `base_consistent` | Pending |
| C07 | Checker adequacy | `test-runs/phase-04-reduction-checker-red.md` | 13 native checker cases green; inductive soundness/completeness pair pending | In progress |
| C08 | Explicit tree boxes | Sharing instruction submitted in phase 04 | raw grammar rejects references; universal serialization and `quote_is_reduction_normal` preserve complete payloads; typed no-duplication theorem pending | In progress |
| C09 | Structural identity | phase 03 red logs plus stale controls in phase 04 | binder/profile layer and checker-level stale rejection green; `system_seal_sound` pending | In progress |
| R01 | Selected fixed point | Fold/unfold artifact rejected | concrete fold/unfold drives checker-accepted G2/boom programs; standalone derivation theorem/demo pending | In progress |
| R02 | Necessitation/composition/introspection | Missing native checker paths in phase 04 red | quotation, boxed composition, and positive introspection checker cases green; derivation theorems pending | In progress |
| G01 | Coding-independent G2 | `test-runs/phase-07-abstract-g2-red.md` | `contradictionFromSelf0`, `self0Excluded`, import separation, and axiom audit | Complete |
| S01 | Structural `self0` preservation | Hypothetical self0 contradiction not eliminable | `self0_preservation` | Pending |
| S02 | Concrete consistency | Exact sealed system not instantiated | `alsjas_consistent`; axiom audit | Pending |
| S03 | Native `self0` | Stale or forged self0 accepted | checked artifact and demo | Pending |
| S04 | Native `self1` | Malformed clash accepted | `collapse`, `self1`; positive/negative demos | Pending |
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
