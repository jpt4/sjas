# ALSJAS Acceptance Matrix

Status: Active checklist for ADR-0002

Date: 2026-08-28

No row is complete merely because its test name exists. Evidence columns must
name a checked theorem, executable test, or retained red/green log.

| ID | Requirement | Red test or probe | Green evidence | Status |
| --- | --- | --- | --- | --- |
| C01 | Canonical S-expression round trip | Parser module absent or malformed input accepted | `sexpr_roundtrip`; parser tests | Pending |
| C02 | Decidable affine typing | Duplicated variable accepted | `typing_decidable`; affinity negatives | Pending |
| C03 | Deterministic reduction | One term steps to unequal results | `step_deterministic` | Pending |
| C04 | Subject reduction | Typed step loses its type | `subject_reduction` | Pending |
| C05 | Normalization | Well-typed core term lacks a normal form | `normalization` | Pending |
| C06 | Canonical forms/base consistency | Closed normal `zero` accepted | `base_consistent` | Pending |
| C07 | Checker adequacy | Forged tree accepted or real derivation unencodable | soundness and completeness pair | Pending |
| C08 | Explicit tree boxes | Shared/back-reference certificate accepted | `tree_payload_preserved`; negative parser tests | Pending |
| C09 | Structural identity | Stale/edited identity accepted | `system_seal_sound`; CLI rejection | Pending |
| R01 | Selected fixed point | Fold/unfold artifact rejected | fixed-point derivations and demo | Pending |
| R02 | Necessitation/composition/introspection | Any required Loeb operation missing | typed derivations and checker tests | Pending |
| G01 | Coding-independent G2 | Abstract assumptions fail to produce formalized G2 | `abstract_computational_g2`; import audit | Pending |
| S01 | Structural `self0` preservation | Hypothetical self0 contradiction not eliminable | `self0_preservation` | Pending |
| S02 | Concrete consistency | Exact sealed system not instantiated | `alsjas_consistent`; axiom audit | Pending |
| S03 | Native `self0` | Stale or forged self0 accepted | checked artifact and demo | Pending |
| S04 | Native `self1` | Malformed clash accepted | `collapse`, `self1`; positive/negative demos | Pending |
| X01 | Copy realizes box contraction | Copy extension cannot discharge repeated box use | `box_contraction_from_copy` | Pending |
| X02 | Copy contradiction | Copy system plus self0 has no explicit contradiction | `copy_boom_typed`; accepted demo | Pending |
| X03 | Matched baseline rejection | Copy boom accepted by baseline | same bytes rejected at copy node | Pending |
| X04 | Copy non-definability | Baseline polymorphic copy candidate typechecks | free-model/logical-relation theorem | Pending |
| X05 | Identity regenerated | Baseline and copy identities compare equal | identity-distinction theorem and CLI | Pending |
| T01 | Trace checker sound | Accepted adjacent pair is not a `Step` | `trace_checker_sound` | Pending |
| T02 | Trace checker complete | Valid finite trace rejected | `trace_checker_complete` | Pending |
| T03 | Turing correspondence | Translation fails to preserve/reflect a step | `tm_translation_step_iff` | Pending |
| T04 | Partial computation representation | Standard TM lacks native representation | finite-run correspondence theorem | Pending |
| T05 | No total-run claim | Core imports total universal evaluator | dependency/source audit | Pending |
| Q01 | Forbidden declarations absent | Seeded forbidden fixture not detected | source-audit self-test | Pending |
| Q02 | Axiom dependencies reviewed | Headline theorem omitted from audit | build-time `#print axioms` manifest | Pending |
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
