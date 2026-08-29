# ALSJAS Theorem Dependency Graph

Status: Normative completion map for ADR-0002

Date: 2026-08-28

## Headline graph

```text
sexpr_roundtrip
  -> surface_roundtrip

affinity_decidable -> typing_decidable -> checker_adequate
                           |                    |
deterministic_step --------+                    |
subject_reduction ------------------------------+
normalization -> canonical_forms -> base_consistent

tree_payload_preserved -> no_hidden_sharing
resource_model_sound  -> no_polymorphic_copy_box

system_seal_sound -> stale_identity_rejected
                  -> baseline_copy_identity_distinct

godel_fixed_point
necessitation
boxed_composition
positive_introspection
box_contraction_from_copy
  -> abstract_computational_g2
  -> copy_boom_typed

proper_subtree_well_founded
normalization
tree_payload_preserved
  -> self0_elimination
  -> self0_preservation
base_consistent + self0_preservation
  -> alsjas_consistent
  -> self0_is_genuine_native_consistency

clash_collapse + self0_checked
  -> self1_checked

copy_boom_typed + baseline_copy_rejected
no_polymorphic_copy_box
baseline_copy_identity_distinct
  -> matched_copy_control_complete

machine_step_decidable
  -> trace_checker_sound
  -> trace_checker_complete
tm_translation_step_iff
  -> tm_finite_run_iff
  -> partial_computations_representable
```

## Architectural dependency prohibition

The following declarations must have an import closure disjoint from
`ALSJAS.Machine` and from any arithmetic program-code library:

- `abstract_computational_g2`;
- `self0_preservation`;
- `alsjas_consistent`;
- `no_polymorphic_copy_box`; and
- `copy_boom_typed`.

Natural numbers may occur as Lean's ordinary structural indexes for lists or
de Bruijn variables. They may not serve as encodings of formulas, systems,
proofs, or programs on any path to these theorems.

## Evidence required per declaration

For each headline theorem, completion requires all four records:

1. the exact Lean declaration and source path;
2. a successful pinned `lake build`;
3. `#print axioms` output; and
4. the acceptance-matrix test or explanation of why the result is purely
   propositional and has no executable control.

An entry in documentation without these records is a target, not evidence.
