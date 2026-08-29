# ALSJAS Theorem Dependency Graph

Status: Completion map for ADR-0002 as reordered by ADR-0003

Date: 2026-08-28

Paper-first revision: 2026-08-29

## Headline graph

```text
paper_review_accepted
  -> intrinsic_profiles_B_J_C
intrinsic_profiles_B_J_C
  -> affine_judgment_and_rules
  -> reduction_matches_paper

affine_judgment_and_rules
  -> affine_occurrence
  -> affine_substitution
  -> subject_reduction

reduction_matches_paper + affine_occurrence
  -> baseline_J_normalization
baseline_J_normalization + subject_reduction
  -> canonical_forms
canonical_forms
  -> base_consistent

reduction_matches_paper + affine_substitution
  -> hereditary_height_nonincrease
hereditary_height_nonincrease + canonical_forms
  -> self0_elimination
base_consistent + self0_elimination
  -> alsjas_consistent
  -> self0_is_genuine_native_consistency

sexpr_roundtrip
  -> surface_roundtrip

affinity_decidable
  -> typing_decidable
typing_decidable + intrinsic_profiles_B_J_C
  -> checker_adequate
reduction_matches_paper
  -> deterministic_step

tree_payload_preserved -> no_hidden_sharing
open_type_model_sound -> no_polymorphic_copy_box
payload_provenance_sound -> no_unknown_payload_copy

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
baseline_J_normalization
tree_payload_preserved
  -> self0_preservation

clash_collapse + self0_checked
  -> self1_checked

copy_boom_typed + baseline_copy_rejected
no_polymorphic_copy_box
no_unknown_payload_copy
baseline_copy_identity_distinct
  -> matched_copy_control_complete

machine_step_decidable
  -> trace_checker_sound
  -> trace_checker_complete
tm_translation_step_iff
  -> tm_finite_run_iff
  -> partial_computations_representable
```

The older names `normalization`, `canonical_forms`, and `self0_preservation`
are retained below for compatibility with the acceptance matrix. Their theorem
statements are governed by Sections 9 and 10 of
[`../theory/alsjas-calculus.md`](../theory/alsjas-calculus.md). No path may skip
`paper_review_accepted` by defining the intrinsic relation around existing
checker behavior.

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
