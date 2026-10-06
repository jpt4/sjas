# Theorem 4.6 at F7's checker — design note

*2026-10-06, branch `f7-enc46` (from `f4-theorem46`). Scope: the certificate
format of F7's concrete checker `Check decCert` (ADR-0006, F7), changed so
that Theorem 4.6 (R4-metatheory.md §4.5) holds there with no encoding
hypothesis. Companion to `docs-theorem46-design.md`, whose §5 and §8 record
the gap this note closes.*

## 1. The gap

`thm46` proves Theorem 4.6 for every checker that meets `CheckSpec` and
`Enc46`, given large certificates of the inputs. F7's checker refutes `Enc46`
(`enc46f7.clj`). Its certificates are `sn 0 ⌜data⌝ pad`, and nothing reads
`pad` except the body's size tests (`m < ‖c‖`, `2f < ‖c‖`, `‖⌜A⌝‖ < ‖c‖`, and
the δ-code bounds `dtB Tᵢ ≤ ‖c‖`). So an accepted code can carry any code,
another certificate included, at no cost in its own nodes. That breaks case 3
of the phantom argument: an output tree may hold a phantom strictly inside.

The padding exists only so that completeness (`check_complete`) can inflate a
certificate until it passes the size tests.

The padding is not the only free position. With the old decoder:
- the root label is never read;
- `decT`'s readings ignore some children: the second child of a unary
  `succ` node `sn 3`, of `tT` (`sn 23`), of a `var` node, and others;
- a constructor chain's last tail is never read;
- a δ-record (`HdDT.delta`) stores its two codes `cc`, `dc` verbatim
  (`encC` was the identity), so a δ-step on a literal certificate puts that
  certificate, verbatim, inside the derivation.

So "remove the padding" alone does not make accepted codes embed accepted
codes only where case 3 expects them. Every free position has to go.

## 2. Options

- **(a) Canonical padding.** Accept only padding `padC k`, and prove that no
  subtree of `padC k` is accepted. The size facts stay tests. The other free
  positions above remain, so this needs the canonicity of (c) anyway.
- **(b) No padding.** Derive the size facts from the encoding, as the paper
  does with E2–E4, so that completeness needs no padding.
- **(c) Canonical certificates** (the addition both need). The decoder
  re-encodes what it decoded and accepts only if the result is the
  certificate itself. Every accepted code is then exactly `encCert` of its
  data, and its subtrees are known.

**Chosen: (b) together with (c).** It makes the size facts consequences of
the encoding, which the task prefers, and it is tractable: each new fact is a
structural induction generated from a table that already exists.

## 3. The new format

A certificate of `Θₘ ⊢ t :¹ A`, with typing tree `T₁` and formation tree
`T₂`, is

    encCert m t A T₁ T₂ = sn 96 ⌜(F, m, t, A, T₁, T₂)⌝ (sl 0),
    F = htDT T₁ + htDT T₂ + 1 (the decoding fuel, now determined by the trees).

Here `⌜(…)⌝` is the old constructor chain, with each field encoded as before
except raw codes.

1. **The certificate label 96.** It is an encoding label (below 97, so inside
   L). No component encoder puts it on an internal node: constructor
   positions are below 70, and the expression encoding's node labels are at
   most 57. This is F7's form of E6: *the certificate label marks an internal
   node only at a certificate's root.*
2. **Raw codes as literal terms.** A raw code `c` (a δ-record's `cc`, `dc`)
   is encoded as its canonical code term, `encC c = encE (codeTerm c)`, and
   decoded by `codeOf ∘ decE`. This is the paper's convention: a code
   literal is written as a term (E4), and its labels become leaves under
   `lblc` (E6).
3. **Canonicity.** `decCert c` runs the old decoder (`decCertPad`, which
   reads the left child) and returns its result `y` only if
   `codeEq c (encCertY y)`. So an accepted code is `encCert` of its own data
   (`decCert_canon`).
4. **No padding.** The size tests stay in the checker's body, which is
   generic over the decoder. `check_spec.clj` is unchanged. But each test is
   now a consequence of the encoding:
   - `m < ‖c‖`: the budget is unary, `‖encN m‖ = m`.
   - `‖⌜A⌝‖ < ‖c‖`: `⌜A⌝` is a proper subtree.
   - `2f < ‖c‖`, f the tokens free in t: `f ≤ m` (context, E3; prop434's
     `cnt_mask_le`), and `f ≤ ‖⌜t⌝‖` (`tok_encE`, one internal node per
     variable occurrence, E4). These are disjoint subtrees under the root
     (E2).
   - `dtB Tᵢ ≤ ‖c‖`: each δ-record carries its code as a literal term with
     more nodes than the code (`dtB_le`).

   Completeness (`check_complete`) builds `encCert` with no padding.
   `budget_canon`, `toksize_canon` and `typesize_canon` derive the first
   three size facts from canonicity alone, without the tests.

The old padded format remains as `encCertPad`/`decCertPad`, for the
historical counterexample (`enc46f7.clj`).

*Where it lives.* The size and label facts about the expression encoding
are `encsize.clj`. The raw-code change and the renamed padded format are in
`certenc.clj`. The canonical format, its size facts, completeness and the
shape of accepted codes are a new namespace, `certcanon.clj`, so that
iterating on it does not reload the generated round trips. Theorem 4.6 and
the corollaries at F7 are `thm46f7.clj`.

## 4. What it gives

**No nesting.** No accepted code is a proper subtree of an accepted code
(`check_nest_free`). Let `c = hfill σ K` be accepted, with `K` a node and a
hole `i` holding an accepted `σ i`. Then:
- `c = sn 96 X (sl 0)`;
- every internal label of `X` is below 96 (`nlb 96`, proved for every
  component encoder);
- so the subtree `σ i` has every internal label below 96;
- but `σ i`, being accepted, has root 96.

**`Enc46` holds at `Check decCert`, for every term measure** (`enc46_canon`).
Its premise, an accepted tree with an accepted tree strictly inside, never
holds. This is stronger than the paper's case 3, which must handle nesting:
there a sub-derivation can itself be a certificate. F7 wraps only the root,
so case 3 is vacuous.

**The large certificates are not needed.** `thm46` takes `Enc46` and the large
certificates for the same `sz`. Since `Enc46` holds for every `sz`,
instantiate `sz := λ_. k+1`. Every certificate is then "large at k", and the
paper's `fᵢ := (λ(y :₀ Nat). gᵢ) N̄` construction is not needed. That
construction would require weakening at the level of derivation trees, which
F7 does not have. So `LargeCert` is discharged, not by completeness as
planned, but because the case that needs it cannot arise.

**Theorem 4.6 at F7** (`thm46_F7`, `thm46_types_F7`). Its hypotheses are only
the paper's:
- the inputs are certifiable: an accepted certificate with labels in L;
- B differs from each Aᵢ;
- the typing derivation.

**Corollaries 4.6′ and 4.6″ at F7** (`cor46_prime_F7`, `cor46_prime_iff_F7`,
`cor46_dprime_F7`, `cor46_dprime_iff_F7`). The same, and further hypotheses
are now derived:
- `⌜□A⌝ ≠ ⌜A⌝` (`box_code_ne`, by size);
- the closedness of the certified types (`CheckSpec`'s decoding and E1);
- the label conditions on the type codes. A type code is a subtree of a
  canonical certificate (`cert_type_lbl`).

## 5. What it costs

- **Certificate size.** No padding, so a certificate is never inflated.
  Three things grow:
  - a δ-record's codes: a code node now costs 3 nodes and a leaf 2, instead
    of 1 and 0;
  - the fuel is fixed;
  - the root gains one leaf.
- **Checking time.** The decoder re-encodes the decoded data and compares it
  with the certificate. This adds one linear pass.
- **Rigidity.** Exactly one code certifies a given (m, t, A, T₁, T₂). A
  certificate with a larger fuel, any padding, or any junk in an unread
  position is rejected. Nothing used those freedoms but completeness's
  padding.
- **Proof.** New generated inductions:
  - `tok_encE`, over 41 expression constructors;
  - `dtB_le`, over the derivation-tree constructors;
  - the internal-label bounds `nlb`, over every encoder;
  - the canonical round trip.
- **Unchanged:**
  - `check_spec.clj` (the checker and its generic soundness);
  - the `DT`/`HdDT`/`SkDT`/`StepDT` data types;
  - every theorem stated for any `CheckSpec` checker.
- **Renamed:** the old `encCert`/`decCert`/`decCert_enc` become
  `encCertPad`/`decCertPad`/`decCertPad_enc`. `enc46f7.clj` is restated at
  `Check decCertPad`. Its `thm46_F7_vacuous` stays as the historical
  counterexample: a theorem about the padded format.

## 6. Success and failure criteria

- **Success:** `thm46_F7`, `cor46_prime_F7` and `cor46_dprime_F7` are proved
  at `Check decCert`, with no `CheckSpec`, `Enc46` or `LargeCert` hypothesis.
  They also need:
  - every concrete-checker result re-proved on the new format (`check_spec`,
    `check_complete`, `check_complete_lbl`, TokSize, TypeSize, and selfjust's
    `consistent_concrete`, `cor37_concrete`, `self_justification`);
  - every label below 100;
  - every suite green.
- **Failure:** any of the following, each stated with its kernel-checked
  counterexample:
  - completeness cannot be kept without free positions;
  - a size fact is not derivable;
  - a cheap D2 term exists below μ(B).
