# Theorem 4.6 in the formalization — design note

*2026-10-05, branch `f4-theorem46`. Scope: R4-metatheory.md §4.5 (Theorem
4.6, the phantom model, Lemma 4.6a, Corollaries 4.6′ and 4.6″), formalized
under ADR-0006. This note records how the phantom model is represented and
how its fundamental lemma reuses the proof of Lemma 3.6. It also records
which encoding facts the theorem needs, stated as explicit hypotheses.*

## 1. What the standard model fixes, and what the phantom model must change

The standard model (`carrier.clj`, `den.clj`/`den_gen.clj`, `sem_gen.clj`)
represents the carrier of `R` by `Code`, the same type as `Syn`. A token
carries no information, so a certificate is just its tree. Then:

- `⟦leaf a⟧ = sl ⟦a⟧` and `⟦node d a r₁ r₂⟧ = sn ⟦a⟧ ⟦r₁⟧ ⟦r₂⟧`;
- `⟦print r⟧ = ⟦r⟧`: print is the identity;
- `⟦itR X g h r⟧` is `Code.rec` over `⟦r⟧`. A leaf `sl l` gives `⟦g⟧ l`, and a
  node gives `⟦h⟧ ⋆ l y₁ y₂`;
- `⟦inspect …⟧` and `⟦reflect …⟧` test `chkf ⟦r⟧ …`;
- `V(R) = { v : cnodes v ≤ k ∧ lblOk v }`.

The phantom model (§4.5) needs certificate values `★ᵢ` with three
properties:
1. footprint 0;
2. `itR` sees `★ᵢ` as a leaf;
3. `print ★ᵢ = cᵢ`, a real certificate of `Aᵢ`.

Property 3 is incompatible with `print` being the identity. Properties 1 and
2 force `★ᵢ` to be a leaf, a `Code.sl`. But every leaf `sl l` is already
`⟦leaf a⟧` in an environment with `⟦a⟧ = l`. Labels are natural numbers, and
usage-0 entries and `Π₀` arguments range over all of them. If `print`
expanded some leaf `sl l`, the ι-step `print (leaf a) ⇝ sleaf a` would
fail at that `l`, and with it Lemma 3.2. So the phantom model must also
change `⟦leaf a⟧`, so that no term builds a phantom.

**The five clauses that differ:** leaf, itR, print, inspect (through print)
and reflect (it must not decode a phantom). Also `V(R)`. Every other clause,
and every other part of the model, is the same.

## 2. One model, parametric in an R-interpretation

Two separate models would need two proofs of the fundamental lemma. That
lemma is about 12 000 lines across `substitution`, `conversion`, `convcase`,
`fundamental`, `outer`, `recsyn`, `mono`, `splitting`, `vweaken`, `unfold`
and `lemma36`. Instead, the model is generalized over an **R-interpretation**
`ri : RInt` (namespace `rint`). The carrier of `R` stays `Code`, which is rich
enough to represent phantom trees by re-coding labels. The interpretation
fixes:

| field | meaning | standard `stdRI` | phantom `phRI J σ` |
| --- | --- | --- | --- |
| `riLf : Nat → Nat` | label of the tree that `leaf a` builds | `l ↦ l` | `l ↦ l + J` |
| `riPr : Code → Code` | `print` | identity | `sl i ↦ σ i` (i < J); `sl l ↦ sl (l − J)` (l ≥ J); `sn` structural |
| `riPf : Code → Bool` | phantom-free, so `reflect` may decode | `true` | every leaf label ≥ J |

The leaf labels `0 … J−1` are the phantoms `★₀ … ★_{J−1}`. `leaf a` never
builds one, since its label is `⟦a⟧ + J ≥ J`.

**Derived operations.** Both are definitions over the fields, not further
fields:
- the label that `itR` passes for a leaf, `riIt ri l`, is the label of
  `riPr ri (sl l)` if that is a leaf, and `ℓ₀ = 0` otherwise. A phantom
  prints to a certificate, which is a node, so `itR` treats `★ᵢ` as a leaf
  labelled `ℓ₀`, exactly as §4.5 says;
- `V(R)` is `{ v : cnodes v ≤ k ∧ lblOk (riPr ri v) }`: the *printed* tree has
  labels in `L`.

The generic clauses:
- `⟦leaf a⟧ = sl (riLf ri ⟦a⟧)`;
- `⟦print r⟧ = riPr ri ⟦r⟧`;
- `itR` is `Code.rec`, with leaf case `⟦g⟧ (riIt ri l)`;
- `inspect` tests `chkf (riPr ri ⟦r⟧) ⟦c⟧`;
- `reflect` decodes `dec (riPr ri v)` when `cnodes v ≤ cap`, `riPf ri v` and
  `chkf (riPr ri v) ⌜D⌝` all hold, and otherwise returns the default.

At `stdRI` every clause is the standard one, definitionally:
- `riPr stdRI v` reduces to `v`;
- `Bool.and true b` reduces to `b`.

**The laws the generic proof uses** (`RLaws ri`):
- **L1** `riPr (sl (riLf a)) = sl a`. This is the ι-step `print (leaf a) ⇝
  sleaf a`; with the definition of `riIt` it gives `riIt (riLf a) = a`, the
  ι-step of `itR` on a leaf.
- **L2** `riPr (sn l v w) = sn l (riPr v) (riPr w)`: the ι-step of `print` on
  a node. The node clause itself is unchanged.
- **L3** `riPf v = true → cnodes (riPr v) = cnodes v`. This is Lemma 2.8 for
  phantom-free trees, and it is what lets `reflect` and `H₁` descend in
  budget as in §3.5.
- **L4** `lblOk (riPr (sl 0)) = true`: the default certificate lies in `V(R)`.

Both instances satisfy them: `stdRI` trivially, and `phRI J σ` by
computation on labels (only `+`, `−` and `<`).

**The consistency side condition** (`PhCons chkf encTy ri`). Where §4.5's
Lemma 4.6a cites Corollary 3.7, the generic proof takes these two statements,
restricted to trees that are *not* phantom-free:
- no such `v` has `chkf (riPr v) ⌜0⌝ = tt`;
- no such `v` or `w` has both `chkf (riPr v) d = tt` and
  `chkf (riPr w) (neg d) = tt`.

These are the paper's `H₁` and `Reflect`-at-`0` cases with phantoms.
- At `stdRI` the premise "not phantom-free" never holds, so `PhCons` is
  trivially true. The standard Lemma 3.6 is therefore an instance of the
  generic proof, with no circularity.
- At `phRI` the condition follows from the standard Corollary 3.7, which holds
  at every size.

So the generic H₁ case splits:
- if both trees are phantom-free, it is §3.5's argument: Lemma 2.4 and the
  outer hypothesis, with the descent given by L3;
- otherwise it is `PhCons`.

The Reflect case splits the same way. A tree with a phantom gives the
default, which lies in `V(D)` for every base type `D ≠ 0`, by L4 at `R`. For
`D = 0`, `PhCons` excludes the evidence.

## 3. How the existing proof is reused

The generic chain is the existing chain, transformed mechanically, plus edits
by hand at the R-specific places. It lives in new namespaces `ri_den`,
`ri_sem`, `ri_model`, `ri_unfold`, `ri_mono`, `ri_splitting`,
`ri_substitution`, `ri_vweaken`, `ri_conversion`, `ri_fundamental`,
`ri_outer`, `ri_recsyn`, `ri_lemma36` and `ri_convcase`.

The mechanical part, done by `formal/tools/gen_ri.py`:
- **Renaming.** Every kernel constant of the chain whose statement or value
  depends on the model (`den`, `denT`, `denAt`, the clauses, `V`, `EnvSat`,
  `Sound`, `OuterIH`, …) gets the suffix `_ri`. The list comes from the warm
  server's record of the constants each namespace declares, closed under
  reference.
- **Reuse.** Constants that do not depend on the model are not copied. These
  are arithmetic, syntactic and skeleton lemmas, `DenFn`, `den0` and
  `pickD`; the generic chain refers to the standard ones.
- **The new parameter.** It is inserted after `chkf dec encTy` in every
  binder list and every application of a renamed constant. `CheckSpec` is
  not model-dependent and keeps its arguments.
- **The clause generators.** `gen_den.py` and `gen_sem.py` take `--ri` and
  emit `ri_den_gen.clj` and `ri_sem_gen.clj`, with the five clauses of §2 and
  `V(R)`.

By hand: the proof steps that read the five clauses or `V(R)`. These are
- `F_leaf`, `F_node`, `F_itR`, `F_prn`, and `coderec_inv`, whose invariant
  becomes `lblOk (riPr ri w)`;
- `F_insp`, `F_refl` and `F_h1`, which gain the phantom branch;
- the `print` and `itR` ι-steps in `conversion`, through L1–L2;
- `V(R)` in `mono` (`V_mono`, `base_R_mono`, Lemma 3.4);
- the templates for these clauses in `substitution`, `vweaken` and
  `convcase`.

Everything else in the copy is the existing text with renamed constants and
the extra parameter.

**Why a copy rather than generalizing the standard chain in place.** In-place
generalization would change the signatures of `den`, `V` and `EnvSat`, which
some 1 900 lines downstream use: `eval`, `theorem4`, `funde`, `section4b/c`,
`prop434`, `prop410`, `certenc` and others. Those proofs rely on the standard
clauses' shapes (`print` the identity, `itR` reading labels directly). The
copy leaves every existing theorem untouched, so there is no regression risk
and no 35-minute reload of everything downstream per iteration.

The cost is a duplicated proof text. The generic chain is the one proof of
the fundamental lemma for both models: its instance at `stdRI` is Lemma 3.6.
Retiring the old chain is a separable follow-up: redefine `den`/`V`/`EnvSat`
as the `stdRI` instances and the old lemma names as wrappers. It is recorded
in ADR-0006, not done here.

## 4. Lemma 4.6a

`lemma46a`: Lemma 3.6 for `phRI J σ`, with `σ i` lblOk codes, given
`CheckSpec`. Its `PhCons` comes from the standard `cor37_refutation` /
`cor37_contradiction` with `conv_all`.

The hypotheses are `CheckSpec` and the labels of the phantom codes, and
nothing about the encoding. The paper's list for 4.6a ("Lemma 2.8 applies
without a phantom") is the law L3, a property of the interpretation.

## 5. Theorem 4.6 and its encoding hypotheses

**Statement** (`thm46`). Let `As = [A₁ … A_j]` with `j ≥ 1`. Suppose
`Θₖ ⊢ t :¹ □A₁ ⊗ ⋯ ⊗ □A_j ⊸ □B`, and:
- `CheckSpec`;
- `Enc46` (below);
- for each `i`, a certificate `cᵢ` of `Aᵢ` — accepted at `⌜Aᵢ⌝`, labels in
  `L` — whose decoded term is larger than `k`;
- `⌜B⌝ ≠ ⌜Aᵢ⌝` for each `i`.

Then some certificate `v` of `B` (accepted at `⌜B⌝`, `lblOk`) has
`cnodes v ≤ k`. That is `k ≥ μ(B)`, with `μ` as a lower bound, as in
`prop434`.

**The proof.**
1. Apply Lemma 4.6a at `n = k`, with `J = j` and `σ i = cᵢ`, in the
   all-token environment. Feed `t` the phantom input
   `((★₀, ⋆), …, (★_{j−1}, ⋆))`, which lies in `V₀`.
2. The output is `(r″, ⋆)` with `cnodes r″ ≤ k` and
   `chkf (riPr r″) ⌜B⌝ = tt`.
3. Three cases:
   1. *Phantom-free:* `riPr r″` is the certificate, by L3.
   2. *`r″ = ★ᵢ`:* `cᵢ` is accepted at `⌜Aᵢ⌝` and at `⌜B⌝`. `CheckSpec`
      decodes one judgment, whose type code is both, so `⌜B⌝ = ⌜Aᵢ⌝`. This
      contradicts the hypothesis.
   3. *A phantom strictly inside:* `Enc46`.

**Holed trees.** `HTree` is a tree over labels whose leaves may be holes
`hole i`. `fill σ K` plugs `σ i` into hole `i`, and `hnodes K` counts `K`'s
own internal nodes. A phantom-model value `r″` is a holed tree in disguise:
`riPr (phRI J σ) r″ = fill σ (toH J r″)` and `cnodes r″ = hnodes (toH J r″)`.

**`Enc46 chkf dec sz`.** This is the one encoding hypothesis case 3 needs. It
is stated for any term measure `sz : Exp → Nat`. If
- `fill σ K` is accepted (at some `d`),
- `K` has a hole and is not itself a hole, and
- every `σ i` used is accepted,

then some hole `i` of `K` has `sz (term (dec (σ i))) ≤ hnodes K`.

In words: an accepted code that embeds accepted codes strictly inside pays,
in its own nodes, at least the size of one embedded certificate's term.

The paper derives it from these facts:
- **E6.** A rule-labelled root is a derivation node, so an embedded
  certificate is a sub-derivation.
- **The two rule facts.** A runtime premise occurs only under runtime rules,
  and its term is a literal subterm of the conclusion's term.
- **E2 and E3.** The root's first child encodes its judgment, which contains
  the term's encoding. A hole cannot lie there (E6 again).
- **E4.** The term's encoding has at least `sz` nodes, for `sz` =
  constructor applications plus variable occurrences.

The theorem takes `Enc46` and the large certificates for the same `sz`, so
any measure for which both hold will do.

**Findings, to be confirmed by the formal proof:**
- *E1 is not needed by the theorem itself.* Stated with `⌜B⌝ ≠ ⌜Aᵢ⌝`, case 2
  uses only `CheckSpec`'s decoding. E1 (inside `CheckSpec`) only turns
  `B ≠ Aᵢ`, for closed types, into that. Corollary `thm46_types` does this.
- *E3 is used.* §4.5's "What the proof uses" lists E1, E2, E4, E6. But case 3
  counts `⌜fᵢ⌝`'s nodes among the root judgment's, which needs the judgment
  to contain the term's encoding: E3, or E2 read as encoding the whole
  judgment.
- *F7's concrete checker does not satisfy `Enc46`.* `Check decCert` accepts
  `sn 0 ⌜…⌝ pad` and reads the padding only through the size tests. So an
  accepted certificate may carry another certificate as its padding,
  outside every derivation node, at no cost in its own nodes. The phantom
  argument's case 3 therefore fails for F7's format, and Theorem 4.6 is
  **not** established for `Check decCert` by this proof. The theorem holds
  for any checker satisfying `Enc46`. Whether it holds for F7 is open: a
  different argument would be needed, or an unpadded format. This is
  recorded in ADR-0006, not hidden.

**The large certificates.** The paper builds them as `fᵢ := (λ(y :₀ Nat).
gᵢ) N̄`. That needs completeness of `Check`: every derivation has an
accepted code. `CheckSpec` gives soundness only. So the theorem takes, for
each `i`, an accepted certificate whose term has `sz > k`. This is the
weakest form the proof uses: case 3 needs only `sz > k`.

## 6. Corollaries

- **4.6′ (D2).** For `□(A ⊸ B) ⊗ □A ⊸ □B` at `Θₖ`:
  - the lower bound is Theorem 4.6 with `As = [A ⊸ B, A]`, and also with
    `As = [A]` (fixing the certificate of `A ⊸ B` does not help);
  - the upper bound is `λz. (lit v, ⋆)` at budget `‖v‖`, for any certificate
    `v` of `B`, as in `prop45_d3`.
- **4.6″ (D3, exactly).**
  - lower bound: Theorem 4.6 with `As = [A]` and `B = □A`;
  - upper bound: `prop45_d3`;
  - `μ(□A) > 2μ(A)`: `prop43`.
- **Negatives.**
  - `B = A₁` must be excluded: `λz. z : □A ⊸ □A` at `Θ₀`, while every accepted
    certificate has at least one node, so "a certificate of `A` with ≤ 0
    nodes" is false.
  - The `Aᵢ` must be certifiable (§4.5's `□0` example), where feasible.

## 7. Tests

One suite per new namespace (`test-formal/lcert/formal_test/<ns>.clj`):
- `b/has?` for every theorem;
- meaningful negatives: a false variant the kernel rejects, e.g. `PhCons`
  dropped from the H₁ case, `print` the identity at `phRI`, `B = A₁`, and the
  identity on `□A₁` at budget 0.

## 8. Outcome (2026-10-06)

Every unit is kernel-checked on branch `f4-theorem46`. ADR-0006 records the
statement index and the deviations.

**The model and Lemma 4.6a.**
- `rint` defines the interpretations, with the laws bundled in the type.
- The generic chain `ri_*` is generated by `tools/gen_ri_all.sh`.
- `lemma46a` is Lemma 4.6a, from `CheckSpec` and `PhOk` alone.
- `lemma36_via_ri` recovers the standard Lemma 3.6 from the generic proof.

**Theorem 4.6** is `thm46` over codes and `thm46_types` over types.
- Its only encoding hypothesis is `Enc46`.
- `thm46_ne_needed` shows that B = A₁ must be excluded.
- `enc46w` shows the hypotheses are jointly satisfiable, at a toy checker.

**Corollary 4.6′.** The bounds are `d2_lower`, `d2_lower1`, `d2_upper` and
`d2_upper_k46`.
- `cor46_prime_iff`: a term exists at Θₖ iff k ≥ μ(B).
- `d2_no_uniform`: no budget serves every A and B.
- Section 6 planned only an upper bound at ‖v‖. The upper bound at every
  k ≥ ‖v‖ is new: ⋆ carries the extra tokens.

**Corollary 4.6″.** The pieces are `d3_lower`, `d3_gap` and `d3_upper_k46`.
- `cor46_dprime_iff`: a term exists at Θₖ iff k ≥ μ(□A).

The findings of §5, as they stand:
- *E1 is not needed.* Confirmed by the kernel: `thm46` never uses CheckSpec's
  E1 clause, and `thm46_types` uses it only for B ≠ Aᵢ ⇒ ⌜B⌝ ≠ ⌜Aᵢ⌝.
- *E3 is used.* This is a reading of the paper's case 3. The formal proof
  cannot confirm or refute it, because `Enc46` packages case 3.
- *F7's checker does not satisfy `Enc46`.* This is now a theorem, in
  `enc46f7`:
  - `check_pad`: padding is free.
  - `enc46_F7`: large certificates of any one type refute `Enc46` at
    `Check decCert`, for every measure.
  - `thm46_F7_vacuous`: at F7's checker, Theorem 4.6's hypotheses are
    contradictory at every budget k ≥ 2 + ‖left c₀‖ + ‖c₀‖, for any
    accepted c₀.
  - Consequence: the lower-bound halves of the corollaries are vacuous at
    F7, and the upper bounds still hold there.
  - Theorem 4.6 at F7 remains open.
  - *Update, 2026-10-06 (branch `f7-enc46`): closed.* F7's certificate
    format is now canonical and unpadded
    ([`docs-f7-enc46-design.md`](docs-f7-enc46-design.md)). `Check decCert`
    names that format; the padded one is `Check decCertPad`, and the three
    results above are now stated there. Enc46 holds at the canonical
    checker (`enc46_canon`), and Theorem 4.6 and both corollaries hold there
    with the paper's hypotheses only (`thm46f7.clj`).

**One planned negative was not added.** §7 listed "`PhCons` dropped from the
H₁ case" as a false variant to reject. There is no kernel test of it, for two
reasons:
- the generic chain takes `PhCons` as a hypothesis of many lemmas, not of one
  statement;
- a rejection would only show that a tactic fails, not that the variant is
  false.

The suites test the phantom model's distinguishing features instead:
- `ri_den`: at `phRI 1` the tree of `leaf 0` is `sl 1`, not `sl 0`;
- `theorem46`: a phantom lies in V₀(□c) at `phRI` but not at `stdRI`.
