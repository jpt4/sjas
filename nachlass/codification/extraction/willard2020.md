# Extraction: Willard2020

> Spine extraction (component C8). Template: charter ADR-0001 §Decision.
> **Anchoring convention**: this witness's printed page number is **PDF page
> − 1** (the title page carries no number). Anchors below are **PDF** pages,
> which is how the rendered images and `pdftotext -f/-l` address them; the
> printed number is one less.
>
> **Text-layer hazards realised here** (charter §H2): `pdftotext` rendered the
> statement symbol **⊙** as `J` throughout §6, and page markers are preceded by
> form feeds that defeat a naive `^=== PAGE` grep. Load-bearing constants were
> read from page images.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2020` |
| Title | How the Law of Excluded Middle Pertains to the Second Incompleteness Theorem and its Boundary-Case Exceptions |
| Venue | arXiv:2006.01057v1 [math.LO], 1 June 2020 (28 pp.) |
| Witness | `nachlass/papers/willard2020.pdf` — byte-identical to `lit/willard_2020_how_lem_pertains_2nd_inc_thm_boundary_case_exceptions.pdf` (sha256 `777816bc…`); one document, two paths |
| Text aid | `../sources-text/willard2020.txt` (~10,100 words) |
| MSC | 03F25; 03F30 |
| Self-described status | "Short conference announcements of these results at ASL-2020's Virtual N. American Meeting and at LFCS-2020"; called an "extended abstract" at p. 15 |
| Structure | §1 Introduction (2–3); §2 Revisiting Some Intuitions of Gödel and Hilbert (4–5); §3 Main Notation and Background Literature (6–9); §4 Main Theorems and Related Notation (9–13); §5 Intuition Behind Theorem 4.4 (14–16); §6 Summary of Theorem 4.5's Proof (16–18); §7 More Elaborate Forms of Theorems 4.4 and 4.5 (18–19); §8 Further Generalizations (19–20); §9 Ironic Events and Related Speculations (20–21); §10 Concluding Remarks (21–23); Appendix: semantic tableau (24); References (25–28) |

### 1a. Relation to `Willard2020-LFCS`, and to this repository's own draft review

Willard states at p. 20: *"The initial **19-page draft** of this article was
accepted by the LFCS-2020 conference and was published by Springer [57]"* —
and **[57]** is "On the Tender Line Separating Generalizations and
Boundary-Case Exceptions for the Second Incompleteness Theorem under Semantic
Tableaux Deduction", LNCS **11972**, pp. 268–286.

So `Willard2020` **is the expanded version of `Willard2020-LFCS`**, not an
independent work. The terminal-period lineage is:

```
Willard2018 (arXiv 1807.04717, "quite preliminary … roughly written summary-abstract")
   → Willard2020-LFCS (LNCS 11972, 19 pp., Jan 2020)
      → Willard2020 (arXiv 2006.01057v1, 28 pp., Jun 2020)
         → Willard2021 (JLC 31(1) 375–392 — the unacquired gap G1)
```

The corpus registry currently rules `Willard2020` spine/C8 and
`Willard2020-LFCS` Tier A/C9 as if unrelated. They are two lengths of one
paper; C9 should treat the LFCS chapter as a *variant witness* and extract only
its deltas. Recorded as **D27**.

**This repository is part of the paper's provenance.** `prose/willard2020draftreview.txt`
(dated `20200521Z`, signed `jpt4`) is a 40-item copy-edit of a pre-publication
manuscript. Its page references map onto this witness at a consistent ≈2.2×
ratio (review p. 15 → here p. 8; p. 28 → p. 13; pp. 54–57 → pp. 25–28), so the
reviewed document is a looser-set manuscript of **this** paper, not a longer
separate one. Several of its corrections are visibly applied in v1 —
"mappings are" (Thm 4.4), "fails to be **a** consistency-preserving mapping"
(Thm 4.5), "Type-NS" (Rem 3.5), the comma in Theorem ++'s "commutative,
distributive", and the added YouTube URLs at refs. [18] and [39]. Others were
not: p. 7's Example 3.3 still reads "Self Justifying" unhyphenated. The
**Acknowledgments (p. 23)** read: *"I thank Seth Chaiken and **James P. Torre,
IV** for several quite helpful comments about how to improve the presentation."*

Consequence for the codification: the reviewed manuscript is **superseded** by
this witness and is not a gap to chase — but the review file is a first-party
provenance record and should be cited as such. Recorded as **D28**.

## 2. Role in corpus

This is Willard's **terminal general statement**. Its thesis is narrow and
sharp, and it is not a "more results" paper:

> Semantic tableau admits partial exceptions to the Second Incompleteness
> Theorem where a formalism recognizes its self consistency and views
> multiplication as a 3-way relation. **We now show these boundary-case
> evasions will collapse if the Law of the Excluded Middle is treated by
> tableau as a schema of logical axioms (instead of as derived theorems).**

Its contribution to the *codification* is disproportionate to its length,
because it supplies three things no earlier paper states as cleanly:

1. **The apparatus-identity thesis** (§3 Example 3.1, restated §7): all common
   apparatuses prove the *same theorems* but at *different proof lengths*, and
   SJAS feasibility tracks length, not theoremhood.
2. **The `GenAC` framing**: self-justification is a property of the **pair**
   (α, D), with `D` decomposed into logical axioms `L_D` plus rules.
3. **The clearest statement of the growth mechanism in the whole corpus**
   (§5): whether a number's binary encoding is long enough to hold the
   derivation that defines it.

## 2a. Declarative versus Infinite-Ranged exceptions (§1, pp. 2–3)

Willard names his own programme's output **"Declarative Exceptions"** to the
Second Incompleteness Theorem: formalisms `α*` that "prove more Π₁ like theorems
than Peano Arithmetic, while offering some type of partial knowledge about their
own consistency". He contrasts these with **"Infinite-Ranged Exceptions"**, the
approach of **Artemov** [4] (related to **Beklemishev** [6] and
Artemov–Beklemishev [5]): Peano Arithmetic generates `T₁, T₂, T₃, …` where each
`T_i` shows some subset `S_i` of PA cannot prove `0=1`, with
`S₁ ⊂ S₂ ⊂ S₃ ⊂ …` and PA the union of the `S_i`. Willard notes that [4]'s
abstract "cautiously used the adjective 'somewhat'", an infinite collection of
theorems standing in for Hilbert's one unifying consistency theorem.

**This is the corpus's only direct engagement with the Artemov/Beklemishev
line**, and a live hook for the Refinement stage. (Added 2026-08-27; §1 had not
been read at C8.)

## 3. Systems and machinery defined

### 3.1 GenAC, and the apparatus-identity thesis

A **Generalized Arithmetic Configuration** (`GenAC`) is an ordered pair (α, D):
α is the **Axiom Basis** (its proper axioms); `D` is the **Deductive
Apparatus**, defined as *the union of its logical axioms `L_D` with its rules
of inference*. Decomposing `D` this way is the paper's key move — the whole
result is about migrating sentences between `L_D` and the derived theorems.

**Example 3.1** compares four apparatuses: `D_E` (Enderton — modus ponens only,
4-part logical-axiom schema), `D_H` and `D_M` (Hájek–Pudlák, Mendelson —
fewer logical axioms, generalization as a second rule), `D_F` (Fitting/Smullyan
tableau — *no* logical axioms, broader rule). Then:

> **AN IMPORTANT POINT is that while proofs have different lengths under
> different apparatuses, all the common apparatuses produce the same set of
> final theorems from an initial common "axiom basis" of α**

with footnote 2 giving the reason: they all satisfy Gödel's Completeness
Theorem. §7 restates it for Z-enrichment: the theorem set is *invariant*, "yet
despite this invariance, the proof-lengths change, quite sharply".

**This is the corpus's apparatus-identity claim, and it is what makes the
codification's apparatus axis non-trivial**: an axis that is extensionally
degenerate but intensionally decisive. Obligation **O38**.

### 3.2 Self-Justifying — a fifth definition of the corpus's central term

**Definition 3.2**: (α, D) is **Self-Justifying** when

- **i.** one of (α, D)'s theorems (or possibly one of α's axioms) states that
  `D` applied to α produces a consistent set of theorems, **and**
- **ii.** the GenAC formalism (α, D) is actually, in fact, consistent.

Closest to `Willard2011`'s pair-valued definition, but sharper: `D` is now an
explicitly structured object, so clause (i) quantifies over an apparatus whose
logical axioms are part of what is being asserted consistent. Extends drift
**D18** and obligation **O25** to **five** definitions across the corpus.

**Example 3.3** restates the Kleene construction: `α_D = α + SelfRef(α, D)`
where ⊕ is the "I AM CONSISTENT" axiom. Kleene [28] first; Kleene, Rogers and
Jeroslow [28, 38, 27] all noted α_D is typically inconsistent.

### 3.3 The growth taxonomy — four types here, five in 2011

**Definition 3.4** uses 3-way predicates `Add(x,y,z)`, `Mult(x,y,z)`, with

```
(1) ∀x ∃z Add(x,1,z)      (2) ∀x∀y ∃z Add(x,y,z)      (3) ∀x∀y ∃z Mult(x,y,z)
```

**Type-M** proves (1)–(3); **Type-A** only (1)–(2); **Type-S** only (1);
**Type-NS** none. `Willard2011`'s taxonomy additionally carries
**Type-Almost-M**, absent here — drift **D29**.

Willard notes parenthetically that the associative, commutative, identity and
distributive axioms have **Π₁ encodings** when expressed with these predicates.
That is the whole reason multiplication can be *present as a relation* while
absent as a total function.

### 3.4 Language and classes

Six **Grounding Functions** (non-growth: `F(a₁,…,a_j) ≤ Maximum(a₁,…,a_j)`):
Subtraction, Division, Maximum, `Log♠(x) = ⌈Log₂(x+1)⌉`, `Root(x,y) = ⌈x^{1/y}⌉`
(image-verified — **ceiling**, matching 2005, against 2001's floor), `Count(x,j)`.
**No Predecessor** — 2001 had seven. Footnote 4 identifies `Log♠` with
Hájek–Pudlák's `|x|`.

**U-Grounding Function** = any of the six, or **Addition** or `Double(x) = x+x`
— eight in all. The language **L\*** is those eight plus `0`, `1`, `=`, `≤`,
and **excludes a multiplication function symbol**, treating multiplication as a
3-way relation via Division.

**Definition 4.1** gives `Δ*₀` / `Π*ₙ` / `Σ*ₙ` by the usual rules, without
multiplication symbols — the same notation as `Willard2005`/`Willard2011`, so
2020 does *not* add a new class notation. **Remark 4.2** defines **Rank-1\***: a
sentence encodable as either `Π*₁` or `Σ*₁`.

### 3.5 The three apparatuses

- **Tab** — semantic tableau per Fitting [15]; root `¬Ψ`, every root-to-leaf
  path closed; six elimination rules (Appendix, p. 24) plus bounded hybrids.
  **Note: no prenex\* normalisation of the root**, unlike `Willard2001` §4.
- **Xtab** — Extended Tableau: Tab plus *any* node may store `℧ ∨ ¬℧`. That is,
  **LEM as a schema of logical axioms rather than as derived theorems**.
- **Tab-1** — from [50] = `Willard2005`: a proof of `Ψ` from α is a sequence
  `(p₁,φ₁),…,(p_k,φ_k)` with `φ_k = Ψ` and each `p_j` a **Tab**-proof of the
  **Rank-1\*** sentence `φ_j` from `α ∪ {φ₁,…,φ_{j−1}}`. The Rank-1\* constraint
  makes Tab-1 strictly less efficient than Xtab.

### 3.6 `IS_D(β)` and Level-1 self-consistency

α has a **Level-1 appreciation of its own self-consistency** under `D` iff it
can verify that `D` produces **no two proofs of a Π\*₁ sentence and its
negation**. `IS_D(β)` (defined in [50] = `Willard2005`) has four groups:

- **Group-Zero**: constants `c̄₀`, `c̄₁`; defines Addition and `Double(x) = x+x`
  — "They will enable our formalism to define any integer `n ≥ 2` using **fewer
  than `3·⌈Log n⌉` logic symbols**." *This is the coding-density requirement in
  its term-construction form* (see §3.9).
- **Group-1**: a finite set `F` of Π\*₁ sentences proving every true Δ\*₀
  sentence — "**Any** finite set of Π\*₁ sentences `F` with this property may be
  used", the same generality clause as `Willard2001` p. 10 (supports D23).
- **Group-2**: `∀p {HilbPrf_β(⌜Φ⌝, p) ⇒ Φ}` for each Π\*₁ sentence Φ. Note the
  base's proofs are **Hilbert-Frege** even when `D` is tableau.
- **Group-3**: the self-referencing Π\*₁ axiom `#`, encoded as

```
∀x∀y∀p∀q  ¬[ Pair(x,y) ∧ Prf_{IS_D(β)}(x,p) ∧ Prf_{IS_D(β)}(y,q) ]        (5)
```

with `Pair(x,y)` a Δ\*₀ formula saying `x` is Π\*₁ and `y` is its negation. The
fixed-point encoding is **not given here** — delegated to [47, 50].

**Definition 4.3 — Consistency Preserving**: `IS_D(β)` is automatically
consistent whenever **all β's axioms hold true under the standard model**. This
is a *fourth* admission condition across the corpus — see drift D24, now
extended.

### 3.7 The two theorems

**Theorem 4.4**: `IS_{Tab-1}(•)` and `IS_{Tab}(•)` are consistency preserving.
**Theorem 4.5**: `IS_{Xtab}(•)` is **not** — `IS_{Xtab}(β)` is automatically
**inconsistent** whenever β proves the usual Π\*₁ associativity, commutativity,
distributivity and identity theorems for addition and multiplication.

**Neither is proved here.** Willard is explicit (p. 13): the proofs "would be
quite lengthy, if they were derived from first principles. Fortunately, it is
unnecessary for us to do so here because we gave a detailed justification of
Theorem 4.4's result for `IS_{Tab-1}(•)` in [50], and one can incrementally
modify … ++ to justify Theorem 4.5." §5 and §6 supply intuition and a summary
respectively. **Theorem 4.4's Tab case is not separately sourced** — [50]
covers `Tab-1` — which leaves a small attribution gap (**G20**).

Footnote 5 records the precise form and its epistemic caveat: [50] shows
**Peano Arithmetic proves that β's consistency implies both the consistency and
the self-justifying property of `IS_{Tab-1}(β)`** — but PA cannot know whether
β is consistent when β = PA, so "the system of PA shall linger in a state of
self-doubt… The main point is, however, that **we humans believe PA is
consistent**, and we can use this fact to confirm that `IS_{Tab-1}(PA)` is BOTH
consistent and able to verify its self-consistency."

### 3.8 The growth mechanism, stated at its clearest (§5)

**Example 5.1** introduces `IS^M_{Tab}(•)` — `IS_{Tab}` plus a multiplication
function symbol, Group-3 updated accordingly — which loses consistency
preservation. The intuition (image-verified, p. 15):

```
x₀ = 2 = y₀ ,      xᵢ = xᵢ₋₁ + xᵢ₋₁ ,      yᵢ = yᵢ₋₁ * yᵢ₋₁
⟹  xₙ = 2^{n+1}  and  yₙ = 2^{2ⁿ}
⟹  Log(xₙ) = n+1  and  Log(yₙ) = 2ⁿ
```

`yₙ`'s encoding has length `2ⁿ`, **far exceeding** the `n+1` steps `ψ₀…ψₙ` that
define it — so `yₙ` has room to encode its own derivation, which is exactly
what a Gödel number needs in order to "self-reference its own definition".
`xₙ`'s encoding has length `n+1`, and "assuming only two bits are needed to
encode each sentence in the sequence `φ₀,…,φₙ`, the length `n+1` … **is
insufficient for encoding this sequence**".

**This is the same fact as `Willard2001`'s Case 11 and D6's "density bounds θ
from below", but stated on the object rather than the proof**: can a number be
big enough to hold its own derivation? Obligation **O39** — the codified
statement should lead with this formulation and derive the bit-counting
versions from it, not the reverse.

### 3.9 The Linear-Sum Effect and statement ⊙ (§6)

**The mechanism of Theorem 4.5.** Hilbert-Frege deduction's modus ponens gives
a proof of `ψ` a length at most proportional to the *sum* of the proof lengths
for `φ` and `φ → ψ` — the **"Linear-Sum Effect"**. Tab lacks it (no modus
ponens). **Xtab recovers it**, via a four-step construction (p. 17):

1. root `¬ψ`;
2. child `φ ∨ ¬φ`, an allowed LEM invocation;
3. branch to siblings `φ` and `¬φ`;
4. below `φ` a subtree no longer than a proof of `φ → ψ`; below `¬φ` a proof
   of `φ`.

The total is bounded by the sum of the two proof lengths plus a negligible
amount. Hence ++ generalises from Hilbert-Frege to Xtab, formalised as
**statement ⊙** (image-verified; `pdftotext` renders ⊙ as `J`):

> **⊙** Any axiom system `A` is automatically inconsistent whenever
> **I.** `A` verifies Successor is a total function; **II.** `A` proves
> addition and multiplication (as 3-way relations) satisfy the usual
> associative, commutative, distributive and identity-operator properties;
> **III.** `A` proves an added theorem (which turns out to be false) affirming
> its own consistency under Xtab.

Willard: "It is **not possible to provide a short proof** for statement ⊙
because it will rest upon the very detailed 'Definable Cut' machinery from
**pages 172-174 of the Hájek–Pudlák textbook [25]**." Footnote 6 notes
`IS_{Xtab}(β)` in fact satisfies something stronger than I, since it recognises
addition as total.

**Proof compression is the active ingredient of the Second Incompleteness
Effect** — not expressive power, not induction, not theoremhood. Obligation
**O40**.

### 3.10 The exact boundary (§7)

**Definition 7.1** — a **Z-Enriched** tableau proof additionally admits
`Υ ∨ ¬Υ` (Line 9) as a logical axiom for any `Υ ∈ Z`, `Z` a set of *sentences*.
**Definition 7.2** — a **Z-Base Variable Enriched** proof admits
`∀x Υ(x) ∨ ¬Υ(x)` (Line 10) for `Υ(x) ∈ Z`, `Z` a set of *formulae free in x*.

Then (p. 19), all **stated without proof**:

| Z | Status |
| --- | --- |
| `Δ*₀` | evasions **survive**, in both the Z-Enriched and Z-Base-Variable-Enriched senses (extending [47] = `Willard2001`) |
| `Π*₁` | **conjectured** to survive — "this fact has not yet been formally proven" |
| `Π*₂` and above | evasions **fail** (extending [49] = `Willard2004`) |

This is the sharpest boundary statement in the corpus and it names a clean open
problem. Footnote 7: the failure at Π\*₂ happens because "the sharp compression
in proof lengths produces Gödel-like Diagonalization compressions".

**Z is the right dial because it is the set of licensed modus-ponens middle
terms.** §6's four-step construction recovers the Linear-Sum Effect by invoking
LEM "of the *particular* form `φ ∨ ¬φ`", where `φ` is the middle term being
simulated; in a Z-enriched proof that step is licensed exactly for `φ ∈ Z`. So
the boundary question is: *how complex must the middle terms be before the
resulting proof compression suffices to run the diagonalization?* Δ\*₀ is not
enough; Π\*₂ is; Π\*₁ is open. Theorems 4.4 and 4.5 are the two endpoints
(`Z = ∅` and `Z` = all sentences) of this one family.

**Two structural observations, not made by Willard**, recorded so C13 need not
re-derive them:

- **Π\*₁ is the class of the system's own axioms.** Group-1 is a finite set of
  Π\*₁ sentences, Group-2's schema (Eq. 4) is Π\*₁, Group-3 is explicitly "a
  self-referencing Π\*₁ axiom" (Eq. 5), and **Level-1 self-consistency is
  *defined* over that class** ("no two proofs … for a Π\*₁ sentence and its
  negation"). Π\*₁-enrichment is therefore precisely the setting in which the
  tableau may branch on the very sentences the consistency axiom quantifies
  over — where self-reference would be expected to bite if anywhere.
- **The two indexings are offset by one level.** For `Υ = ∀v φ(v)` with
  `φ ∈ Δ*₀`, `Υ ∨ ¬Υ ≡ ∀v ∃w (φ(v) ∨ ¬φ(w))`, which by Definition 4.1 is
  **Π\*₂**. So `Z = Π*₁` already introduces logical axioms whose prenex form
  carries the complexity Willard identifies as fatal *when it is the disjunct's*
  complexity. The conjecture thus asks whether the Π\*₂ failure is driven by the
  syntactic form of the added axioms or by the strength of the modus-ponens
  steps they license — the former predicts failure at Π\*₁, the latter
  survival. Note this computation depends on a prenex-normalisation convention
  the paper never fixes (**O42**).

**Do not collapse Definitions 7.1 and 7.2.** At `Z = Δ*₀`, Definition 7.2 admits
`∀x (Υ(x) ∨ ¬Υ(x))`, which is *itself* Π\*₁. So the settled Δ\*₀ case already
licenses some Π\*₁ enrichment *axioms* — just not Π\*₁ *disjuncts*.

**What is actually established is less than it appears.** "A fully detailed
justification will not be provided here" governs **both** the Δ\*₀ positive and
the Π\*₂ negative; each is asserted as an extension of an earlier paper. §7 does
not establish a boundary with one rung open — it **asserts** a boundary with one
rung open (gap G22). `Willard2021` (gap G1) is the likely location of the
proofs and possibly of the answer; see G1's priority note.

### 3.11 Resolution, and the θ primitive (§8)

Resolution "shares numerous characteristics with tableau" [15], so Theorems 4.4
and 4.5 generalise: **`IS_{Res}(•)` is consistency preserving, `IS_{Xres}(•)` is
not**, where an **Xres**-proof allows all LEM instances of L\* as logical
axioms. Stated without proof.

Willard also points to [55] = `Willard2016`'s **Θ primitive**, an
"indeterminate" functional object formalising an endless sequence of integers.
If its conjecture holds — "as we are almost certain it is" — then a substantial
**Type-NS** arithmetic could simultaneously recognise its own Hilbert-Frege
consistency **and** constructively enumerate all of `0,1,2,3,…`, evading ++
entirely. This is the corpus's most speculative live claim and it belongs in
the frontier appendix, flagged as unproven (**G21**).

### 3.12 Retrospective comparison of `IS(A)` with `IS_{Tab-1}(β)` (§10, p. 22)

Willard's own comparison of `Willard2001` against `Willard2005`:

> Our older "IS(A)" formalism was actually **simpler**, but it was
> substantially **weaker** because it only recognized the non-existence of a
> proof of 0 = 1 from itself. In contrast, `IS_{Tab-1}(β)`'s Group-3 axiom can
> corroborate that no two simultaneous proofs exist for a Rank-1\* sentence and
> its negation. This is an important distinction, because the **First**
> Incompleteness Theorem indicates no decision procedure exists for separating
> all true from false Rank-1\* sentences.

This is the Level-0 / Level-1 distinction with its *reason*, in Willard's own
words, and it is the best available justification for treating the levels as a
codification axis rather than a presentational variation. Obligation **O41**.

### 3.13 The motivation layer (§2, §9)

§2 assembles the historical case that neither Hilbert nor Gödel regarded the
consistency programme as wholly terminated: Hilbert's statement ∗ [26]; Gödel's
1931 caveat ∗∗ [20] ("there might conceivably be finite proofs which cannot be
stated in P"); Gödel's 1933 Vienna lecture [21]; von Neumann arguing "against
Gödel himself" (Yourgrau [58] p. 58); Gerald Sacks's recollection [39] that
Gödel thought the programme "very much alive and even more interesting than it
initially was"; Anil Nerode's report [32] of Tennenbaum's similar conversations;
and Harvey Friedman's YouTube lecture [18] expressing openness to partial
exceptions. **Willard's repeated observation that Gödel published only ~85
pages** is his explanation for why the private hesitancy is little known.

§9 ("Ironic Events") records that LFCS-2020 met 4–7 January, before Covid was
widely known, and argues that self-justifying formalisms matter for future AI
systems. This is motivation-layer material with no mathematical content; it is
where `Willard2013`/`Willard2014`/`Willard2018`'s speculative line terminates.

## 4. Numbered-item inventory

Anchors are **PDF** pages (printed = PDF − 1).

| Label | p. | Statement digest | Proof |
| --- | --- | --- | --- |
| Example 3.1 | 6 | Four apparatuses `D_E`, `D_H`, `D_M`, `D_F`; **all common apparatuses prove the same theorems from a common α, at different proof lengths** (footnote 2: by Gödel Completeness) | n/a |
| Definition 3.2 | 6 | `GenAC` (α, D) is **Self-Justifying** iff (i) a theorem or axiom states `D` applied to α is consistent, and (ii) (α, D) is in fact consistent | n/a |
| Example 3.3 | 7 | `α_D = α + SelfRef(α,D)`, the ⊕ "I AM CONSISTENT" axiom; Kleene [28] first, and Kleene/Rogers/Jeroslow all noted α_D is typically inconsistent | n/a |
| Definition 3.4 | 7 | 3-way `Add`/`Mult`; totality axioms (1)–(3); **Type-M / Type-A / Type-S / Type-NS** | n/a |
| Theorem ++ | 8 | (Solovay's modification of Pudlák, using Nelson and Wilkie-Paris methods) No Type-S GenAC with successor satisfying `x'≠0`, `x'=y' ⇔ x=y` can verify its own consistency when `D` is Hilbert-Frege and α treats addition and multiplication as 3-way relations with the usual axioms | cited |
| Remark 3.5 | 8 | Frames ++ and its consequences: some Type-NS systems verify consistency under Hilbert-Frege; some Type-A systems do so under tableau; most Type-M systems cannot (refining ++ with Adamowicz–Zbierski [2]) | cited |
| Remark 3.6 | 9 | Other evasion routes: Gentzen/Kreisel-Takeuti cut-free [19,29]; interpretational (Adamowicz, Bigorajska, Friedman, Nelson, Pudlák, Visser); **Artemov's Infinite-Range perspective [4]** (an infinite schema of theorems replacing one consistency theorem) | n/a |
| Definition 4.1 | 10 | Bounded quantifiers; `Δ*₀`, `Π*ₙ`, `Σ*ₙ` without multiplication symbols | n/a |
| Remark 4.2 | 10 | **Rank-1\*** = encodable as either `Π*₁` or `Σ*₁` | n/a |
| Definition 4.3 | 13 | `IS_D(•)` is **Consistency Preserving** iff `IS_D(β)` is consistent whenever all β's axioms hold in the standard model | n/a |
| Theorem 4.4 | 13 | `IS_{Tab-1}(•)` and `IS_{Tab}(•)` are consistency preserving | cited ([50]; §5 gives intuition only) |
| Theorem 4.5 | 13 | `IS_{Xtab}(•)` fails; `IS_{Xtab}(β)` is automatically **inconsistent** when β proves the usual Π\*₁ associativity/commutativity/distributivity/identity theorems | sketch (§6 via ⊙) |
| Example 5.1 | 14 | `IS^M_{Tab}(•)` — `IS_{Tab}` plus a multiplication function symbol — loses consistency preservation; the `xₙ = 2^{n+1}` vs `yₙ = 2^{2ⁿ}` growth contrast explains why | sketch |
| Statement ⊙ | 17 | Any `A` is automatically inconsistent given (I) Successor total, (II) the usual 3-way associativity/commutativity/distributivity/identity, (III) a theorem affirming its own Xtab consistency | stated-only (rests on Hájek–Pudlák pp. 172–174) |
| Definition 7.1 | 18 | **Z-Enriched** tableau: `Υ ∨ ¬Υ` (Line 9) admitted as a logical axiom for any sentence `Υ ∈ Z` | n/a |
| Definition 7.2 | 18 | **Z-Base Variable Enriched** tableau: `∀x Υ(x) ∨ ¬Υ(x)` (Line 10) for formulae `Υ(x) ∈ Z` free in `x` | n/a |
| §7 Δ\*₀ result | 19 | `Willard2001`'s tableau evasions extend to Z-Enriched and Z-Base-Variable-Enriched proofs when `Z = Δ*₀` | stated-only |
| §7 Π\*₂ result | 19 | `Willard2004`'s results extend to show the evasions **fail at and above `Π*₂`** | stated-only |
| §7 Π\*₁ conjecture | 19 | The `Δ*₀` evasions are conjectured to continue at the `Π*₁` level — "not yet been formally proven" | stated-only |
| §8 Res/Xres | 20 | `IS_{Res}(•)` is consistency preserving; `IS_{Xres}(•)` is not | stated-only |
| Appendix | 24 | Semantic tableau proof: root `¬Ψ`, all root-to-leaf paths closed; six elimination rules for `∧ ∨ → ¬ ∀ ∃`, plus bounded hybrids (a) and (b); Xtab adds `℧ ∨ ¬℧` at any node | n/a |

**Unnumbered but load-bearing**: `GenAC` and the `L_D` / rules decomposition
(p. 6); Non-Growth, Grounding and U-Grounding functions and `L*` (pp. 9–10);
Tab / Xtab / Tab-1 (pp. 11–12); Level-1 appreciation of self-consistency
(p. 11); the `IS_D(β)` four groups and Eq. (5) (pp. 12–13); the Linear-Sum
Effect and its four-step Xtab construction (pp. 16–17); Hilbert's ∗ and Gödel's
∗∗ (pp. 4–5).

## 5. Notation table

Feeds `../registry/notation.md`.

## 6. Replicated context

- The Hilbert ∗ / Gödel ∗∗ pairing is set up in §2 and returned to in §9 and
  §10 — three passes over one point.
- The tableau definition appears **twice in full**: §4 p. 11 and the Appendix
  p. 24.
- The `xₙ` vs `yₙ` growth contrast is stated three times in §5 (pp. 14, 15, 16).
- Sacks's YouTube quotation appears in footnote 1 (p. 5) and again inside
  reference [39] (p. 27).
- **What is *not* replicated**: the fixed-point encoding, the grounding-function
  axioms, and the proofs of Theorem 4.4 are all delegated to [47] and [50]. This
  paper is a summary; it carries almost no self-contained machinery.

## 7. Discrepancies and errata

1. **`Willard2001`'s title is mis-cited again.** Ref. [47] reads "Self-verifying
   systems, the incompleteness theorem and **the tangibility reflection
   principle**". The witness's own title page reads "Self-Verifying Axiom
   Systems, the Incompleteness Theorem and **Related Reflection Principles**".
   This is the *same* error `Willard2005` made — so it is a systematic
   self-citation error persisting **19 years**, not a one-off. Strengthens
   **D20**.
2. **`2^{n+1}` should be `2ⁿ + 1`** (p. 15): "y_n's binary encoding has a
   `2^{n+1}` length" contradicts `Log(yₙ) = 2ⁿ` three lines above. Image-verified.
   Harmless to the argument.
3. **Theorem 4.4's `Tab` case has no cited source.** [50] is credited for
   `Tab-1` only; the `Tab` case is asserted. Gap **G20**.
4. **Ref. [46] calls the 1993 venue the "Third Kurt Gödel Colloquium"**, where
   `Willard2001` ref. [40] called it the "Third Kurt Gödel **Symposium**".
   Willard is inconsistent about Symposium/Colloquium across papers; the C7
   erratum about the *1997* venue (Fifth, not Third) is unaffected.
5. **Subtraction is defined with `x − y = 0` when `x ≤ y`** (p. 9), where
   `Willard2001` p. 9 wrote `x < y`. Equivalent (both give 0 at `x = y`).
6. **Six grounding functions with `Root` as a ceiling** here, against
   `Willard2001`'s seven with `Root` a floor and an extra `Predecessor`, while
   `Log♠ = ⌈Log₂(x+1)⌉` **matches** 2001 and the ceiling `Root` matches 2005.
   The set is a genuine mix across papers — further evidence for **D23** that
   these choices are not load-bearing.
7. **No prenex\* requirement on the tableau root** (Appendix, p. 24), where
   `Willard2001` §4 required the root to store `¬Φ` rewritten in prenex\* form.
   Since 2001's footnote 8 leaned on that normalisation to force the closing
   pair to be `Δ⁻₀`, the omission matters and must not be silently reconciled —
   flag at C13 (**O42**).
8. **`Willard2013`-style speculative material is present in a mathematical
   paper** (§9's Covid and AI passages). Not an error; noted so the codified
   statement's motivation layer draws from §2 and §10 rather than §9.
9. Several of `prose/willard2020draftreview.txt`'s corrections were applied and
   at least one was not — p. 7 still reads "Self Justifying" unhyphenated where
   Definition 3.2 hyphenates it.

## 8. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Full read of all 28 pages from the text layer; form-feed-stripped awk sweep for numbered items and headings | 16 numbered items + 5 unnumbered results |
| 2 | 2026-08-26 | Visual control pass, `pdftoppm -r 130 -png`, pp. 10, 12, 15, 17 | 0 new items; **1 text-layer correction** (⊙ rendered as `J`); erratum 2 confirmed as genuine |
| 3 | 2026-08-27 | Completion read of pp. 1–3 | §1's Declarative / Infinite-Ranged material (§2a above); coverage now complete |

Zero-new-items re-pass **not yet run** — saturation is **open** pending a third
pass (charter criterion A1).
