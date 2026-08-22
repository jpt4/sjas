# Drift Ledger

Cross-paper discrepancies, definitional drift, and bibliographic identity
questions — **retained, never silently resolved**. Each entry states the
conflicting readings with anchors; where the codified statement must choose a
presentation, it cites the entry and states both readings. Populated chiefly
during extraction (C4+) and the C6 subsumption audit; seeded now with what
the C1/C2 passes established.

Format per entry: id, topic, the readings with sources/anchors, status
(`open` | `explained:YYYY-MM-DD` — an explanation is recorded but the variant
readings remain on record).

---

## D1 — TR 93-10 citation identity ("March 1994, 50 pages")

- Reading 1: the scanned artifact titles itself "Technical Report 93-10"
  (title page) and its preface dates the published KGC abbreviation to
  25 Aug 1993 (`papers/1993technicalreport/`, collated witness pp. 1-2).
- Reading 2: JSL 2001 ref. [41] cites "Self-Verifying Axiom Systems and the
  Incompleteness Theorem, SUNY-Albany Technical Report, March 1994. (This
  50-page technical report expands the abbreviated proofs in the 12-page
  Extended Abstract [40].)"
- Working identification: same artifact under a loose date/page description
  (55 printed pages + 4 figure pages vs "50 pages"; series number 93-10 vs
  issue date March 1994).
- **Resolved by C5 (2026-08-21).** The TR's own reference list (printed p. 55,
  read from the page image) contains `[So94] R. Solovay, private
  communications (**April 1994**)`, cites the already-published KGC chapter as
  `[Wi93]`, and forward-references `[Wi94]` as a forthcoming report. A report
  citing April-1994 communications cannot have been finalized before April
  1994. The document is therefore a **1994 text carrying a 1993-series report
  number**, which is exactly why Willard cites it in JSL 2001 as a
  "SUNY-Albany Technical Report, March 1994". Both readings describe one
  artifact; neither is an error.
- Residual minor discrepancies, recorded and not pursued: "March" (JSL 2001)
  versus April-or-later (TR content); "50-page" (JSL 2001) versus 55 printed
  pages plus four figure pages.
- Status: explained:2026-08-21 (gaps ledger G9 closed).

---

## D7 — The deduction method becomes a definitional parameter

- Reading 1 (`Willard1993`, p. 325): "Define an axiom system `A` to be
  **self-verifying** iff (i) one of the theorems implied by `A` is the
  statement of its own consistency, and (ii) the axiom system is in fact
  consistent." No deduction method appears.
- Reading 1' (`Willard1993-TR`, printed p. 10): the *notation* `IS^s_d(A,G)`
  does carry a deduction parameter `d`, "omitted, indicating the deduction
  corresponded by default to semantic tableaux"; and `Willard1993` §6
  proposes `IS(A,g,d)` for "IS-like systems".
- Reading 2 (`Willard2005`, p. 2): the pair `(α, D)` is definitional —
  "Introspectively Unified Logic" is a property of the pair, and
  "**Self-Justifying**" is a property of α, namely that some frequently
  employed `D` makes the pair introspectively unified.
- Consequence: "self-verifying" (1993, a property of a system) and
  "self-justifying" (2005, a property of a system quantified over methods) are
  **not the same predicate**, and the intermediate notion — the pair — has no
  1993 counterpart in the definition, only in the notation. The codified
  statement must fix one vocabulary and record the other two.
- Status: open.

---

## D8 — `IS*(A)` versus `IS^{Σ₁}(A)` within the 1993 pair

- `Willard1993` p. 330 names the Σ₁-strengthened system `IS*(A)`.
- `Willard1993-TR` printed p. 9 names the same system `IS^{Σ₁}(A)`, and uses
  the superscript slots systematically (`IS^{LΔ₀}`, `IS^{LΣ₁}`, `IS^{UΣ₁}`,
  `IS^{UΔ₀}`, `IS^{LΠ₁}`).
- The chapter's `IS*` collides visually with nothing in 1993 but must not be
  confused with `Willard2001`'s or `Willard2005`'s starred *formula classes*
  (`Π*₁`, `Σ*₁`), where the star means something entirely different.
- Status: open.

---

## D9 — The formula classes are defined differently in the two 1993 witnesses

- `Willard1993-TR` (printed p. 4): a quantifier is **bounded** iff it occurs
  as `∃v ≤ z` or `∀v ≤ z` with `z` a constant or variable symbol; a wff is
  **Δ₀** iff all its quantifiers are bounded; the canonical **Π₁**/**Σ₁**
  forms are a universal/existential prefix over a Δ₀ matrix.
- `Willard1993` (p. 327): Φ is **Π₁** iff "each variable `x` introduced by an
  existential quantifier in that sentence is required to either have a value
  less than or equal to the **maximum of the universally quantified variables
  enclosing it**, or `x` is required to be less than a prespecified constant"
  — a majorization-style condition, and the chapter adds "(The Δ₀ sentences
  are defined slightly differently in some textbooks.)"
- These are close but not identically stated, and the results are sensitive to
  exactly this class. The TR's form is the one that survives into
  `Willard2005`'s `Π*₁`/`Σ*₁`.
- Status: open — the codified statement must adopt one and note the other.

---

## D10 — The Group-1 function list differs between the two 1993 witnesses

- `Willard1993` (p. 326): addition, subtraction, division **plus eleven**
  further functions — `Count`, `Shift`, `Remove`, `Extract`, `Compress`,
  `Andreverse`, `Andmacro`, `Andmultiply`, `Andexpand`, `Address`, `Width`
  (fourteen in all).
- `Willard1993-TR` (printed p. 3): "The **eight** formally defined functions of
  `G₀` will consist of Addition, Subtraction, Division, plus… `StringCount`,
  `Shift`, `Extract`, `Andreverse`, `Address`."
- The TR is the later and more refined witness (it cites the chapter), so the
  reduction from fourteen to eight is a deliberate simplification, not an
  omission by the abstract.
- **Relativized by TR Remark 6** (printed p. 34): `G₀`'s definition "was quite
  arbitrary", and the theorems extend to *any* set of **slowly growing**
  functions (`f̂[i,j](x̄) ≤ i·2^j·Max(x̄)`). The lists therefore do not compete;
  the growth class is the invariant. Same relativization applies to D11.
- Note `Andmultiply(x,y,z)` in the chapter's list — multiply, then mask with
  `z`, hence non-growth: a *bounded* multiplication was admissible from the
  start, which sharpens what "multiplication is excluded" actually means.
- Status: open.

---

## D11 — The 1993 and 2005 signatures share only three function symbols

- `Willard1993-TR` `G₀` (eight): Addition, Subtraction, Division,
  StringCount, Shift, Extract, Andreverse, Address — bit-string manipulation.
- `Willard2005` U-Grounding (eight): Subtraction, Division, Root, Maximum,
  Logarithm, Count, Addition, Double — arithmetic-flavoured.
- Common: **Addition, Subtraction, Division only** (1993's `StringCount(x,y,j)`
  counts repetitions of `y`'s `j`-bit encoding in `x`; 2005's `Count(x,j)`
  counts `1` bits among `x`'s rightmost `j` — different functions).
- Both have exactly eight members and both obey the same non-growth
  discipline, which makes the coincidence of *cardinality* a trap: any claim
  of the form "Willard's system recognizes exactly these eight functions" must
  name its source paper.
- **Largely dissolved by the C5 appendix pass.** Three findings converge:
  (i) TR Remark 6 — `G₀` is "quite arbitrary"; any **slowly growing** set works;
  (ii) TR Appendix A's Added Comment (printed p. 52) — only **four** of the
  eight are needed for the arithmetization ("our discussion technically only
  needed `G₀`'s atomic functions of Addition, StringCount, Shift and Extract…
  The sole purpose of `G₀`'s functions of Andreverse, Address, Subtraction and
  Division… was to simplify the presentation");
  (iii) several 2005 primitives are 1993 *derived* functions — `Double` is
  1993's `Scalar₂` ("`2x, 3x, 4x`" abbreviate `Scalar₂, Scalar₃, Scalar₄`), and
  `Maximum`/`Minimum`, primitive in 2005, are derived in 1993 as
  `x + (y−x)` on truncated subtraction.
  So the two signatures are related by promotion and derivation within one
  growth class, not by disagreement. What must still be said explicitly is that
  neither list is canonical.
- Status: open (reduced to a presentational obligation, O10/O13).

---

## D12 — Addition-totality changes logical form between 1993 and 2005

- `Willard1993-TR` printed p. 4: "The axiom indicating that the addition
  function is total can obviously be written as a **Π₂ sentence**. Since all
  `G₀`'s other functions are non-growth, its remaining axioms can be written
  as Π₁ sentences."
- `Willard2005`: addition and `Double` are **function symbols of the
  U-Grounding language**, so the corresponding axioms are Π\*₁ — which is what
  permits Definition 4's Normed class to require *every* axiom to be Π\*₁ or
  Σ\*₁, and hence permits Theorem 1's proof to assert that "all these axioms
  can be encoded as Π\*₁ sentences".
- Reading: the U-Grounding *function* language exists precisely to eliminate
  the 1993 Π₂ axiom. 2005 §2 says as much ("our notation is much simplified
  under a language employing function symbols also for addition and Double")
  without stating the Π₂→Π\*₁ consequence, which is the load-bearing part.
- Status: open — a genuine explanatory point the codified statement should
  make explicit.

---

## D13 — The affinity/duplication insight has a 1993 origin

Recorded against prior in-house work, not against Willard.

- Reading 1 (`docs/adr/ADR-0001-affine-tree-sjas.md`, "Decisive Finding"; and
  `docs/design/affine-tree-sjas.md` §4): the addition/multiplication boundary
  is, on a native carrier, the **affinity/duplication** boundary — a variable
  occurring twice in a term — presented as a finding produced by transposing
  Willard's engine onto trees, and sourced to `Willard2005` (Definitions 4–5,
  Theorem 1, §5.2, Remark 4) with `Willard2001`/`Willard2020`.
- Reading 2 (`Willard1993`, p. 328): Willard states the identification himself,
  in the arithmetic setting. `∀x∃z : z = x*x` fails the memory argument, and:
  "At first, it may appear that this counterexample is artificial because it
  requires at least **two occurrences of the same variable `x`** on the right
  sides of (2.4). However, the proof of Gödel's Incompleteness Theorem used
  essentially the same **double appearance of a variable** when it provided a
  counterexample to self-verification via a diagonalization argument."
- Assessment: the affine-tree insight is **corroborated, not contradicted** —
  which strengthens it, since the design's central identification turns out to
  be Willard's own. What needs correcting is only the attribution: the
  documents present it as a discovery of the transposition and do not cite the
  1993 passage. The codified statement should attribute it here.
- Status: open (composition obligation O9).

---

## D14 — Proof compression: Willard permits pointer sharing, the NC-SJAS plan forbids it

- Reading 1 (`Willard1993`, p. 327): "our results are slightly **strengthened**
  if we assume one is allowed to slightly compress a proof `p` by physically
  writing the bit representation of any 'long constant' `k` only once and
  storing its other appearances as **pointers** to this long representation."
- Reading 2 (`native-computational-sjas-plan.md`, cost rules; carried into
  `docs/design/affine-tree-sjas.md` §6 and its DAG negative control): "No DAG
  sharing, memoization, normalization, or host callback may count as free
  proof compression", and DAG-compressed certificates are rejected.
- These are not in contradiction — Willard's allowance is a bounded
  constant-sharing that makes the *encoding* more efficient and hence the
  bit-counting argument tighter, whereas the plan's rule forbids sharing that
  would make *proof size* an unfaithful cost measure. But they point opposite
  ways rhetorically, and the codified statement should record what Willard
  actually permits rather than importing the implementation-side rule.
- **Sharpened by the C5 visual pass.** In the TR the sharing is not an optional
  strengthening but part of the official encoding: printed p. 15 introduces the
  symbols `u#` and `v#` that "act as **pointers to constants**", and Lemma 5.2's
  parenthetical gives the reason — "the two analogous mappings *onto*
  `⌜Φ(ĉ[j])⌝` **are unknown by `IS(PA+)` to be total functions**". Expanding a
  long constant in place is precisely what the system cannot prove total, so
  the pointer form is a **totality device**, not a cost optimisation. Any
  transposition that bans sharing outright must supply its own answer to the
  problem the pointers solve.
- Status: open.

---

## D2 — Willard2006a cited under a pre-publication title

- Reading 1: `Willard2005` reference [72] (p. 43) cites "A new variant of
  Hilbert styled generalization of the second incompleteness theorem and some
  exceptions to it", Annals of Pure and Applied Logic (2006), noting that a
  200-word abstract appeared at the 2nd St. Petersburg Conference on Logic and
  Computability (2003).
- Reading 2: the published article (corpus key `Willard2006a`) is "A
  generalization of the Second Incompleteness Theorem and some exceptions to
  it", APAL 141 (2006), pp. 472–496.
- Working identification: same article; the 2005 citation uses a
  pre-publication title that names the Hilbert-styled framing explicitly.
  Confirm during the C9 extraction of `Willard2006a`; note that the dropped
  words ("new variant of Hilbert styled") are exactly the ones that locate the
  paper on the apparatus axis, so the pre-publication title is the more
  informative one for the concordance.
- Status: open.

---

## D3 — Three distinct constants around the compactification exponent

Within `Willard2005` the following are separate quantities and are easy to
conflate (see the C4 fidelity check, F1/F2):

- **6** — bits per language symbol in the Gödel encoding (Appendix A, p. 39: a
  "byte" is six bits; base-64; 24 symbols coded 32–55);
- **5** — the constant in Eq. (20), `U-Height(p) < (1/5)·Log₂(p)`, bounding
  accumulated U-Grounding *function* symbols along a branch, not all symbols;
- **1/4** — the compactification exponent θ actually proven (Theorems 2, 4).

The admissible window is `1/d ≤ θ < 1/3` for coding density `d`, non-empty
exactly when `d > 3`. Willard's Eq. (20) gives `d = 5`, and he selects
θ = 1/4 inside `[1/5, 1/3)`.

- Cross-paper link to confirm at C6: Willard 2011 Definition D.1 part (iv)
  promotes the Eq. (20) condition to a definitional clause admitting "any
  natural method" satisfying ≥ 5J bits for J function symbols (Gödel number
  ≥ 32^J). If confirmed, 2005 Eq. (20) and 2011 D.1(iv) are the same condition
  under two presentations, and the codified statement should present them as
  such.
- Status: open (C6 confirmation pending).

---

## D4 — The Solovay private-communication thread

Not a discrepancy but a provenance chain that must survive codification.

- `Willard2005` reference [52] (p. 42): "Several private telephone
  communications during April of 1994 describing Solovay's generalization of
  one of Pudlák's theorems [41], using the additional formalisms of Nelson and
  Wilkie-Paris… Solovay never published this result (which we call Theorem ∗
  in Section 4) or any of his other observations about 'Definable Cuts'."
  Echoed in the Acknowledgment (p. 38).
- `Willard2020-LFCS` reference 41 cites the same April-1994 telephone
  conversations.
- The only expositions of Theorem ∗ are Willard's own: a 4-page interpretation
  in `Willard2001` Appendix A, and §4 here.
- Consequence: Theorem ∗ enters the codified statement as a **cited,
  unpublished** result whose sole exposition is Willard's, and the result
  matrix must mark it accordingly.
- Status: explained:2026-08-21 (chain recorded; expositions to be compared at
  C7).

---

## D5 — `IS(A)` versus `IS_D(A)`

- `Willard2001` defines `IS(A)`: Level(0-) self-consistency, semantic tableaux.
- `Willard2005` defines `IS_D(A)`: Level(1) self-consistency, apparatus
  parameter `D`, Tab−U\*₁−List as the headline instance; §7 (p. 37) states the
  two advances explicitly.
- The names differ by one subscript while the group architecture is shared and
  Group-1/Appendices are delegated back to `Willard2001`. The codified
  statement must keep them typographically distinct and state the delta once.
- Confirm the exact `IS(A)` definition at C7.
- Status: open.

---

## D6 — Density/θ relation misstated in the affine-tree re-derivation

Recorded against prior in-house work, not against Willard. Full analysis: the
C4 fidelity check (`../extraction/willard2005-fidelity-check.md`).

- Reading 1 (`docs/adr/ADR-0001-affine-tree-sjas.md`, Decisive Finding): "The
  ratio yields the compactification exponent θ ≤ 1/5 < 1/3 that Theorem 1
  needs." Reading 1' (`docs/design/affine-tree-sjas.md` §3): "θ = 1/c < 1/3
  follows" with `c ≥ 4`; and "their ratio is θ ≤ 1/c < 1/3".
- Reading 2 (primary source): coding density bounds θ from **below**
  (`θ ≥ 1/d` via Lemma 1's hypothesis, footnote 5 p. 27), while Definition 5
  bounds it from above (`θ < 1/3`). Willard proves θ = 1/4 with `d = 5`.
- Assessment: the design conclusions survive — density 5 yields the window
  `[1/5, 1/3)`, the claimed (1/5)-compactification is the tightest admissible
  choice in that window, and the design's own remark that "density 3 is the
  θ = 1/3 boundary itself" is *correct* under the corrected reading (it is the
  `d > 3` condition). What is wrong is the stated derivation: a window is
  collapsed to a point and one inequality is inverted. The design document is
  internally inconsistent, stating the relation correctly in §8 ("lands at 1/4
  from a coding of ≥ 4–5 bits/symbol") and incorrectly in §3–§4.
- Also recorded there: the affine-tree §10 treatment of cut-free sequents as an
  established affirmative case drops the proof-status caveat that the 2005
  witness (Remark 1) is stated-only. The apparatus survey states it correctly.
- Additional obligation for the codified statement: Theorem 1's final step
  `m − 1 > m^{2θ}` is glossed as immediate by Willard *and* by the
  re-derivation, but needs `m ≥ 3` when θ = 1/4. Harmless (m is a proof's
  Gödel number) but it should be discharged explicitly rather than inherited.
- The affine-tree documents belong to another branch and are **not edited** by
  this workstream.
- Status: open (carry into C14–C15 composition).

---

## D15 — Why multiplication is fatal: two different explanations

- Reading 1 (`Willard1993-TR` §12, printed p. 32): multiplication-totality lets
  the system prove `∀a∀b∃c c=(ab)^λ`, which upgrades a bounded reflection
  statement into **all** local Π₁ reflection statements (Lemma 12.1) — and
  Proposition 5 has already shown local Π₁ reflection to be inconsistent. The
  fatal step is *derivability of a reflection principle*.
- Reading 2 (`Willard2005` Remark 4, p. 21): multiplication as a ninth function
  symbol makes value growth "overwhelm Definition 5's constraints", so no
  θ-compactification exists. The fatal step is *envelope arithmetic*.
- Both are Willard's, neither supersedes the other, and they are not obviously
  the same argument. The codified statement should present them as two routes
  to one boundary rather than paraphrasing either as *the* reason multiplication
  is excluded.
- Status: open (composition obligation O14).

---

## D16 — Axiom versus theorem: a recurring structural device

Not a discrepancy but a device the corpus reuses, which the codified statement
should name once rather than re-explain per paper.

- `Willard1993-TR` Appendix B (printed p. 54): INVALID.1 carries LΠ₁ reflection
  **as an axiom schema**; INVALID.2 has the same sentences only **as theorems**
  (Lemma 12.1). Under a cut-free apparatus this is not cosmetic — "semantic
  tableaux proofs allow only axioms (and their 'cut-free' deductions) to appear
  in the intermediate stages of a proof (i.e. provable theorems are
  disallowed)", so Figure 4 must reconstruct "the essential cut-free
  implications" instead of citing the principle.
- `Willard2005` §6.1 (pp. 30–32): `IS_D(A)` *proves* `Υ(k,m)` while
  `NS^{k,m}_D(A)` has it *as a Group-0 axiom*; both can prove it, but only the
  latter can use it in one step, and the proof-length gap `C > 2^m_k` changes
  what the Group-3 "this" denotes — producing opposite consistency results
  (Theorems 5 and 6).
- Same device, twelve years apart, on different principles (Π₁ reflection;
  `Υ(k,m)`). The codified statement should present *axiom-versus-theorem under
  a cut-free apparatus* as a named mechanism of the boundary.
- Status: open.

