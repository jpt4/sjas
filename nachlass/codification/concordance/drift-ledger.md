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
- **Settled by the C5 completion pass.** Willard states the dependency outright
  at printed p. 18: "**Lemma 5.3 would not follow, had this section employed
  slightly different definitions (such as omitting the symbol `u#`)**, because
  `T(p,k) ≤ L_k p` could then fail for all constants `L_k`." He also scopes the
  loss: "even without such a formalism supporting Lemma 5.3, most of our seven
  main theorems, **including Propositions 1, 2a, 3, 5, & 7 will remain valid**"
  — so Propositions 2b, 2c, 4 and 6 are the ones that depend on it. The
  constant-pointer compression is therefore **necessary to four of the origin
  results**, and a no-sharing cost model must either replace it or give those
  results up.
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

---

## D17 — Cut-free object systems, cut-using metatheory

- `Willard1993-TR` Remark 2 (printed p. 24): "our proofs shall *apply a cut
  rule at the meta-theoretical level*… we will *often omit constructing
  formally the cut-free proofs* of such theorems β: Instead, *their theoretical
  existence* will be established implicitly via the Cut Elimination Theorem."
- The object systems are cut-free (that is the whole point of Propositions
  1–4); the reasoning *about* them uses cut freely. This is not a contradiction
  — the metatheory is not the object system — but it is easy to misread, and it
  qualifies the proof-status of every result whose proof invokes Meta-Logic.
- It also sharpens D16: the axiom-versus-theorem distinction bites inside the
  system precisely *because* cut is unavailable there.
- Status: open — the codified statement should state the convention once, where
  it presents the origin proofs.

---

## D18 — "Self Justifying" changes referent between 2005 and 2011

- `Willard2005` (p. 2): the **pair** `(α, D)` is an "**Introspectively Unified
  Logic**"; an axiom system α is "**Self-Justifying**" iff *there exists* some
  frequently employed `D` making the pair introspectively unified.
- `Willard2011` (§1): "The ordered pair `(α, d)` will be called **Self
  Justifying** when: i … ii …" — i.e. 2011's *Self Justifying* **is** 2005's
  *Introspectively Unified Logic*, and 2005's *Self-Justifying* (the
  existential over `d`) has no 2011 name.
- With `Willard1993`'s **self-verifying** (a property of a system, no `d` at
  all — drift D7), the corpus now carries **three** closely-named,
  non-equivalent predicates. The codified statement must fix one vocabulary
  and tabulate the other two.
- Status: open.

---

## D19 — A third formulation of Solovay's unpublished theorem

| Source | Statement |
| --- | --- |
| `Willard1993-TR` p. 10 | No consistent **Gentzen-style sequent calculus system employing cuts** can prove a finite set `F` of PA's Π₁ theorems, recognize Subtraction, non-zero Division and Successor as total, and prove its own consistency |
| `Willard2005` Theorem ∗, p. 14 | No consistent system satisfying the **"Successor-Based Arithmetic" criteria** (footnote 1) can prove the non-existence of a **Hilbert** proof of `0=1` from itself |
| `Willard2011` Theorem 2.1, p. 4 | No α containing the **Type-S** axiom (with `x'≠0`, `x'=y' ⇔ x=y`) can recognize its own **Hilbert** consistency while treating `+`, `∗` as 3-way relations with the usual identity/associative/commutative/distributive properties |

All three are attributed to the same unpublished April-1994 private
communication (drift D4). They are not obviously interchangeable: the 1993
version is about sequent calculus with cuts and names three total functions;
the 2005 and 2011 versions are about Hilbert deduction and differ in how they
characterize the admissible systems. The codified statement must present
Theorem ∗ / Theorem 2.1 with its formulation named, not as a single fixed
result.

- Status: open.

---

## D20 — `Willard2001`'s title is mis-cited by Willard and by our inventory

- The paper's own title page reads: "**Self-Verifying Axiom Systems, the
  Incompleteness Theorem and Related Reflection Principles**".
- `Willard2005` ref. [67] cites it as "Self-verifying systems, the
  incompleteness theorem and **the tangibility reflection principle**".
- `Willard2011` ref. [61] gets the subtitle right but writes "Self-Verifying
  **Systems**" for "Self-Verifying **Axiom** Systems".
- Our `nachlass/paperlist` inherited the 2005 form, and `registry/corpus.md`
  described the paper by it.
- **Corrected in C6** in both files, with this entry recording the provenance
  of the error. The "tangibility reflection principle" is a *construct within*
  the paper (and the title of the KGC-5 chapter `Willard1997`), not the 2001
  title — a confusion easy to inherit because both papers exist.
- Status: explained:2026-08-21.

---

## D21 — The four paradigms and the four configurations do not correspond element-wise

- `Willard2011` p. 3 says Theorems 5.9, 5.11 and 6.6 "unite these four
  paradigms", citing **[59, 64, 66, 68]** = `Willard1993`, `Willard2005`,
  `Willard2006a`, `Willard2009`.
- Appendix D supplies **four configurations** ξ\*, ξ\*\*, ξ⁻, ξ^R.
- But Willard's own attributions map ξ\* **and** ξ\*\* to `Willard2005`
  (§5 and §5.3), ξ⁻ to `Willard2006a` Theorem 3, ξ^R to `Willard2009` —
  leaving **`Willard1993` without a configuration of its own**.
- This is defensible (1993 and 2005 §5 are the same Type-A/tableaux paradigm,
  both covered by ξ\*), but the two lists enumerate different things — papers
  in one case, paradigms in the other. The codified statement must not present
  a four-to-four correspondence.
- Status: open (full analysis in `../extraction/willard2011-subsumption-audit.md` §2.2).


## D22 — `Willard2001`'s witness has a post-publication bibliography

| Source | Statement |
| --- | --- |
| Witness cover note | "The text in this pdf file is identical to my JSL 2001 article, except that I have used a larger type faunt" |
| Witness ref. [45] | "A Generalization of the Second Incompleteness Theorem and Some Exceptions to It, Annals of Pure and Applied Logic 141 (**2006**) pp. 472-496" |
| Witness ref. [44] | "A longer more detailed version of this paper **will appear soon in the JSL**" |

- A 2001 article cannot cite a 2006 one, so the claim of textual identity fails
  at least for the reference list; [45] appears **nowhere in the body**, which
  is consistent with a later insertion. [44]'s surviving forward reference shows
  the update was partial rather than a wholesale revision.
- Consequence: this witness is authoritative for the mathematics and for
  Willard's own later citation practice, **not** for the JSL 2001 text. Cite
  section and item labels; never JSL page numbers from this witness.
- Status: open (gap G15).

## D23 — Seven grounding functions versus six

| Source | Statement |
| --- | --- |
| `Willard2001` p. 9 | **Seven**: Subtraction, Division, **Predecessor**, Maximum, Logarithm, Root, Count. `Logarithm(x) = ⌈Log₂(x+1)⌉`, `Root(x,y) = ⌊x^{1/y}⌋` |
| `Willard2005` | **Six** (no Predecessor), with `⌊Log₂(x)⌋` and `⌈x^{1/y}⌉` — **opposite roundings on both** |
| `Willard2011` | ξ⁻ is described as having "six" |

- Both the count and the roundings differ. **Neither is load-bearing**, and
  `Willard2001` says so itself (p. 10): Group-1 may be expanded by "any larger
  set `F` of additional non-growth functions that is axiomatized by any finite
  set of Π⁻₁ axioms", with all theorems preserved; Remark C.5 adds that even
  such extensions keep the Group-3 axioms Π⁻₁.
- The only real constraint is **non-growth** (`F(a₁,…,a_j) ≤ Maximum(a₁,…,a_j)`)
  plus finite Π⁻₁ axiomatisability. The codified statement should present that
  condition and exhibit one concrete set, not adjudicate between six and seven.
- Status: **resolved** — presentational, not substantive. Recorded so a reader
  comparing papers does not mistake it for a divergence.

## D24 — The admission condition on the base system has three names

| Source | Statement |
| --- | --- |
| `Willard2001` p. 12 | `A` is **Regularly Consistent** iff `Prf_A(x,y)` has a Δ⁻₀ encoding **and** the Group-2 axioms (7) are **valid in the Standard Model** for every Π⁻₁ sentence `Φ` |
| `Willard2005` | `A` is **Π\*₁-true** |
| `Willard2011` | the base `B^ξ` is a Σ^ξ₁-complete set true in Standard-M |

- These are not the same predicate. 2001's condition is a conjunction that
  carries an **encoding requirement** (Δ⁻₀ `Prf_A`) alongside the semantic one;
  2005's and 2011's are purely semantic, with the encoding requirement carried
  separately (2011 by the generic configuration's `g` component).
- Note also that "regularly consistent" names a **soundness** condition, not a
  consistency condition — the word is misleading in isolation.
- Status: open — C13 must state one admission condition and tabulate the rest.

## D25 — The coding-density constant, apparently 6 / 5 / 6-over-5 / 32

| Source | Statement |
| --- | --- |
| `Willard2001` pp. 22, 28, 51 | `2^{⌊β_i/6⌋−2}`; `β_i − β_{i−1} ≥ 6`; "a byte … consisting of **six bits**"; formula encodings in **base 64** |
| `Willard2001` p. 51 | constants encoded in **base 32**, one tag bit per byte |
| `Willard1993`/`-TR` | six-bit bytes, base 32; `i* > i^{6/5}` |
| `Willard2005` Eq. (20) | `U-Height(p) < (1/5)Log₂ p` |
| `Willard2011` | "Conventional Tableaux Encoding Requirement": Gödel number `≥ 32^J` |

- **These do not conflict.** One scheme: a byte is six bits, of which five are
  base-32 payload and one is a tag. 2001 counts **bits per byte** (6, base 64);
  2005 and 2011 count **payload bits per byte** (5, base 32); 1993's exponent
  `6/5` is exactly the ratio of the two.
- Status: **resolved on sight**. Recorded because a naive collation produces
  four incompatible-looking requirements; obligation O33 requires the codified
  statement to state the bits/payload distinction once, explicitly.

## D26 — Where the size threshold lives: object language versus metatheory

| Source | Statement |
| --- | --- |
| `Willard2001` Eqs. (9), (10), (16), (61), (71) | The size comparison is **inside the Group-3 axiom**: `y^λ < z/x`, `Size(y) ≤ x−1`, `Size(y) < x ≤ z/2`, `y·Log₂(x+2) < Log₂ z` |
| `Willard2001` Eq. (8) | `IS(A)`'s Group-3 has **no** size comparison — the flat `∀y ¬SemPrf(⌜0=1⌝,y)` |
| `Willard2005`, `Willard2011` | The threshold is metatheoretic — θ, `Good(N)`, `♯` are conditions on the *analysis*, not clauses of an axiom |

- The corpus therefore contains **both** designs, and 2001 contains both itself:
  `IS(A)` is Level-0 (flat axiom, all the bounding in the metatheory) while
  `IS^λ(A)`, `ISREF(A)`, `ISTR(A)`, `ISM(A)` are Level-1 (the bound is a clause
  the system itself quantifies over).
- **This corrects a claim made in
  `../discussion/2026-08-21-engine-machinery-and-rosser.md`**, where the
  comparison to Rosser's trick asserted that Willard's threshold "lives entirely
  in the metatheory". That is true of `Willard2005`/`Willard2011` and of 2001's
  `IS(A)`, and **false** of 2001's schematic systems. The Rosser contrast itself
  survives — Rosser alters the provability predicate of a *fixed* theory,
  whereas Willard's Group-3 clause is an axiom of a *new* theory whose ordinary
  provability predicate is unchanged — but the "entirely metatheoretic" phrasing
  must not be carried into the codified statement.
- Status: open — C13 must present the Level-0/Level-1 distinction as a design
  axis, and correct the discussion record's phrasing at composition time.

## D27 — `Willard2020` and `Willard2020-LFCS` are one paper at two lengths

| Source | Statement |
| --- | --- |
| `Willard2020` p. 20 | "The initial **19-page draft of this article** was accepted by the LFCS-2020 conference and was published by Springer [57]" |
| `Willard2020` ref. [57] | "On the Tender Line Separating Generalizations and Boundary-Case Exceptions for the Second Incompleteness Theorem under Semantic Tableaux Deduction" … "It **preceded the current article**, and its shorter manuscript can be found on pp. 268-286 of Volume 11972" |
| `registry/corpus.md` | rules `Willard2020` spine/C8 and `Willard2020-LFCS` core/Tier-A/C9, with no relation recorded |

- The terminal lineage is `Willard2018` (arXiv 1807.04717, self-described as "a
  quite preliminary announcement … in an essentially roughly written
  summary-abstract form") → `Willard2020-LFCS` (LNCS 11972, 19 pp.) →
  `Willard2020` (arXiv 2006.01057v1, 28 pp.) → `Willard2021` (JLC, gap G1).
- Consequence: C9 should extract `Willard2020-LFCS` as a **variant witness**,
  recording only its deltas against this record, not as an independent paper.
  The same applies to `Willard2018`.
- Status: open (action assigned to C9).

## D28 — this repository is inside `Willard2020`'s provenance

| Source | Statement |
| --- | --- |
| `prose/willard2020draftreview.txt` | Dated `20200521Z`, signed `jpt4`; 40 numbered copy-edits keyed to a manuscript's page/line |
| `Willard2020` p. 23 | "ACKNOWLEDGMENTS: I thank Seth Chaiken and **James P. Torre, IV** for several quite helpful comments about how to improve the presentation." |

- The review's page references map onto the arXiv witness at a consistent ≈2.2×
  ratio (review p. 15 → witness p. 8; p. 28 → p. 13; pp. 54–57 → pp. 25–28), so
  the reviewed document is a looser-set manuscript of *this* paper.
- Corrections **applied** in v1 include "mappings are" (Thm 4.4), "fails to be
  **a** consistency-preserving mapping" (Thm 4.5), "Type-NS" (Rem 3.5), the
  comma in Theorem ++, and the added YouTube URLs at refs. [18] and [39]. At
  least one was **not**: p. 7 still reads "Self Justifying" unhyphenated.
- Status: **resolved** — recorded so the codified statement's provenance
  apparatus can cite the review as a first-party artifact rather than treating
  it as an untraced loose file, and so G19 is closed as accepted rather than
  chased.

## D29 — the growth taxonomy has five classes in 2011 and four in 2020

| Source | Statement |
| --- | --- |
| `Willard2011` | Type-M / Type-**Almost-M** / Type-A / Type-S / Type-NS |
| `Willard2020` Definition 3.4 | Type-M / Type-A / Type-S / Type-NS |

- `Willard2020` defines the classes purely by which of the totality axioms
  (1)–(3) a system proves, which admits exactly four possibilities in the
  nested reading. **Type-Almost-M is not a point on that lattice** — and the
  reason is sharper than "an intermediate condition on multiplication"
  (corrected 2026-08-27 after reading `Willard2011` p. 3). Its definition is:
  α proves `∀x∀y∃z x+y=z` and `∀x∀y∃z x*y=z` **as theorems while treating none
  of (1)–(5) as axioms**, with those two written using genuine `+` and `*`
  **function symbols** where (1)–(3) use the 3-way predicates. So Type-Almost-M
  is the **theorem-versus-axiom** distinction, not a totality-strength one, and
  "many axiom systems that use function symbols '+' and '*' fall technically
  into" it.
- **That is precisely the distinction `Willard2020`'s result turns on** — "if one
  promotes this schema of theorems into formalized logical axioms, then the
  meaning of the pronoun 'I' in our self-referencing engine changes". So the
  taxonomy and the LEM result share one underlying axis, which neither paper
  says. Type-Almost-M systems (`Willard2009`) "verify their Herbrand but not
  also semantic tableaux consistency".
- The codified statement must therefore not present 2020's four-class scheme as
  a simplification of 2011's five-class scheme; they are indexed differently.
  Establish which of the two indexings the codification adopts, and state where
  Almost-M sits in it.
- Status: open (C13).

## D30 — the Linear-Sum construction is `Willard2002a` Lemma 6.3, eighteen years early

| Source | Statement |
| --- | --- |
| `Willard2002a` Lemma 6.3 (p. 25) | root `¬Ψ`; the axiom `Θ_Υ ⇒ Υ`; `⇒`-elimination splitting into `¬Θ_Υ` and `Υ`; proof `p` below `¬Θ_Υ`; proof `q` below `Υ` |
| `Willard2020` §6 (p. 17) | root `¬ψ`; the axiom `φ ∨ ¬φ`; `∨`-branching into `φ` and `¬φ`; a proof of `φ → ψ` below `φ`; a proof of `φ` below `¬φ` |

- **The same construction.** 2002a drives it with an *implication* axiom, 2020
  with a *LEM* axiom; both restore linear-sum proof composition to a cut-free
  apparatus by making a **branching axiom** available.
- The underlying fact recurs five times across the corpus: `Willard2001` Lemma
  7.1's comment; `Willard2002a` Theorem 2.2's proof sketch ("super-exponentially
  longer"); `Willard2002a` `V₄`'s CLARIFICATION ("a redundant axiom can
  super-exponentially shorten … cut-free … proofs"); `Willard2002a` Lemma 6.3;
  `Willard2020` §6, where it is finally named and made the mechanism.
- Status: open — C13 should present one construction with its two instantiations.

## D31 — "Conventional Deciphering Property" names two different conditions

| Source | Statement |
| --- | --- |
| `Willard2002a` Definition 5.1 | a **finite subset** `F ⊆ α` suffices to prove, for each canonical binary term `N̲`, that `N` is the Gödel number of an axiom of α |
| `Willard2004` Remark 1 | a **`Σ*₀` sentence `Test(n)`** is true exactly when `n` is the Gödel number of an axiom of α |

- These are not equivalent: 2002a's is a *finite-axiomatisability-of-recognition*
  condition, 2004's a *definability* condition. `Willard2006a` adds a third
  related notion, **Concise Encoding**, described as "slightly stronger than a
  requirement that a `Δ⁻₀` predicate identify all α's axioms".
- `Willard2002a`'s own abstract further calls its property the "Conventional
  **Encoding** Property" while Definition 5.1 says "Deciphering".
- Status: open — C13 must name the three conditions distinctly.

## D32 — Definable Cut has four inequivalent definitions

| Source | Clauses |
| --- | --- |
| `Willard2001` p. 5 | (a) `Υ(k̄)` for each fixed `k`; (b) downward closure; (c) closure under successor |
| `Willard2002a` Eq. (1) | `Υ(0)`; closure under successor |
| `Willard2006a` Eq. (4) | `ϕ(0)`; closure under successor; downward closure |
| `Willard2020-LFCS` Definition 4 | `Φ(0)`, `Φ(1)`, `Φ(2)`; closure under **`ω_j`**; downward closure |

- 2002a's omits downward closure; 2001's replaces `Υ(0)` with a schema over all
  fixed numerals; LFCS's replaces successor-closure with closure under a
  **fast-growing** function, giving the `Ω_i`-style cut of the Pudlák /
  Wilkie–Paris tradition inside `L*`.
- This matters for obligation **O29**: the claim "tangibility is a definable cut
  minus successor-closure" is stated against `Willard2001`'s definition and must
  be re-checked against whichever definition C13 adopts.
- Status: open (C13).

## D33 — `Willard2020`'s `Tab-1` is `Willard2004`'s `Tab₁List`

| Source | Statement |
| --- | --- |
| `Willard2004` p. 7–8 | `Tab-ℜ-List` proof: `(t₁,p₁)…(t_n,p_n)`, each `p_i` a tableaux proof of `t_i` from α plus earlier `t`s, every `t_i ∈ ℜ`. `Tab₁List` sets `ℜ = Π*₁ ∪ Σ*₁` |
| `Willard2020` p. 11 | `Tab-1` proof: `(p₁,φ₁)…(p_k,φ_k)`, each `p_j` a Tab-proof of the **Rank-1\*** sentence `φ_j` from α plus earlier `φ`s |

- Identical, with `Rank-1*` (2020's name for `Π*₁ ∪ Σ*₁`) replacing `ℜ`.
  `Willard2020` attributes `Tab-1` to [50] = `Willard2005`; the construct is
  already general in 2004.
| `Willard2009` p. 19 | `Herb−k` proof: `(t₁,p₁)…(t_n,p_n)`, each `p_i` a **Herbrand** proof of `t_i` from α plus earlier `t`s, every intermediate `t_i` in `Level-k` (`= Π^R_k ∪ Σ^R_k`) |

- Status: **resolved** — one apparatus, **four** names (`Tab₁List`, `Tab-1`,
  `Willard2005`'s form, and `Willard2009`'s `Herb−k`). Fix one at C13.
- `Willard2009` p. 20 is the only place Willard writes the correspondence down
  himself: he defines `Tab−k` as `Herb−k` with tableaux proofs and `Π*_k`/`Σ*_k`
  intermediates, and adds the gloss that `Π*_k`/`Σ*_k` are "roughly analogous"
  to `Π^R_k`/`Σ^R_k` **except that they contain no multiplication function
  symbol**, using the 3-way relation `M(x,y,z)` instead. That one sentence is
  the corpus's clearest statement of what separates the starred classes from
  the R-superscripted ones.

## D34 — 2004's `ℜ` dial and 2020's `Z` dial are different parameterisations

| Source | Parameterises |
| --- | --- |
| `Willard2004` | **ℜ** — which *intermediate theorems* a `Tab-ℜ-List` proof may use |
| `Willard2020` §7 | **Z** — which *LEM instances* are admitted as logical axioms |

- Both report the same shape of answer — a `Π*₁`-level positive and a `Π*₂`
  negative — but they are not the same dial, and 2020 does not say so.
- **This sharpens the open conjecture.** On 2004's dial the `Π*₁ ∪ Σ*₁` case is
  **proved** (Theorem 3, at Level(1)); on 2020's dial the `Π*₁` case is
  **conjectured**. So `Willard2020`'s conjecture is precisely the claim that
  2004's proved positive result **transfers from the `ℜ` dial to the `Z` dial**.
- Status: open — appended to gap G22 and obligation O46.

## D35 — the Non-Growth condition itself varies

| Source | Condition |
| --- | --- |
| `Willard2001`, `Willard2004`, `Willard2020` | `F(a₁,…,a_j) ≤ Maximum(a₁,…,a_j)` |
| `Willard2006a` p. 6 | `F(a₁,…,a_j) ≤ Maximum(**2**, a₁,…,a_j)` |

- The `2` is real (image-verified) and is needed because 2006a's
  `Logarithm(x) = 1 + ⌊Log₂x⌋` exceeds `Maximum(x)` at small arguments.
- Recorded because D23 concluded that the *choice of grounding functions* is not
  load-bearing; that conclusion stands, but the **defining condition** they must
  satisfy is itself not stated identically, and the codified statement should
  give one condition that all four papers' function sets satisfy.
- Status: open (C13).

## D36 — `Willard2011`'s subsumption of `Willard2006a` is partial

| 2006a contributes | Carried by `Willard2011`? |
| --- | --- |
| `ISCE(A)`, a Hilbert-apparatus self-justifying system | **yes** |
| The **naming-convention axis** (incremental / additive / multiplicative) | **no** — ξ parameterises language, base and coding `g`, but not how constants are *named* |
| **Continuously Expanding** | **no** |
| **Infinitely Far-Reaching** / `ISINF(A)` | **no** |
| Theorems 4/4\*/7 (Hilbert negatives) | partially — cited, not reproduced |

- 2011 absorbs 2006a's positive *system* and drops its *boundary axis*. This is
  the second element-wise failure of the unification claim, alongside **D21**.
- Status: open — closes the C6-deferred `Willard2006a` subsumption check.

## D37 — the LFCS chapter is not superseded by the arXiv expansion

| Source | Statement |
| --- | --- |
| `Willard2020-LFCS` Appendix B | contains **Definition 4 (Locally-J-Closed)**, **Theorem 3**, the `ω_j` hierarchy, and the explanation of why Theorems 1 and 2 diverge |
| `Willard2020` | contains none of these |
| `Willard2020-LFCS` p. 16 | "a formal proof of Theorem 3 will be **postponed until a longer version of this article**" |

- **This partially reverses D27's recommendation.** The LFCS chapter is not a
  deltas-only variant: it is the only place in the corpus that explains how
  `Willard2020`'s statement ⊙ reaches Hájek–Pudlák's definable-cut machinery —
  via LEM → Linear-Sum Effect → short proofs of the Locally-J-Closed conditions
  → the cut diagonalization.
- The "longer version" that was to carry Theorem 3's proof is **not** the arXiv
  paper, which drops it. That is direct textual support for the priority raised
  on **G1** (`Willard2021`), previously inferred only from its title and date.
- Status: open — extracted as a first-class record, `willard2020-lfcs.md`.

## D38 — the boundary has three dials, and only one has its threshold located

| Source | Dial | Positive side | Negative side |
| --- | --- | --- | --- |
| `Willard2004` | **ℜ** — which intermediate theorems a `Tab-ℜ-List` proof may use | `Π*₁ ∪ Σ*₁`, at Level(1) — **proved** (Theorem 3) | `Π*₂` and `Σ*₂`, at Level(0−) — **sketched** (Theorem 2) |
| `Willard2006a` | **Hybrid(H)** — the naming convention's growth rate, `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}` | **H = 1** — Theorem 3's exception "remains valid" | **H > 1** — Theorems 4 and 4\* "can be generalized" |
| `Willard2020` | **Z** — which LEM instances are logical axioms | `Δ*₀` — stated-only | `Π*₂` and above — stated-only; `Π*₁` **conjectured** |
| `Willard2009` | **Herb−k** — which intermediate theorems a `Herb−k` proof may use (Definition 7) | **k = 1** — stated-only (Item II) | **k = 2** — stated-only (Item I) |

- Four different parameterisations of one boundary, none of which Willard
  relates to the others. **Amended 2026-08-27** on reading `Willard2009`'s
  Section 6: the claim that Hybrid(H) is the only dial with an exactly stated
  threshold no longer holds. `Herb−k` has one too (positive at `k = 1`,
  negative at `k = 2`), as does `Willard2002c`'s `R(i,j)` (D46). What is now
  distinctive about Hybrid(H) is only that it is on the **Hilbert** side.
- The `Herb−k` dial is the weakest-supported of the four: both halves are
  **stated without proof** — "we will not prove results I and II here because
  each has a rather long proof" — with Item I said to be analogous to
  `Willard2004`'s main theorem and Item II to follow from hybridizing
  `Willard2005`'s with `Willard2009` §4's machinery. It is nevertheless the
  only dial that runs on Herbrand deduction, so it is the only evidence that
  the boundary phenomenon is apparatus-independent.
- The codified statement should present the boundary as a single phenomenon
  with three known coordinatisations, and say for each whether the transition
  point is proved, sketched or conjectured.
- Status: open (C13). Supersedes the narrower D34.

## D39 — `Willard2004`'s W₄/W₅ are already Z-enrichment axioms

| Source | Statement |
| --- | --- |
| `Willard2004` Eqs. (12), (13) | `W₄ = ∀α∀t∀n [Prf_α(t,n) ∨ ¬Prf_α(t,n)]`; `W₅ = ∀g∀h [Subst(g,h) ∨ ¬Subst(g,h)]` |
| `Willard2020` Definition 7.2 | a Z-Base-Variable-Enriched proof admits `∀x Υ(x) ∨ ¬Υ(x)` for `Υ(x) ∈ Z` |

- `W₄` and `W₅` are exactly Definition 7.2's shape, for the two `Σ*₀` predicates
  `Prf` and `Subst`, and Lemma 5's proof uses them precisely to generate the two
  `∨`-Elimination splits that let the argument case-split.
- So the LEM-as-axiom device that `Willard2020` makes its centrepiece is already
  **in the base theory of the 2004 negative theorem**, sixteen years earlier, in
  restricted form. `Willard2020`'s Z-enrichment generalises it from two fixed
  `Σ*₀` predicates to an arbitrary class.
- This is the family resemblance behind D38's first and third dials, and it
  makes the Π\*₁ conjecture legible: 2004 already knows that LEM at `Σ*₀` is
  safe and that `Π*₂` intermediate theorems are fatal.
- Status: open (C13).

## D40 — the corpus carries two toolkits, and `Willard2006a` says why

| Source | Statement |
| --- | --- |
| `Willard2006a` p. 12 | "Definable Cuts are not always ideally suited for generalizing the Second Incompleteness Theorem when D represents a **cut-free** deduction method, such as Herbrand deduction, semantic tableaux or the cut-free sequent calculus" — hence **Generalized Gödel Sentences** |

- Definable cuts and their thinnings serve the Hilbert-side results
  (`Willard2006a` Theorems 1, 2, 4, 7); generalized Gödel sentences
  `⧫^λ_D(α)`, built on `ShortPrf^λ_{α,D}`, serve the cut-free ones
  (`Willard2002a`, `Willard2004`). `Willard2004` is explicitly a hybrid.
- The codified statement should introduce both toolkits once, with this reason,
  rather than letting each paper's apparatus appear ad hoc.
- Status: open (C13).

## D41 — coding compression is load-bearing on the tableaux side and merely clarifying on the Hilbert side

| Source | Statement |
| --- | --- |
| `Willard2001` p. 28, `Willard2020` §5 | density is what makes the positive tableaux engine work; without it Case 11 and Lemma 4.2 fail |
| `Willard2006a` p. 15 | "Our results also generalize in various forms for **non-compressed encodings**, where `C_i` has an `O(i)` bit-length instead… uncompressed encodings are inherently unnatural" |
| `Willard2006a` p. 37 | "All our theorems have analogs under such uncompressed encoding methods, but they are **substantially more meaningful** when one uses efficiently compressed encodings" |

- So D6/O30's thesis — that coding density is a hypothesis rather than a
  convenience — holds for the **tableaux** results but is explicitly disclaimed
  for `Willard2006a`'s **Hilbert** results, where compression affects
  significance rather than validity.
- Record the distinction; a codified statement that makes density universally
  load-bearing would misstate the Hilbert side.
- Status: open (C13).

## D42 — `Willard2020`'s `Xtab` is TR 93-10's "tableaux proof with cuts", verbatim

| Source | Statement |
| --- | --- |
| `Willard1993-TR` printed p. 33 | "Define a **tableaux proof with cuts** to be a deduction formalism identical to semantic tableaux, except that it is permissible to include the sentence **`Φ ∨ ¬Φ`, for any sentence `Φ`, in any node of a proof tree**." |
| `Willard2020` p. 11 | "Its definition is identical to Tab-deduction, except that for any sentence `φ` in our language `L*`, the sentence `φ ∨ ¬φ` is allowed as an internal node in an Xtab proof tree." |
| `Willard1993-TR` printed p. 36 | "(Curiously, the ZF *half* of this 2-part system can be viewed as performing deduction *with Gentzen-style cuts*, if ZF is defined to include the axiom `Φ ∨ ¬Φ` for each sentence `Φ`.)" |

- The two **definitions** are word-for-word the same construct, twenty-seven
  years apart. That is the whole of D42's claim, and it rests on printed p. 33
  alone, which is a definition and not a remark.
- **Correction, 2026-08-27.** An earlier version of this entry said the TR
  "states the underlying identification in both directions: admitting LEM as
  logical axioms *is* admitting cuts." It does not. Printed p. 36 is a hedged
  parenthetical about **one system** — ZF "*can be viewed as* performing
  deduction with Gentzen-style cuts" under that definition — not a general
  claim, not an equivalence, and not asserted in Willard's own voice as a
  theorem. It **corroborates** that Willard treats LEM-as-axioms and cuts as
  closely related; it does not state their identity. The definitional content is
  entirely in p. 33's naming of the apparatus.
- The results differ in the expected way. TR Proposition 7 is a **positive**
  result for it (consistent once all growth functions, including Successor, are
  removed from Group-1); `Willard2020` Theorem 4.5 is the **negative** result for
  the case where Addition stays total. Together they are the (a)/(b) pair below.
- Consequence: `Willard2020`'s headline is not the discovery of `Xtab` but the
  proof that the apparatus the TR already defined is fatal at Type-A.
- Status: open — C13 should present `Xtab` with its 1993/94 provenance.

## D43 — TR 93-10 supplies the argument `Willard2020` §7 omits for `Δ*₀`

| Source | Statement |
| --- | --- |
| `Willard2020` p. 19 | "our results from [47] can be expanded to show that their evasions … can be extended to both the cases of Z-Enriched and Z-Base Variable Enriched mechanisms, when Z represents the `Δ*₀` class" — with "a fully detailed justification will not be provided here" |
| `Willard1993-TR` printed p. 35, footnote | "One way to formally extend `IS(A)` to include **`Δ₀` cutting** is to simply add to `IS(A)` an axiom **`Φ ∨ ¬Φ` for each `Δ₀` sentence `Φ`** (and verify that all our Propositions remain valid under such a modification). However, this is essentially unnecessary because `IS(A)`, without modification, can **simulate a `Δ₀` cut rule with only a polynomial increase in proof length**. This nice property holds because if `Φ` is `Δ₀` then `IS(A)`'s Group-2 axioms include the sentence `Prf_A(⌜Φ ∨ ¬Φ⌝) ⊃ Φ ∨ ¬Φ`." |

- The TR's construction **is** Definition 7.1's Z-enrichment at `Z = Δ*₀`, and it
  is asserted safe **with a reason**: those LEM instances are already derivable
  through Group-2 at polynomial cost, so promoting them to axioms yields **no
  additional proof compression** — precisely the condition `Willard2020` §6's
  Linear-Sum analysis needs for the enrichment to be harmless.
- **This materially improves gap G22.** The `Δ*₀` flank of the Π\*₁ conjecture is
  not merely asserted; a justification exists in the corpus's founding technical
  report. It also sharpens the conjecture: the question at `Π*₁` is exactly
  whether Group-2 can still supply the LEM instances cheaply enough, which it
  demonstrably can at `Δ₀` and demonstrably cannot once unbounded quantifiers
  enter (TR printed pp. 7 and 35).
- Status: open — cross-referenced from G22 and obligation O48.

## D44 — the corpus's two branches are derived on TR 93-10's printed p. 2

| Source | Statement |
| --- | --- |
| `Willard1993-TR` printed pp. 1–2 | Solovay's theorem forbids proving all PA's Π₁ theorems about Subtraction and Non-zero Division while **simultaneously** (a) "employing a Gentzen-style deduction method **WITH A CUT RULE**" and (b) "recognizing Addition (or even successor) as a total function" |
| same, printed p. 2 | "Solovay's Theorem is the almost direct complement to our **Propositions 1 & 7**, in that these theorems show that self-verifying axioms systems can be constructed when **either** condition (a) **or** (b) is relaxed" |

- **Proposition 1 relaxes (a)** — cut-free, Addition total. That is the tableaux
  line: `IS(A)`, `IS^λ(A)`, `IS_D(A)`, `IS_{Tab}`.
- **Proposition 7 relaxes (b)** — cuts permitted, all growth functions including
  Successor removed (confirmed printed p. 36). That is the Hilbert line:
  `ISREF(A)`, `ISCE(A)`, `ISINF(A)`.
- So the corpus's entire two-branch structure is a direct reading of Solovay's
  1994 theorem, stated in the founding report. The codified statement should
  introduce the branches this way rather than as two independent programmes.
- Status: open (C13).

## D45 — the engine's truncation operator bounds different quantifiers in different papers

| Source | Operator | What it bounds |
| --- | --- | --- |
| `Willard2001` p. 22 | `Ψ̃⇓m` | **all** unbounded quantifiers, after replacing parameters by their values |
| `Willard2002c` p. 8 | `Ψ^M` | only the previously-unrestricted **universal** quantifiers; bounded universals and **all** existentials are unchanged |
| `Willard2005` Eqs. (16)–(17) | `App∀(a)` / `App∃(b)` | universals and existentials **separately**, by two different bounds |
| `Willard2011` Definition 4.2 | `Scope_E(Υ,N)` | **both** unbounded universals and unbounded existentials, by the same `E(N)` |

- Four papers, four truncation conventions, all called on to do the same job:
  make a false self-consistency axiom locally true so a minimal counterexample
  can be cornered.
- The differences are not cosmetic. `Willard2002c`'s universals-only truncation
  is what lets `Check` compare `X = ∀a φ(a)` against `Y = ∃a ¬φ(a)` asymmetrically;
  `Willard2005` splits the two bounds precisely so they can be tuned against each
  other; `Willard2011` re-merges them and pays for it with the `♯` machinery.
- C13 must fix **one** operator and restate each paper's lemmas against it, or
  the engine will appear to be four unrelated arguments. Pairs with **O27**.
- Status: open (C13).

## D46 — `Willard2002c` locates the boundary more finely than any successor

| Source | Statement |
| --- | --- |
| `Willard2002c` p. 16 | "Theorem 2 and its generalizations **collapse when `R(2,1)` deduction replaces `R(1,1)`**" — with a `Π₁⁻` sentence `W` provable from IΣ₀ blocking Addition-total systems |
| `Willard2004` Theorem 2 | no consistent `α ⊃ V_A` proves its Level(0-) consistency under **`Tab-Π*₂-List`**; none `⊃ V_B` under **`Tab-Σ*₂-List`** |

- `R(i,j)` carries **two** indices — intermediate theorems may be `Π*ᵢ` **or**
  `Σ*ⱼ`. `Willard2002c` reports the transition at `R(1,1) → R(2,1)`, i.e. from
  **raising the `Π` index alone while holding `Σ` at 1**.
- `Willard2004`'s dial has collapsed to a single class, so its two negative
  results move both indices together and cannot express the finer statement.
- Consequence for **D38**: the `ℜ` dial is a *projection* of a
  two-dimensional parameter, and the corpus's sharpest located threshold is in
  the paper ruled Tier B. Obligation **O60**.
- Status: open (C13).

## D47 — `Willard2006-WoLLIC` and `Willard2009` are not a superset pair

| Source | Statement |
| --- | --- |
| `Willard2006-WoLLIC` p. 10, Clarifying Comment | "It does not indicate that `Diag(i)` is a logically valid statement… **In fact, `Diag(1)` and `Diag(2)` can be proven to be logically invalid statements** (see footnote 1). In contrast, Theorem 4 (below) will prove `Diag(3)` is logically valid." |
| `Willard2009` p. 13, Clarifying Comment | "It does not indicate that `Diag(Ax-3)` is a logically valid statement… In order to establish prove that `Diag(Ax-3)` is also valid, we will need the added force of Theorem 4 below." |

- The journal version drops the conference version's **indexed** notation
  `Diag(i)`, the `Diag(1)`/`Diag(2)` invalidity claim, and footnote 1's
  three-sentence statement of the programme. Its §5 also replaces the
  conference's "third facet of a 3-part project" framing (`Ax-1` obeys / `Ax-2`
  obeys / `Ax-3` evades) with the Kreisel–Takeuti–Pudlák comparison.
- So the conference form is **not** a proper part of the journal form, though
  the journal is more than twice its length and adds Remarks 1–4, Definition 8,
  Appendix A and the whole of Appendix B. Of the corpus's three
  conference/journal pairs, only `Willard2000-TAB`/`Willard2002a` is a strict
  expansion; this one and `Willard2020-LFCS`/`Willard2020` both drop material.
- The dropped claim is the sharpest form of the paper's thesis and should be
  carried from the conference witness — obligation **O69**.
- Status: open (C13).

## D48 — `Willard2009`'s Appendix B is not self-contained

| Source | Statement |
| --- | --- |
| `Willard2009` p. 24 | "The discussion in this abbreviated appendix will **assume that the reader is familiar with [55]'s proof** that `Ax-1` and `Ax-2` satisfy the semantic tableaux version of the Second Incompleteness Theorem. We will also often rely upon the notation convention from the second paragraph of Section 2 of [55] (which defined semantic tableaux deduction's **eight elimination rules**)." |

- `[55] = Willard2007-APAL` ("Passive Induction and a Solution to a Paris-Wilkie
  Open Question", APAL 146 (2007) pp. 124–149), a **Tier C** corpus item. Lemma
  5's proof invokes its rules 6, 7 and 8 by number; Lemma 7 invokes its
  "passive induction"; Theorem 5 defers "the remaining details" to [50,55].
- The same shape as `Willard2011`'s deferral of `Tab-U*₁` to `Willard2005`
  (D19): a paper the codification treats as Tier B rests on one it ranks Tier C.
  **`Willard2007-APAL` must be extracted before Theorem 5 can be presented with
  its proof**, not merely with its statement.
- Status: open — actioned as gap **G32**.

## D49 — the two-branch derivation, restated 23 years later in new vocabulary

| Source | Statement |
| --- | --- |
| `Willard1993-TR` p. 2 | Solovay's theorem forbids Gentzen-with-cut and Addition-totality *together*; **Proposition 1** relaxes the apparatus (→ the tableaux line), **Proposition 7** relaxes totality (→ the Hilbert line) |
| `Willard2016` Example 3.3, item b (p. 6) | "[51, 54, 55] developed two methods for generalized arithmetics to confirm their own consistency, **whose natural hybridizations are precluded by ++**": either a **Type-NS** system verifying its consistency under a Hilbert-style method, or a **Type-A** system verifying it under `d_F`'s tableaux apparatus |

- Same derivation, same forbidding theorem, entirely different vocabulary. The
  1993 text names the two branches by proposition number; the 2016 text names
  them by the **Type** taxonomy and the apparatus symbol. Nothing in either
  text says they are the same split.
- `Willard2016` adds a third datum the TR does not have: Willard [52, 57]
  refined `++` with Adamowicz–Zbierski [1, 2] to show **Type-M systems cannot
  recognize their semantic tableaux consistency** — closing the remaining cell.
- The codified statement should present the split once, in one vocabulary, and
  record both namings — obligation **O71**.
- Status: open (C13).

## D1-supplement — `Willard2016` corroborates the 1994 Solovay communication

`Willard2016` Example 3.3 (p. 6): "**Solovay [44] privately communicated to us
in 1994 an analog of theorem ++.** Many authors have noted Solovay has been
reluctant to publish his nice privately communicated results on many occasions
[11, 25, 35, 37, 39, 49]." Independent confirmation, twenty-two years later, of
the dating that closed **D1** and gap **G9** — TR 93-10's `[So94]` citation is
to an April-1994 private communication, so the TR is a 1994 text carrying a
1993 series number. `Willard2006-WoLLIC`'s reference **[26]** is the fullest citation of it anywhere
in the corpus, and it was found only by the full-document visual pass of
2026-08-27: "R. Solovay, **Telephone conversations between Solovay and Willard
(during April of 1994)** describing Solovay's generalization of one of Pudlák's
theorems [23], using some of Nelson's and Wilkie-Paris's methodologies. **The
Appendix A of [34] offers a 4-page interpretation of the fundamental intuition
behind Solovay's idea.**" — where [23] is Pudlák's "Cuts, Consistency Statements
and Interpretations", JSL 50 (1985) 423–442, and [34] is `Willard2001`. So the
corpus's own four-page exposition of the theorem that forces its whole
two-branch architecture is **`Willard2001` Appendix A**, and the April-1994
dating is fixed in print rather than inferred.

`Willard2016` also credits **Pudlák's 1985 article [39]**
with "the majority of `++`'s essence, chronologically before Solovay's
observations", and notes Buss–Ignjatovic, Hájek and Švejdar [11, 23, 45] and
Appendix A of `Willard2001` as subsequent explorations.

## D50 — ten formula-class notations for one idea

| Paper | Classes |
| --- | --- |
| `Willard1993-TR`, `Willard2001` | `Π₁`, `Σ₁` over the grounding language |
| `Willard2002a`, `Willard2004`, `Willard2005`, `Willard2011`, `Willard2020` | `Δ*₀`, `Π*ₙ`, `Σ*ₙ` — starred: **no multiplication function symbol**, multiplication as the 3-way relation `M(x,y,z)` |
| `Willard2004` | `Q*ₙ` — the Level(n+) classes |
| `Willard2009`, `Willard2006-WoLLIC` | `Δ^R₀`, `Π^R₁`, `Σ^R_k` — quantifier-bounding terms may use **only `Max`** |
| `Willard2016` §5 | `Δ^ANC₀`, `Π^ANC_n`, `Σ^ANC_n` — over the Additive-Naming-Convention language |
| `Willard2016` §5 | `Δ^Q₀`, `Π^Q_n`, `Σ^Q_n` — over `L^Q`, with every `C_J` replaced by `E_{J−1}` |

- Every one of these is "the bounded formulas of *this* language". **Two**
  relations between them are stated anywhere in the corpus, both by
  `Willard2009`: p. 20 says the starred classes are "roughly analogous" to the
  R-superscripted ones **except that they contain no multiplication function
  symbol** (drift D33), and **footnote 3 on p. 13** says "the `Δ^R₀` class of
  formulae is **broader than** `Δ⁻₀`" — so `Willard2001`/`Willard2002c`'s
  minus-superscript class is a proper subclass of `Willard2009`'s. Every other
  pair is unrelated in print.
- `Willard2016`'s two are the most constrained: a formula lies in `Δ^Q₀` **iff**
  it is built by the `C_J → E_{J−1}` substitution, which is what guarantees
  every primary term in it is an *observable*.
- The codified statement needs one canonical class notation with a translation
  table, not ten. Status: open (C13).

## D53 — `Root(x, y)` rounds two different ways

| Source | Definition |
| --- | --- |
| `Willard2002c` p. 3 | `Root(x, y) = ⌈ x^{1/y} ⌉` — **ceiling** |
| `Willard2016` p. 9 | `Root(x, y) = ⌊ x^{1/y} ⌋` when `y ≥ 1`, and zero otherwise — **floor** |

- The same name, in the same list of grounding functions, in two papers. Neither
  says the other is different. `Willard2016` additionally fixes the `y = 0` case,
  which `Willard2002c` leaves undefined.
- Whether it matters is not obvious: `Root` is a Non-Growth function under either
  rounding when `y ≥ 1` (since `⌈x^{1/y}⌉ ≤ x` for `x ≥ 1`, `y ≥ 1`), so the
  Non-Growth requirement survives — but a codified grounding language must pick
  one, and any statement proved about `Root`'s exact values inherits the choice.
- Status: open (C13).

## D54 — six tableaux rules or eight, depending on the paper

| Source | Presentation |
| --- | --- |
| `Willard2007-APAL` Definition 1 (p. 4) | **Eight** rules. "For the sake of simplifying our presentation, this definition views the bounded quantifier `∀v ≤ s` as a syntactic object **different from** the unbounded quantifier `∀v`", so rules 6 and 8 are primitive |
| `Willard2020-LFCS` Appendix A (p. 15) | **Six** rules. `∀v ≤ s Φ(v)` is "an abbreviation for `∀v {v ≤ s → Φ(v)}`" and likewise for `∃`, so the bounded rules **a** and **b** are *derived* |
| `Willard2002c` §2 (p. 7) | **Six** rules, plus a PRENEX\* normalisation requirement on the root and a restriction that `∀`-elimination terms be built from Grounding Functions over **previously introduced** parameters |

- One calculus, three presentations, and the count of "the elimination rules"
  differs between them. This matters for citation: `Willard2009` Appendix B
  invokes "[55]'s **eight** elimination rules" **by number** — rules 6, 7 and 8
  — so it is keyed to `Willard2007-APAL`'s numbering specifically, and a reader
  holding `Willard2020-LFCS`'s six-rule list cannot follow it (gap G32).
- `Willard2002c`'s extra restriction on `∀`-elimination terms is not
  presentational: it bounds what parameters a candidate tree may mention, which
  is what makes the `(L,M)`-Conservative valuation well defined.
- The codified statement should fix one presentation and give the numbering map.
- Status: open (C13).

## D55 — "Consistency Preserving" has two inequivalent definitions

| Source | Definition |
| --- | --- |
| `Willard2020` Definition 4.3 (p. 13) | `IS_D(•)` is Consistency Preserving iff `IS_D(β)` is automatically consistent **whenever all the axioms of β hold true under the standard model of the natural numbers** |
| `Willard2016` Definition 5.2 (p. 18) | `I(•)` is Consistency Preserving iff `I(β)` is consistent **whenever the union of β with the Groups 0 and 1 axiom schemas is consistent** |

- The 2020 hypothesis is **semantic** (β is true in ℕ); the 2016 hypothesis is
  **syntactic and weaker in one direction, stronger in another** (β together
  with the target system's own base groups is consistent). Neither implies the
  other: a β true in ℕ may be inconsistent with Group-1 if the two disagree,
  and a β consistent with Groups 0–1 need not be true in ℕ.
- This matters because `Willard2016` Example 5.3's headline consequence — "**if
  PA is consistent then ISCE(PA+) is self-justifying**" — is read off the 2016
  definition, while `Willard2020` Theorem 4.4's `IS_Tab` result is read off the
  2020 one. Stating both as "consistency preserving" without qualification
  makes them look like instances of one theorem schema when they are not.
- Status: open (C13); the codified statement must name the two hypotheses
  separately.
