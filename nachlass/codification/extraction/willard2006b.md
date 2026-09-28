# Extraction: Willard2006b

> Tier C extraction (component C11). Template: charter ADR-0001 §Decision.
> Recorded as the **journal form** of the floating-point / simulated-real line,
> against the conference precursor [`willard2005-tab.md`](willard2005-tab.md)
> (LNCS 3702; footnote 1 here; also extracted 2026-08-29). Read in full rather
> than as deltas — the numbering and theorem inventory diverge substantially
> (drift **D70**); the conference form also carries the JSL page-range typo
> recorded as **D57**.
> **Anchoring**: PDF page = printed page of the author's copy (1–11). The JSL
> pagination of record is 71 (2006) pp. 1189–1199 (author-copy header still
> carries placeholder Volume 00 / XXX 0000).

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2006b` |
| Title | On the Available Partial Respects in Which an Axiomatization for Real Valued Arithmetic Can Recognize Its Consistency |
| Running header | *Real Valued Arithmetics Can Partially Recognize Their Own Consistency* |
| Venue of record | Journal of Symbolic Logic **71** (2006), pp. **1189–1199** |
| Witness | `nachlass/papers/willard2006_real_valued_arithmetic_author_jsl6.pdf` (11 pp., author-archived TeX copy) |
| Text aid | `../sources-text/willard2006b.txt` |
| Companion | `Willard2005-TAB` (TABLEAUX 2005, Springer LNCS **3702**, pp. 292–306) — footnote 1, p. 2: "This result was first announced at the Tableaux-2005 Symposium" |
| Related gaps | **G3**, **G36** (ASL-2005 floating-point TR behind `Willard2005` ref. [71]); this journal form and the TAB chapter are the published successors |
| Structure | §1 Introduction (pp. 1–2); §2 General Formalism (pp. 2–4); §3 The Formal Nature of Simulated Arithmetic (pp. 4–8); §4 Three Related Incompleteness Results (pp. 9–10); §5 Overall Perspective (pp. 10–11); Acknowledgment (p. 11). **No references section** in this author copy — every citation is a literal `[??]` placeholder |

## 2. Role in corpus (Willard's own claims, recorded not verified)

Willard positions this paper as the venue shift announced in `Willard2005`
Remark 7: change from integer to **floating-point / simulated real** arithmetic,
so that a self-justifying system can recognize addition, multiplication,
subtraction **and division** as total — something no Type-M integer axiomatization
in the corpus can do under cut-free deduction.

Own framing (§1, p. 2; §5, pp. 10–11):

1. **Simulated real arithmetic is "quite unlike" integer arithmetic** — an axiom
   system can simultaneously recognize its semantic-tableaux / Tab−1 consistency
   **and** the totality of all five simulated-real operations (Defs. 4–5).
2. The evasion houses a **limited Gentzen-style cut rule** (Tab−1 / Tab−ℜ) for
   simulated reals that "has no analog for integer-based arithmetics".
3. Dualistic: new **thresholds** where G2 applies to simulated reals (Hilbert;
   Long Multiplication under tableaux) **and** where it can be evaded (truncated
   floating-point under Tab−1).
4. The robustness of the evasion lives in the **Tier(1)** and **Tier(1)⊕**
   classes, which capture much of Numerical Analysis and almost none of Number
   Theory (Remarks 1–2).

**Main positive result is Theorem 2**; **main negatives are Theorems 5 and 6**.
Theorem 1 is imported from `Willard2005` (`IS_D(A)` under Tab−1); Theorem 4 is
Solovay's Type-S Hilbert negative, cited as background.

## 3. Systems defined

### 3.1 Simulated Real Arithmetic (Defs. 3–6, pp. 3–4)

Not a named axiom system — an **instruction-set semantics** over NN/IPN
encodings:

| Piece | Content |
| --- | --- |
| Simulated real `𝐑ᵢ` | Ordered pair `(Mᵢ, Eᵢ)` (IPN mantissa / exponent), or NN pair `(mᵢ, eᵢ)` mapped by `F` (Def. 2) |
| Value (Def. 3) | (1) if `Eᵢ ≠ ∞` and `0 ≠ Mᵢ ≠ ∞`: `𝐑ᵢ = Mᵢ · 2^{−⌊Log₂(\|Mᵢ\|)⌋} · 2^{Eᵢ}`; (2) if `Eᵢ = ∞` and `Mᵢ` is a power of 2: zero with `Log(Mᵢ)` digits after the binary point; (3) otherwise: overflow |
| `Θ_S` (Def. 4) | Graph of truncated floating-point `+`, `×`, `−`, `÷` with hardware rounding: output mantissa bit-length `L = max(len(m₁), len(m₂))` |
| `Expand` / `Θ_E` (Def. 5, Eq. 2) | One extra zero bit of mantissa precision: `m₂ = 2·m₁ − b₁` and `e₁ = e₂` |
| `LongMult` (Def. 6) | **Untruncated** product: `m₃`'s bit-length is `L*` or `L*−1` where `L*` = sum of input mantissa lengths — **outside** the simulated-arithmetic instruction set |

### 3.2 `IS_D(A)` under Tab−1 (Theorem 1, p. 5) — imported

Cited from `[??]` = `Willard2005`: for any U-Grounding `A` whose theorems are
standard-model valid, there is a consistent self-justifying `IS_D(A)` that
(1) proves all Tier(1) theorems of `A`, (2) proves all eight U-Grounding
operations total, (3) affirms its own Tab−1 consistency (no Tab−1 proof of
`0=1`). This paper does **not** re-prove it.

### 3.3 `IS_D(A')` for simulated reals (Theorem 2, p. 7) — the floating-point SJAS

Let `Ψ₁…Ψ₅` be Lemma 2's five `Π*₁` totality sentences (four `Θ_S` operations
plus `Expand`). Let `A' = A ∪ {Ψ₁,…,Ψ₅}`. Then `IS_D(A')` is consistent,
verifies its own Tab−1 consistency, and proves all five simulated-real
operations total. Immediate from Theorem 1 + Lemma 2.

**This is the only construction in the witnessed corpus in which a
self-justifying system recognizes multiplication as a total function** — and
only under Def. 4's **truncated** floating-point multiplication, not under
integer `M(x,y,z)` or Def. 6's `LongMult`.

### 3.4 Auxiliary taxonomy (p. 1)

`Type-S` / `Type-A` / `Type-M` — successor / addition / multiplication totality,
as elsewhere in the corpus. Used here to locate Solovay (Type-S, Hilbert) and
Willard's prior Type-M tableaux negatives against the new floating-point
threshold.

## 4. Numbered-item inventory

Proof-status values: `full` | `sketch` | `cited` | `stated-only` | `n/a`.

### Definitions

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| Definition 1 | 2 | **NN / IPN.** NN = non-negative integers; IPN = any positive or negative whole number, plus a reserved symbol for `∞` | n/a | — |
| Definition 2 | 2 | **`F: NN → IPN`.** `Even(x) ∈ {1,−1}`, `Half(x) = ⌊x÷2⌋`; `F(x) = Even(x)·Half(x)` when `x ≠ 1`, and `F(1) = ∞` | n/a | Def. 1 |
| Definition 3 | 3 | **Simulated real-number.** Bold `𝐑ᵢ = (Mᵢ, Eᵢ)` with the three-case value convention above. Added Comment: NN notation `(mᵢ, eᵢ)` is mapped by `F` | n/a | Defs. 1–2 |
| Definition 4 | 3 | **`Θ_S(m₁,e₁,m₂,e₂,m₃,e₃)`.** Truncated floating-point graph for `S ∈ {+,×,−,÷}` under max-mantissa-length rounding | n/a | Def. 3 |
| Definition 5 | 3 | **`Expand(𝐑)` / `Θ_E`.** Extra zero mantissa bit. Eq. (2): `m₂ = 2·m₁ − b₁ AND e₁ = e₂` | n/a | Def. 3 |
| Definition 6 | 3–4 | **`LongMult`.** Untruncated multiplicative product; formally **outside** the simulated-arithmetic instruction set | n/a | Def. 3 |
| Definition 7 | 5 | **`Tab−ℜ` proof.** Sequence `(t₁,p₁)…(tₙ,pₙ)` of tableaux proofs; each `pᵢ` may use prior `tⱼ` as axioms; intermediates `t₁…tₙ₋₁` lie in class `ℜ`. **`Tab−k`** = `Tab−ℜ` with `ℜ = Tier(k)`. "A type of Gentzen-like deductive cut rule for sentences that belong to … `ℜ`" | n/a | Tier(k) (p. 5) |

### Theorems

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| **Theorem 1** | 5 | **(Cited from `Willard2005`.)** For U-Grounding `A` standard-model-valid, there is consistent self-justifying `IS_D(A)` proving all Tier(1) theorems of `A`, all eight U-Grounding totals, and affirming its own Tab−1 consistency | cited | `Willard2005` Thms. 1, 4, 5 |
| **Theorem 2** | 7 | **Main positive.** For such `A`, there is `A' ⊇ A` with `IS_D(A')` consistent, self-justifying under Tab−1, and proving all five Lemma-2 simulated-real totals | full (immediate from Thm 1 + Lem 2) | Thm 1, Lem 2 |
| **Theorem 3** | 7–8 | There is a function `F` mapping each Tier(1)⊕ formula `φ` to an equivalent Tier(1) formula `Φ`, under any axiom system proving all `IΣ₀`'s `Π*₁` theorems | sketch | Lem 1; rules (A)(B) p. 8 |
| Theorem 4 | 9 | **(Solovay 1994 refinement of Pudlák Thm 2.3.)** No consistent Type-S system formalizing `A`/`M` as 3-way relations with associative/distributive/idempotent axioms can prove the non-existence of a Hilbert proof of `0=1` from itself | cited | Solovay; Pudlák; Nelson; Wilkie–Paris |
| **Theorem 5** | 9 | There is a `Π⁻₁` sentence `W` such that no consistent Grounding-language `α ⊇ W` can both affirm its own **Hilbert** consistency and prove Def. 4's **simulated-real addition** total | sketch (via Thm 4: `𝐑 ↦ 𝐑+𝐑` lifts successor on IPN exponents) | Thm 4 |
| **Theorem 6** | 9–10 | There is a `Π⁻₁` sentence `W` such that no consistent `α ⊇ W` can both take Def. 6's **LongMult** as total and affirm non-existence of a **semantic tableaux** proof of `0=1` from itself | sketch (via prior Type-M tableaux negative, lifted from NN to IPN mantissas) | prior Type-M tableaux result (`Willard2002a` line) |

### Lemmas and remarks

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| Lemma 1 | 5–6 | `Θ_S` (`+`,`×`,`−`,`÷`), `Θ_E`, and `Θ_G` (real `≥`) each have a `Δ*₀` encoding. Route: `Δ₀` → Paris–Dimitracopoulos `Δ₀'` → `Δ*₀` via Eq. (3)'s multiplication graph | sketch | Defs. 4–5; Eq. (3) |
| Lemma 2 | 6–7 | Each of Def. 4's four operations, and `Expand`, has totality expressible as a **`Π*₁`** sentence. Via the growth bound `**`: `m₃ ≤ Double(Max(m₁,m₂))` and `e₃ ≤ Double(Double(Max(e₁,e₂)))`, so Eq. (5) with `t = Double(Double(Max(m₁,m₂,e₁,e₂)))` is `Π*₁`; Expand via Eq. (6) | full | Lem 1; Eqs. (4)–(6) |
| Remark 1 | 8 | Most Numerical-Analysis efficiency theorems are Tier(1)⊕; with `A = PA`, `IS_D(A)` proves all PA's Tier(1)⊕ numerical theorems; Tab−1 modus ponens applies to any Tier(1)⊕ sentence via Theorem 3 | n/a | Thms. 2–3 |
| Remark 2 | 8–9 | Number Theory theorems almost never fall in Tier(1) (bounds too tight: `K·Max(v̄)`); Numerical Analysis Cauchy-sequence theorems do, because Tier(1)⊕ real bounds reach essentially `𝐑^J`. G2 applies to both fields but grants Numerical Analysis more evasion room | n/a | Thm 3 |
| Remark 3 | 10 | Contrasts Thms. 2 and 6 via `𝐑₀ = 2.0` (`k` bits), `𝐑ᵢ₊₁ = 𝐑ᵢ * 𝐑ᵢ`: Def. 4 stores `𝐑ₙ = 2^{2ⁿ}` in `k+n` bits; Def. 6 needs `O(k·2ⁿ)` bits. Numerical Analysis is forced to Def. 4 — so Theorem 2's evasion is the computationally relevant one | n/a | Thms. 2, 6 |

## 5. Notation table

| Symbol | Meaning | First anchor |
| --- | --- | --- |
| `A(x,y,z)`, `M(x,y,z)` | 3-way relations for `x+y=z`, `x*y=z` | p. 1 |
| Type-S / Type-A / Type-M | Successor / addition / multiplication totality axioms | p. 1 |
| NN, IPN | Non-negative integers; signed integers plus `∞` | Def. 1, p. 2 |
| `F`, `Even`, `Half` | NN→IPN encoding; `F(1)=∞` | Def. 2, p. 2 |
| `𝐑ᵢ`, `(Mᵢ,Eᵢ)`, `(mᵢ,eᵢ)` | Simulated real (bold); IPN / NN mantissa–exponent pairs | Def. 3, p. 3 |
| `Θ_S`, `Θ_E`, `Θ_G` | Truncated op graph; Expand graph; real `≥` | Defs. 4–5, Lem 1 |
| `LongMult` | Untruncated real multiplication | Def. 6, p. 3 |
| Non-Growth / Grounding / U-Grounding | `F ≤ Max`; six non-growth ops; eight = six + `+` + `Double` | p. 4 |
| `Δ*₀`, `Π*ₙ`, `Σ*ₙ`, Tier(k) | U-Grounding formula classes; Tier(k) = `Π*ₖ ∪ Σ*ₖ` | pp. 4–5 |
| `Tab−ℜ`, `Tab−k` | Tableaux-list deduction with intermediate class `ℜ` / Tier(k) | Def. 7, p. 5 |
| `IS_D(A)` | Self-justifying system of `Willard2005` under Tab−1 | Thm 1, p. 5 |
| `Δ₀`, `Δ₀'` | Classical bounded / relation-only bounded classes (encoding ladder) | Lem 1, p. 6 |
| Eq. (3) | `Δ*₀` multiplication graph via division | p. 6 |
| `**` bound; `t` | `m₃ ≤ Double(Max(m₁,m₂))`, `e₃ ≤ Double(Double(Max(e₁,e₂)))`; `t = Double(Double(Max(m₁,m₂,e₁,e₂)))` | pp. 6–7 |
| `⟨m,e⟩`, `\|⟨m,e⟩\|^J`, `≪_L^J` | Simulated-real notation; absolute power; bounded-real envelope | p. 7 |
| `Δ⊕₀`, `Π⊕₁`, `Σ⊕₁`, Tier(1)⊕ | Classes allowing bounded **real** quantifiers | p. 7 |
| `Π⁻₁` | `Π*₁` sentences containing **no** Addition or Double symbols | p. 9 |
| `Root(x,y)` | `⌈x^{1/y}⌉` (ceiling — matches `Willard2002c`, not `Willard2016`'s floor; drift **D53**) | p. 4 |

## 6. Replicated context

| Block | Extent | Relation |
| --- | --- | --- |
| U-Grounding language, `Δ*₀`/`Π*ₙ`/`Σ*ₙ`, Non-Growth | §3 pp. 4–5 | Standard SJAS preamble; same six Grounding + Addition + Double as `Willard2005` |
| `Tab−ℜ` / `Tab−k` | Def. 7, p. 5 | Same apparatus as `Willard2005`'s `Tab−ℜ−List` / `Tab−U*₁−List` and `Willard2020`'s `Tab-1` (drift D33) |
| `IS_D(A)` construction and Tab−1 self-justification | Theorem 1 | **Delegated entirely** to `Willard2005`; this paper only instantiates it at `A'` |
| Solovay Type-S Hilbert negative | Theorem 4 | Same private-communication thread as `Willard2005` Theorem ∗ / `Willard2001` Appendix A |
| Type-M tableaux negative (integer) | Theorem 6's substrate | Delegated to prior work (`Willard2002a` line); lifted here from NN to IPN mantissas |
| Floating-point totality idea | whole paper | Announced in `Willard2005` Remark 7; first published as `Willard2005-TAB`; polished here. Full-length TR still missing (**G3**/**G36**) |
| `Root = ⌈·⌉` | p. 4 | Same ceiling convention as `Willard2002c`; conflicts with `Willard2016`'s floor (**D53**) |

## 7. Discrepancies and errata

**Internal to this paper.**

- **E1 (author-copy bibliographic collapse).** Every in-text citation is the
  literal placeholder `[??]`; the header still reads "Volume 00, Number 0,
  XXX 0000". The inserted note on p. 1 correctly identifies JSL 71 (2006)
  pp. 1189–1199. Treat mathematics as authoritative; do not reconstruct the
  reference list from this witness.
- **E2 ("gendre").** p. 9: "outside the **gendre** of Theorem 4's formalism"
  — typo for *genre*, confirmed on the page image.
- **E3 (Pudlák spelling).** Consistently "Puldák" / "Puldlák" in the author
  copy (pp. 1, 6, 9), as in several other Willard author copies.

**Cross-paper (drift / obligation seeds).**

- **D70 — conference/journal inventory is not a renumbering.**
  `Willard2005-TAB` and this paper share Defs. 1–4 and the simulated-real
  semantics, but the theorem map is not 1–1: TAB's Theorem 1 is Solovay
  (here Theorem 4); TAB's Theorems 3/6 are the positive floating-point
  results (here compressed into Theorem 2, with `IS_D` imported as Theorem 1
  from the integer `Willard2005`); TAB's Theorem 7 is this Theorem 3; TAB's
  Theorems 4–5 are this Theorems 5–6. TAB also keeps Expand-totality as a
  separate Lemma 3; here it is folded into Lemma 2. The journal form is
  shorter (11 vs 15 pp.) and drops TAB's literature survey and several
  framing paragraphs — **not** a strict expansion (contrast
  `Willard2000-TAB`/`Willard2002a`; same shape as `Willard2006-WoLLIC`/
  `Willard2009`, drift D47).
- **D71 — Def. 3's value formula is silently destroyed by `pdftotext`.**
  Text aid renders `𝐑ᵢ = Mᵢ · 2 − ⌊Log₂(|Mᵢ|)⌋ · 2^{Eᵢ}`-like garbage;
  the page image is `Mᵢ · 2^{−⌊Log₂(|Mᵢ|)⌋} · 2^{Eᵢ}`. Any codified statement
  of simulated-real semantics must be taken from the image (or from a
  carefully reconstructed LaTeX), never from the text layer.
- **O90 — truncated vs LongMult is the floating-point boundary dial.**
  Theorem 2 affirmatively allows Def. 4's truncated `×` as total under Tab−1
  self-justification; Theorem 6 forbids Def. 6's `LongMult`. Remark 3's
  `2^{2ⁿ}` bit-count example is the computational content of that dial.
  This is the corpus's **only** affirmative multiplication-totality result
  for a self-justifying system, and it must not be stated as "multiplication
  is total" without the truncated-mantissa qualifier.
- **O91 — Tier(1)⊕ is where the evasion's robustness is claimed to live.**
  Remarks 1–2 argue that Numerical Analysis theorems fall in Tier(1)⊕ while
  Number Theory theorems do not; Tab−1 modus ponens then applies to the
  translated Tier(1) forms. The codified statement's motivation chapter
  should carry this field-level framing, attributed, as Willard's own account
  of *why* the floating-point exception matters.

**Gap-ledger interactions.**

- Footnote 1 confirms `Willard2005-TAB` as the first announcement; G3/G36's
  unpublished TR remains the only candidate for a fuller proof write-up than
  either published form supplies (both Theorem 5 and Theorem 6 are sketches
  here; Theorem 3 is a sketch; Theorem 2 is immediate from cited Theorem 1).
  The conference form additionally defers several derivations to a "longer
  version" (TAB's gap **G37**); this journal form's Theorem 3 partially takes
  up TAB Theorem 7's Tier(1)⊕ translation, still as a sketch.
- `Willard2005-TAB`'s printed citation of this paper as "pp. 1189–**1999**" is
  a digit slip for 1189–1199 (drift **D57**).

## 8. Saturation record

| Pass | Date | Method | New numbered items | Result |
| --- | --- | --- | --- | --- |
| 1 | 2026-08-29 | Full text read of pp. 1–11; uncapped case-insensitive sweep (`D EFINITION`/`T HEOREM`/`L EMMA`/`R EMARK`) | 7 Defs, 6 Thms, 2 Lemmas, 3 Remarks | Inventory §4 populated |
| 1v | 2026-08-29 | **Visual control pass**, all 11 pages rendered `pdftoppm -r 130 -png` and read as images | 0 new items | Load-bearing formulas confirmed; §8.1 |
| 2 | 2026-08-29 | Re-pass of numbered-item sweep against inventory | 0 | Saturation closed |

Coverage **complete** (pp. 1–11). Page images at
`page-images/willard2006b/p-01.png` … `p-11.png`.

### 8.1 Visual control — what the text layer lost

| Source (visual) | `pdftotext` renders | Consequence |
| --- | --- | --- |
| Def. 3: `𝐑ᵢ = Mᵢ · 2^{−⌊Log₂(\|Mᵢ\|)⌋} · 2^{Eᵢ}` | `Mi · 2 − b Log2 ( \| Mi \| ) c · 2 Ei` | **Severe.** Exponentiation, unary minus, and floor delimiters collapse into subtraction and stray letters. Image governs (D71) |
| Bold `𝐑ᵢ` | bare `Ri` | Simulated-real symbol lost |
| `Π⁻₁` / `Π*₁` / `Π⊕₁` | mangled `Π−` / `Π∗` / `Π⊕` line-breaks (`Π−` often split across lines as `Π−` + `1`) | Class names need visual confirmation at first use |
| `≪_L^J`, `Double^{L'}`, `Double^{J'}` | `JL`, `DoubleL`, `DoubleJ` | Super/subscripts dropped |
| `⌈Log(\|J\|)⌉`, `⌊x÷2⌋`, `⌈x^{1/y}⌉` | `d … e` / `b … c` letter-delimiters | Same floor/ceiling hazard as the C4 pilot |
| `IS_D(A)` | `ISD (A)` (D reads as superscript/adjacency) | Keep as `IS_D(A)` matching `Willard2005` |
| Eq. (3) division form `z/x = y ∧ (z−1)/x < y` | stacked fraction layout flattened | Confirmed equivalent on the page |

Confirmed faithful in the text layer (no visual correction needed): Def. 2's
`F(1)=∞`; Eq. (2)'s `m₂ = 2·m₁ − b₁ AND e₁ = e₂`; Eqs. (4)–(6) totality
shapes; the `**` mantissa/exponent bound; Theorem 2's five-`Ψᵢ` construction;
Remark 3's `k+n` vs `O(k·2ⁿ)` bit counts; the typo "gendre".

Mechanical sweep note (corpus-wide JSL hazard, restated from the C4 pilot):
`pdftotext` emits smallcaps headings as `D EFINITION` / `T HEOREM` / etc., so
a naive `^Definition` grep **misses every heading**.
