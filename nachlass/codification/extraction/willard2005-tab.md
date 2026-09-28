# Extraction: Willard2005-TAB

> Tier C extraction (component C11). Conference form of the **simulated
> real-valued / floating-point** line whose journal successor is
> `Willard2006b` (JSL 71 (2006) pp. 1189–1199). Companion to the ASL-2005
> talk and unlocated University of Albany TR recorded as gaps **G3** / **G36**,
> and the published successor named by `Willard2005` Remark 7.
> **Anchoring**: PDF page = author's printed page (1–15). Springer LNCS 3702
> pagination is pp. 292–306 and is not used here.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2005-TAB` |
| Title | On the Partial Respects in which a Real Valued Arithmetic System Can Verify its Tableaux Consistency |
| Venue | TABLEAUX 2005, Springer LNCS **3702**, pp. 292–306 |
| Witness | `nachlass/papers/willard2005_real_valued_tableaux_author_tab5.pdf` (15 pp.) |
| Journal successor | `Willard2006b` — *Journal of Symbolic Logic* 71 (2006) pp. **1189–1199** (page range corrected from this witness's typo; drift **D72**) |
| Funding | NSF Grant CCR 99-02726 |
| Related gaps | **G3**, **G36** (ASL-2005 floating-point TR still unlocated); **G37** (deferred "longer version" proofs) |

The witness's own citation line (p. 1) records this chapter as the "preliminary
result" and names the JSL draft as the "more polished final draft". That is the
same conference→journal shape as `Willard2000-TAB`/`Willard2002a` and
`Willard2006-WoLLIC`/`Willard2009`: **read as a full witness**, not as a delta
against the journal form (now also extracted: `willard2006b.md`).

## 2. Role in corpus — the one place multiplication-totality coexists with self-justification

Willard's integer-arithmetic affirmative results never let multiplication be a
total *function* while recognizing tableaux consistency (`Willard2000-TAB` /
`Willard2002a` make that impossible for Type-M systems). This paper changes the
**venue** from integer arithmetic to **simulated real-arithmetic** — an
instruction set "slightly more general and powerful than the common floating
point instructions on a digital computer's hardware" (p. 1) — and shows that
the existing self-justifying frameworks `IS(A)`, `IS-1(A)` and `IS_D(A)` can
then prove that addition, multiplication, subtraction, division and `Expand`
are each total **among simulated reals**, while still recognizing Level(1) or
`Tab-1` consistency.

That is exactly the transfer `Willard2005` Remark 7 announced and referred out
to the ASL-2005 talk plus its accompanying TR (**G3**/**G36**). This chapter
and `Willard2006b` are the published witnesses for that claim; the TR remains
the only candidate for the "full-length" deferred proofs (gap **G37**).

The paper's own framing of what it contributes (pp. 1–2, 13–14):

1. **Positive half** (Theorems 3 and 6): simulated-real totality of the five
   operations coexists with Level(1) / `Tab-1` self-consistency.
2. **Negative half** (Theorems 4 and 5): the evasion does **not** lift to
   Hilbert consistency under truncated real-addition totality (`AddComp`), nor
   to Level(0-) tableaux consistency under **untruncated** real-multiplication
   totality (`LongMult`). Obligation **O90**.
3. **Robustness claim** (Theorem 7 + the Tier(1)^⊕ discussion): for real-valued
   arithmetic, `Tab-1`'s Gentzen-like cut is far more useful than for integer
   arithmetic, because bounded *real* quantifiers range up to essentially
   `R^J` rather than a scalar multiple of their integer inputs. Obligations
   **O91**, **O92**.

## 3. Systems defined

This paper mostly **applies** systems defined elsewhere; the new content is the
simulated-real language profile and the two negative threshold sentences.

**Imported (cited):**

- **`IS(A)`** and **`IS-1(A)`** from [35, 37, 39] = `Willard1993` /
  `Willard2001` / `Willard2002c` (Theorem 2's summary). Level(0-) and Level(1)
  tableaux self-consistency respectively; both recognize integer-addition as
  total; both treat integer multiplication as a 3-way relation.
- **`IS_D(A)`** (also written `ISD(A)`, and identified with the last page of
  [39]'s `IS-1*(A)`) from the forthcoming [41] = `Willard2005`. Recognizes its
  consistency under **`Tab-1`** deduction. Drift **D75**.

**Defined / constructed here:**

- **Simulated real-arithmetic** (Definitions 1–5): NN / IPN integers, the
  bijection `F`, mantissa–exponent pairs `R_i = (M_i, E_i)`, the four
  truncated operations `Θ_S` for `S ∈ {+, ×, −, ÷}` under hardware-style
  rounding (output mantissa length = max of input lengths), and `Expand(R)`.
- **`A'`** (Theorem 3's proof): `A` plus the five `Π*_1` totality sentences
  `Ψ_1 … Ψ_5` for `+`, `×`, `−`, `÷`, `Expand`. Then **`IS-1(A')`** is the
  system realizing Theorem 3, and **`IS_D(A')`** realizes Theorem 6.
- **`AddComp` / `LongMult`** (Definition 6): Grounding-language (`Δ⁻_0`)
  predicates for truncated real-addition and **untruncated** real-multiplication.
  These are the payloads of the two negative theorems.
- **`Tab-ℜ` / `Tab-1`** (pp. 11–12): hybrid deduction allowing a Gentzen-like
  cut on intermediate theorems from class `ℜ`; `Tab-1` is the case
  `ℜ = Tier(1)`. Terminology attributed to [40] = `Willard2004`; the construct
  is the same apparatus as `Tab₁List` / `Tab-U*_1-List` (drift **D33**).
- **`Tier(1)^⊕`** (p. 13): `Π_1^⊕` / `Σ_1^⊕` sentences whose stems allow
  **bounded real quantifiers** `∃⟨m,e⟩ ≪_L^J R` and `∀⟨m,e⟩ ≪_L^J R` in
  addition to the usual bounded integer quantifiers. Theorem 7 translates every
  such sentence into an equivalent ordinary `Tier(1)` sentence.

**Taxonomy restated (pp. 2–3):** Type-M / Type-A / Type-S / Type-NS, keyed to
Equations (1)–(3) for integer successor / addition / multiplication totality —
the same classification later canonicalized in `Willard2016` Example 3.3.

## 4. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| **Theorem 1** | 3 | Solovay's modification of Pudlák's Theorem 2.3 (via Nelson and Wilkie–Paris): no consistent **Type-S** axiom system, with integer addition and multiplication as 3-way relations `A` and `M`, can prove the non-existence of a Hilbert-proof of `0=1` from itself | cited |
| Level(n) / Level(0-) | 6 | Unnumbered pair of definitions: **Level(n)** = no `Π*_n` sentence has simultaneous tableaux proofs of both itself and its negation; **Level(0-)** = no proof of `0=1`. Uses `Π*_n` / `Σ*_n` (U-Grounding) in §§3,5 and `Π⁻_n` / `Σ⁻_n` (Grounding only) in §4 | n/a |
| **Theorem 2** | 6 | Summary of [35, 37, 39]: for consistent U-Grounding `A`, one can build consistent `IS(A)` and `IS-1(A)` that (1) prove all of `A`'s `Π*_1` theorems, (2) recognize integer-addition as total, (3) recognize Level(0-) / Level(1) tableaux consistency respectively | cited |
| **Definition 1** | 7 | **NN** = non-negative integers; **IPN** = positive-or-negative whole numbers plus a reserved symbol for `∞` | n/a |
| **Definition 2** | 7 | Bijection `F: NN → IPN`: `Even(x)·Half(x)` for `x ≠ 1`, and `F(1) = ∞`. Lower-case letters denote NN; upper-case denote IPN | n/a |
| **Definition 3** | 7 | **Simulated Real-Number** `R_i = (M_i, E_i)`: (1) if `E_i ≠ ∞` and `0 ≠ M_i ≠ ∞` then `R_i = M_i · 2^{−⌊Log₂\|M_i\|⌋} · 2^{E_i}`; (2) if `E_i = ∞` and `M_i` is a power of 2, then `R_i` is zero with `Log(M_i)` fractional bits; (3) otherwise an overflow symbol (e.g. after division by zero). NN pairs `(m_i, e_i)` map via `F` | n/a |
| **Definition 4** | 7–8 | `Θ_S(m₁,e₁,m₂,e₂,m₃,e₃)` for `S ∈ {+, ×, −, ÷}`: truncated floating-point combination under hardware rounding — output mantissa bit-length `L = max(length(m₁), length(m₂))`, closest feasible approximation at that length | n/a |
| **Lemma 1** | 8 | Each `Θ_S` has a `Δ*_0` encoding. `+`/`−` routine; `×`/`÷` initially `Σ*_1`, compressed to `Δ*_0` by splitting mantissas into equal-length substrings so existential quantifiers become bounded. "**A much more meticulous analysis in a longer version of this paper**" (gap **G37**) | sketch |
| **Lemma 2** | 8 | Totality of each `S` is encodable as a `Π*_1` sentence. Eq. (8) is the natural `Π*_2` form; the bound `**` (`m₃ ≤ Double(Max(m₁,m₂))` and `e₃ ≤ Double(Double(Max(e₁,e₂)))`) plus `t = Double(Double(Max(m₁,m₂,e₁,e₂)))` yields the `Π*_1` form Eq. (9) | full |
| **Definition 5** | 8 | **`Expand(R)`**: same value as `R` with one extra mantissa bit. `Θ*(m₁,e₁,m₂,e₂)` is the `Δ*_0` formula Eq. (10): `m₂ = Double(m₁) − Count(m₁, 1) ∧ e₁ = e₂` | n/a |
| **Lemma 3** | 9 | `Expand` totality is `Π*_1` via Eq. (11): `∀m₁ ∀e₁ ∃m₂ ≤ Double(m₁) ∃e₂ ≤ e₁ Θ*(…)` | full |
| **Theorem 3** | 9 | For every consistent U-Grounding `A` there is consistent `α` that (1) proves all of `A`'s `Π*_1` theorems, (2) recognizes integer-addition as total, (3) confirms simulated-real `+`, `×`, `−`, `÷`, `Expand` are each total, (4) recognizes its own **Level(1)** semantic tableaux consistency. Construction: `A' = A ∪ {Ψ₁…Ψ₅}`, take `IS-1(A')` | full |
| **Definition 6** | 9–10 | **`AddComp`**: `Δ⁻_0` truncated real-addition (same rounding as Def. 4). **`LongMult`**: `Δ⁻_0` **untruncated** real-multiplication (no floating-point truncate-and-round; mantissa bit-length doubles) | n/a |
| **Theorem 4** | 10 | There is a `Π₁⁻` sentence `W` such that no consistent Grounding-language `α ⊃ W` can both prove its own **Hilbert** consistency and prove Eq. (12)'s `Π₂⁻` sentence asserting `AddComp` is total. Mechanism: from (12) infer successor-totality via the IPN specialization Eq. (13) at `C̄ = +1`, then apply Theorem 1. "**A longer version of this paper**" will detail the derivation from (13) (gap **G37**). Statement uses `Π₁⁻`; proof sketch once writes `Π₁*` — drift **D74** | sketch |
| **Theorem 5** | 11 | There is a `Π₁⁻` sentence `W` such that no consistent `α ⊃ W` can both prove its Level(0-) tableaux consistency and prove Eq. (14)'s `Π₂⁻` sentence asserting `LongMult` is total. Via IPN, `LongMult` forces integer multiplication-totality; take `W = V ∪ Φ` where `V` is the Type-M threshold of [36]/[38] = `Willard2000-TAB`/`Willard2002a` | sketch |
| **Theorem 6** | 12 | Stronger form of Theorem 3: same (1)–(3), but (4) is recognition of consistency under **`Tab-1`** deduction. Proof: identical to Theorem 3 with `IS-1` replaced by `IS_D` from [41] = `Willard2005` | sketch |
| **Theorem 7** | 13 | There is a computable `F` mapping each `Tier(1)^⊕` formula `φ` to a logically equivalent `Tier(1)` formula `Φ`. Proof via LinH functions [43]; "**rather lengthy. It will thus appear in a longer version of this article**" (gap **G37**; partially taken up in `Willard2006b` Theorem 3) | stated-only |

**Unnumbered but load-bearing** (recorded here so saturation does not miss them):

- **Introspectively Unified Logic** / **Self-Justifying** (p. 3): pair `(α, D)` where `α` proves its own `D`-consistency and is in fact consistent.
- **`Tab-ℜ` proof** (pp. 11–12): sequence `(t₁,p₁)…(tₙ,pₙ)` with each `p_i` a tableaux proof of `t_i` from `α` plus earlier `t`'s, and every intermediate `t_i` in class `ℜ`.
- **`≪_L^J` / Bounded Real Quantifiers / `Δ₀^⊕` / `Tier(1)^⊕`** (pp. 12–13): the floating-point enrichment of `Tier(1)` that Theorem 7 translates back.

## 5. Notation table

| Symbol | Meaning | Anchor |
| --- | --- | --- |
| `A(x,y,z)`, `M(x,y,z)` | 3-way integer addition / multiplication relations | p. 1 |
| Type-M / A / S / NS | Integer totality taxonomy keyed to Eqs. (1)–(3) | p. 2 |
| U-Grounding / Grounding | Eight-function signature (six non-growth + `+` + `Double`) vs six non-growth only | pp. 5–6 |
| `Π*_n` / `Σ*_n` / `Δ*_0` | Hierarchy over U-Grounding | p. 5 |
| `Π⁻_n` / `Σ⁻_n` / `Δ⁻_0` | Hierarchy over Grounding (no growth functions) | p. 6 |
| Level(n), Level(0-) | Tableaux consistency strengths | p. 6 |
| `IS(A)`, `IS-1(A)`, `IS_D(A)` / `ISD(A)` | Imported self-justifying systems | pp. 6, 12 |
| NN, IPN, `F`, `Even`, `Half` | Integer formalizations and the bijection | p. 7 |
| `R_i = (M_i, E_i)`, `(m_i, e_i)` | Simulated real (IPN / NN encodings) | p. 7 |
| `Θ_S`, `Θ*` | Truncated real operation; Expand relation | pp. 7–8 |
| `Expand(R)` | One extra mantissa bit of precision | p. 8 |
| `t = Double(Double(Max(m₁,m₂,e₁,e₂)))` | Totality bound for Eq. (9) | p. 8 |
| `AddComp`, `LongMult` | Truncated real-addition; untruncated real-multiplication | pp. 9–10 |
| `C̄` | The IPN constant `+1` | p. 10 |
| `Tab-ℜ`, `Tab-1`, `Tier(k)` | Hybrid cut-restricted tableaux; `ℜ = Tier(1)` | pp. 11–12 |
| `⟨m,e⟩`, `\|⟨m,e⟩\|^J`, `≪_L^J` | Mantissa–exponent pair; absolute power; length-and-magnitude constraint | p. 12 |
| `Δ₀^⊕`, `Π₁^⊕`, `Σ₁^⊕`, `Tier(1)^⊕` | Hierarchy allowing bounded real quantifiers | p. 13 |
| `Δ*_0` Stem | The bounded-quantifier matrix of a `Tier(1)` sentence | p. 13 |

## 6. Replicated context

- **Equations (1)–(3)** and the Type-M/A/S/NS taxonomy: standard corpus framing, restated here before the venue change.
- **Theorem 1** (Solovay–Pudlák): same statement as `Willard2005`'s Theorem ∗ and `Willard2001` Appendix A.
- **Theorem 2** (`IS(A)` / `IS-1(A)`): summary of the affirmative integer-addition line; no new proof.
- **`Tab-1` / `IS_D(A)`**: deferred to [40]/[41] = `Willard2004`/`Willard2005`; this paper only *applies* them to `A'`.
- **Type-M threshold `V`** used in Theorem 5: imported from `Willard2000-TAB` / `Willard2002a`.
- Literature survey of localized `(α,D,φ)`-consistency (Eqs. (4)–(6), Sequence / Induction) is the same contrast with self-justifying systems that `Willard2005` §4 develops at greater length.

What is **not** replicated — and is this paper's contribution — is the simulated-real encoding, the five totality sentences, Theorems 3–7, and the Tier(1)^⊕ robustness argument.

## 7. Discrepancies and errata

- **JSL page-range typo (p. 1).** This witness prints "Journal of Symbolic Logic 71 (2006) pp. 1189–**1999**". The journal successor `Willard2006b` correctly prints **1189–1199**. Drift **D72**.
- **Theorem 4's class for `W`.** The theorem statement (p. 10) says a `Π₁⁻` sentence; the proof sketch (p. 10) once writes "`Π₁*` sentence `W`". Under the paper's own convention (§3), `*` is U-Grounding and `−` is Grounding — and Theorem 4's `α` is explicitly Grounding-language. Drift **D74**.
- **`≪` second exponent typo (p. 12).** The notation line introduces `⟨m,e⟩ ≪_L^J ⟨n,e⟩` but clauses 1–2 quantify over `⟨n,f⟩`. The body uses `f`; treat the heading's repeated `e` as a slip.
- **Definition numbering restart.** Page 6's Level(n) / Level(0-) pair is numbered "1." / "2."; pages 7–10 then restart at **Definition 1** for NN/IPN. Drift **D73**.
- **`ISD(A)` vs `IS_D(A)`.** Page 12 writes both `ISD (A)` (no subscript) and identifies it with [39]'s `IS-1*(A)` and [41]'s forthcoming system — i.e. `Willard2005`'s `IS_D(A)`. Drift **D75**.
- **Conference / journal inventory is not a renumbering.** Shared Defs. 1–4 and the truncated-vs-LongMult dial, but theorem numbers diverge after Definition 4 (drift **D70**). The journal form is shorter, not a strict expansion.
- **Def. 3's value formula is destroyed by `pdftotext`** (drift **D71**): image reads `M_i · 2^{−⌊Log₂(|M_i|)⌋} · 2^{E_i}`; text layer collapses exponentiation and floors into subtraction.
- Doubled words: "it is it is useful" (p. 2); "one one throws away" (p. 11).

## 8. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-29 | Full text read of pp. 1–15 (including references); uncapped, case-insensitive numbered-item sweep | 7 Theorems, 3 Lemmas, 6 Definitions, plus the Level(n)/Level(0-) pair and the Tab-ℜ / Tier(1)^⊕ apparatus definitions |
| 2 | 2026-08-29 | **Visual control pass**, all 15 pages rendered `pdftoppm -r 130 -png` and read as images | 0 new items; confirmed: Def. 3's three-case real encoding; Lemma 2's bound `**` and term `t`; Eq. (10)'s `Double(m₁) − Count(m₁, 1)`; Eq. (13)'s `C̄`; `≪_L^J`; the p. 1 "1189–1999" typo; Theorem 4's `Π₁⁻` statement vs proof-sketch `Π₁*` |

Coverage **complete** (pp. 1–15; pp. 14–15 are references). Re-pass after the visual control found **zero** new numbered items. Deferred proofs recorded as gap **G37** / obligation **O93**.
