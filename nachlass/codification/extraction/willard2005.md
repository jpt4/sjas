# Extraction: Willard2005

> Pilot extraction (component C4). Template: charter ADR-0001 §Decision.
> Page anchors are the witness's printed page numbers, which coincide with its
> PDF page numbers (verified for pp. 2, 10, 20, 30, 41). The JSL pagination of
> record is 1171–1209; author-copy printed page *n* corresponds to
> approximately JSL page 1170 + *n* (the archived copy is reformatted, so JSL
> anchors are not exact).

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2005` |
| Title | An Exploration of the Partial Respects in Which an Axiom System Recognizing Solely Addition as a Total Function Can Verify Its Own Consistency |
| Venue of record | Journal of Symbolic Logic 70 (2005), pp. 1171–1209 |
| Witness | `nachlass/papers/willard2005_addition_total_consistency_author_jsl5.pdf` (43 pp., author-archived copy from the SUNY Albany page, `jsl5.pdf`) |
| Text aid | `../sources-text/willard2005.txt` (22,107 words) |
| Submitted | August 2004 (Remark 7, p. 30) |
| Structure | §1 Introduction (pp. 1–4); §2 Description of Main Results (pp. 4–8); §3 Formal Description of `IS_D(A)` and `NS^{k,m}_D(A)` (pp. 8–12); §4 Literature Survey (pp. 12–18); §5 Consistency Preservation of `IS_D(·)` (pp. 18–30, subsections 5.1–5.3); §6 Inconsistency of `NS^{k,m}_D(A)` (pp. 30–37, subsections 6.1–6.2); §7 Concluding Remarks (pp. 37–38); Appendix A Gödel encoding (pp. 39–40); Appendix B Part-(B) compactification proof (pp. 40–41); References (pp. 41–43) |

## 2. Role in corpus (Willard's own claims, recorded not verified)

Willard positions this paper as the **hybridization** of his two earlier
affirmative paradigms (§1 p. 3, §4 pp. 14–15):

- **(A)** α recognizes addition but not multiplication as total, D is a
  cut-free method (semantic tableaux, Herbrand) — his [65, 67] = `Willard1993`,
  `Willard2001`;
- **(B)** α assumes neither addition nor multiplication total, D is Hilbert
  deduction, with an "additive naming convention" replacing the
  addition-totality axiom — his [72], the forthcoming APAL paper
  (= `Willard2006a`).

`IS_D(A)` is the hybrid: addition **is** total, and D admits modus ponens for
Π\*₁/Σ\*₁ sentences (Tab−U\*₁−List). Claimed advances over `IS(A)` of
`Willard2001` (§1 p. 2; §5.3 p. 28; §7 p. 37):

1. **Level(1)** rather than Level(0-) self-consistency;
2. deduction strengthened from semantic tableaux to Tab−U\*₁−List (a restricted
   modus ponens);
3. the proof is "significantly simpler" — Definition 5 "did not exist in our
   prior papers" (p. 19) and Theorem 1 replaces the hard-to-visualize
   contradiction proof of [67]'s Theorem 4.3 (Remark 2, p. 21).

Claimed near-maximality (§1 pp. 2–4, §4 pp. 15–17, §7 pp. 37–38): four
generalizations of G2 bound `IS_D(A)` from above — Pudlák/Solovay (Hilbert
consistency), his [70] (Tab−Σ\*₂/Tab−Π\*₂), his [66, 68] (multiplication-total
vs. cut-free Level(0-)), and this paper's own Theorem 6.

**This paper's own two main results are Theorems 5 and 6** (p. 11), a matched
positive/negative pair.

## 3. Systems defined

### 3.1 `IS_D(A)` (§3, pp. 8–10)

Parameterized by an arbitrary axiom system `A` in the U-Grounding language and
a deduction method `D`. Purpose (p. 8): simultaneously (1) prove all `A`'s
Π\*₁ theorems, (2) recognize addition as a total function, (3) contain a
Kleene-like self-referencing axiom asserting its own **Level(1)** consistency
under `D`. Four axiom groups:

| Group | Content | Cardinality |
| --- | --- | --- |
| Group-0 | Four axioms: constants `c̄₀`, `c̄₁`; the growth functions addition and `Double(x) = x+x`. Net effect: every `n ≥ 2` is named by a term `n̄` using `O(log n)` function-symbol occurrences | finite (4) |
| Group-1 | A finite set `F` of Π\*₁ sentences such that `F` ∪ Group-0 proves **every ∆\*₀ sentence true in the standard model**. Any such finite set will do; Table I of [67] gives one | finite |
| Group-2 | Schema: for each Π\*₁ sentence Φ, the axiom `∀p { Prf_A(⌜Φ⌝, p) → Φ }` (Eq. 5), in Π\*₁ normalized form | **infinite schema** |
| Group-3 | One single Π\*₁ sentence asserting that D yields no proofs of both a Π\*₁ sentence and its negation from Groups 0–3 *including itself* | finite (1) |

**Group-3's fixed point** (pp. 9–10). The naive form is Eq. (6):
`∀x∀y∀p∀q ¬[ Pair(x,y) ∧ Prf_{IS_D(A)}(x,p) ∧ Prf_{IS_D(A)}(y,q) ]`.
Its precise construction uses `UNION(A)` = Groups 0+1+2 and four ∆\*₀
predicates (p. 9): (i) `Prf^D_{UNION(A)}(t,p)`; (ii) `ExPrf^D_{UNION(A)}(h,t,p)`
(proof from `UNION(A)` plus the added axiom with Gödel number `h`);
(iii) Gödel's substitution `Subst(g,h)`; (iv) `SubstPrf^D_{UNION(A)}(g,t,p)`,
the hybridization of (ii) and (iii), given the ∆\*₀ encoding Eq. (7):
`Prf^D_{UNION(A)}(t,p) ∨ ∃h ≤ p [ Subst(g,h) ∧ ExPrf^D_{UNION(A)}(h,t,p) ]`.
Then `Γ(g)` is Eq. (8) — Eq. (6) with both `Prf` occurrences replaced by
`SubstPrf^D_{UNION(A)}(g,·,·)` — and with `n` the Gödel number of `Γ(g)`, the
Group-3 axiom is the Π\*₁ sentence `Γ(n̄)`. Consequently
`Prf_{IS_D(A)}(t,p)` is itself ∆\*₀-encodable as
`SubstPrf^D_{UNION(A)}(n̄,t,p)`.

Willard flags the **Π\*₁ (not Π\*₂) encoding as essential**: "Our results would
be substantially less significant if Equation (6) had a weaker Π\*₂ encoding"
(p. 10). The ∆\*₀ encodability of (i)–(iii) is imported from Appendices C and D
of [67] via the theory of LinH functions [20, 25, 73].

### 3.2 `NS^{k,m}_D(A)` (Definition 3, §3 p. 11) — the negative control

Identical to `IS_D(A)` except: (a) Group-0 gains the Σ\*₁ axiom
`Υ(k,m) ≡ ∃z Log^k(z) ≥ m` (Eq. 9); (b) the Group-3 axiom is structurally
identical but its "this" now refers to a system containing `Υ(k,m)` **as an
axiom**. Both systems can *prove* `Υ(k,m)`; the difference is only that
`NS^{k,m}_D(A)` has it as an axiom, which shortens proofs by a constant
`C > 2^m_k` and thereby changes what the Group-3 sentence asserts (§6.1,
pp. 30–32).

### 3.3 `N_D(A,Ψ)` (§6 Remark-adjacent, p. 37)

The generalization of `NS^{k,m}_D(A)` where an arbitrary sentence `Ψ` is the
added Group-0 axiom. **Σ\*₁/Π\*₁ asymmetry**: Theorem 6 makes `N_D(A,Υ(k,m))`
inconsistent for a valid **Σ\*₁** `Ψ`, but when `Ψ` is a valid **Π\*₁**
sentence, Theorem 5 generalizes and `N_D(A,Ψ)` is *always* consistent (p. 37).

### 3.4 Auxiliary / cited systems

`PA` (Peano Arithmetic); `PA+` (PA extended with U-Grounding function symbols;
footnote 7 p. 32 permits PA+ to use multiplication internally, provided the
Π\*₁ theorems fed to Group-2 contain no multiplication symbol); `Q`
(Tarski–Mostowski–Robinson); `Q₀` (Nelson); `IΣ₀`, `IΣ₀+Exp`, `IΣ₀+Ω₁`; GB and
ZF set theories; "Successor-Based Arithmetic" (footnote 1, p. 13).

## 4. Numbered-item inventory

Proof-status values: `full` (complete proof printed), `sketch` (proof sketch
only), `cited` (attributed to another source), `stated-only` (asserted without
proof), `n/a` (definitional).

### Definitions

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| Definition 1 | 10 | **Consistency Preserving.** A map ℑ from an axiom system `A` (U-Grounding language) to `ℑ(A)` is consistency-preserving iff `ℑ(A)` is guaranteed consistent whenever `A`'s Π\*₁ theorems are valid in the standard model. Willard: "the main problem considered in this article is to identify the axiom mapping functions ℑ(·) that are consistency-preserving" | n/a | — |
| Definition 2 | 11 | **Log notation.** `Log` = base-2 logarithm rounded down; `Log^k` = k-fold iterate; `2^m_k` = least `z` with `Log^k(z) ≥ m`; `Υ(k,m)` = the Σ\*₁ sentence `∃z Log^k(z) ≥ m` (Eq. 9) | n/a | — |
| Definition 3 | 11 | **`NS^{k,m}_D(A)`.** As `IS_D(A)` but (a) Group-0 gains `Υ(k,m)`; (b) Group-3's "this" is sensitive to (a) | n/a | §3 `IS_D(A)`, Def. 2 |
| Definition 4 | 19 | **Normed(a,b).** For `a > b ≥ 1`, α is Normed(a,b) iff (1) every axiom of α is a Π\*₁ sentence satisfying `App∀(a)` or a Σ\*₁ sentence satisfying `App∃(b)`; **(2) α's language employs the eight U-Grounding function symbols and absolutely no other function symbols** — "designed to preclude α from employing a multiplication function symbol — or any of its many functional equivalents" | n/a | App∀/App∃ (Eqs. 16–17, p. 18) |
| Definition 5 | 19 | **θ-Compactified.** For α Normed(a,b) with `a > b ≥ 1`, Φ a Π\*₁ or Σ\*₁ sentence, `p` the Gödel number of a D-proof of Φ from α, θ < 1/3 a positive constant, and `p ≤ ⌈a/b⌉ + 1` (Eq. 18): D is θ-compactified iff every such tuple `(α,Φ,p,a,b)` satisfies **(A)** if Φ is Σ\*₁ then Φ has `App∃(b·p^θ)` validity, and **(B)** if Φ is Π\*₁ then Φ has `App∀(a/p^θ)` validity. Willard: "the main engine used to prove `IS_D(·)`'s consistency preservation property… It did not exist in our prior papers" | n/a | Def. 4 |
| Definition 6 | 33 | **CheckProof / F.** `CheckProof(t,p,q)` is a ∆\*₀ formula stating that `Prf_{NS^{k,m}_D(PA+)}(t,p)` holds and `q` is a D-proof of `∃p Prf_{NS^{k,m}_D(PA+)}(t,p)` (Eq. 25) from `NS^{k,m}_D(PA+)`. `F(t,p)` = the minimal `q` satisfying `CheckProof(t,p,q)` | n/a | §6.2 f(s,b) |

### Theorems

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| Theorem ∗ | 14 | **(Cited, Solovay.)** No consistent axiom system satisfying the Successor-Based Arithmetic criteria (footnote 1) can prove the non-existence of a Hilbert proof of `0=1` from itself. Attributed to Solovay's generalization of Pudlák's Theorem 2.3 [41] using Nelson and Wilkie–Paris [34, 64], "as approximately summarized in [52]'s 4-page proof" | cited | Pudlák [41], Nelson [34], Wilkie–Paris [64] |
| Theorem 1 | 19 | **The engine.** If D is θ-compactified for some positive θ < 1/3, then the `IS_D(·)` mapping is consistency-preserving | full | Defs. 1, 4, 5 |
| Theorem 2 | 26 | Semantic tableaux satisfies θ-compactification **when θ = 1/4** | full (Part A §5.2 p. 26; Part B Appendix B pp. 40–41) | Lemmas 1, 2; Eq. (20) |
| Theorem 3 | 27 | `IS_D(·)` is consistency-preserving when D = semantic tableaux | full (immediate) | Thms. 1, 2 |
| Theorem 4 | 28 | Tab−U\*₁−List deduction satisfies θ-compactification **when θ = 1/4** | full (induction on the proof list, reducing to Thm. 2) | Thm. 2; Eqs. (21)–(23) |
| Theorem 5 | 29 | **Main positive result.** `IS_D(·)` is consistency-preserving when D = Tab−U\*₁ deduction: `IS_D(A)` is automatically consistent whenever all `A`'s Π\*₁ theorems hold in the standard model | full (immediate) | Thms. 1, 4 |
| Theorem 6 | 34 | **Main negative result.** For D = Tab−U\*₁−List (stated to generalize to all cut-free methods, e.g. semantic tableaux, Herbrand), there exists `k₀` such that for every `k > k₀`, `NS^{k,m}_D(PA+)` is **inconsistent** | full for the stated Tab−U\*₁−List variant; the "generalizes to all cut-free methods" clause is stated-only | Lemma 4, Cor. 1, Def. 6, Def. 3 |

### Lemmas and corollary

| Label | Page | Statement digest | Proof | Depends |
| --- | --- | --- | --- | --- |
| Lemma 1 | 23 | **Positive Branch.** For `a > b ≥ 1`, Z Normed(a,b), T a Z-based deduction tree whose **U-Height < Log₂a − Log₂b**: T possesses at least one Positive(a,b) branch. Proved by the greedy `Probe(a,b,T)` algorithm (4 steps) plus induction with **nine subcases (a)–(i)**, one per rule: axiom, ∃-elim unbounded (rule 5), ∃-elim bounded (rule 6), ∀-elim unbounded (rule 7), ∀-elim bounded (rule 8), ∧-elim, ¬-elim, ∨-elim, →-elim | full | Positive(a,b) defn. (Conditions I–II, p. 22), Def. 4 |
| Lemma 2 | 25 | **Non-Closure.** No Positive(a,b) branch of a Z-based deduction tree T can be closed by containing both a sentence Υ and its negation. Turns on footnote 3: in Normed systems, Υ and the strict form `¬Υ` can co-occur on a branch only if both are ∆\*₀, and Condition II-c forces ∆\*₀ sentences to be true under the valuation | full | Cond. II-c; footnote 3 |
| Lemma 3 | 33 | There is a constant `C > 0` such that if `q = F(t,p)` then `Log(q) < O{[Log(p)]^C}` — i.e. verifying a proof is quasi-polynomially cheap in the proof's bit-length | **sketch** ("Proof Sketch"; asserts an analogous construction to the prior literature [1, 4, 8, 14, 20, 38, 40, 41, 50, 61, 64], with Appendix A as the encoding) | Def. 6, Appendix A |
| Corollary 1 | 33 | There is `a₀ > 0` such that if `a > a₀` and `p < 2^b_{a−1}` is a proof of an arbitrary theorem `t` from `NS^{k,m}_D(PA+)`, then `F(p,t) < 2^b_a` | full (immediate from Lemma 3) | Lemma 3 |
| Lemma 4 | 33 | There is `a₀ > 0` such that for `a > a₀` and any constant `b`, the Π\*₁ sentence Eq. (26) is both true **and provable from Peano Arithmetic** — the formalized Π\*₁ rendering of Corollary 1 for `t = f(a−1,b)` | full (immediate from Cor. 1 + PA formalization) | Cor. 1 |

### Remarks

| Label | Page | Content | Proof |
| --- | --- | --- | --- |
| Remark 1 | 20 | Theorem 1's machinery also yields consistency preservation for D = **cut-free sequent calculus**, **Herbrand deduction**, **Tab−Q\*₁−List**, and a **Hilbert variant where every intermediate formula must be Q\*₁**. Method: first show D satisfies Definition 5, then apply Theorem 1. Explicitly not carried out — "we will not have the page space to do so" | **stated-only** |
| Remark 2 | 21 | Methodological: proofs by contradiction are hard to visualize; [67]'s Theorem 4.3 had this problem; Theorem 1's proof is much easier to visualize | n/a |
| Remark 3 | 21 | **Why the generalizations of G2 hold**: the excluded deduction methods (Hilbert per Solovay; Tab−Σ\*₂/Tab−Π\*₂ per [70]) fail Parts (A)/(B) of Definition 5. "Some significant generalizations of the Second Incompleteness Theorem take place at exactly the level where Definition 5's formalism becomes no longer applicable" | n/a (interpretive) |
| Remark 4 | 21 | **The destruction mode.** Parts (A)/(B) of Definition 5 are **impossible to satisfy** if Normed's Part-2 is expanded to allow multiplication as a **ninth function symbol** — "intuitively because multiplication has a faster growth property than addition, which will overwhelm Definition 5's constraints." Moreover the Level(0-) tableaux result of [68] implies **no useful analog of Definition 5** exists for systems recognizing multiplication as total, **under any deduction method D, cut-free or otherwise** | first clause stated-only (intuitive); second clause cited to [68] |
| Remark 5 | 27 | The Prenex\* conventions (I: the tableau root stores `¬Φ` in Prenex\* form; II: `¬Υ` for proper Σ\*₁ Υ is not itself counted Π\*₁) are **not necessary** for Theorems 2/3; they only simplify footnote 3 and the openings of Theorem 2's Parts (A)/(B) | stated-only |
| Remark 6 | 30 | The **Tangibility Reflection Principles** of [67] can be hybridized with these results so that Tab−U\*₁ deduction is incorporated into systems verifying their tangibility principles (also compatible with Tab−Q\*₁ and Remark 1's other 1-deductive methods). "The details for constructing such hybrids are very lengthy. They are therefore not done here" | **stated-only** |
| Remark 7 | 30 | Submission history (August 2004) and the **floating-point application**: changing venue from integer to floating-point arithmetic lets Theorem 5's `IS_D(A)` recognize addition, multiplication, subtraction and division as **total** over computer real numbers (mantissa/exponent bit-string pairs, hardware rounding so the output mantissa is no longer than the longer input mantissa). Summarized by conference announcement [71] "and its accompanying full-length technical report" | stated-only; refers out to [71] + an unlocated TR |
| Remark 8 | 36 | **Three stronger versions of Theorem 6**: (1) slightly larger `(k,m)` extends inconsistency to semantic tableaux, Herbrand deduction, and cut-free sequent calculus; (2) `NS^{k,m}_D(A)` stays inconsistent with `A` **empty** for large enough `(k,m)` (footnote 8: definable-cut induction); (3) for each cut-free D there is a finite non-growth `A` and large `(k,m)` where a modified `NS^{k,m}_D(A)` **dropping the addition- and doubling-totality Group-0 axioms** is inconsistent — inconsistent "even though it fails to recognize the assured existence of any integer larger than `2^m_k`" | **stated-only** (all three) |

## 5. Notation table

| Symbol | Meaning | First anchor |
| --- | --- | --- |
| `A(x,y,z)`, `M(x,y,z)` | 3-way relations for `x+y=z`, `x*y=z` | p. 1 |
| Non-Growth function | `F(a₁..aⱼ) ≤ Maximum(a₁..aⱼ)` | p. 4 |
| Grounding Functions | The six: integer subtraction (truncated), integer division (`x÷0 = x`), `Root(x,y)`, `Maximum`, `Logarithm`, `Count(x,j)` | p. 4 |
| U-Grounding Functions | The six grounding functions **plus** addition and `Double(x)=x+x` — **eight** total | p. 4 |
| `n̄` | U-Grounded binary representation of `n`: a term using ≤ `2 Log₂ N` occurrences of `+`/`Double` applied to `1` (example Eq. 3: 25) | pp. 4–5 |
| `∆\*₀`, `Π\*ₙ`, `Σ\*ₙ`, `Q\*ₙ` | U-Grounding-language analogues of ∆₀/Πₙ/Σₙ/Qₙ; ∆\*₀ = all quantifiers bounded, relations `=` and `<`; `Q\*ₙ` = Boolean combinations | p. 5 |
| Level(n) consistency | No Π\*ₙ sentence Υ with `(α,D)`-proofs of both Υ and ¬Υ | p. 5 |
| Level(0-) consistency | No proof of `0=1` from `(α,D)` | p. 5 |
| Introspectively Unified Logic | The pair `(α,D)` where (i) α proves D applied to α is consistent, and (ii) α is in fact consistent | p. 2 |
| Self-Justifying | α such that `(α,D)` is introspectively unified for some frequently employed D | p. 2 |
| Prenex\* Normalized | Sentence that is Π\*ᵢ or Σ\*ᵢ for some `i ≥ 0` | p. 6 |
| Tab−ℜ−List | Sequence `(t₁,p₁)…(tₙ,pₙ)` of tableau proofs; each `pᵢ` may use `t₁…tᵢ₋₁` as axioms; intermediates must lie in class ℜ. Variants: `Tab−Q\*ₖ`, `Tab−U\*ₖ` (`U\*ₖ = Σ\*ₖ ∪ Π\*ₖ`), `Tab−Σ\*ₖ`, `Tab−Π\*ₖ` | p. 7 |
| K–Deduction | Inference methodology with modus ponens for Σ\*ₖ/Π\*ₖ but not higher | p. 1 |
| `App∀(a)`, `App∃(b)` | Envelope conditions restricting **unbounded** quantifiers to `≤ a` / `≤ b`; bounded quantifiers unchanged (Eqs. 16–17) | p. 18 |
| U-Length | Number of U-Grounding function symbols in a node sentence | p. 22 |
| U-Depth `∆(s)` | Sum of `s`'s U-Length with the U-Lengths of all its ancestors | p. 22 |
| U-Height | Maximum U-Depth among a deduction tree's leaves | p. 22 |
| `VAL(u)`, Positive(a,b) | Valuation of parameter symbols with `Val(u) ≤ b·2^{∆(s)}` (Eq. 19) and node conditions II-a/b/c | pp. 22–23 |
| Z–Based Deduction Tree | Tree whose root is an axiom of Z (not a negated target) and whose branches need not close | pp. 21–22 |
| Definable Cut `ϕ(x)` | Eq. (10); Thinning = Eq. (11) | pp. 12–13 |
| Cut-Localized D-consistency | Eq. (15): `∀p { ϕ(p) → ¬Prf^D_α(⌜0=1⌝, p) }` | p. 13 |
| `Consₐ(n)` | Finitistic consistency: no proof of `0=1` from α of length `< n` | p. 32 |
| `f`, `f(s,b)` | Gödel diagonalization sentences for `NS^{k,m}_D(PA+)`; `f(s,b)` restricted to proofs `p < 2^b_s`; Π\*₁ via `Γ(n̄)`, Eq. (24) | pp. 32–33 |
| `ζ(p)` | Map from an `NS`-proof to the shortest `IS`-proof of the same theorem; proof-length gap bounded by `C > 2^m_k` | p. 31 |
| byte | **Six** bits; proofs are base-64 integers; 24 language symbols coded 32–55 | p. 39 |

## 6. Replicated context

Material this paper shares with, or imports from, its siblings — the
de-duplication map's first entries:

| Block | Extent | Relation |
| --- | --- | --- |
| U-Grounding language, `∆\*₀`/`Π\*ₙ`/`Σ\*ₙ`, `n̄` notation | §2 pp. 4–5 | Standard preamble across the SJAS corpus |
| Semantic tableau rules 1–8 | §2 pp. 6–7 | Restated from Fitting/Smullyan [13, 51]; the only divergence is the Prenex\* root requirement (and Remark 5 says even that is dispensable) |
| Group-0/1/2/3 architecture | §3 pp. 8–10 | The `IS(A)` architecture of [67], modified: Level(1) Group-3, Tab−U\*₁−List D |
| Group-1's finite Π\*₁ set `F` | p. 8 | **Delegated** to Table I of [67]; "any other alternate finite set of Π\*₁ axioms is equally suitable" |
| ∆\*₀ encodability of `Prf`, `ExPrf`, `Subst` | p. 10 | **Delegated** to Appendices C and D of [67] via LinH theory [20, 25, 73] |
| Theorem ∗ and the definable-cut literature | §4 pp. 12–18 | Survey; Theorem ∗ itself delegated to [52] and to [67]'s 4-page Appendix A |
| B-adic Gödel encoding | Appendix A pp. 39–40 | "roughly analogous to" Hájek–Pudlák [20] and Wilkie–Paris [64] natural B-adic encodings |
| Eq. (20) `U-Height(p) < (1/5)Log₂(p)` | p. 26 | The 2005 form of the coding-density condition later promoted to a definitional clause (Willard 2011 Definition D.1 part iv: ≥5J bits for J function symbols, Gödel number ≥ 32^J). **Cross-paper link to verify at C6.** |

## 7. Discrepancies and errata

**Internal to this paper.**

- **E1 (argument-order slip).** Definition 6 and Lemma 3 write `F(t,p)`
  (theorem first, proof second); Corollary 1 writes `F(p,t)` for the same
  function (p. 33). The intended reading is `F(t,p)`.
- **E2.** "T HEOREM 2. ." — doubled period before the statement (p. 26).
- **E3.** The abstract uses `Π₁`/`Σ₁` where the body's notation is `Π\*₁`/`Σ\*₁`
  (p. 1); the starred forms are the technically correct ones for the
  U-Grounding language, as §2 p. 5 explains at length.
- **E4 (author-copy artifact).** The archived copy's inserted note reads "This
  paper was pubished in 2005" (p. 1) — typo not present in the JSL version of
  record.
- **E5 (internal notation drift).** `Tab−U\*₁−List` (Theorems 4, 6) and
  `Tab−U\*₁` (Theorems 5, Remark 6) are used interchangeably for the same
  method.

**Cross-paper (seed entries for the drift ledger).**

- **D2 candidate — citation title drift.** Reference [72] is cited here as "A
  new variant of Hilbert styled generalization of the second incompleteness
  theorem and some exceptions to it", Annals of Pure and Applied Logic (2006);
  the published article (corpus key `Willard2006a`) is titled "A generalization
  of the Second Incompleteness Theorem and some exceptions to it", APAL 141
  (2006), pp. 472–496. Pre-publication title. Confirm at C9.
- **D3 candidate — the density constant.** Eq. (20)'s constant is **5**
  (`U-Height(p) < (1/5)Log₂ p`) while Appendix A's encoding uses **6**-bit
  bytes, and Theorem 2's θ is **1/4**. The three constants are distinct and
  must not be conflated (see the fidelity check, `fidelity-check.md` F1).
- **D4 candidate — the Solovay private-communication thread.** Reference [52]
  is "Several private telephone communications during April of 1994", echoed in
  the Acknowledgment ("I thank Robert Solovay for some telephone conversation
  we had in 1994"). The same thread reappears as reference 41 of
  `Willard2020-LFCS`. Solovay never published Theorem ∗; the only expositions
  are Willard's own ([67] Appendix A, 4 pp.; §4 here). Track as a provenance
  chain, not a discrepancy.
- **D5 candidate — `IS(A)` vs `IS_D(A)`.** [67]'s system is `IS(A)`
  (Level(0-), semantic tableaux); this paper's is `IS_D(A)` (Level(1),
  D-parameterized). The names are close and the group architecture shared;
  the codified statement must keep them typographically distinct. Confirm the
  exact `IS(A)` definition at C7.

**Gap-ledger interactions.**

- Remark 7's "accompanying full-length technical report" for [71] is exactly
  gap **G3** (the unlocated ASL-2005 floating-point TR). This extraction
  confirms G3's provenance and raises its value: the TR is the only
  full-length source for the floating-point totality result.
- Remark 1, Remark 6 and Remark 8 are the paper's principal **stated-only**
  claims. Remark 1 in particular is the 2005 witness for the cut-free
  sequent-calculus and Herbrand affirmative cases, which the 2026-07-28
  apparatus audit treats as affirmative — here they are asserted without proof
  (gap **G8** neighborhood; recorded as F3 in the fidelity check).

## 8. Saturation record

| Pass | Date | Method | New numbered items | Result |
| --- | --- | --- | --- | --- |
| 1 | 2026-08-21 | Full sequential read of the page-marked text extraction (43 pp.), plus a mechanical sweep for smallcaps item headings (`D EFINITION`/`T HEOREM`/`L EMMA`/`R EMARK`/`C OROLLARY`) | 26 (Defs. 1–6, Thms. 1–6 + Theorem ∗, Lemmas 1–4, Corollary 1, Remarks 1–8) | Complete first pass; inventory §4 populated |
| 2 | *pending* | Re-pass required for saturation closure (charter A1) | — | — |

Mechanical sweep command (reproducible):

```bash
awk '/^=== PAGE [0-9]+ ===$/{p=$3}
     /^(D EFINITION|T HEOREM|L EMMA|R EMARK|P ROPOSITION|C OROLLARY)/ {printf "p%s:%s\n", p, $0}' paged.txt
```

Note the smallcaps rendering: `pdftotext` emits JSL's smallcaps headings with a
space after the first letter, so a naive `^Definition` grep finds only prose
cross-references and **misses every actual heading**. This is a corpus-wide
extraction hazard; it applies to all JSL-typeset witnesses (`Willard2001`,
`Willard2002a`, `Willard2006b`).
