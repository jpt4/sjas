# Extraction: Willard2002a

> Tier A extraction (component C9). Template: charter ADR-0001 §Decision.
> **Anchoring**: PDF page = printed page of the author's copy (self-numbered
> 1–33); JSL pagination is 67 (2002) pp. 465–496 and is *not* used here.
> **Text-layer hazard**: `pdftotext` renders the `\underbrace` notation `N̲` as
> `|{z}`; load-bearing constants read from page images.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2002a` |
| Title | How to Extend The Semantic Tableaux And Cut-Free Versions of the Second Incompleteness Theorem Almost to Robinson's Arithmetic Q |
| Venue | Journal of Symbolic Logic 67 (2002) pp. 465–496 |
| Witness | `nachlass/papers/willard2002_semantic_tableaux_robinson_q_author_jsl2.pdf` (33 pp.) |
| Funding | NSF Grant CCR 99-02726 |
| Structure | §1 Introduction (1–4); §2 Notation and Intuition (4–8); §3 The Two Main Theorems (9–12); §4 The Consistency of Q+V (12–20); §5 (20–23); §6 Two Further Generalizations (23–29); Appendix (30–33) |

## 2. Role in corpus

**The negative half of the corpus's central boundary.** Its result is that the
semantic-tableaux Second Incompleteness Theorem holds almost down to Robinson's
Q — and, read against `Willard2001`'s positive results, it locates the boundary
exactly:

> the combination of these prior exceptions … together with our new
> generalization of it, will establish that **it is the very act of changing
> Multiplication from a 3-variable relation to a total function which is the
> exact juncture point where the Semantic-Tableaux version of Gödel's Second
> Incompleteness Theorem becomes valid.** *(p. 4)*

It also **answers a Paris–Wilkie open question of 1981** (§6): the tableaux G2
generalises to `IΣ₀` and all its natural extensions.

## 3. Machinery

**Language**: Robinson's Q — Addition **and** Multiplication as function
symbols. So this paper uses *conventional* `Δ₀`/`Π₁`, not the starred classes:
it lives on the multiplication-total side by construction.

**Tableaux** (§2): six rules; rule 6 instantiates a **whole ∀-block**
`∀v₁…∀v_k Υ ↦ Υ(t₁,…,t_k)`; rule 5 introduces a "pseudo-constant symbol"
(`Willard2001` says "parameter symbol"). **No prenex\* normalisation of the
root** — as in `Willard2020`, against `Willard2001` (obligation O42).

**Consistency notion** (Eq. 3): `∀p ¬SemPrf_α(⊥, p)` — "the weakest possible
definition", deliberately chosen because "it is preferable to employ the weakest
available definition when generalizing the Second Incompleteness Theorems."

**Linear Compressed Encoding**: `g` encodes `i₁,…,i_m` with `Log(g)`
proportional to `Σ Log(i_j + 2)` — "the most efficient possible method to do a
formal Gödel encoding". The density condition, on the negative side.

**Definition 2.1 / the `K`-superscript detour**: `Log(x,k)` is the k-fold
iterated logarithm; `SemPrf^K_α(x,y,z)` says `SemPrf_α(x,y)` **and**
`y < Log(z,K)`. Because α recognises nothing faster than multiplication as
total, this device supplies "the local existence of numbers sufficiently larger
than y". `D^K(α) = Γ(n̄)` with `Γ(g) = ∀h∀y∀z {Subst(g,h) ⇒ ¬SemPrf^K_α(h,y,z)}`.
Willard is explicit (p. 8) that `K` appears **only** on the left of Theorem
2.3(B), so the final theorems concern the conventional notion (3).

**`V = V₁ ∧ V₂ ∧ V₃ ∧ V₄ ∧ V₅`**, five Π₁ clauses:

| Clause | Content |
| --- | --- |
| `V₁` | `S(x,y,z)`, `P(x,u,z)` are Δ₀ graphs of Subtraction and iterated Log, and are total with their usual properties (Lemma 3.1) |
| `V₂` | `A₁…A₅` (Lemma 3.2's five Log/Subtraction laws) plus `A₆`: Addition and Multiplication are associative, commutative, distributive |
| `V₃` | `Subst` is single-valued: `∀g∀h∀h*{Subst(g,h) ∧ Subst(g,h*)} ⇒ h = h*` |
| `V₄` | a least-witness clause: `Υ(α,k,g,h,y,z) ⇒ ∃h*≤h ∃y*≤y ∃z*≤z Υ(α,k,g,h*,y*,z*)` |
| `V₅` | `∀y∀z∀α∀k {[FinAx4(α) ∧ k ≥ α ∧ Paradox(y,z,α,k)] ⇒ ∃x < z SemPrf_α(⊥,x)}` |

**`V₄`'s CLARIFICATION is the paper's most transferable remark.** `V₄` is
*provable from Q*, yet is included as an axiom, because

> a redundant axiom can super-exponentially shorten the length of some cut-free
> and/or Semantic Tableaux proofs.

That is the **2020 Linear-Sum Effect thesis, stated in 2002** — see §7 below.

## 4. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Theorem 2.2 | 7 | Tableaux closure under modus ponens: `α ⊢_S Λ`, `α ⊢_S Θ`, `α ⊢_S Λ∧Θ ⇒ Ξ` give `α ⊢_S Ξ`, by Gentzen cut-elimination. **Its proof sketch notes the composed proof "can certainly be super-exponentially longer than the combined lengths"** — the absence of the Linear-Sum Effect for tableaux | sketch |
| Theorem 2.3 | 7 | A finite extension α of Q proving (A) `∀p ¬SemPrf_α(⊥,p)`, (B) `{∃y∃z SemPrf^K_α(⌜D^K(α)⌝,y,z)} ⇒ ∃x SemPrf_α(⊥,x)`, (C) `Subst` single-valued, is **inconsistent** | full |
| Lemma 3.1 | 9 | Δ₀ formulae `S`, `P` for Subtraction and iterated Log; `V₁` asserts their totality | full (Appendix) |
| Lemma 3.2 | 9 | Five Π₁ properties `A₁…A₅` of Subtraction and `Log(x,k)` | full |
| Lemma 3.3 | 10 | `Map(α,k,d)` and hence `Paradox(y,z,α,k)` are Δ₀ | sketch |
| **Theorem 3.4** | 11 | **Q+V is consistent** | full (§4) |
| **Theorem 3.5** | 11 | No consistent **finite** extension α of Q+V can prove `∀x ¬SemPrf_α(⊥,x)` | full |
| Comment 3.6 | 11 | Many Π₁ sentences `V` work; the choice trades proof length between 3.4 and 3.5. Willard deliberately chose one making **3.5 extremely short and 3.4 long**, and warns the reader that 3.5's proof will look "alarmingly abbreviated" | n/a |
| Lemma 4.1 | 12 | `V₁`–`V₄` are valid Π₁ sentences in the standard model | full |
| Lemma 4.2 | 12 | If `V₅`'s antecedent holds then `y < Log(z, 2^{1,000})` — because `k ≥ α` and α needs "substantially more than 1,000 bits" to encode `Q+V₁+…+V₄` | full |
| Definition 4.3 | 13 | The **canonical binary representation** `N̲`: a term of length `O(Log N)` using only `0,1,2` in Horner form `(b₀ + 2·(b₁ + 2·(…)))` | n/a |
| Lemma 4.4 | 13 | A Δ₀ sentence with `m` bounded quantifiers bounded by `≤ n` has a tableaux proof *or* disproof of size `O(s·n^{2m})` — brute-force search simulated inside the tableau | sketch |
| Definition 4.5 | 14 | **Closed Subtree Rooted in Ψ**: a tableaux structure whose root is `Ψ` rather than `¬Ψ` | n/a |
| Lemma 4.6 | 14 | A proof `p` of (20) maps to a closed subtree `q` rooted in the existential, path lengths growing by at most a constant **`C ≅ 12`** for §2's definition (image-verified). Willard notes `C` is apparatus-dependent | stated (transformation "trivial, and omitted") |
| Lemma 4.7 | 14 | A tableaux proof from Q+V of `∃r Log(r, e̲) > n̲` has length `O{[LogLog(z)]^C}`, built from `u₀ = 2`, `u_{i+1} = (u_i)²`, `z < u_n ≤ z²` — **multiplication-totality is what makes this short** | sketch |
| Lemma 4.8, 4.9 | 18 | `V₅` is valid in the standard model (completing Theorem 3.4) | full |
| Definition 5.1 | 20 | **Conventional Deciphering Property**: a *finite* `F ⊆ α` suffices to prove, for each canonical binary term `N̲`, that `N` is a Gödel number of one of α's axioms | n/a |
| Theorem 5.2 | 21 | Theorem 3.5 for **infinite** α satisfying Definition 5.1 | full |
| Definition 5.3 / `V₆` | 22 | A Π₁ sentence: if `β ⊇ Q` and `p` proves `0=1` from `β+φ`, then `∃r < p^K` proving `¬φ` from β. **`K = 2`** suffices for §2's tableaux | n/a |
| Theorem 5.4 | 22 | With `W = V ∧ V₆`: no **ω-consistent** finite extension α of **Q** can prove the non-existence of a tableaux proof of the invalid Σ₁ sentence `¬W` from itself | full |
| Remark 5.5 | 22 | Theorem 5.4 extends to infinite α with the Conventional Deciphering Property | stated-only |
| **Remark 5.6** | 23 | With `Q*` = Q but multiplication a 3-way relation: there **are** ω-consistent extensions of `Q*` that *can* prove the non-existence of a proof of `¬W` from themselves — namely `IS^λ(A)` of `Willard2001`, "on account of its tangibility reflection principle". Moreover **`IS^λ(A)` can internally recognise its own ω-consistency**. Also: all theorems generalise to Resolution, Herbrand and Cut-Free Sequent Calculus with `V` replaced by `V_d` | cited / stated-only |
| Definition 6.1 | 24 | **Hyper-Constructive Semantic Proof** `α ⊢_H Υ`: ∃`Θ_Υ` with the axiom `Θ_Υ ⇒ Υ` in α and `α ⊢_S Θ_Υ` | n/a |
| Definition 6.2 | 24 | **Hyper-Inclusion** `α ⊵ β + Υ` ≡ `α ⊇ β ∧ α ⊢_H Υ` | n/a |
| **Lemma 6.3** | 25 | If `α ⊢_H Υ` via proof `p` of `Θ_Υ`, and `q` proves `Ψ` from `α+Υ`, then some `r` proves `Ψ` from `α` with length exceeding `q`'s by at most `∝ p`. **Five-step construction** — see §7 | full |
| Theorem 6.4 | 25 | Any consistent extension α of **IΣ₀** with the Conventional Deciphering Property cannot prove the non-existence of a tableaux proof of `0=1` from itself — **answering Paris–Wilkie 1981** | sketch |
| Theorem 6.6 | 27 | There is a Π₁ sentence `V⁺` provable from IΣ₀ such that every α with the Conventional Deciphering Property satisfies: if α is a consistent extension of Q with `α ⊢_H V⁺`, then α cannot prove its own tableaux consistency | sketch |
| Theorem A.1 | 30 | `L` is LinH-recognisable **iff** some Δ₀ formula defines it | full |
| Claim A.3 | 31 | `P(x,u,z)` (`Log(x,u) = z`) is Δ₀ — extending Bennett's `P₁` result | full |

## 5. The two lineage findings

### 7a. Lemma 6.3 is the Linear-Sum construction, eighteen years early

`Willard2020` §6 recovers the Linear-Sum Effect for `Xtab` in four steps: root
`¬ψ`; child `φ ∨ ¬φ`; branch to `φ` and `¬φ`; insert a proof of `φ → ψ` below
`φ` and a proof of `φ` below `¬φ`. Lemma 6.3's construction is:

1. root `¬Ψ`;
2. the axiom `Θ_Υ ⇒ Υ` immediately below;
3. `⇒`-elimination splitting into `¬Θ_Υ` and `Υ`;
4. insert `p` (the proof of `Θ_Υ`) below `¬Θ_Υ`;
5. insert `q` (the proof of `Ψ` from `α+Υ`) below `Υ`.

**These are the same construction.** 2002a drives it with an *implication axiom*
via `⇒`-elimination; 2020 drives it with a *LEM axiom* via `∨`-elimination. Both
restore linear-sum proof composition to a cut-free apparatus by making a
**branching axiom** available. Recorded as drift **D30** and obligation **O43**.

The same fact appears a third time as `V₄`'s CLARIFICATION (p. 10) — a redundant
axiom super-exponentially shortens cut-free proofs — and a fourth as Theorem
2.2's proof sketch. `Willard2001`'s Lemma 7.1 comment is the fifth.

### 7b. The squaring sequence, on the negative side

Lemma 4.7 builds `u₀ = 2`, `u_{i+1} = (u_i)²` and uses **multiplication-totality**
to reach `z` in `O(Log Log z)` steps — which is what makes the negative proof's
subtree short. This is the *same sequence* that `Willard2001` Remark 4.5
footnote 7 uses to show the positive engine collapses. One device, both sides.
See `willard2006a.md` §3 and obligation **O44**.

## 6. Discrepancies and errata

1. **The property is named twice.** The abstract and §1 call it the
   "Conventional **Encoding** Property"; Definition 5.1 and every later use call
   it the "Conventional **Deciphering** Property". Same object.
2. **`Willard2004` redefines the same name differently** — there it is "a Σ\*₀
   sentence `Test(n)` such that `n` is an axiom's Gödel number iff `Test(n)`",
   which is not Definition 5.1's finite-subset condition. Drift **D31**.
3. **Definable Cut is two-clause here** (Eq. 1: `Υ(0)` and `∀v Υ(v) ⇒ Υ(v+1)`),
   three-clause in `Willard2001` p. 5 and three-clause but differently in
   `Willard2006a` Eq. (4). Drift **D32**.
4. Rule 6 instantiates a whole ∀-block, where `Willard2001`/`Willard2004`/
   `Willard2020` instantiate one variable at a time. Immaterial, noted.

## 7. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Text-layer read of pp. 1–14 and 20–28; awk item sweep across all 33 pp. | 24 numbered items |
| 2 | 2026-08-26 | Visual control pass, p. 14 | 0 new; confirmed `C ≅ 12`, the underbrace notation, and `u_{i+1} = (u_i)²` |
| 3 | 2026-08-27 | Completion read of pp. 15–19 and 29–33; uncapped item re-sweep | **2 new items** (Claim A.2, Definition 6.5), both dropped by pass 1's `length<95` filter |

**Coverage complete** (`registry/coverage.md`: pp. 1–33). Lemmas 4.8 and 4.9
verified `full`. Zero-new-items re-pass **not yet run**; saturation **open**.

### Added at the 2026-08-27 completion read

- **Lemma 4.8** (p. 15) defines **z-tiny** (`Log(x_i) ≤ O(LogLogLog z)`) and
  **z-adequately small** (`Log(x_i) ≤ O{[LogLog z]^C}`), and splits its proof
  tree into `x₀…x₄`. Its first branch split is `⇒`-Elimination **on the
  redundant `V₄` axiom** — a third site of the branching-axiom construction in
  this paper, alongside `V₄`'s CLARIFICATION and Lemma 6.3 (drift D30).
- **Lemma 4.9** (p. 18) completes Theorem 3.4 via `f(z) = D·[LogLog z]^C`, a
  constant `M`, and a vacuous-satisfaction argument for `z ≤ M`.
- The comment after Lemma 4.7 explains why Lemmas 4.4/4.6/4.7 are abbreviated:
  "our main objectives are not centered around the specific Π₁ sentence `V`".
- **§7's open question**: what is the shortest Π₁ sentence `V` making `Q+V` a
  threshold? "likely that our current sentence `V` could be shortened considerably".
- **§7's closing note is the mirror of `Willard2001` Remark 4.5.** The tableaux
  G2 collapses when multiplication becomes a 3-way relation "essentially [because]
  **Lemma 4.7 and its seemingly trivial short 3-paragraph proof would then become
  no longer valid**", after which "all the other successive stages of our proof
  will collapse in a one-by-one, step-by-step manner". `Willard2001` localises
  the *positive* engine's collapse to the loss of Lemma 4.2; this localises the
  *negative* theorem's collapse to the loss of Lemma 4.7 — **and both lemmas are
  about the squaring sequence** (obligation O44).
- **Claim A.2** (p. 31), missed by pass 1: `P*(x,u,z)` has a LinH procedure; the
  `z ≠ 0` clause forces `(1+Log s_i) ≤ (2/3)(1+Log s_{i−1})`, giving `O(Log x)`.
- **Definition 6.5** (p. 27), missed by pass 1: supporting notation for Theorem 6.6.

## M1 exclusions

| Label | Reason |
| --- | --- |
| Definition 2.1 | Prose mention / non-header; no free-standing Definition 2.1 in the paper |
| Lemma 4.6a | Internal case of Lemma 4.6 |
| Lemma 4.6b | Internal case of Lemma 4.6 |
