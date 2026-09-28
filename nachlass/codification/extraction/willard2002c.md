# Extraction: Willard2002c

> Tier B extraction (component C10), taken **first** because C9's reads showed
> four other papers delegate machinery to it. Template: charter ADR-0001.
> **Anchoring**: PDF page *n* = printed page *n + 280* (PDF 1 = LNCS 281).
> Anchors below are **PDF** pages.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2002c` |
| Title | Some New Exceptions for the Semantic Tableaux Version of the Second Incompleteness Theorem |
| Venue | TABLEAUX 2002, Springer LNCS **2381**, pp. 281–297 |
| Witness | `nachlass/papers/willard2002_new_exceptions_tableaux_author_tab2.pdf` (17 pp.) |
| Funding | NSF Grant CCR 99-02726 |
| Structure | §1 Introduction (1–3); §1.1 Notation and main theorem (3–4); §1.2 Definition of `IS-1(A)` (4–7); §2 Conservative branches and Lemma 1 (8–9); §2.1 Structure of main proof (9–12); §2.2 Proofs of (11) and (12) (12–16); Generalizations (16); References (17) |

## 2. Role in corpus — badly under-ruled at Tier B

Four later papers delegate to this one, and `Willard2004`'s appendix says
outright: "we will assume that the reader has already examined our prior paper
[34] and **has a copy of it on his desk**." It is the source of:

- the **Level hierarchy** (Level-N consistency), which `Willard2004` formalises
  and `Willard2005`, `Willard2011` and `Willard2020` all index by;
- **`IS-1(A)`** and its Group-3 axiom, whose `Pair`-form is verbatim the shape of
  `Willard2011` Eqs. (36)/(37) and `Willard2020` Eq. (5);
- the **PROBE** algorithm, **(L,M)-Conservative Branches**, **`Top`**,
  **`Check`**, **`Constraint`** and **`MaxVal`** — the machinery `Willard2004`'s
  appendix invokes by name without restating;
- **`R(i,j)` Tableaux-Hierarchy deduction**, the two-index original of the
  apparatus later called `Tab₁List`, `Tab-U*₁-List` and `Tab-1`.

## 3. The methodological principle (p. 2)

> it is desirable to use the **weakest** possible definition among the
> alternatives `D₁, D₂, D₃, …` when one is seeking to **generalize** the Second
> Incompleteness Theorem. On the other hand, the opposite is true when one seeks
> to develop **boundary–case exceptions** … They become more wide–reaching when
> they use the **strongest feasible** `D_i` available.

This is why `Willard2002a` and `Willard2004` use Level(0-) while `Willard2005`
and this paper push to Level(1): the choice of consistency notion is **forced by
which side of the boundary one is working on**, not free. Obligation **O59**.

## 4. `IS-1(A)`

**Language**: seven Grounding Functions — Subtraction (`x−y=0` when `x ≤ y`),
Division, `Maximum`, `Logarithm`, `Predecessor`, `Root(x,y) = ⌈x^{1/y}⌉`,
`Count`. Classes written `Π_n⁻` / `Σ_n⁻` (as `Willard2001`/`Willard2006a`).

| Group | Content |
| --- | --- |
| Zero | constants for 0 and 1; Addition total as Eq. (1) `∀x∀y∃z x = z − y` |
| 1 | a finite set `F` of `Π₁⁻` sentences fixing the seven functions against `=` and `<` on constant inputs — "**Any** finite set of `Π₁⁻` sentences `F` with this property may be used" |
| 2 | `∀y {Prf^Φ_A(y) ⊃ Φ}` (Eq. 2) for each `Π₁⁻` sentence `Φ` |
| 3 | Eq. (3) `∀x∀y∀p∀q ¬[Pair(x,y) ∧ Prf_{IS-1(A)}(x,p) ∧ Prf_{IS-1(A)}(y,q)]` — **Level-1** |

Remark 1 defers the full `Π₁⁻` encoding of Group-3 to "techniques similar to
Appendixes B through D from our article [24]" = `Willard2001`.

**Admission condition**: "the union of the axiom system `A` with §1.2's Group-1
axiom schema is a consistent system" — a **seventh** variant across the corpus
(drift D24): a consistency requirement, not a truth requirement.

## 5. The engine

**(L,M)-Conservative Branch** (p. 8, image-verified). A branch `β` of a
`Φ`-Based Candidate Tree is Conservative iff some `(L, M, VAL)` satisfies:

- **A.** a new parameter `u` introduced at depth `d` has `Val(u) ≤ Min(M, L·2^d)` (6)
- **B.** `VAL(c̄_K) = K` for the two Group-zero constants
- **C.** `VAL` commutes with function symbols
- **D.** every `Ψ` on `β` has `Ψ^M` valid in the Standard Model

where **`Ψ^M`** bounds only the *previously-unrestricted **universal*** quantifiers
— bounded universals and **all** existentials are left alone. That is a
different truncation from `Willard2001`'s `Ψ̃⇓m`, which bounds every unbounded
quantifier. Drift **D45**.

**Lemma 1**: no candidate tree drawing axioms from a **PRENEX\***-normalised α
can both contain a Conservative Branch and be a tableaux proof. Its proof turns
on prenex\* form — a third paper in which that convention is load-bearing
(obligation **O42**).

**Vacuous Truth** (Eqs. 7–8) is reused by name from `Willard2001` Remark 4.4.

**`Top(P,Q)`** (9) is the minimality condition; **`Check(X,Y,P,Q)`** (10), for
`X = ∀a φ(a)` and `Y = ∃a ¬φ(a)`, holds iff `∀a [a ≤ ½·Max(P,Q)] ⊃ φ(a)`;
**`Constraint(t,β)`** says `t` is a candidate tree with a conservative branch `β`.

**PROBE** (p. 12) builds `Beta(T,L,M)` by four rules: start at the root; at a
binary split take the left child if `Ψ_a^M` is valid, else the right; at a single
child assign a new `∃`-parameter the **smallest** value consistent with earlier
assignments and quit if (6) fails; iterate.

**The two settings**, and the corpus's `½`:

| Lemma | `L` | `M` | `T` | case |
| --- | --- | --- | --- | --- |
| 2 | `1` | `½·Max(P,Q) − 1` | `Q` | `Check` holds |
| 3 | `½·Max(P,Q)` | `Max(P,Q) − 1` | `P` | `Check` fails |

**This is where `Willard2011`'s A-/E-Stability constant `½` comes from**, and the
pair mirrors `Willard2001`'s Lemma 4.8 (`k=0, m=y−1`) / Lemma 5.3 (`k=x, m=z−1`).
The density constant recurs too: `MaxVal(i+1) ≤ Max[L, 2·MaxVal(i)]` with the
proof tree's height `< ⅓ Log₂(M)` — the same `⅓` as `Willard2002a` Lemma 4.2,
`Willard2004`'s appendix and `Willard2006a` Eq. (27).

Footnote 2 restates `Willard2001`'s footnote-9 subtlety: Group-3's validity
cannot be assumed inside Lemma 2's proof, because Lemma 2 exists to establish it.

## 6. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Remark 1 | 5 | Group-3's full `Π₁⁻` encoding needs `Willard2001` Appendices B–D | cited |
| Remark 2 | 5 | `IS-1(A)` is not consistent merely because Group-3 says so — Part-i without Part-ii | n/a |
| Remark 3 | 5 | `IS-1(A)` becomes **inconsistent** if Group-3's `Prf` is made Hilbert rather than tableaux, by Pudlák and Solovay | cited |
| Remark 4 | 5 | It is likewise infeasible to add Multiplication-totality; points to `Willard1993` pp. 328–331 and `Willard2001` Remark 4.5 for the intuition | cited |
| Remark 5 | 6 | **Announces `Willard2004` Theorem 1** during refereeing: a `Π₁⁻` sentence `W` provable from **IΣ₀** blocks any consistent Addition-total α from recognising its **Level-2** tableaux consistency. "There is no space to insert the proof … we will display it elsewhere" | stated-only |
| Remark 6 | 6 | **Initialization Segment**: `Υ(0) ∧ ∀v {Υ(v) ⊃ Υ(v+1)}`; surveys the cut-localised literature and states that Willard's systems "**do not have their consistency statements localized**" | n/a |
| Lemma 1 | 8 | No PRENEX\*-normalised candidate tree with a Conservative Branch is a tableaux proof | full |
| Theorem 1 | 11 | Eqs. (11) and (12) imply `∀X∀Y∀P∀Q {Top(P,Q) ⊃ ¬ω(X,Y,P,Q)}` (13) | full |
| Theorem 2 | 11 | **`IS-1(•)` is consistency-preserving** whenever `A` ∪ Group-1 is consistent | full |
| Lemma 2 | 13 | With `L=1`, `M=½Max(P,Q)−1`, `T=Q`: PROBE finds an (L,M)-Conservative Branch in `Q` | full (8 cases) |
| Lemma 3 | 13 | With `L=½Max(P,Q)`, `M=Max(P,Q)−1`, `T=P`: PROBE finds one in `P` | full (8 cases, 6 shared) |
| `R(i,j)` / `IS-1*(A)` | 16 | `R(i,j)` Tableaux-Hierarchy proofs; `IS-1*(A)` uses `R(1,1)` in Group-3. Its consistency-preservation, and the strengthening to `Willard2001`'s Tangibility Reflection Principle, are **deferred to "a longer version of this paper"** | stated-only |
| `R(2,1)` collapse | 16 | Theorem 2 "and its generalizations **collapse** when `R(2,1)` replaces `R(1,1)`": a `Π₁⁻` sentence `W` provable from IΣ₀ blocks Addition-total systems from recognising the non-existence of an `R(2,1)` proof of `0=1` | stated-only |

## 7. `R(i,j)` — the finest-grained threshold in the corpus

An **`R(i,j)` Tableaux-Hierarchy Proof** is a list `(t₁,p₁)…(t_n,p_n)` with each
`p_k` a tableaux proof of `t_k`, axioms of `p_m` drawn from α or `t₁…t_{m−1}`,
and **every intermediate `t_k` of `Π*ᵢ` or `Σ*ⱼ` format**.

So the apparatus has **two** indices here, which every successor collapses to
one: `R(1,1)` = `Tab₁List` (`Willard2004`) = `Tab-U*₁-List` (`Willard2005`) =
`Tab-1` (`Willard2020`). Drift **D33** extended.

And the threshold is stated **more precisely than anywhere else**: `R(1,1)`
works, **`R(2,1)` fails** — the transition comes from raising the `Π` index alone
while holding `Σ` at 1. `Willard2004`'s `Tab-Π*₂-List` / `Tab-Σ*₂-List` pair
moves both indices and so cannot see this. Drift **D46**, obligation **O60**.

## 8. Discrepancies and errata

1. **Within-paper notation clash**: the body uses `Π_n⁻`/`Σ_n⁻` throughout, but
   the `R(i,j)` definition on p. 16 writes `Π*ᵢ`/`Σ*ⱼ` (starred). Image-verified.
2. **Remark 5 drifts against `Willard2004`** in two ways: here `W` is provable
   from **IΣ₀** and the level is **Level-2**; there `W` is a `Π*₁` theorem of
   **PA** and the level is **Level(2+)**.
3. Two further deferrals to "a longer version of this paper" (`IS-1*`'s
   consistency preservation; the tangibility strengthening) — the same pattern
   as `Willard2004`'s four, and likewise unpublished. Folded into gap **G26**.

## 9. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Full text read of pp. 1–17; uncapped, case-insensitive item sweep | 12 items (6 Remarks, 3 Lemmas, 2 Theorems, plus the `R(i,j)` block) |
| 2 | 2026-08-27 | Visual control pass, pp. 8, 10, 16 | 0 new; confirmed Eq. (6) `Min(M, L·2^d)`, `Ψ^M`'s universals-only truncation, `Check`'s `½`, `MaxVal(i+1) ≤ Max[L, 2·MaxVal(i)]`, height `< ⅓ Log₂(M)`, and the `R(i,j)` / `R(2,1)` statements |

Coverage **complete** (pp. 1–17). Zero-new-items re-pass not yet run.
