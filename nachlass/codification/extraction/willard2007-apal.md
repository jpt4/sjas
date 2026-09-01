# Extraction: Willard2007-APAL

> Tier C extraction (component C11), promoted to **full depth**: gap **G32**
> records that `Willard2009`'s Appendix B rests on this paper, invoking its
> tableaux elimination rules **by number**.
> **Anchoring**: PDF page = printed page of the author's copy (1–48); the
> Annals of Pure and Applied Logic pagination is 146 (2007) pp. 124–149 and is
> not used here.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2007-APAL` |
| Title | Passive Induction and a Solution to a Paris-Wilkie Open Question |
| Venue | Annals of Pure and Applied Logic 146 (2007) pp. 124–149 |
| Witness | `nachlass/papers/willard2007_passive_induction_author_apal7.pdf` (48 pp.) |
| Preprint | SUNY Albany Computer Science Technical Report **SUNYA-CS-06-01**, 1 February 2006 — cited under that number by `Willard2006-WoLLIC` [40] |
| Companion | `Willard2006-WoLLIC` / `Willard2009` ([45] there) |
| Self-description | "This paper is self-contained. It will not require a knowledge of our earlier results." |

## 2. Role in corpus — the negative half of the Kolodziejczyk pair

`Willard2006-WoLLIC` §5 calls the work "the third facet of a 3-part project":
[35] = `Willard2002a` showed `Ax-1` obeys, **this paper** shows `Ax-2` obeys,
and the WoLLIC/`Willard2009` line shows `Ax-3` evades. This is facet two.

Both halves answer **L. A. Kolodziejczyk's email of 16 November 2005**, which
pointed out that `IΣ₀`'s two standard induction schemes are formally different
and asked whether `Willard2002a`'s result generalises to the second. Willard's
answer here is yes, and by a **new and simpler method**.

It also supplies four things the rest of the corpus depends on:

1. **Definition 1's eight elimination rules** — the corpus's canonical
   definition of semantic tableaux deduction, invoked by number in
   `Willard2009` Appendix B (gap **G32**, obligation **O70**).
2. **Passive Induction**, a third way of recovering cut-like power inside a
   cut-free calculus, beside `Xtab`/LEM and the TabList apparatus.
3. **Appendix A's Gödel encoding**, the corpus's coding-density scheme in its
   most explicit form (drift **D25**).
4. **Hyper-Constructivity restated**, which names `Willard2002a` Definition 6.1
   as the ancestor of the Linear-Sum Effect (drift **D30**).

**Willard's own closing statement of the axiom-versus-theorem distinction**
(p. 30, the sharpest in the corpus): "**One of these systems** is even able to
prove the theorem that multiplication is a total function, in a context where
[45]'s main result would **collapse entirely** if such a statement about
multiplication's totality property was merely changed from being a **theorem**
into becoming an **axiom**." Here `[45]` is `Willard2006-WoLLIC` (ENTCS 165,
2006, pp. 213–226) — this paper predates `Willard2009`, which is that paper's
journal expansion. Drift **D29**, obligation **O74**.

## 3. Systems defined

`Q₀` is the eight `Π₁` sentences (9)–(16); `Q₁ = Q₀ +` multiplication totality
(17); `Q₂ = Q₁ +` squaring totality (18). All three prove the same theorems —
"the only difference … are the **lengths of the proofs** they generate".

**Type-i** is the induction scheme: Type-1 is Eq. (1), the Hájek–Pudlák / Kaye /
Krajíček textbook scheme; **Type-2 is Eq. (2), the Wilkie–Paris scheme** of
APAL 35 (1987) pp. 261–302. **Type-i-j** combines Type-i induction with `Q_j`,
giving six systems; **T-1** and **T-2** abbreviate Type-1-0 and Type-2-0. Since
every Type-i-j equals or extends one of these two, results about T-1 and T-2
cover all six.

**Correspondence to `Willard2009`**: Type-1 is its `Ax-1`, Type-2 its `Ax-2`.
But **`Q₀` is not the same system in the two papers** — here it is eight axioms
with no `Max`; there it is nine, including a `Max` axiom. Drift **D51**.

## 4. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| **Definition 1** | 4 | **Semantic tableaux deduction, in eight rules** — the corpus's canonical statement. (1) `∧`-elim; (2) `¬`-elim with seven sub-rules (a)–(g) incl. De Morgan and the four quantifier dualities; (3) `∨` branching; (4) `⇒` branching to `¬Θ`, `Γ`; (5) `∃v Θ(v) ↦ Θ(U)`, `U` a new parameter; (6) `∃v ≤ s Θ(v) ↦ U ≤ s ∧ Θ(U)`; (7) `∀v Θ(v) ↦ Θ(t)`, `t` free in `Θ`; (8) `∀v ≤ s Θ(v) ↦ t ≤ s ⇒ Θ(t)`. A branch is **closed** iff it holds some `Λ` and `¬Λ`; a proof of `Φ` is a tree rooted at `¬Φ` all of whose branches are closed | n/a |
| **Definition 2** | 6 | `SemPrf^K_α(x,y,z)`: `SemPrf_α(x,y)` holds **and** `y ≤ Log(z,K)`, the `K`-fold iterated logarithm. Abbreviated `y ≤ Log^K(z)` | n/a |
| Remark 1 | 6 | Bennett's dissertation gives exponentiation a `Δ₀` graph; `Willard2002a` Lemma 3.1 extends it to `Log(x,k)` and hence to `SemPrf^K`. `y` is **`K`-small** iff `∃z y ≤ Log(z,K)` (4). Such generalizations "are easier to derive when one has available a **cut-permissive** method of deduction … than when only a cut-free deductive calculi is available", because the former can use Definable Cuts | n/a |
| **Definition 3** | 7 | `D(α)`, Gödel's sentence, and the **Generalized Gödel Diagonalization Sentence** `D^K(α) = Γ(N̄)` for `Γ(g) = ∀y∀z∀h { Subst(g,h) ⇒ ¬SemPrf^K_α(h,y,z) }` (5)/(6). "It is only our **intermediate steps** (rather than our final theorems) that will be affected by this change" | n/a |
| **Theorem 1** | 8 | If `α ⊇ Q` proves **A** `∀p ¬SemPrf_α(⊥,p)`, **B** `Subst` functionality, and **C** `{∃y∃z SemPrf^K_α(⌈D^K(α)⌉,y,z)} ⇒ ∃x SemPrf_α(⊥,x)`, then `α` is **inconsistent**. Imported from `Willard2002a` Theorem 2.3 — "the **only** theorem that we will be using from our earlier work" | cited |
| **Theorem 2** | 8 | Theorem 1 with **C** weakened to **C\***, which restricts `y > Log^K M` for a second standard constant `M`. Proof: consistency makes (7) `∀y ≤ Log^K M ¬SemPrf^K_α(⌈D^K(α)⌉,y,M)` true, and it has finite quantifier range so `Q` proves it; with (8) this yields **C** from **C\*** | full |
| **Definition 4** | 13 | `Type-i-j`; **T-1** and **T-2** abbreviate Type-1-0 and Type-2-0 | n/a |
| **Definition 5** | 13 | **Tableaux `Δ₀` Compression**: for every `Δ₀` sentence `Ψ` and every `Γ`, if `α` proves `Ψ` in length `L` and `Ψ ⇒ Γ` in length `G`, there is a semantic tableaux proof of `Γ` of length `O(L + G)`. **The Linear-Sum Effect under a third name** (drift D30) | n/a |
| **Lemma 1** | 13 | Tableaux `Δ₀` Compression holds of (i) any `α` containing `Φ ∨ ¬Φ` for every `Δ₀` `Φ`; (ii) **both T-1 and T-2**; (iii) any extension of them. Part (i) is the LEM-as-axiom construction: insert `¬Ψ ∨ ¬¬Ψ` below the root, branch by Rule 3, hang `Ψ`'s proof under `¬Ψ` and `Ψ ⇒ Γ`'s under `¬¬Ψ`. **Part (ii) is Passive Induction**: instantiate Eq. (20)'s induction axiom at the *passive* formula `¬Ψ`, eliminate `∀x`, `∀z` at the constant `1`, apply `⇒`-elim, and after eight further steps the tree splits into four branches **A** `(¬¬¬Ψ) ∧ (¬¬Ψ)`, **B** `¬¬Ψ`, **C** `¬Ψ`, **D** `¬(0 ≤ 1)`. A and D close at once; B and C carry exactly the sentences of Part (i)'s (22)/(23) | full |
| **Definition 6** | 16 | `Υ_n` (28), the `O(n)`-symbol `Δ₀` sentence asserting `v₀ = 2`, `v_i = v_{i−1}·v_{i−1}`, hence `v_n = 2^{2^n}`. **Verbatim the sentence `Willard2009` Remark 1 calls `Υ_n`** | n/a |
| **Lemma 2** | 16 | For some constant `d`, `Υ_n` has an `O(n^d)` proof from T-1 or T-2. Via `Φ_i =def {Υ_{i−1} ⇒ Υ_i}` (29), each with an `O(i^c)` proof (Appendix D), summed by `n` iterations of Lemma 1(ii): `Σ i^c = O(n^{c+1})` (30), so `d = c + 1` | full |
| **Definition 7** | 16 | `Local(n, Θ)` — `Θ`'s universals rebounded to `2^{2^n}`; **formally** (31), which is `Υ_n` with an added bracketed clause `∀x₁ ≤ v_n … ∀x_k ≤ v_n φ(x̄)`. Willard: `v_n` "represents the quantity `2^{2^n}`. However unlike `2^{2^n}` itself, `v_n`'s implicit representation of this quantity **is permitted to appear in a `Δ₀` formula**!!!" | n/a |
| **Lemma 3** | 17 | If `Θ` (a `Π₁` sentence) has a length-`L` proof from T-i, then `Local(n,Θ)` has one of length `O(L + n^d)`. Because `Υ_n ⇒ Local(n,Θ)` costs `O(L + n Log n)` (footnote 6), and Lemma 1(ii) sums it with Lemma 2's `O(n^d)` | full |
| **Theorem 3** | 18 | If `Θ` has a length-`L` tableaux proof from T-i and `Local(n,Θ) → Γ` a length-`J` one, then `Γ` has one of length `O(J + L + n^d)`. "It allows us to **almost (not quite) simulate a Gentzen-style deductive cut rule for `Π₁` sentences**" — and unlike `Willard2002a` §6 it covers **both** T-1 and T-2 | full |
| **Definition 8** | 20 | `N̲` (**`N` with an underbrace**) is the **canonical binary representation** of `N`: the length-`O(Log N)` term `(b₀ + 2·(b₁ + 2·(b₂ + 2·(…(b_{m−1} + 2·b_m)))))` over the constants 0, 1, 2. `⌈Ψ⌉` is `Ψ`'s Gödel number and `⌈Ψ⌉` underbraced its canonical binary form | n/a |
| Definition 9 | 20 | `\|p\| = Log₂(p)`, the **bit**-length of a proof | n/a |
| Definition 10 | 20 | `SemPrf_i(t,p)`: `p` is a tableaux proof of `t` from T-i | n/a |
| **Definition 11** | 20 | **Trivial Manipulation Property** of a `Π₁` sentence **`℧`**: (A) `IΣ₀` proves `℧`; (B) whenever `Ψ` has a proof `p`, the union of T-i with `Local(p, ℧)` proves `SemPrf_i(⌈Ψ⌉̲, p̲)` by some `q` with `q ≤ O(2^{2^{√p}})` (33) | n/a |
| **Lemma 4** | 21 | Such an `℧` exists. **Informal justification only** — "one of those propositions whose proof is trivial, although somewhat tedious … its formal proof employs a rather lengthy coding argument". Any finite number of `IΣ₀`-provable `Π₁` clauses may be loaded into `℧`, including `Π₁` forms of multiplication and squaring totality (34)/(35) | sketch |
| Remark 2 | 21 | Eq. (33) "is a **sharp over-shoot**"; `q ≤ O(2^{√p})` is achievable, and any tower of 2s would serve, because Theorem 2's `K` can be set to the tower's height. "The reader should truthfully **not pay too much attention** to the details of Equation (33)'s inequality" | n/a |
| **Definition 12** | 22 | **Trivial Exponentiation Property** of `℧*`: (A) `IΣ₀`-provable; (B) for any `j`, `Local(j, ℧*)` with T-i supports an `O(j^d)` tableaux proof of `∃z LogLog(z) ≥ j̲` (36) | n/a |
| **Lemma 5** | 22 | Such an `℧*` exists. Via Lemma 2 and `LogLog(v_j) = j` (37); the clause `∀x LogLog(x²) = LogLog(x) + 1` is what is loaded into `℧*` | sketch |
| **Definition 13** | 23 | **Trivial Substitution Property** of `℧**`, using `Subst(g,h)` in the canonical-binary form of Definition 8 | n/a |
| **Lemma 6** | 24 | Such an `℧**` exists. "Trivial and similar in overall structure to the proofs for Lemmas 4 and 5" — **no proof given** | stated-only |
| Definition 14 | 24 | **Trivial-M** `= ℧ ∧ ℧* ∧ ℧**` | n/a |
| Corollary 1 | 24 | Trivial-M is `IΣ₀`-provable and has all three properties. Immediate from Lemmas 4–6 | full |
| Definition 15 | 24 | `D^K(1)`, `D^K(2)` — `D^K(α)` at `α =` T-1, T-2 | n/a |
| **Lemma 7** | 24 | If `p` proves `D²(i)` from T-i, then T-i `+ Local(p, Trivial-M)` proves `¬D²(i)` by a `q` with `\|q\| = O(2^{√p} + p^d)`. **Proof given in detail** as "very central": a five-part tree — root `¬¬D²(i)` reduced to `D²(i)`; `Υ_p` inserted; `p+1` rounds of Rule 6 creating parameters `U_i = 2^{2^i}`; three `∀`-eliminations substituting `p̲`, `U_p`, `M̲`; then `⇒`/`¬`/`∨`-elims producing the three leaves (43)–(45), each closed by Lemmas 4–6 | full |
| **Theorem 4** | 26 | If `p` proves `D²(i)` then T-i proves `0 = 1` by a `q` with **`\|q\| < c·[2^{√p} + p^d]`** (46). Via `\|q\| ≤ O(\|q₁\| + \|q₃\| + \|q₄\|)` (49) and the three bounds (50)–(52) plus Lemma 7 | full |
| Corollary 2 | 28 | Theorem 4 holds for any extension `α` of T-1 or T-2. "Same as Theorem 4's proof" | full |
| Remark 3 | 28 | The Herbrand analogue is deferred to **Appendix E**, which "is too brief to be a formal proof. Thus, it may be **very beneficial for some other author to compose a more formal proof** about this subject" | n/a |
| **Lemma 8** | 29 | For any `α ⊇` T-1 or T-2 there are `K`, `M` with `α` proving condition **C\*** (54) about itself. Because Theorem 4 and Corollary 2 "can be carried out formally within the domain of `IΣ₀`", with `K = 2` and `M` large | sketch |
| **Theorem 5** | 29 | If `α ⊇` T-1 or T-2 proves "there exists no semantic tableaux proof of 0=1 from `α`", then `α` is **inconsistent**. Theorem 2's A from the hypothesis, B from `α ⊇ IΣ₀`, C\* from Lemma 8 | full |
| **Theorem 6** | 29 | **The main result.** Every consistent extension of T-1 or T-2 is unable to prove a theorem asserting its own semantic tableaux consistency. Contrapositive of Theorem 5 | full |
| **Definition 16** | 37 | **Semantic Tableaux Fragment** supporting `Ψ₁ … Ψ_K`: a subtree `S` of `T` whose root is an axiom of `α`, one of whose branches — the **Major Branch** — contains all the `Ψ_i`, and all of whose other branches close. The Clarifying Comment notes the major branch need **not** close inside `S`. **`Willard2009` Lemma 5 calls this the "pivotal branch"** — drift D52 | n/a |
| Fact 1 | 35 | (Appendix C) An `O(N + M)` linear-sum bound for `y`-focused `Π₁` sentences under Type-2-1, obtained from the Type-2 induction axiom at `φ̄(y) = ¬¬φ(y)` | sketch |
| Fact 3 | 38 | (Appendix C) Under T-2, a semantic tableaux fragment of at most `L·K` nodes exists whose major branch contains all of `u_i = u_{i−1}·u_{i−1} ∧ u_{i−1} ≥ 2 ∧ u_i ≥ 2` (67) for `i = 1 … K`, so `u_i = 2^{2^i}`. By induction on `K`, using the Type-2 induction axiom at `φ*(y) = ∃a ≤ y·y [a = y·y ∧ y ≥ 2 ∧ a ≥ 2]` (68) | sketch |

## 5. Appendices

**A (p. 30) — the Gödel encoding.** Twenty language symbols; a **byte is six
bits**; a proof is a base-64 integer; the twenty symbols take codes **32–51**;
the `i`-th variable is `⌈log₃₂(i+1)⌉ + 1` bytes, first byte `V̂`, remainder `i`
in **base 32**, each byte's lead bit `0`; parameters use `Û` likewise. Two
parenthesis pairs — one punctuating sentences, one delimiting the proof tree,
so `(ψ₁(ψ₂)(ψ₃(ψ₄)))` (55) encodes a four-node tree. Willard calls this
"approximately analogous to what Wilkie–Paris have called a **natural B-adic
encoding**". Drift **D25**.

**B (p. 32) — extending `Willard2002a` to Type-1-0.** `Willard2002a` produced
`Π₁` sentences `V` and `V*` such that no consistent `α ⊃ Q₁ + V` proves its
tableaux consistency and no consistent `α ⊃ Q₂ + V*` proves its Herbrand
consistency. Setting `W = V ∧ V* ∧ (58) ∧ (59)` gives both for `Q₀ + W`. The
appendix also states the **Hyper-Constructive** `∗` invariant in its clearest
form and explains *why* it matters: "the super-exponential growth in proof
length, that normally occurs when one connects together two cut-free proofs,
can be **circumvented** when `V` is the connecting sentence because of its
associated `O(G + L)` linear growth rate."

**C (p. 33, added at the anonymous referee's suggestion) — the pre-Passive-Induction
route.** Restates `Willard2002a` Definition 6.1: `α` has a **Hyper-Constructive
Semantic Proof** of `Υ` iff some `Θ_Υ` — the **Hyper-Constructed Representative**
— satisfies (1) `Θ_Υ ⇒ Υ` is a formal axiom of `α` and (2) `Θ_Υ` is a theorem of
`α`. Conditions (a) and (b) then give the `O(M + N)` bound. For Type-2 "a precise
analog of Hyper-Constructivity will be simply **unavailable** (since an analog of
Condition (a) is absent)"; Facts 1–3 and Definition 16's fragment supply the
replacement.

**D (p. 40) — Lemma 2's remaining step**, the five-part tableaux construction
bounding `Φ_i`'s proof by `O(i^c)`.

**E (p. 42) — the Herbrand analogue, informally.** Squaring as a Skolem function
writes `2^{2^n}` in `n` steps; Skolemized *multiplication* needs `2^n − 1`. The
repair is to abandon Skolem **functions** for an "**ascending sequence** of
Skolemized **constants**". `Υ_n` is `Δ₀`, so it may appear as a passive-inductive
subformula and act as an "almost" or "virtual" axiom, giving the Herbrand
analogue `∗` of Lemma 3. Willard: "Our discussion in this appendix was certainly
informal … we would anticipate that there are some types of proofs of the
Herbrandized version of this incompleteness theorem that partially resemble the
analysis techniques used in Sections 3 and 4."

## 6. Discrepancies and errata

- **`Q₀` names two different systems.** Here it is the eight `Π₁` axioms
  (9)–(16); in `Willard2009` it is **nine**, adding a `Max` axiom. Drift **D51**.
- Eq. (57) prints `∀x ∃w x∗x = z` — the bound variable is `w`, the equated
  variable `z`. Likewise Eq. (59). Both should read `z` throughout (or `w`); the
  main text's (18) has it right as `∀x ∃z x∗x = z`.
- Eq. (43) as printed carries a stray trailing `p̲` outside the `Subst(·,·)`
  parentheses.
- "quite apart from quite apart from its relevance" (p. 4) — duplicated phrase.

## 7. Extraction hazards found

**H2, in its worst form so far: `pdftotext` renders `℧` as the digit `0`.**
Definitions 11–14 and Lemmas 4–6 are about a `Π₁` sentence named **`℧`** (and
`℧*`, `℧**`); the text layer prints "A `Π₁` sentence **0** will satisfy the
Trivial Manipulation Property", which reads as plausible English and is wrong.
A registry row taken from the text layer would have recorded a numeral where the
paper has a symbol.

The underbrace hazard recurs: `N̲`, `⌈Ψ⌉̲`, `p̲`, `M̲` all render as `|{z}` on a
separate line, exactly as in `Willard2009` Appendix B. And Eq. (33)'s
`q ≤ O(2^{2^{√p}})` renders with the tower flattened, which matters because
Theorem 4's Eq. (46) really is the *single* exponential `2^{√p}` — the two are
consistent only because (33) bounds `q` and (46) bounds `|q| = Log₂ q`.

## 8. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Full read of pp. 1–48 including all five appendices; uncapped, case-insensitive item sweep | 16 Definitions, 6 Theorems, 8 Lemmas, 3 Remarks, 2 Corollaries, 3 Facts |
| 2 | 2026-08-27 | **Visual control pass**, pp. 4, 20, 26, 29 | 0 new items; **`℧` recovered** from the text layer's `0`; Definition 1's eight rules, Eq. (33)'s double exponential and Theorem 4's single exponential all confirmed as printed |
| 3 | 2026-08-29 | **Full-document visual pass**, all 48 pages rendered `pdftoppm -r 130 -png` and read; registries populated; Fact 2 added (Appendix C companion, previously swept but unrecorded) | 0 new numbered items beyond Fact 2's registry row; coverage Images **1-48**, state `complete`; G32 closed |

Coverage **complete** (pp. 1–48; pp. 44–48 are the reference list). Registries:
44 results / 3 systems / 11 notation rows under this key.

## M1 exclusions

| Label | Reason |
| --- | --- |
| Definition 6.1 | Cross-reference / non-header citation, not a free-standing APAL Definition 6.1 |
| Theorem 2.3 | Citation of prior literature, not a 2007-APAL numbered item |
| Theorem 6.4 | Citation / cross-reference, not a free-standing 2007-APAL Theorem 6.4 |
