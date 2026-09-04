# Replication Map (C13)

`prose/sjas_synthesis.txt` asks the codification to **de-duplicate replicated
context**. This file says what is replicated, where, and what each recurrence
adds — because in almost every case one recurrence is the fullest and the rest
are compressions of it. C14–C15 should present each block **once**, from the
source named here, and cite the rest as variants.

Nine blocks account for most of the repetition across ~600 pages.

---

## 1. The doubling/squaring device

The single most repeated object in the corpus. Two sequences from the same
starting point, one additive and one multiplicative:

`x₀ = y₀ = 2`, `x_i = x_{i−1} + x_{i−1}`, `y_i = y_{i−1} · y_{i−1}`, so
`x_n = 2^{n+1}` and `y_n = 2^{2^n}` — hence `Log(x_n) = n+1` but
`Log(y_n) = 2^n`. Only the additive sequence's encoding is short enough to hold
its own derivation.

| Source | Form | Adds |
| --- | --- | --- |
| `Willard1993-TR` printed p. 6 | `a₀ ≥ 2`, `a_{i+1} = (a_i)²` — "`a_n` requires at least `2^n` bits"; then `b_{i+1} = b_i + b_i` | **The origin occurrence, in the full-length text.** Framed as a philosopher's hedge: asked to build the squaring sequence, the expected reply is that "multiplication is a function in theory but not always in practice", whereas "the philosopher would not give such a quick answer about addition". Added 2026-09-02, image-verified; the map previously carried only the 12-page published abbreviation |
| `Willard1993` pp. 328–331 | prose explanation | the earliest statement; `Willard2002c` Rem. 4 points here |
| `Willard2001` Rem. 4.5 fn. 7 | `u₀ = 2`, `u_{i+1} = u_i²`, `VALUE(u_n) = 2^{2^n}` | localises the failure to **one lemma** (no analogue of Lemma 4.2) |
| `Willard2000-TAB` Lem. 8 + §5 | `u_{i+1} = (u_i)²`, `z < u_n ≤ z²`, `O{LogLog z}` nodes | **why the negative proof needs it** (O76) |
| `Willard2002a` Lem. 4.7 | same | the "true reason" paragraph in §7 |
| `Willard2006a` Eqs. (1)–(3) | **three** conventions: incremental, additive, multiplicative | the three-way ordering, and *Continuous Expansion* |
| `Willard2007-APAL` Def. 6, Fact 3 | `Υ_n`; `u_i = u_{i−1}·u_{i−1}` in `L·K` nodes | the tableaux-fragment construction |
| `Willard2009` Rem. 1, Lem. 5 | `Υ_n` vs `Υ^R_n`: `O(n)` vs `2^n` characters | the **encoding-length** form |
| `Willard2016` §5 | `C_j = 2^{j−1}` vs `C*_j = 2^{2^{j−2}}` | the naming-convention form |
| `Willard2020-LFCS` p. 11, `Willard2020` §5 | Eqs. (7)–(8) | the cleanest published statement |

**Present once from `Willard2020-LFCS` p. 11** (clearest), with
`Willard2006a` p. 7's three-way ordering (incremental too slow, multiplicative
too fast, additive the compromise) and `Willard2000-TAB` §5's observation that
the *negative* proofs consume the same device. Obligations **O44**, **O50**,
**O76**.

---

## 2. Recovering the cut inside a cut-free calculus

Three devices, never presented together, all solving the same problem:
`Willard2002a` Thm 2.2 notes that Gentzen cut elimination guarantees the
combined proof **exists** while its length "can certainly be super-exponentially
longer".

| Device | Source | Mechanism |
| --- | --- | --- |
| **LEM as axioms** (`Xtab`) | `Willard2020` App.; `Willard2007-APAL` Lem. 1(i) | admit `℧ ∨ ¬℧`; branch; hang the two proofs |
| **Restricted-cut hierarchy** | `Willard2005` §2; `Willard2004` §6; `Willard2009` Def. 7 | `Tab-ℜ-List` / `Tab₁List` / `Herb−k`: intermediate theorems confined to a class |
| **Passive Induction** | `Willard2007-APAL` §3 | instantiate the induction axiom at a **variable-free** formula; four branches, two of which close at once |

All three yield the **Linear-Sum Effect**: a length-`O(L+G)` proof of `Γ` from
proofs of `Ψ` and `Ψ ⇒ Γ`. It also carries three names — *Tableaux `Δ₀`
Compression* (`Willard2007-APAL` Def. 5), *Hyper-Constructivity*
(`Willard2002a` Def. 6.1), and the unnamed `∗` invariant of
`Willard2007-APAL` App. B. Drift **D30**; obligation **O43**.

**Present once**, naming all three devices and all three names for the effect.
Passive Induction is the one a reader is least likely to know and the one
`Willard2009` Appendix B depends on.

---

## 3. The Gödel encoding appendix

Four papers carry an essentially identical appendix. A byte is **six bits**; a
proof is an integer in **base 64**; the `i`-th variable is
`⌈log₃₂(i+1)⌉ + 1` bytes, first byte a variable marker, remainder `i` in
**base 32**, lead bit `0`; two parenthesis pairs, one punctuating sentences and
one delimiting the proof tree.

| Source | Symbols | Codes |
| --- | --- | --- |
| `Willard2001` App. B | — | — |
| `Willard2005` App. A | **24** | 32–55 |
| `Willard2006a` App. A | — | — |
| `Willard2007-APAL` App. A | **20** | 32–51 |
| `Willard2016` §6.1 | Byte-Style; `1 + ⌈Log₃₂(j+1)⌉` bytes | — |

The counts differ only because the languages do. All four call it
"approximately analogous to what Wilkie–Paris have called a **natural B-adic**
encoding". Drift **D25**.

**Present once from `Willard2005` App. A**, noting that the symbol count is a
function of the language profile (Axis 1), and that `Willard2016` App. A.1
derives its `1/6` constant directly from the six-bits-per-symbol figure.

---

## 4. The branch that stays open

Four names for one construct (drift **D56**), and it is where every short-proof
argument in the corpus lays down its squaring ladder.

| Source | Name |
| --- | --- |
| `Willard2002c` §1.2 | the `(L,M)`-**Conservative Branch** returned by PROBE |
| `Willard2004` §5 | **Partial Proof**, its **Open Branch**, its **Bottom Node** |
| `Willard2007-APAL` Def. 16 | **Semantic Tableaux Fragment**, its **Major Branch** |
| `Willard2009` Lem. 5 | the **pivotal branch** |

Both `Willard2004` and `Willard2007-APAL` add the same Clarifying Comment: the
open branch need not close *inside* the fragment, because the fragment sits
inside a larger tree where every branch eventually does.

---

## 5. The envelope / stability apparatus

One evolving apparatus, five vocabularies (drift **D33**).

`Willard1993-TR` `ANCESTOR(N)` / ancestor-consistent / maximal → `Willard2001`
`(k,m)`-Affirmative Branch → `Willard2002c` `(L,M)`-Conservative Branch +
**PROBE** + Check's `½` → `Willard2004` **G-good** with `ℑ(H) = Σ χ(p_i)` →
`Willard2005` `App∀(a)` / `App∃(b)` / **Normed(a,b)** / `θ`-Compactification →
`Willard2011` `Scope_E` / `Good(N)` / `♯`.

The constants travel with it: `Willard2002c`'s tree-height bound `⅓ Log₂(M)`
and PROBE's `½`; `Willard2004`'s `ℑ(P) < ⅓ Log₂(G)`; `Willard2005`'s
`U-Height(p) < ⅕ Log₂(p)` (20), `θ < ⅓` (Def. 5) and `θ = ¼` (Thm 2). The
density-5 encoding is exactly what puts `¼` inside the admissible window
`[⅕, ⅓)`. Drift **D6**.

**Present once from `Willard2005` §5** (the fullest treatment, with the
nine-subcase `Probe(a,b,T)` induction), with `Willard2011`'s `Scope_E`/`♯` as
the compressed reformulation.

---

## 6. Solovay's theorem

Cited in at least six papers, never published by Solovay, and load-bearing for
the whole architecture.

| Source | What it adds |
| --- | --- |
| `Willard2000-TAB` [18], `Willard2002c` [16], `Willard2006-WoLLIC` [26], `Willard2009` [36], `Willard2005` [52], `Willard2020` [41] | the April 1994 telephone conversations; "Solovay never published" |
| `Willard2005` §4 | **Theorem `∗`** stated formally, with *Successor-Based Arithmetic* defined |
| `Willard2006a` p. 4 | **Theorem 1 (Pudlák 1985)** and **Theorem 2 (Solovay 1994)** as numbered theorems — the most formal presentation |
| `Willard2001` App. A | a **4-page proof of a weaker version**, "not quite as strong as the broader version of Solovay's Theorem 2" (`Willard2006a` item 7) |

**Present once from `Willard2006a` p. 4**, with `Willard2001` App. A named as
the corpus's only extended exposition **and flagged as proving a weaker
result**. Obligation **O75**.

---

## 7. The four axiom groups

Every affirmative system in the corpus has the same four-group shape, and every
paper redefines it.

- **Group-0** — constants and the naming convention (Axis 1 lives here)
- **Group-1** — a *finite* set of base axioms proving all true `Δ₀`-class sentences
- **Group-2** — the reflection schema, one axiom per `Π₁`-class theorem of the base theory (infinite; Axis 4 attacks this)
- **Group-3** — the single self-referential "I am consistent" sentence (Axis 3 lives here)

Fullest statements: `Willard2005` §3 (tableaux line), `Willard2016` §5 (Hilbert
line, reviewing `Willard2006a`'s `ISCE`). **Present once as a template**, with
per-system deltas in the systems registry.

---

## 8. The Kleene self-referential axiom

`⊕` (`Willard2016` Ex. 3.5), `⊕⊕` (ISCE Group-3), `⊕⊕⊕` (IQFS Group-3), `#`
(`Willard2006a` p. 2), `Diag(α,D)` / `℧_D(α)` (`Willard2009`, `Willard2005`),
`SelfRef(α,d)`. All the same fixed-point construction.

The warning is replicated too, and should be carried once, in Willard's words:
Kleene, Rogers and Jeroslow "each emphatically warned their readers that most
axiom systems similar to `α*` were useless on account of their inconsistency,
*although they were technically well-defined*" — and Willard's own gloss, "the
encoding of `SelfRef(α,d)` is relatively easy … but this sentence is ironically
**typically useless**!"

---

## 9. The motivation preamble

Replicated across `Willard2013`, `Willard2014`, `Willard2016` §2,
`Willard2018`, `Willard2020` §2 and `Willard2020-LFCS` §4.

Distinct components, each to be told **once**:

- **Hilbert 1926** (statement `∗`): "the situation … is in the long run intolerable"; the tombstone motto.
- **Gödel 1931** (statement `∗∗`): Theorem XI "represents no contradiction of the formalistic standpoint of Hilbert".
- **Gödel privately**, on two independent chains: **Sacks** (his IAS assistant, 2014 lecture) and **Nerode via Tennenbaum** (volunteered at LFCS 2020) — obligation **O77**.
- **Friedman 2014** and **Buss 1997** (KGC5, Vienna) as the questions that prompted specific papers.
- **Willard's own limits**: **O24**, **O35**, **O65**, **O72**, **O79**.

Willard's historical section is fullest in `Willard2016` §2; the Gödel
recollections are only in `Willard2020`/`Willard2020-LFCS`.

---

## 10. Literature surveys

`Willard2005` §4 (Items A–F), `Willard2006a` §2 (eleven numbered items) and
`Willard2009` §5 cover the same ground: Kreisel–Takeuti, Nelson, Pudlák,
Vopěnka–Hájek–Švejdar, Wilkie–Paris, Krajíček, Visser, Buss–Ignjatović.
`Willard2006a` §2 is the fullest. It also carries the **definable-cut
disambiguation** that obligation **O78** requires, stated independently at
`Willard2004` p. 348 and `Willard2009` p. 16.
