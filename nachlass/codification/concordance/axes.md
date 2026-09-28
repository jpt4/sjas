# The Five Variation Axes (C13)

`prose/sjas_synthesis.txt` warns that Willard's papers are overlapping but
**not coincident** formalizations, and that SJAS properties are delicately
sensitive to precise definitions. These are the five dimensions along which the
formalizations actually differ. Every system in
[`../registry/systems.md`](../registry/systems.md) sits at one point of this
five-dimensional space, and almost every apparent contradiction between two
papers is a difference in coordinates rather than a disagreement.

Canonical system names are frozen in [`genealogy.md`](genealogy.md).

---

## Axis 1 — Language profile: which functions are **total**

The single most consequential axis. It is what Solovay's theorem constrains and
what every negative result turns on.

| Position | Total | Non-total (relations only) | Systems |
| --- | --- | --- | --- |
| **Type-M** | successor, addition, multiplication | — | `Q`, `PA`, `PA-UGrounding`; **no SJAS lives here** |
| **Type-A** | successor, addition | multiplication (`M(x,y,z)`) | `SJAS-Tableaux-Level1`, `SJAS-TabList-Level1`, `SJAS-Add-Level1`, `SJAS-SimReal-*` |
| **Type-S** | successor | addition, multiplication | *empty* — excluded by Solovay's Theorem `∗` under any Hilbert-style apparatus |
| **Type-NS** | none | all three | `SJAS-NoAddition-Hilbert`, `ISCE`, `ISINF`, `IQFS` |

The **Type** vocabulary is `Willard2016` Example 3.3; `Willard2020-LFCS`
Remark 1 uses it too. Earlier papers say "recognizes Addition but not
Multiplication as a total function", which is the same distinction.

**Why Type-S is empty** is the architecture of the corpus. Solovay's Theorem
`∗` (`Willard2005` §4, `Willard2006a` Theorem 2) says that a system proving
merely `∀x ∃z Add(x,1,z)`, with addition and multiplication as 3-way relations
carrying their associative/commutative/distributive/identity axioms, cannot
prove its own Hilbert consistency. So on the Hilbert line one must drop to
Type-NS; on the tableaux line one may stay at Type-A because Theorem `∗`
does not apply there.

**The grounding functions** are what remains when the arithmetic functions go.
Six in `Willard2002c`, `Willard2016` (subtraction, division, maximum,
logarithm, root, count); **seven** in `Willard2006a` (adding predecessor);
**eight** in `Willard2005` and `Willard2020-LFCS` (the U-Grounding set — six
plus addition and `Double`). The Non-Growth condition itself varies (drift
**D35**), and `Root` rounds up in four papers and down in `Willard2016`
(**D53**).

**Formula classes** track the profile, and there are eleven of them (**D50**):
`Π₁`/`Σ₁`; `Δ*₀`/`Π*ₙ`/`Σ*ₙ` (no multiplication symbol); `Q*ₙ`;
`Δ^R₀`/`Π^R₁` (bounding terms use only `Max`); `Δ⁻₀`/`Π⁻₁` (grounding
primitives); `Δ^ANC₀`; `Δ^Q₀`. Only two relations between them are ever stated
in print, both by `Willard2009`.

---

## Axis 2 — Deductive apparatus

| Apparatus | Cut? | Systems | Where defined |
| --- | --- | --- | --- |
| **Hilbert-Frege** (`d_E`, `d_M`, `d_H`, `d_ER`) | yes | `SJAS-NoAddition-Hilbert`, `ISCE`, `ISINF`, `IQFS` | `Willard2016` Ex. 3.1, Def. 5.6; `Willard2020` Ex. 3.1 |
| **Semantic tableaux** (`Tab`, `d_F`) | no | `SJAS-Origin`, `SJAS-Tableaux-Level*`, `SJAS-SimReal-Level1` | six to eight rules; see D54 |
| **Herbrand** | no | `SJAS-Herbrand-Level0`, `Ax-3` | `Willard2009` §2; `Willard2007-APAL` App. E |
| **Cut-free sequent calculus** | no | claimed only | `Willard2002a` p. 5; `Willard2005` Rem. 1 |
| **Resolution** (`Res`) | no | claimed only | `Willard2002a` p. 5 (2002); `Willard2020` §8 |
| **TabList family** (`Tab-ℜ-List`, `Tab₁List`, `Tab-1`, `Herb−k`, `R(i,j)`) | *restricted* cut | `SJAS-TabList-Level1`, `SJAS-Tab1-Level1` | `Willard2005` §2; `Willard2004` §6; `Willard2009` Def. 7 |
| **Xtab / Xres** | cut *recovered* via LEM axioms | `SJAS-Xtab-NegControl` | `Willard2020` Appendix |

**The apparatus-identity thesis.** `Willard2020` Example 3.1 states it in
Willard's own voice — "while proofs have different lengths under different
apparatuses, **all the common apparatuses produce the same set of final
theorems**" — justified in footnote 2 by Gödel Completeness. So the apparatus
axis is **not** a strength axis; it is a *proof-length* axis. That is precisely
why it can carry a Second Incompleteness boundary: `Willard2002a` Theorem 2.2
notes that cut elimination guarantees the combined proof **exists** while its
length "can certainly be super-exponentially longer".

**Three devices recover cut-like power inside a cut-free calculus**, and the
corpus never presents them together: LEM-as-axioms (`Xtab`), the TabList
restricted-cut hierarchy, and **Passive Induction** (`Willard2007-APAL` §3).
See [`replication-map.md`](replication-map.md) §2.

**Apparatus-generality is claimed five times and proved never.**
`Willard2002a` p. 5 (Herbrand, resolution, cut-free sequent); `Willard2005`
Rem. 1 (adds `Tab-Q*₁-List` and a `Q*₁`-restricted **Hilbert** variant);
`Willard2004` §7; `Willard2006a` Added Comment (p. 30); `Willard2020` §8
(`Res`/`Xres`). All `stated-only`. Obligation **O80**.

---

## Axis 3 — Consistency notion: what the Group-3 axiom asserts

| Level | Asserts | Origin |
| --- | --- | --- |
| **primitive / Level(0−)** | there is no proof of `0=1` from me | `SJAS-Origin`, `SJAS-Tableaux-Level0` |
| **Level(1)** | for no `Π*₁` sentence `Ψ` are there proofs of both `Ψ` and `¬Ψ` | `Willard2002c` |
| **Level(n)** | as Level(1), over `Π*ₙ` | `Willard2004` §1 |
| **Level(n+)** | as Level(n), over `Q*ₙ` (Boolean combinations) | `Willard2004` §1 |
| **Herb−k / Tab−k** | the *intermediate theorems* of a proof are confined to Level-k | `Willard2009` Def. 7 |

`Willard2004` §1: "All definitions of consistency, from Level(0−) up to
Level(n+) **for any n**, are equivalent to each other under strong enough
models of Arithmetic. However, many weak axiom systems do not have a
mathematical strength to formally prove and recognize this equivalence." The
level hierarchy is therefore non-trivial **only** for the weak systems the
corpus studies — which is exactly the point.

`Willard2002c` states why Level(1) is a real gain over Level(0−): "there exists
**no decision procedure** for enumerating all true `Π⁻₁` sentences."

---

## Axis 4 — Axiomatization finiteness

Group-2 (the reflection schema) is infinite by construction: one axiom per
`Π*₁` sentence of the base theory. Three papers reduce it.

| Position | Systems | Result |
| --- | --- | --- |
| **Infinite Group-2** (default) | `SJAS-Add-Level1`, `ISCE`, `IQFS`, most others | — |
| **Finite, via Braced theorems** | `Willard2011` App. G | **Three added sentences** suffice (Thm G.2); for any fixed `c`, all `Π^ξ₁` theorems with ≤ `c` quantifiers are provable *purely* (Thm G.3) |
| **Finite, via kernel images** | `Willard2014` `IS_D^#(β)` | For standard-model-valid `A` and kernel index `i`, a finite `β_{A,i}` exists such that `IS_D^#(β_{A,i})` proves the `i`-th **kernel image** of every `Π*₁` theorem of `A` (Thm 3) |
| **Finite, asserted** | `Willard2016` Rem. 7.3 | The same reduction "will routinely generalize" to `ISCE`, `IQFS`, `IQFS*` — `cited`, not proved |

**The bound is real and Willard states it.** `Willard2011` Theorem G.3 answers
the finite-axiomatization question **only up to a quantifier-count bound `c`
fixed in advance** (obligation **O23**). `Willard2014` pays a different price:
the finite system proves not the theorems of `A` but their **kernel images**
`∀x Test_i(⌜Ψ⌝,x)`, and `Willard2014` Example 2 notes that PA equates `Ψ` with
its kernel image while a weak system proving (9) may not. Neither reduction is
free, and the codified statement must say what each costs.

---

## Axis 5 — Base-theory assumptions: the admission condition

Every preservation theorem has the shape *if the base theory is good, the
constructed system is consistent*. "Good" means three different things.

| Hypothesis | Where | Systems |
| --- | --- | --- |
| all of `A`'s **`Π*₁` theorems** are valid in the standard model | `Willard2005` Def. 1 | `SJAS-Add-Level1`, `SJAS-TabList-Level1` |
| **β together with Groups 0 and 1** is consistent | `Willard2016` Def. 5.2 | `ISCE`, `IQFS` |
| **all β's axioms** hold in the standard model | `Willard2020` Def. 4.3 | `SJAS-Tableaux-Level1`, `SJAS-Tab1-Level1` |

All three are called "Consistency Preserving" and none implies another (drift
**D55**). This is the axis most likely to produce a false unification, because
the phrase is identical and the theorems look like instances of one schema.

`Willard2016` Example 5.3 shows what the strongest form buys: **"If PA is
consistent then `ISCE(PA+)` is self-justifying"** — and PA is "sufficiently
agile" to prove exactly that.

---

## How to use these axes

To locate any result: fix a **profile** (Axis 1) and an **apparatus** (Axis 2);
the **level** (Axis 3) then says how strong a consistency claim is on the
table, and the **admission condition** (Axis 5) says what must be assumed to
get it. Axis 4 is orthogonal — it asks whether the answer can be written down
in finitely many sentences.

The result matrix in [`result-matrix.md`](result-matrix.md) is the
(Axis 1 × Axis 2) projection, with the Axis 3 level recorded per cell.
