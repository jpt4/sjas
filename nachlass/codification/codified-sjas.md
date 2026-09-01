# The Codified SJAS

*A single mature statement of Dan Willard's Self-Justifying Axiom Systems,
1993–2021.*

> **Status.** C15 — full draft. All ten chapters written. Acceptance tests
> T1–T5, the results/statement cross-map and the AAR are C16.

---

## How to read this document

Willard published on self-justifying axiom systems for nearly thirty years,
across roughly twenty papers and six hundred pages. The papers overlap heavily
but are **not coincident**: the same system appears under five names, the same
device under ten, and the same word — "cut" — means two unrelated things. This
document states the subject once.

Four conventions govern it.

**Provenance is carried, not summarised.** Every substantive claim names its
source and page. Where two papers differ, both readings are given and the
difference is sent to the drift ledger rather than resolved silently.

**Proof status is never rounded up.** A result is `full`, `sketch`, `cited`, or
`stated-only`, in the sense of `registry/results.md`. Several results that read
as settled in the literature are `sketch`, and this document says so where they
are used, not only where they are introduced.

**Willard's own limits are quoted, not paraphrased.** He was more careful about
what his results do *not* show than most of his readers have been. Those
statements appear in §1.4 and are referred back to throughout.

**Quotation is marked.** A `>` block in **quotation marks** is verbatim Willard.
A `>` block **without** quotation marks is an editorial restatement of a
numbered result in this document's own notation — faithful in substance, but
not his words. §2.4's Solovay theorem, §6.4–§6.5's Theorems and Lemmas, §7.1's
and §7.5's theorem statements are restatements; §1.2, §1.4, §5.3, §6.7 and §9.2
are verbatim.

**Names are frozen.** System names follow
[`concordance/genealogy.md`](concordance/genealogy.md); topic slugs and the
five axes follow [`concordance/axes.md`](concordance/axes.md).

Supporting apparatus: [`registry/`](registry/) (corpus, systems, results,
notation, coverage, gaps), [`concordance/`](concordance/) (axes, genealogy,
result matrix, replication map, drift ledger, composition obligations),
[`extraction/`](extraction/) (per-paper records).

---

## 1. Motivation and the historical arc

### 1.1 The question, and the two forms it takes

Hilbert's Second Problem asked for a consistency proof of arithmetic by
finitary means. Gödel's Theorem XI of 1931, generalised in the second edition
of Hilbert–Bernays (1939), is normally read as closing it.

`Willard2016` Remark 4.9 observes that the question was never one question, and
splits it:

> **Q-1** Are any axiom systems able to prove theorems verifying their own
> consistency **in a robust sense**?
>
> **Q-2** Can logic systems **"appreciate"** their own consistency in some
> **REDUCED** sense, that is diluted but not fully immaterial?

Q-1's answer is unambiguously **no** — by Gödel 1931, Hilbert–Bernays 1939, and
the Pudlák–Solovay result of §2.4. Everything in this corpus is an answer to
**Q-2**, and saying so at the outset is the most honest available framing of
the subject. It is also Willard's own. Obligation **O72**.

### 1.2 Why the question survived Gödel

Two of the principals declined to treat the matter as closed.

**Hilbert** never withdrew the justification for his programme
(`Willard2016` §2, statement `∗`):

> "Let us admit that the situation in which we presently find ourselves with
> respect to paradoxes is in the long run intolerable. Just think: in
> mathematics, this paragon of reliability and truth, the very notions and
> inferences, as everyone learns, teaches, and uses them, lead to absurdities.
> And where else would reliability and truth be found if even mathematical
> thinking fails?"

**Gödel** inserted a caveat into the 1931 paper itself (statement `∗∗`):

> "It must be expressly noted that Theorem XI represents no contradiction of
> the formalistic standpoint of Hilbert. For this standpoint presupposes only
> the existence of a consistency proof by finite means, and there might
> conceivably be finite proofs which cannot be stated in P or in …"

Gödel's biographers record that he "was cautious not to prejudge" the matter
for several years, began endorsing the theorem's generality in a 1933 lecture,
and fully embraced it after Turing.

Two independent chains of testimony report that he privately continued to
doubt its reach much later. **Gerald Sacks**, Gödel's assistant at the IAS for
a year, said in a lecture of 2 June 2014 that Gödel told him during 1961–62
that he "did not think" Hilbert's programme's goals "were erased" by the
Incompleteness Theorem, and considered it "very much alive and even more
interesting than it initially was". **Anil Nerode**, at LFCS 2020 on 7 January
2020, said he could reinforce that account, having heard **Stanley Tennenbaum**
report the same from Gödel directly.

These are recollections, not documents. Willard notes that Gödel "published
fewer than 85 pages in his life" and "never explicitly recorded, during the
second half of his life, his partial reluctance". They are presented here as
what they are. Obligation **O77**.

### 1.3 What this corpus is

Between 1993 and 2021 Willard constructed arithmetics that assert their own
consistency **as an axiom** and are nevertheless consistent. They evade Gödel's
Second Incompleteness Theorem not by redefining consistency but by **growth
restriction** — by being too weak to prove that some ordinary function is
total.

`Willard2020` distinguishes his results from the other known family of
exceptions. His are **Declarative Exceptions**: formalisms weak enough that the
self-referencing mechanism stays contradiction-free while still proving more
`Π₁`-like theorems than one might expect. Artemov's are **Infinite-Ranged
Exceptions**: PA generates theorems `T₁, T₂, …` each establishing the
consistency of a subset `S_i`, with `S₁ ⊂ S₂ ⊂ …` and `PA = ∪S_i`. Willard
calls the two "rigorous results that are nicely compatible with each other",
both acknowledging that G2 "will admit no full-scale exceptions".

### 1.4 The limits, in Willard's words

Four statements, from four papers, that any honest presentation must carry.

**On what self-justification buys** (`Willard2011` Remark 6.16b) — a system
proving its consistency from its own SelfCons axiom has "essentially a
**1-line proof**", which is instinctive faith rather than proof-justification.
Obligation **O24**.

**On the absence of an ideal system** (`Willard2001` §1) — Willard lists
criteria a fully satisfying self-justifying arithmetic would meet and states
that no system meets them all. Obligation **O35**.

**On the programme's scope** (`Willard2016` §9):

> "All our published articles about self-justifying arithmetics have emphasized
> that evasions of the Second Incompleteness Effect rested on using arithmetics
> that were **weaker than traditional arithmetics** in, at least, some
> respects. (The Second Incompleteness Theorem's significance in refuting the
> original objectives of Hilbert's Consistency Program is thus, simply,
> **undeniable**.)"

Obligation **O65**.

**On whether the whole thing is a trick** (`Willard2016` §8, statement `###`) —
raised by Willard against himself, in a section he added late so that the
meaning of his main theorem "could not be confused":

> "Is it not **almost cheating** when an axiom system verifies its own
> consistency by using ⊕'s formalized 'I am consistent' axiom as an
> intermediate step, to verify its own consistency? After all, such a technique
> can verify its own consistency only in a **technically purely legalistic
> sense**."

His answer is Definition 8.1's **Platonic Stability**: a thinking being does
not need a sophisticated justification of its own reasoning, only a formalism
in which *presuming* its own consistency provably will not spin it into
inconsistency. Obligation **O79**.

---

## 2. Preliminaries

### 2.1 Generalized arithmetic

Following `Willard2016` §3 and `Willard2020` §3, a **generalized arithmetic**
is an ordered pair `(α, D)`:

- the **axiom basis** `α` — the proper axioms;
- the **deductive apparatus** `D` — the rules of inference together with the
  built-in logical axioms `L_D` those rules use.

Separating the two is what makes the apparatus a variable rather than a
background assumption, and it is the move on which the whole corpus depends.

### 2.2 Self-justification

`Willard2016` Definition 3.4. The pair `(α, D)` is a **self-justifying
configuration** when:

1. one of `(α, D)`'s theorems — **or possibly one of `α`'s axioms** — states
   that `D` applied to `α` produces a consistent set of theorems; and
2. the axiom system `α` **is in fact consistent**.

Clause (i)'s disjunction is not incidental. Under a cut-free apparatus, a
sentence held as an **axiom** licenses inferences that the same sentence held
as a **theorem** does not, and several results in this corpus turn on exactly
that difference (§5.4, obligation **O74**).

Clause (ii) is what makes the subject hard. Kleene's fixed-point theorem makes
clause (i) trivial to satisfy: adjoin to `α` the sentence

> `⊕` There is no proof, using `D`, of `0 = 1` from the union of `α` with
> **this sentence** (looking at itself).

`Willard2005` §1 records the warning: "Kleene, Rogers and Jeroslow [22, 24, 46]
each warned their readers that most axiom systems, similar to `α*`, were useless
on account of their inconsistency, **although they were technically
well-defined**." `Willard2016` Example 3.5 puts it more mildly — `α^d` "**may
be** inconsistent" — and adds the gloss: the encoding "is relatively easy, via
an application of the Fixed Point Theorem, but this sentence is ironically
**typically useless**!"

The corpus is the study of when it is not useless.

### 2.3 Threshold and anti-threshold

`Willard2009` Definition 1. Write `α ⊇ β` to mean that `α`'s set of formal
**axioms** includes all of `β`'s — "stronger than the more modest construct
that `α` proves all `β`'s theorems". Then for a consistent `α` and apparatus
`D`, the pair `(α, D)` is:

- a **Threshold** for the Second Incompleteness Effect iff *every* consistent
  `α* ⊇ α` fails to prove its own `D`-consistency;
- an **Anti-Threshold** otherwise — some consistent `α* ⊇ α` proves a theorem
  affirming its own `D`-consistency.

That `⊇` is axiom containment, not theorem containment, is load-bearing;
obligation **O63**.

### 2.4 Solovay's theorem, and why there are exactly two branches

The constraint the whole corpus is built around was communicated to Willard by
**Robert Solovay in telephone conversations during April 1994**, generalising a
1985 theorem of Pudlák with methods of Nelson and Wilkie–Paris. Solovay never
published it. It is cited in at least six of Willard's papers; its most formal
presentation is `Willard2006a` pp. 4–5, as **Theorem 2**:

> Let `β` be any consistent axiom system canonically formalizing arithmetic
> (that is, with predicates `A(x,y,z)`, `M(x,y,z)` for `x+y=z`, `x·y=z`) such
> that **(A)** `β` proves successor totality `∀x ∃z A(x,1,z)`, and **(B)** `β`
> proves a `Π₁` theorem giving `A` and `M` the associative, commutative,
> distributive and identity properties. Then `β` **cannot** prove the
> non-existence of a Hilbert-style proof of `0=1` from its own axioms.

The corpus's only extended exposition of it is `Willard2001` Appendix A — and
that appendix proves a **weaker** version, "not quite as strong as the broader
version of Solovay's Theorem 2", with the virtue of a four-page proof
(`Willard2006a` item 7). Obligations **O5**, **O75**.

**The architecture follows immediately.** `Willard1993-TR` p. 2 reads the
theorem as forbidding two things *together*: a cut-permitting apparatus, and
Addition-totality. That leaves exactly two directions of retreat, and the
corpus is the exploration of both:

| Branch | Retreat | 1993 derivation | 2016 restatement |
| --- | --- | --- | --- |
| **Tableaux line** | keep totality, **drop the cut** | Proposition 1 | Type-A under `d_F` |
| **Hilbert line** | keep the cut, **drop totality** | Proposition 7 | Type-NS under Hilbert |

`Willard2016` Example 3.3(b) restates the same split twenty-three years later
in the Type vocabulary — the two methods "whose natural hybridizations are
precluded by `++`" — without mentioning the 1993 derivation. Obligations
**O57**, **O71**; drift **D49**.

A third line, opened in 2009, belongs to neither: it changes only the
**axiomatization** (§7.4).

---

## 3. Language profiles

### 3.1 The Type classification

The first axis. Which of successor, addition and multiplication does the system
prove **total**? Following `Willard2016` Example 3.3, with `A(x,y,z)` and
`M(x,y,z)` as 3-way relations:

| Type | Proves total | Systems |
| --- | --- | --- |
| **Type-M** | successor, addition, multiplication | `Q`, `PA`, `PA+`; **no SJAS** |
| **Type-A** | successor, addition | the whole tableaux line |
| **Type-S** | successor only | **empty** |
| **Type-NS** | none | the whole Hilbert line |

**A fifth class sits off this lattice.** `Willard2011` defines **Type-Almost-M**:
`α` proves `∀x∀y∃z x+y=z` and `∀x∀y∃z x·y=z` **as theorems while treating none
of the totality sentences as axioms**, and writes them with genuine `+` and `·`
**function symbols** where the four classes above use 3-way predicates. So
Type-Almost-M is **not a point on the totality lattice at all** — it is the
axiom-versus-theorem distinction of §5.4 wearing a totality-shaped name, and
"many axiom systems that use function symbols '+' and '\*' fall technically
into" it.

**This codification adopts the four-class lattice** of `Willard2016` and
`Willard2020` as Axis 1, and places Type-Almost-M **beside** it rather than
within it, as a second and independent coordinate. Two consequences follow.
`Willard2011`'s `ξ^R` is a Type-Almost-M system — Appendix D: "`ξ^R` is thus a
Type Almost-M system that can prove multiplication is a total function (but
which does not contain Equation (5)'s totality statement as an axiom)" — and so
does **not** belong in the Type-A row (§8). And Willard's remark that
Type-Almost-M systems "verify their Herbrand but not also semantic tableaux
consistency" is the same phenomenon as `Ax-3`'s (§7.4), which is why that
section and this one describe one fact. Drift **D29**.

**Type-S is empty by Solovay's theorem** (§2.4), and that emptiness is the
corpus's organising fact. On the Hilbert line one must fall all the way to
Type-NS; on the tableaux line one may stay at Type-A, because Solovay's theorem
is about Hilbert-style deduction and does not apply.

Willard weakens the *theory*, never the vocabulary: "total function" keeps its
usual meaning throughout, and a system that fails to prove `∀x∀y∃z M(x,y,z)`
is simply a system that fails to prove it. Obligation **O26**.

### 3.2 The grounding functions

When the arithmetic functions are given up as *functions*, something must
remain to build terms from. A function `F` is **Non-Growth** when
`F(a₁,…,a_j) ≤ Max(a₁,…,a_j)`; the corpus's base language is a set of such
functions, called the **Grounding** (or Ground-Level) functions.

The set varies, and the variation is not cosmetic:

| Source | Count | Members |
| --- | --- | --- |
| `Willard2002c`, `Willard2016` | **six** | subtraction, division, maximum, logarithm, root, count |
| `Willard2006a` | **seven** | the six plus predecessor |
| `Willard2005`, `Willard2020-LFCS` | **eight** (`U-Grounding`) | the six plus **addition** and `Double(x) = x+x` |

The U-Grounding set is the tableaux line's language: addition is total there,
so it may be a function symbol. The six-function set is the Hilbert line's.

Two variations are recorded rather than resolved: the Non-Growth condition
itself differs (`Willard2006a` writes `≤ Maximum(2, a₁,…,a_j)`; drift **D35**),
and `Root(x,y)` rounds **up** in `Willard1998`, `Willard2002c`, `Willard2004`
and `Willard2005` but **down** in `Willard2001` and `Willard2016` (drifts
**D53**, **D80**). The corpus is split, not one outlier against a consensus,
and a codified grounding language must choose.

### 3.3 Formula classes

The bounded-formula hierarchy is relativised to whichever language is in play,
and the corpus contains **ten** such relativisations (drift **D50**), of which
the principal families are:

| Notation | Restriction | Papers |
| --- | --- | --- |
| `Δ₀`, `Π₁`, `Σ₁` | conventional | `Willard1993-TR`, `Willard2007-APAL` |
| `Δ*₀`, `Π*ₙ`, `Σ*ₙ` | **no multiplication symbol**; `M(x,y,z)` instead | `Willard2002a`, `Willard2004`, `Willard2005`, `Willard2011`, `Willard2020` |
| `Q*ₙ` | Boolean combinations, for Level(n+) | `Willard2004` |
| `Δ^R₀`, `Π^R₁`, `Σ^R_k` | quantifier-bounding terms use **only `Max`** | `Willard2009`, `Willard2006-WoLLIC` |
| `Δ⁻₀`, `Π⁻ₙ`, `Σ⁻ₙ` | over the grounding primitives | `Willard2001`, `Willard2002c`, `Willard2006a`, `Willard1998` |
| `Δ^ANC₀`, `Π^ANC_n` | over the Additive-Naming language | `Willard2016` §5 |
| `Δ^Q₀`, `Π^Q_n` | over `L^Q`, every `C_J` replaced by `E_{J−1}` | `Willard2016` §5 |

**Only two relations between them are ever stated in print**, both by
`Willard2009`: the starred classes are "roughly analogous" to the
R-superscripted ones "except that they contain no multiplication function
symbol" (p. 20), and `Δ^R₀` is "broader than `Δ⁻₀`" (footnote 3, p. 13). Every
other pair is unrelated in the literature. Obligation **O12**.

Two further cautions. `Willard1993-TR` writes `Δ₀/Π₁/Σ₁` for what 2005 writes
starred — the classes are the same, the notation is not. And it is the **growth
class**, not the signature, that is load-bearing: `Willard1993-TR` Remark 6
says so directly (obligation **O13**).

---

## 4. Deductive apparatuses

### 4.1 The catalogue

`Willard2016` Example 3.1 and `Willard2020` Example 3.1 name four textbook
apparatuses and one of Willard's own:

| Symbol | Source | Rules | Logical axioms |
| --- | --- | --- | --- |
| `d_E` | Enderton §2.4 | modus ponens only | a 4-part schema |
| `d_M` | Mendelson §2.3 | modus ponens + generalization | compressed |
| `d_H` | Hájek–Pudlák §0.10 | modus ponens + generalization | compressed |
| `d_F` | Fitting, Smullyan | tableau elimination rules | **none** |
| `d_ER` | `Willard2016` Def. 5.6 | modus ponens only | **six** schemas |

`d_ER` is `d_E` with tautologies proved rather than assumed — "because the
problem of identifying tautologies is NP-hard" — using Mendelson's three
propositional schemas plus Enderton's three quantifier schemas.

**Definition** (`Willard2016` Def. 3.2). A **Hilbert-style** apparatus is any
`D` that employs a modus ponens rule and satisfies Gödel's Completeness
Theorem. The definition is by property, not by presentation.

### 4.2 The apparatus-identity thesis

`Willard2020` Example 3.1 states it in Willard's own voice:

> **AN IMPORTANT POINT** is that while proofs have different lengths under
> different apparatuses, **all the common apparatuses produce the same set of
> final theorems** from an initial common axiom basis of `α`

— justified in footnote 2 "because all the common apparatuses satisfy the
requirements of Gödel's Completeness Theorem".

So the apparatus axis is **not** a strength axis. It is a **proof-length**
axis. That is precisely why it can carry an incompleteness boundary:
`Willard2002a` Theorem 2.2 notes that cut elimination guarantees the combined
proof **exists** while its length "can certainly be **super-exponentially
longer**". The apparatus axis is extensionally degenerate and intensionally
decisive, and the codified statement must say both. Obligations **O38**,
**O40**.

### 4.3 "Cut" means two unrelated things

The corpus's most dangerous collision, and Willard disambiguates it twice, in
two different forms. `Willard2009` p. 16:

> "In the proof-theory literature, the definition of a **'Definable Cut'** is
> **formally unrelated** to Gentzen's notion of a sequent calculus deductive
> **'cut rule'**, despite their very similar sounding names."

and more briefly at `Willard2004` p. 348, after defining a Definable Cut by
Eq. (4):

> "Definable Cuts have been studied by a very extensive literature […].
> **They are unrelated to Gentzen's notion of a Sequent Calculus 'Deductive Cut
> Rule'.**"

- The **cut rule** is Gentzen's. Its absence defines the cut-free apparatuses;
  its recovery is what §4.5 is about.
- A **Definable Cut** is a formula `φ` with `α ⊢ φ(0)`, `α ⊢ ∀x(φ(x) ⇒
  φ(x+1))`, `α ⊢ ∀x∀y<x(φ(x) ⇒ φ(y))` — the Nelson–Pudlák–Wilkie–Paris device.
  `Willard2009` adds the fact that makes them ubiquitous: "**all** axioms
  systems, strictly weaker than Peano Arithmetic, contain some definable cut
  that is not provably equivalent to the full set of integers."

Willard's own systems are **not** cut-localized (`Willard2002c` Remark 6): their
consistency statements are global, not relativised to a definable cut.
Obligations **O52**, **O78**.

### 4.4 The tableau apparatus, and which definition to fix

Semantic tableaux is presented three ways across the corpus (drift **D54**):
**eight** primitive rules with bounded quantifiers as distinct syntactic objects
(`Willard2007-APAL` Def. 1, `Willard2005` §2), or **six** with bounded
quantifiers as abbreviations (`Willard2020` Appendix, `Willard2020-LFCS` App. A,
`Willard2002c` §2). `Willard2002a` gives six with a *simultaneous*
multi-variable `∀`-elimination.

This document fixes **`Willard2007-APAL` Definition 1's eight rules** as
canonical, because `Willard2009` Appendix B invokes them **by number** — its
Lemma 5 uses rules 6, 7 and 8 — and a reader holding a six-rule list cannot
follow it. Two conventions travel with the choice and must be stated rather
than inherited: whether the root is `¬Φ` **rewritten in prenex\* form**
(`Willard2002c`, `Willard2005`: yes), and whether `∀`-elimination terms may
mention only **previously introduced** parameters (`Willard2002c`,
`Willard2005`: yes). Obligation **O42**.

### 4.5 Three devices that recover the cut

All three solve the problem §4.2 names — cut elimination preserves theoremhood
but destroys length — and no paper presents them together.

| Device | Source | Mechanism |
| --- | --- | --- |
| **LEM as axioms** (`Xtab`) | `Willard2020` App. | admit `℧ ∨ ¬℧` for **arbitrary** `℧`; branch; hang the two subproofs |
| **LEM at `Δ₀` only** | `Willard2007-APAL` Lem. 1(i) | the same construction, but `Φ ∨ ¬Φ` is required only for every **`Δ₀`** sentence `Φ` — a weaker hypothesis, and a point on the `Z` dial (§8.1) |
| **Restricted-cut hierarchies** | `Willard2005` §2; `Willard2004` §6; `Willard2009` Def. 7 | `Tab-ℜ-List`, `Tab₁List`, `Tab-1`, `Herb−k`, `R(i,j)`: a sequence of proofs whose **intermediate theorems** are confined to a class |
| **Passive Induction** | `Willard2007-APAL` §3 | instantiate the induction axiom at a formula with **no free variables**; four branches result, two of which close immediately |

Each yields the **Linear-Sum Effect**: from proofs of `Ψ` (length `L`) and
`Ψ ⇒ Γ` (length `G`), a proof of `Γ` of length `O(L + G)`. The effect carries
three names of its own — *Tableaux `Δ₀` Compression* (`Willard2007-APAL`
Def. 5), *Hyper-Constructivity* (`Willard2002a` Def. 6.1), and the unnamed `∗`
invariant of `Willard2007-APAL` Appendix B. Drift **D30**; obligations
**O43**, **O55**.

Passive Induction deserves emphasis: it obtains the effect from the **induction
schema the system already has**, with no added axiom, and it is what
`Willard2009`'s Appendix B depends on.

### 4.6 Apparatus generality: claimed five times, proved never

`Willard2002a` p. 5 (2002) — Herbrand, **resolution**, cut-free sequent
calculus, subject to replacing `V` by an apparatus-specific `V_d`.
`Willard2005` Remark 1 — cut-free sequent, Herbrand, `Tab-Q*₁-List`, and a
`Q*₁`-restricted **Hilbert** variant. `Willard2004` §7. `Willard2006a` p. 30.
`Willard2020` §8 — `Res`/`Xres`.

All `stated-only`. The corpus asserts apparatus-generality for both its
negative results (from 2002) and its affirmative ones (2020) and proves it for
neither. Obligations **O4**, **O67**, **O80**.

---

## 5. The construction

Every affirmative system in the corpus has the same four-part shape. This
section states it once; per-system deltas are in
[`registry/systems.md`](registry/systems.md).

### 5.1 The four axiom groups

Given a base theory `A` (or `β`) and an apparatus `D`, the constructed system
`IS_D(A)` consists of:

**Group-0 — constants and the naming convention.** Axioms fixing `C₀ = 0`,
`C₁ = 1`, `C₂ = 2`, together with the scheme that generates the rest of the
integers. This is where the language profile of §3 lives, and on the Hilbert
line it is the *only* thing that varies between systems (§5.5).

**Group-1 — the finite base.** A **finite** set of `Π₁`-class axioms which,
with Group-0, proves every **true** `Δ₀`-class sentence and gives `=` and `≤`
their transitivity, reflexivity, symmetry and total-ordering properties. Any
finite set with these properties will serve; the particular choice is
immaterial.

**Group-2 — the reflection schema.** For each `Π₁`-class sentence `Φ` of the
base theory, the axiom

> `∀y { Prf_A(⌜Φ⌝, y) ⇒ Φ }`

This group is **infinite** by construction — one axiom per theorem of `A` — and
it is what Axis 4 (§5.6) attacks.

**Group-3 — the self-referential axiom.** A single sentence asserting, of the
union of Groups 0–2 **with itself**, that no `D`-proof of `0=1` exists. Built
by the Kleene fixed point over a substitution predicate.

The essential technical requirement is that Group-3 admit a **`Π₁`-class**
encoding. `Willard2005` stresses that a `Π*₁` — not `Π*₂` — encoding is what
makes the preservation argument work.

### 5.2 What Group-3 asserts: the consistency level

Group-3 can say more or less, and how much it says is an independent axis.

| Level | The axiom asserts | Introduced |
| --- | --- | --- |
| **primitive / Level(0−)** | there is no `D`-proof of `0=1` from me | `Willard1993-TR`, `Willard2001` |
| **Level(1)** | for no `Π*₁` sentence `Ψ` do proofs of both `Ψ` and `¬Ψ` exist | `Willard2002c` |
| **Level(n)** | as Level(1), over `Π*ₙ` | `Willard2004` |
| **Level(n+)** | as Level(n), over `Q*ₙ` | `Willard2004` |

`Willard2004` §§1–2 state the crucial qualification:

> All definitions of consistency, from Level(0−) up to Level(n+) **for any n**,
> are equivalent to each other under strong enough models of Arithmetic.
> However, many weak axiom systems do not have a mathematical strength to
> formally prove and recognize this equivalence.

So the hierarchy is non-trivial **only** for the weak systems this corpus
studies — which is the point, not a defect. `Willard2002c` gives the concrete
reason Level(1) is a genuine advance on Level(0−): "there exists **no decision
procedure** for enumerating all true `Π⁻₁` sentences." Obligations **O41**,
**O45**, **O59**.

### 5.3 Why the growth restriction is what makes it work

The corpus's single unifying device. Two sequences from the same start
(`Willard2020-LFCS` p. 11, Eqs. 7–8):

> `x₀ = y₀ = 2`,  `x_i = x_{i−1} + x_{i−1}`,  `y_i = y_{i−1} · y_{i−1}`

so `x_n = 2^{n+1}` and `y_n = 2^{2^n}`, hence

> `Log(x_n) = n + 1`  but  `Log(y_n) = 2^n`.

A system that can name `y_n` can name, in `O(n)` symbols, an integer whose
binary encoding is `2^n` bits long — long enough to swallow the system's own
proof of its own consistency, and short enough to be written down. A system
restricted to `x_n` cannot. **That asymmetry is the mechanism of the entire
corpus**, and it recurs in nine places under nine guises (see
[`concordance/replication-map.md`](concordance/replication-map.md) §1).

`Willard2006a` pp. 7–8 gives the three-way ordering that organises the Hilbert
line: the **incremental** convention `C_i = C_{i−1}+1` "grows too slowly" to be
Continuously Expanding; the **multiplicative** `C_i = C_{i−1}·C_{i−1}` is too
fast to be self-justifying; the **additive** `C_i = C_{i−1}+C_{i−1}` is
"simultaneously *sufficiently slow* to satisfy Theorem 3's self-justification
property while also *sufficiently fast* to satisfy the continuous expansion
property."

And the same device is what the **negative** proofs consume.
`Willard2000-TAB` §5:

> Many readers will smile with amusement when they learn **the true reason**
> that the Semantic Tableaux version of the Second Incompleteness Theorem
> breaks down when Multiplication is changed from a total function into a
> 3-way relation. It is essentially that **Lemma 8 and its short proof then
> become no longer valid**. … **Without it, our proof of the Semantic Tableaux
> version of the Second Incompleteness Theorem collapses entirely!**

`Willard2002a` §7 and `Willard2004` p. 4 say the same of their own proofs. So
the affirmative and negative halves of the corpus are two readings of one
observation. Obligations **O44**, **O50**, **O76**.

### 5.4 Axiom versus theorem

Under a cut-free apparatus, holding a sentence as an **axiom** is not the same
as proving it. Three results turn on this.

`Willard2005` §6 exhibits `NS^{k,m}_D(A)`, identical to `IS_D(A)` except that
the `Σ*₁` sentence `Υ(k,m)` is added to Group-0. **Both systems prove
`Υ(k,m)`.** But in `NS` it is an axiom, which shortens proofs by a constant
`C > 2^m_k` and thereby changes what the word "this" in the Group-3 axiom
refers to. `IS_D(A)` is consistent; `NS^{k,m}_D(A)` is **inconsistent** for
most `(k,m,D,A)`.

`Willard2007-APAL` p. 29 states the distinction most sharply, of
`Willard2009`'s system:

> **One of these systems** is even able to prove the theorem that
> multiplication is a total function,
> in a context where [45]'s main result would **collapse entirely** if such a
> statement about multiplication's totality property was merely changed from
> being a **theorem** into becoming an **axiom**.

And `Willard2016` Remark 7.4 uses the same lever in the benign direction:
`IQFS^R` adds sentences already provable from Groups 0–1 **as axioms**, purely
to shorten proofs, with no effect on consistency.

Drift **D29**; obligations **O18**, **O74**.

### 5.5 The Hilbert line: the naming convention is the system

On the Hilbert line the four groups barely change between papers. What changes
is Group-0's naming convention — that is, the growth rate of §5.3.

| Convention | `C_i` | System | Verdict |
| --- | --- | --- | --- |
| incremental | `C_{i−1} + 1` | `ISREF(A)` (`Willard2001` Thm 3.4) | **works**, not Continuously Expanding |
| **additive** | `C_{i−1} + C_{i−1}` | `ISCE(A)` (`Willard2006a` Thm 3) | **works**, Continuously Expanding |
| Hybrid(H) | `⌈2^{[Log i]^H}⌉·C_{i−1}` | `Willard2006a` p. 30 | `H=1` works, `H>1` fails |
| multiplicative | `C_{i−1}·C_{i−1}` | `Willard2006a` Thm 4 | **fails** |
| θ Up-Walking | three constants only | `IQFS` (`Willard2016`) | works, *conditionally* |

`ISCE` **is** `ISREF` with the convention upgraded — `Willard2006a` says so:
`Willard2001`'s Theorem 3.4 "is the degenerate incremental-naming version".
Obligation **O47**.

`IQFS` is the line's endpoint and its most radical member. Its Group-0 replaces
the naming schema entirely with four **Up-Walking axioms** governing an
indeterminate primitive `θ` that walks the powers of 2 — so the system needs
only the constants 0, 1, 2 to reach every integer, where `ISCE` needed
infinitely many. `θ` is admitted precisely because it is **under**determined: a
**Q-Function**, one for which the axioms permit uncountably many distinct
solution sequences (`Willard2016` Def. 3.6). Its term for `n` costs
`O(Log³ n)` symbols as a tree, `O(Log n)` as a DAG.

### 5.6 Finiteness of the axiomatization

Group-2 is infinite by construction. Three results reduce it, and each charges
a price the codified statement must state.

**`Willard2011` Appendix G** — **three added sentences** suffice to prove every
*Braced* theorem purely and to assert self-consistency (Thm G.2, `sketch`); and for any
constant `c`, all `Π^ξ₁` theorems with at most `c` quantifiers are provable
purely (Thm G.3, `sketch`). *The price:* `c` is fixed in advance. This is the answer to
the finite-axiomatization question put to Willard in this repository's own
correspondence (obligation **O23**).

**`Willard2014` §§5–6** — `IS_D^#(β)`, a finite-Group-2 variant. For
standard-model-valid `A` and a kernel index `i`, a finite `β_{A,i}` exists such
that `IS_D^#(β_{A,i})` proves the `i`-th **kernel image** of every `Π*₁`
theorem of `A` (Thm 3, `sketch`). *The price:* the finite system proves not the theorems
of `A` but their kernel images `∀x Test_i(⌜Ψ⌝,x)`, and Example 2 notes that PA
equates the two while a weak system may not.

**`Willard2016` Remark 7.3** — the same reduction "will routinely generalize"
to `ISCE`, `IQFS` and `IQFS*`. *Status:* `cited`, not proved.

---

## 6. The preservation engine

Every affirmative result in the corpus has the same shape: *given a base theory
`A` satisfying some condition, the constructed system `IS_D(A)` is consistent.*
This section states the machinery that proves it. `Willard2005` §5 is the
fullest treatment and is followed here; the other papers' versions are given as
the variants they are.

### 6.1 What has to be proved, and the three things it can mean

The construction of §5 makes clause (i) of self-justification trivial. Clause
(ii) — that the result is *in fact consistent* — is the whole difficulty, and
it is what "consistency preserving" names.

`Willard2005` Definition 1 fixes the target: a map `ℑ(•)` from an axiom basis
to a system is **consistency preserving** iff `ℑ(A)` is consistent whenever
`A`'s `Π*₁` theorems are valid in the standard model.

Two other papers use the same phrase for different hypotheses:

| Hypothesis on the base theory | Source |
| --- | --- |
| `A`'s **`Π*₁` theorems** are valid in the standard model | `Willard2005` Def. 1 |
| **`β` together with Groups 0 and 1** is consistent | `Willard2016` Def. 5.2 |
| **all `β`'s axioms** hold in the standard model | `Willard2020` Def. 4.3 |

They are not interchangeable, and the relations between them are not uniform.
`Willard2005`'s **hypothesis** is the weakest of the three — only `A`'s `Π*₁`
theorems need be true, not the whole theory — so as a *property of the map* it
is correspondingly the **strongest**: 2020-preservation follows from
2005-preservation, not the reverse. `Willard2016`'s is **incomparable** with
both: a theory true in ℕ may still be inconsistent with a particular Group-1,
and a theory consistent with Groups 0–1 need not be true in ℕ. All three are
the hypothesis of a headline theorem. Drift **D55**: the phrase is never to be
written unqualified, and the direction of any comparison must be stated.

### 6.2 The envelope: bounding how far a sentence really reaches

The difficulty is that a cut-free proof of `0 = 1` could in principle be
enormous, and the system's own axioms — including the self-referential one —
must be shown consistent against *all* of them. The device that makes this
tractable runs through the entire corpus: attach to each sentence a **bound on
how far its unbounded quantifiers actually reach**, then show the bound
survives every deduction rule.

`Willard2001` Definition 1.1 is the common ancestor: `Φ^i_j` is `Φ` with its
unbounded universals rebounded by `i` and its unbounded existentials by `j`.
The construct is then rebuilt five times:

| Source | Form | Note |
| --- | --- | --- |
| `Willard2001` Def. 1.1 | `Φ^i_j`; `Φ^i = Φ^i_∞` | the ancestor |
| `Willard2002c` §1.2 | the `(L,M)`-**Conservative Branch**: `Val(u) ≤ Min(M, L·2^d)` for a new parameter at depth `d` | with **PROBE** and the `½` |
| `Willard2004` §6 | **`G-good`**, measured by `ℑ(H) = Σ χ(p_i)` | the direct ancestor of `Normed` |
| `Willard2005` §5 | **`App∀(a)`** / **`App∃(b)`** (Eqs. 16–17) | bounded quantifiers explicitly **unaffected** |
| `Willard2011` §4 | **`Scope_E(Υ,N)`** with `E(N) = 2^N`; **`Good(N)`**; **`♯(Υ)`** = the largest such `N` | one measure replacing the `(a,b)` pair |

`Willard2011` Definition 4.5 gives the balance its simplest statement in the
corpus — a configuration is **Tight** when every inconsistent `β` above the
base satisfies `Log(q_β) ≥ ♯(β) + 2`. That is the growth restriction of §5.3,
written as an inequality between a proof's length and its envelope.

The construct that carries the envelope through a proof is likewise renamed at
every appearance — the conservative branch (`Willard2002c`), the **Partial
Proof** with its **Open Branch** and **Bottom Node** (`Willard2004` §5), the
**Semantic Tableaux Fragment** with its **Major Branch** (`Willard2007-APAL`
Def. 16), the **pivotal branch** (`Willard2009` Lem. 5). One construct, four
names; drift **D56**.

### 6.3 `Normed(a,b)` and `θ`-Compactification

`Willard2005` Definition 4. For `a > b ≥ 1`, a system `α` is **`Normed(a,b)`**
iff

1. every axiom of `α` is a `Π*₁` sentence satisfying `App∀(a)` or a `Σ*₁`
   sentence satisfying `App∃(b)`; and
2. **`α`'s language employs the eight U-Grounding function symbols and
   absolutely no other function symbols** — a clause Willard says is "designed
   to preclude `α` from employing a multiplication function symbol — **or any
   of its many functional equivalents**".

Clause 2 is where the language profile of §3 enters the engine. It is not a
convenience; §6.7 shows the engine is *impossible* without it.

`Willard2005` Definition 5. For `α` `Normed(a,b)`, `Φ` a `Π*₁` or `Σ*₁`
sentence, `p` the Gödel number of a `D`-proof of `Φ` from `α`, a positive
constant `θ < 1/3`, and the side condition `p ≤ ⌈a/b⌉ + 1` (Eq. 18) — the
apparatus `D` is **`θ`-Compactified** iff every such tuple satisfies

- **(A)** if `Φ` is `Σ*₁`, then `Φ` has `App∃(b·p^θ)` validity;
- **(B)** if `Φ` is `Π*₁`, then `Φ` has `App∀(a/p^θ)` validity.

In words: a proof of length `p` can stretch the envelope by at most a factor
`p^θ`. Willard calls this "the main engine used to prove `IS_D(·)`'s
consistency preservation property… **It did not exist in our prior papers**".

### 6.4 Theorem 1 — the engine

> **`Willard2005` Theorem 1** (p. 19, `full`). If `D` is `θ`-Compactified for
> some positive `θ < 1/3`, then the `IS_D(·)` mapping is consistency
> preserving.

This is the corpus's central theorem, and it is entirely **apparatus-neutral**:
it says nothing about tableaux, Herbrand or Hilbert. Any apparatus that
compactifies gets a self-justifying system for free. Everything apparatus-
specific is pushed into the single hypothesis, which each apparatus must then
be shown to satisfy.

One side condition is glossed in the printed argument and must be restored:
the closing inequality needs **`m ≥ 3`**, since `g > h` unfolds to
`m − 1 > m^{2θ}`, which fails for small `m`. Obligation **O1**.

### 6.5 Discharging the hypothesis for semantic tableaux

`Willard2005` §5.2 and Appendix B. Some machinery first.

A **`Z`-Based Deduction Tree** differs from a tableaux proof tree in two ways
(p. 22): its root stores an **axiom of `Z`** rather than the negation of a
target theorem, and its branches are **not required to close**. For such a
tree: the **U-Length** of a node sentence is its count of U-Grounding function
symbols; its **U-Depth** `Δ(s)` is its U-Length plus those of all its
ancestors; the tree's **U-Height** is the maximum U-Depth over its leaves.

A branch `β` is **`Positive(a,b)`** when some valuation `VAL` on `β`'s
parameters satisfies

- **I.** every new parameter `u` introduced at a node `s` has
  **`Val(u) ≤ b·2^{Δ(s)}`** (Eq. 19); and
- **II.** every node sentence `s` on `β` satisfies: **(a)** if `s` is `Π*₁` it
  has `App∀(a)` validity under `VAL`; **(b)** if `s` is `Σ*₁` it has `App∃(b)`
  validity; **(c)** if `s` is `Δ*₀` it is logically valid under `VAL`.

Two lemmas then do the work.

> **Lemma 1** (p. 23, `full`). For `a > b ≥ 1`, `Z` a `Normed(a,b)` system and
> `T` a `Z`-Based Deduction Tree whose U-Height is less than
> `Log₂ a − Log₂ b`, the tree `T` possesses **at least one `Positive(a,b)`
> branch**.

The proof is constructive: an algorithm `Probe(a,b,T)` walks down from the root,
choosing at each binary split the child whose sentence satisfies condition II
and, at each parameter introduction, assigning `VAL(u)` the **smallest** value
consistent with `Positive(a,b)` and with earlier assignments. Correctness is an
induction with **nine subcases (a)–(i)**: one for the case where the node
stores an axiom of `Z`, and one for each of the **eight** elimination rules of
§4.4. Case (c), bounded `∃`-elimination, is where the argument turns — `t` is
built from inputs no larger than `b·2^{Δ−L}` by at most `L` growth-function
applications, "where the **Double operation is the fastest available growth
function**", giving `Val(t) ≤ b·2^Δ`. That sentence is exactly where clause 2
of `Normed(a,b)` is consumed: admit multiplication and the bound fails.

> **Lemma 2** (p. 25, `full`). No `Positive(a,b)` branch of a `Z`-Based
> Deduction Tree can be **closed** — that is, it cannot carry both some `Υ` and
> its negation.

The argument turns on a fine point recorded in footnote 3: a branch can carry
`Υ` and its *strictly encoded* negation only when **both are `Δ*₀`**, because a
proper `Σ*₁` or `Π*₁` sentence has an unbounded quantifier to the left of every
other logical symbol, so `¬Υ` is then neither. And condition II-c makes every
`Δ*₀` sentence on the branch valid under `VAL` — so `Υ` and `¬Υ` would both be
valid, which is impossible. Obligation **O27**: the delicacy is in the
*negation*, and it must be stated, not inherited as a gloss.

> **Theorem 2** (p. 26, `full`). Semantic tableaux satisfies
> `θ`-Compactification **when `θ = 1/4`**.

Part (A) is proved in §5.2 by contradiction — a `Σ*₁` sentence `Φ` with a proof
`p` but a fallacious `App∃(p^θ·b)` interpretation yields a `Normed(p^θ·b, b)`
system `Z` for which Eq. (20) puts `(p, Z)` inside Lemma 1's hypothesis, so `p`
has a positive branch, which Lemma 2 says cannot close — contradicting that `p`
was a proof. Part (B) is Appendix B, "roughly a permutation of the ideas from
Part (A)'s proof — with the roles of the universal and existential quantifiers
now reversed".

> **Theorem 3** (p. 27, `full`). `IS_D(·)` is consistency preserving when `D`
> is semantic tableaux. *Immediate from Theorems 1 and 2.*

### 6.6 The three constants, and which way the inequality runs

Three distinct constants meet in Theorem 2's proof, and conflating them is the
easiest available error (obligation **O2**):

| Constant | What it is | Source |
| --- | --- | --- |
| **`1/5`** | the **coding density** — `U-Height(p) < (1/5) Log₂(p)` | `Willard2005` Eq. (20), from Appendix A's six-bit-byte encoding "as well as any other reasonable encoding methodology" |
| **`1/4`** | the value of `θ` **proved** for tableaux | Theorem 2 |
| **`1/3`** | the **ceiling** `θ` must stay below for Theorem 1 to apply | Definition 5 |

Density bounds `θ` from **below**, not above: a denser encoding permits a
*smaller* admissible `θ`. The relation is the window

> `1/d ≤ θ < 1/3`,  with `d = 5` here, and `θ = 1/4` chosen inside it.

`ADR-0001` originally recorded this as "`θ ≤ 1/5`", which inverts it; the
correction is drift **D6** and obligation **O3**. This was the C4 pilot's
principal finding and the reason the pilot existed.

### 6.7 Why the boundary sits where it does

`Willard2005` Remark 4 is the strongest apparatus-independent statement in the
corpus. Definition 5's parts (A) and (B)

> "are **impossible to satisfy** when Part-2 of the definition of a
> `Normed(a,b)` system is expanded to allow for multiplication to appear as a
> **ninth** function symbol. This is intuitively because multiplication has a
> faster growth property than addition, which will **overwhelm** Definition 5's
> constraints."

The sentence that follows is stronger, and its ground must be carried with it —
it is **not** a consequence of the growth argument above but a corollary of a
separately cited theorem:

> "**Moreover, a Level(0-) styled semantic tableaux generalization of the Second
> Incompleteness Theorem in [68] implies that** no useful analog of Definition 5
> can be found for axiom systems recognizing multiplication as a total function,
> under any possible deduction method `D`, whether cut-free or otherwise."

`[68]` is `Willard2002a`. The apparatus-independence is therefore inherited from
a particular incompleteness theorem rather than read off the compactification
machinery, and it is only as strong as that theorem — whose relevant result,
`Willard2002a` Theorem 6.4, is `sketch`.

Remark 3 adds why the *level* boundary falls where it does: `Tab-Π*₂` and
`Tab-Σ*₂`-List deduction defeat the programme "because their deductive methods
**do not satisfy** Parts (A) and (B) of Definition 5's requirements", so "some
significant generalizations of the Second Incompleteness Theorem take place at
**exactly the level where Definition 5's formalism becomes no longer
applicable**."

Two distinct claims live here and should not be run together. That
`θ`-Compactification **fails** once multiplication is admitted is Willard's own
growth argument about Definition 5. That **no useful analog** of it exists under
any apparatus whatever is inherited from `Willard2002a`, and carries that
paper's proof status.

### 6.8 The other apparatuses

`Willard2005` Theorem 4 (`full`) shows `Tab-U*₁-List` is `θ`-Compactified at
`θ = 1/4`, by induction over the proof list reducing to Theorem 2; Theorem 5
(`full`) is the corresponding preservation result and the paper's main positive
theorem.

Remark 1 asserts that Theorem 1's machinery also covers the **cut-free sequent
calculus**, **Herbrand deduction**, **`Tab-Q*₁-List`**, and a `Q*₁`-restricted
variant of **Hilbert** deduction — "although we will not have the page space to
do so". That is four apparatus claims, all `stated-only`, and the last of them
is the only place in the corpus where a Hilbert-style method is claimed for the
tableaux-line engine. Obligations **O4**, **O67**, **O80**.

### 6.9 The Hilbert line's engine

The Hilbert line does not use `θ`-Compactification. Its preservation results
are proved directly, by minimal counterexample:

| System | Theorem | Status | Mechanism |
| --- | --- | --- | --- |
| `ISREF(A)` | `Willard2001` Thm 3.4 | `full` | minimal-`i` triple `(Ψ, p, i)` |
| `IS(A)` | `Willard2001` Thm 4.3 | `full` | `A` regularly consistent ⟹ `IS(A)` consistent |
| `ISCE(A)` | `Willard2006a` Thm 3 | `full` | as Thm 3.4, with additive naming |
| `IQFS(β)` | `Willard2016` Thm 6.7 | `full`, **conditional** | on Conjecture 6.6 |

`Willard2016`'s conditionality must be carried **wherever the result is used**,
not only where it is introduced (obligation **O66**). Theorem 6.7 is a full
proof of a conditional statement; Conjecture 6.6 is `stated-only`; and
`Willard2020` §8 nevertheless draws a strong consequence from it. §9.2 states
what the appendix's evidence does and does not establish.

---

## 7. The boundary and the negative side

Half of this corpus proves that self-justification is **impossible**, and those
results are what give the affirmative half its meaning. They also behave
differently in one respect worth stating at the outset: **the negative results
are non-constructive while the positive ones are not** (obligation **O37**).
An affirmative result exhibits a system; a negative result shows that a
hypothetical short proof would have to exist, and derives a contradiction from
its minimality.

### 7.1 The tableaux boundary and the Paris–Wilkie question

In 1981 Paris and Wilkie asked whether `IΣ₀` satisfies the Second
Incompleteness Theorem for cut-free deduction. The corpus answers it in **two
opposite directions**, and both answers are Willard's (obligation **O62**).

`Willard2002a` supplies the affirmative-for-incompleteness half:

> **Theorem 3.4** (p. 11, `full`) — `Q + V` is consistent, for an explicitly
> constructed `Π₁` sentence `V`.
>
> **Theorem 3.5** (p. 11, `full`) — **no consistent finite extension `α` of
> `Q + V` can prove its own semantic tableaux consistency.**
>
> **Theorem 5.2** (p. 21, `full`) — the same for **infinite** systems with the
> Conventional Deciphering Property.
>
> **Theorem 5.4** (p. 22, `full`) — with `W = V ∧ V₆`, no `ω`-consistent finite
> extension of `Q` can prove the non-existence of a tableaux proof of the
> invalid `Σ₁` sentence `¬W` from itself.
>
> **Theorem 6.4** (p. 25, `sketch`) — any consistent extension of `IΣ₀` with
> the Conventional Deciphering Property cannot prove the non-existence of a
> tableaux proof of `0 = 1` from itself.

`Willard2007-APAL` extends this from the textbook axiomatization to the
Wilkie–Paris one:

> **Theorem 6** (p. 29, `full`) — every consistent extension of `T-1` **or
> `T-2`** is unable to prove a theorem asserting its own semantic tableaux
> consistency.

Its route is **Passive Induction** (§4.5), which is both simpler than
`Willard2002a`'s argument and, unlike it, covers both induction schemes.

### 7.2 Why multiplication is the trigger

Willard says three times, in three papers, that the negative proofs break at
exactly the point where multiplication stops being a total function — and each
time he names the *specific lemma* that dies.

`Willard2000-TAB` §5:

> Many readers will smile with amusement when they learn **the true reason**
> that the Semantic Tableaux version of the Second Incompleteness Theorem
> breaks down when Multiplication is changed from a total function into a
> 3-way relation. It is essentially that **Lemma 8 and its short proof then
> become no longer valid.** … **Without it, our proof … collapses entirely!**

`Willard2002a` §7 says the same of its Lemma 4.7 — "and its *seemingly trivial*
short 3-paragraph proof" — adding that "all the other successive stages of our
proof will **collapse in a one-by-one, step-by-step manner**". `Willard2004`
p. 4 says it of its own construction. In each case the dying lemma is the one
that builds `u₀ = 2`, `u_{i+1} = u_i·u_i` in `O(n)` nodes.

So the growth device of §5.3 is not merely how the affirmative systems are
built: **it is what the negative proofs consume.** The two halves of the corpus
are two readings of one observation, and `Willard2005` Remark 4 (§6.7) closes
the circle by showing the affirmative engine is impossible on the other side of
the same line. Obligation **O76**.

Earlier statements of the same point are in `Willard1993` pp. 328–331 and
`Willard2001` Remark 4.5, the latter localising the failure precisely:
`ISMULT(A)` satisfies an analogue of the `++` invariant but has **no analogue
of Lemma 4.2**, because "multiplication allows a sequence of parameters
`u₀, u₁, …, u_n` to grow at a *very much, much faster rate*".

### 7.3 The negative controls

Each affirmative result is paired with a minimally different system that fails.
The controls isolate exactly one variable, and they are as load-bearing as the
theorems.

**Axiom versus theorem.** `Willard2005` Theorem 6 (p. 34, `full`):
`NS^{k,m}_D(PA+)` is **inconsistent** for `k` above a threshold. It differs
from the consistent `IS_D(PA+)` only in holding `Υ(k,m)` as an axiom rather
than proving it (§5.4). The proof exhibits an explicit **seven-step
`Tab-U*₁-List` proof `W`** of the Gödel sentence `℧(k−1,m)`, short enough that
its own existence is precluded by what `℧(k−1,m)` asserts. Remark 8 lists three
strengthenings; the "generalizes to all cut-free methods" clause is
`stated-only`.

**Multiplication.** `Willard2001`'s `ISMULT(A)`, and `Willard2020`'s
`IS^M_Tab(β)`.

**LEM as logical axioms.** `Willard2020` Theorem 4.5 (p. 13, `sketch`):
`IS_Xtab(β)` is **automatically inconsistent** whenever `β` proves the usual
`Π*₁` associativity, commutativity, distributivity and identity properties. So
admitting excluded middle as a *logical axiom* — which is exactly what recovers
the cut (§4.5) — destroys the construction. The identification of LEM-as-axioms
with cuts is already in `Willard1993-TR`; obligation **O55**.

### 7.4 The axiomatization line: one theory, opposite properties

`Willard2009` holds the theory fixed and varies **only the axiomatization**.
`Ax-1`, `Ax-2` and `Ax-3` prove **the same theorems** (Theorem 1, `sketch` —
and the premise of everything that follows), and yet:

| System | Herbrand | Semantic tableaux |
| --- | --- | --- |
| `Ax-1`, `Ax-2` | **threshold** — **not a Willard result**: `Willard2009` p. 18 credits Adamowicz–Zbierski [1,3] and Salehi [33] | **threshold** — `Willard2007-APAL` Thm 6, `full` |
| **`Ax-3`** | **anti-threshold** — `Willard2009` Thm 4, `full` | **threshold** — Thm 5, `sketch` |
| `Ax-4` | anti-threshold — Lem. 6, `sketch` | threshold — Lem. 5, `sketch` |

`Willard2007-APAL` Theorem 6 concerns **semantic tableaux only** — "unable to
prove a theorem asserting its own *semantic tableaux* consistency" — and belongs
in that column alone. The corpus supplies no Willard proof for the
`Ax-1`/`Ax-2` Herbrand cell.

The explanation is that the three systems **cannot prove that they are
equivalent**. `Willard2007-APAL` p. 3 states it: "these three systems will be
unable to formally recognize that they prove the same set of theorems (although
they actually do generate identical sets of theorems)." The equivalence itself
(`Willard2009` Theorem 1) is `sketch`. This evades by **re-axiomatising, not by
weakening** — the one place in the corpus where nothing is given up
(obligation **O61**).

Two cautions. **Theorem 5 is the counterweight**: the same `Ax-3` that evades
the Herbrand effect **obeys** the tableaux one, and quoting Theorem 4 without
it misrepresents the result as a general evasion (obligation **O64**). It is
`sketch`. And the sharpest form of the thesis survives only in the **conference
witness**: `Willard2006-WoLLIC` p. 10 states that `Diag(1)` and `Diag(2)` are
**provably logically invalid** while `Diag(3)` is valid — the same diagonal
construction failing and succeeding under logically equivalent axiomatizations.
The journal version drops it (drift **D47**, obligation **O69**).

### 7.5 The Level dial

Fix Type-A and tableaux; vary what Group-3 quantifies over (§5.2).

> **`Willard2004` Theorem 1** (p. 6, `full`). There is a `Π*₁` theorem `W` of
> PA such that no consistent **finite** `α` containing `W` in the U-Grounding
> language — hence with Addition total — can recognize its own **Level(2+)**
> tableaux consistency.

With `Willard2002c`'s Level(1) positive result, this brackets the boundary in
what `Willard2004` §1 calls a "**very narrow** gap". Remark 1 trades the
finite-cardinality hypothesis for the Conventional Deciphering Property
(`stated-only`). Theorem 2 gives the companion `Π*₂`/`Σ*₂` negative at
Level(0−) under `Tab-ℜ-List` (`sketch`).

This is the only dial in the corpus whose negative half is *proved*. Even so,
its proof defers the `T* ≤ N²` step to "a longer version of this paper", in a
footnote that says in terms that it "**should not be considered a formal
proof**" (gap **G35**).

### 7.6 What the negative side does not establish

Three qualifications the codified statement must carry.

**Deferred proofs.** `Willard2004` defers four obligations to a longer version
that does not exist — the `T* ≤ N²` bound, Lemma 7's formal proof, Theorem 2's
derivation, and Theorem 3's PROBE construction (**G35**). `Willard2000-TAB`
Lemma 1 refers to an "unabridged version" holding an infinite-cardinality
strengthening of its Theorem 4 and generalizations to Herbrand deduction, the
cut-free sequent calculus and **resolution** (**G30**, accepted at C12 after
the nachlass search found nothing). These are open, not discharged by the
journal forms (obligation **O93**).

**Proof status.** Of the governing negative results, `Willard2002a` Thms 3.5,
5.2, 5.4, `Willard2004` Thm 1 and `Willard2007-APAL` Thm 6 are `full`; but
`Willard2002a` Thm 6.4, `Willard2004` Thm 2, `Willard2009` Thm 5 and
`Willard2020` Thm 4.5 are `sketch`.

**Apparatus generality.** `Willard2002a` p. 5 claims the negative results
generalize to Herbrand deduction, resolution and the cut-free sequent calculus,
subject to replacing `V` by an apparatus-specific `V_d` — eighteen years before
the corresponding affirmative claim in `Willard2020` §8, and equally unproved.

---

## 8. The result matrix

The full source-anchored grid is
[`concordance/result-matrix.md`](concordance/result-matrix.md). Its shape:

| Profile ↓ / Apparatus → | Hilbert | Tableaux | TabList | Herbrand | Xtab |
| --- | --- | --- | --- | --- | --- |
| **Type-M** | **−** Pudlák/Solovay | **−** `sketch` | — | **−** `full` (**but §7.4**) | — |
| **Type-A** | **−** by Solovay `∗` | **+** Level(1) `full` | **+** Level(1) `full` | — | **−** `sketch` |
| **Type-S** | **−** Solovay `∗`, *cited, unpublished* | not studied | — | — | — |
| **Type-NS** | **+** — by naming convention, §5.5 | — | — | — | — |
| **Type-Almost-M** *(off-lattice, §3.1)* | — | **−** `Ax-3`, `sketch` | — | **+** Level(0^R) `Willard2011` `ξ^R`; **+** `Ax-3` `full` | — |

### 8.1 State the boundary once, then coordinatise it

The corpus contains **five** parameterisations of "how much may I have before
the effect bites", and no paper relates any two of them. They are one boundary
in five coordinate systems, not five results (obligations **O49**, **O60**;
drift **D38**).

| Dial | Positive | Negative | Status of the two halves |
| --- | --- | --- | --- |
| **`ℜ`** — TabList intermediate class | `Π*₁ ∪ Σ*₁` | `Π*₂`, `Σ*₂` | `sketch` / `sketch` |
| **Level(n)** — what Group-3 quantifies over | Level(1) | Level(2+) | `full` / `full` (G35) |
| **Hybrid(H)** — naming growth rate | `H = 1` | `H > 1` | `stated-only` / `stated-only` |
| **`Herb−k`** — Herbrand intermediate class | `k = 1` | `k = 2` | `stated-only` / `stated-only` |
| **`Z`** — which LEM instances are logical axioms | `Δ*₀` | `Π*₂` and above | `stated-only`; **`Π*₁` open** |

Two of these must **not** be merged. `Willard2004`'s `ℜ` parameterises which
*intermediate theorems* a TabList proof may use; `Willard2020`'s `Z`
parameterises which *LEM instances* are logical axioms. Both report a `Π*₁`-level
positive and a `Π*₂` negative, and `Willard2020` does not say they are different
dials (drift **D34**, obligation **O46**). §9.1 shows that this distinction is
exactly what makes the corpus's open problem statable.

Three dials name their transition point rather than bracketing it:
`Hybrid(H)` at `H = 1`, `Herb−k` at `k = 1`, and — most finely — `Willard2002c`'s
**two-index `R(i,j)`**, where `R(1,1)` works and **`R(2,1)` fails**. The `ℜ` dial
of `Willard2004` is a *projection* of `R(i,j)` onto one index (drift **D46**),
so the two-index form carries strictly more information and is the sharpest
localisation in the corpus. All three named thresholds are `stated-only` on both
halves. Obligation **O60**.

### 8.2 Reading the matrix honestly

Counted by status across the governing results: **the affirmative side is
mostly `full`, the negative side is mostly `sketch`, and both exactly-located
dials are `stated-only` throughout.** A presentation that showed the boundary
as sharply determined everywhere would overstate what the corpus proves.

The cells that are empty are empty for reasons, not by oversight: Type-S under
Hilbert is closed by Solovay; Type-NS is studied only under Hilbert and Type-A
only under cut-free apparatuses, because those are the two retreats §2.4 allows;
cut-free sequent calculus and resolution are claimed but never proved; and the
real-valued line (`Willard2005-TAB`, `Willard2006b`) sits outside the integer
grid entirely, since `IS_D(A′)` recognises multiplication as total **over
simulated reals** — its primary witness, the ASL-2005 technical report, is
unlocated (**G36**).

---

## 9. Frontier and open problems

### 9.1 The `Π*₁` conjecture, and what it actually asks

`Willard2020` §7 conjectures that the `Δ*₀` evasions "will continue at the
`Π*₁` level, but **this fact has not yet been formally proven**". This is the
corpus's sharpest open problem, and drift **D34** sharpens it further.

On `Willard2004`'s `ℜ` dial, the `Π*₁ ∪ Σ*₁` case is **affirmative but only
`sketch`** (Theorem 3, at Level(1); gap **G35** records that its PROBE
construction was deferred to a longer version that does not exist, and accepts
that status as permanent). On `Willard2020`'s `Z` dial, the `Π*₁` case is
**conjectured**. Since the two dials are different parameterisations (§8.1), the
conjecture is precisely the claim that

> **`Willard2004`'s affirmative `ℜ`-dial result transfers to the `Z` dial.**

The transfer question is therefore harder than it first looks: what would be
transferred is itself only sketched.

`Willard2004`'s `W₄` and `W₅` (Eqs. 12–13) make the connection concrete: they
are already Z-enrichment axioms in `Willard2020`'s sense, for the two `Σ*₀`
predicates `Prf` and `Subst`, and Lemma 5's proof uses them exactly to generate
the `∨`-elimination splits its case analysis needs. So the LEM-as-axiom device
that `Willard2020` makes its centrepiece is present in restricted form in the
base theory of the 2004 negative theorem, sixteen years earlier (drift **D39**).

### 9.2 Conjecture 6.6, and why `IQFS` is conditional

`Willard2016` Conjecture 6.6 is **the corpus's one unproved load-bearing
lemma**: Theorem 6.7 — `IQFS`'s consistency preservation, and the whole θ line
— holds only if it does, and Remark 6.8 says so.

Willard writes that "we are **essentially 100 % confident** that Conjecture
6.6 is true" (p. 24), and the appendix argues the case. §A.1 derives the constant:
`θ` is the only growth-permitting primitive in `L^Q`, and the `Π^Q_1`/`Σ^Q_1`
definitions forbid `θ` outside the `E_j` terms, so a proof can reach `n > 2`
only by isolating `d+1` distinct powers of 2; at six bits per symbol this
forces `Log₂(P)` to exceed `Log₂(K)` by a factor of at least 6 — and Willard
notes the `1/6` is "a **conservative overestimate**".

§A.2 explains why it is nonetheless only a conjecture, and the reason is
striking: **the analogue of Conjecture 6.6 is false when Successor replaces
`θ`**, and the Pudlák–Solovay result `++` is proved precisely *by using that
falsity*. The conjecture therefore asserts exactly the failure of the mechanism
that proves the theorem it evades. Three differences are offered — `θ`'s
iterates may be **monotonically decreasing**, `θ` is a Q-Function with `ℵ₁`
solution vectors, and the `Π₁` axioms about `Add`/`Mult` are "fully irrelevant
to `θ`'s behavior".

One further qualification is Willard's own and belongs in any presentation:
`IQFS(β)` "**does not need** an actual formal proof … All it needs, from a
strictly minimalistic perspective, is for the Conjecture 6.6 to hold true
**under the Standard Model** (even if its formal proof is independent from …
Peano Arithmetic and/or ZF Set Theory)."

Obligation **O53** requires both frontier problems to be carried, not just the
better-known one.

### 9.3 A smaller open question, and a refuted conjecture

`Willard2002a` §7 poses an explicit problem: **what is the simplest and
shortest `Π₁` sentence `V` such that `Q + V` is a threshold** for the tableaux
effect? Willard notes his own `V` "could be shortened considerably", having
been chosen to shorten the *proofs* rather than the sentence; Comment 3.6
explains the trade-off between the two halves of the argument.

And one conjecture in the corpus is **refuted by Willard's own later work**:
`Willard1993` §6 conjectured that the cut-permitting `IS_+(A)` and `IS*_+(A)`
also satisfy part (ii) of self-verification. They do not — but **no Willard
paper says so**. The refutation is an inference drawn by this codification
(obligation **O8**) from four sources: TR 93-10 Proposition 7, Solovay's
theorem, `Willard2004` and `Willard2005` Theorem 5. It is carried here as a
codifier's synthesis, not as a result Willard published.

### 9.4 The nachlass, and what remains unwitnessed

C12 extracted nine witness-grade records from Willard's papers, including the
2025 Tab/Xtab and boundary drafts, the 2008 ZCF/`WZF` set-theory notes, and the
Hájek correspondence. They are witness-grade: they record what Willard was
working on, not established results. The 2008 ZCF drafts extend the
**finite-set reading** of SJAS whose origin is `Willard2001` Remark 3.8, not
2008 (obligations **O36**, **O94**). The 2025 drafts carry title-page and
referee witnesses for `Willard2021` (obligation **O95**).

The principal unwitnessed items are recorded as gaps rather than papered over:

| Item | Gap | Status |
| --- | --- | --- |
| `Willard1997`, KGC5 Vienna, LNCS 1289 pp. 319–334 | **G2** | blocked; no open-access copy |
| `Willard2021`, JLC 31(1) pp. 375–392 | **G1** | unacquired; the terminal paper |
| ASL-2005 floating-point technical report | **G36** | confirmed to exist by `Willard2005` [71]; primary witness for the real-valued line |
| `[Wi94]`, "The ETR Reflection Principles…" | **G14** | unlocated; the **only** source of TR 93-10 Proposition 8's proof |
| TABLEAUX 2003 position paper | **G4** | existence unconfirmed |

Two further items are unwitnessed but **accepted** rather than open: the
unabridged `Willard2000-TAB` (**G30**) and `Willard2004`'s longer version
(**G35**), both searched for at C12 and not found in the nachlass.

---

## 10. Provenance apparatus

### 10.1 Conventions

Provenance is inline: every claim above names its paper, its numbered item and
its page. Proof statuses are `registry/results.md`'s and are restated at the
point of *use*, not only at introduction — §6.9 and §8.2 exist for that reason.
Divergences between papers are cited to the drift ledger and left standing.

### 10.2 The machine-checked layer

| Registry | Contents |
| --- | --- |
| [`registry/corpus.md`](registry/corpus.md) | 45 items ruled by the throughline test; 33 extracted |
| [`registry/systems.md`](registry/systems.md) | 62 rows → ~20 systems, names frozen at C13 |
| [`registry/results.md`](registry/results.md) | 524 numbered items, 24 canonical topics |
| [`registry/notation.md`](registry/notation.md) | 224 symbol rows |
| [`registry/coverage.md`](registry/coverage.md) | what was actually read, as page ranges — 33 rows, all `complete` |
| [`registry/gaps.md`](registry/gaps.md) | 37 gaps, each actioned or accepted |

`audit.sh` enforces referential integrity, the type/proof/topic enumerations,
and — since 2026-08-27 — that every `complete` coverage row images the witness
**in full**. It is green as a merge condition.

### 10.3 Two standing hazards

Every formula in this document was read from a **rendered page image**, not
from extracted text. Two reasons, both discovered the hard way.

**H1.** `pdftotext` renders JSL small-caps headings as `D EFINITION`, so naive
greps miss every heading; the sweep recipe must be uncapped and
case-insensitive.

**H2.** `pdftotext` substitutes plausible ASCII for mathematical typography and
**fails silently**. Recorded instances: Fraktur `ℑ` as `=`; a script glyph as
`f`; dropped numeral overbars, tower super/subscripts and floor/ceiling
delimiters; `1/3` collapsed to `31`; `⊙` as `J`; `y′` as `y 0`; `C*_j =
2^{2^{j−2}}` flattened to `2 2 j−2`; two differently decorated `ψ`s collapsed
to one. Worst of all, **`℧` (U+2127) renders as the digit `0`**, so
`Willard2007-APAL` Definition 11 reads "a `Π₁` sentence **0**" — plausible
English, wrong content, and nothing on the page flags it. That glyph is used
four different ways across four papers.

A criterion applied to a corrupted source cannot detect the corruption. This is
why the visual-control rule is unconditional: **every page of every document,
read twice, once as text and once as an image.**

### 10.4 Where the composition obligations stand

The 87 obligations in
[`concordance/composition-obligations.md`](concordance/composition-obligations.md)
are the checklist this document was composed against. Ids run O1–O80 and
O90–O96; O81–O89 were never issued. Obligations discharged in this draft are
cited inline at the point of discharge; those still outstanding are marked in
the register.
