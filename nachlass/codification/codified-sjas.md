# The Codified SJAS

*A single mature statement of Dan Willard's Self-Justifying Axiom Systems,
1993–2021.*

> **Status.** C14 — skeleton complete; §§1–5 (the core statement: what an SJAS
> *is*) written. §§6–9 carry their sources, obligations and gap markers and are
> composed at C15.

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
the Pudlák–Solovay result of §2.3. Everything in this corpus is an answer to
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

Kleene, Rogers and Jeroslow each warned that the resulting `α^D` is usually
**inconsistent** — "although they were technically well-defined". Willard's own
gloss (`Willard2016` Example 3.5): the encoding "is relatively easy, via an
application of the Fixed Point Theorem, but this sentence is ironically
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
presentation is `Willard2006a` p. 4, as **Theorem 2**:

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
and `Root(x,y)` rounds **up** in `Willard2002c`, `Willard2004`, `Willard2005`
and `Willard2020-LFCS` but **down** in `Willard2016` (drift **D53**).

### 3.3 Formula classes

The bounded-formula hierarchy is relativised to whichever language is in play,
and the corpus contains **eleven** such relativisations (drift **D50**):

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

The corpus's most dangerous collision, and Willard disambiguates it twice —
`Willard2004` p. 348 and `Willard2009` p. 16:

> In the proof-theory literature, the definition of a **"Definable Cut"** is
> **formally unrelated** to Gentzen's notion of a sequent calculus deductive
> **"cut rule"**, despite their very similar sounding names.

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

Semantic tableaux is presented four ways across the corpus (drift **D54**):
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
| **LEM as axioms** (`Xtab`) | `Willard2020` App.; `Willard2007-APAL` Lem. 1(i) | admit `℧ ∨ ¬℧` for arbitrary `℧`; branch; hang the two subproofs |
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

`Willard2004` §1 states the crucial qualification:

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
corpus**, and it recurs in ten places under ten guises (see
[`concordance/replication-map.md`](concordance/replication-map.md) §1).

`Willard2006a` p. 7 gives the three-way ordering that organises the Hilbert
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

> it is even able to prove the theorem that multiplication is a total function,
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
*Braced* theorem purely and to assert self-consistency (Thm G.2); and for any
constant `c`, all `Π^ξ₁` theorems with at most `c` quantifiers are provable
purely (Thm G.3). *The price:* `c` is fixed in advance. This is the answer to
the finite-axiomatization question put to Willard in this repository's own
correspondence (obligation **O23**).

**`Willard2014` §§5–6** — `IS_D^#(β)`, a finite-Group-2 variant. For
standard-model-valid `A` and a kernel index `i`, a finite `β_{A,i}` exists such
that `IS_D^#(β_{A,i})` proves the `i`-th **kernel image** of every `Π*₁`
theorem of `A` (Thm 3). *The price:* the finite system proves not the theorems
of `A` but their kernel images `∀x Test_i(⌜Ψ⌝,x)`, and Example 2 notes that PA
equates the two while a weak system may not.

**`Willard2016` Remark 7.3** — the same reduction "will routinely generalize"
to `ISCE`, `IQFS` and `IQFS*`. *Status:* `cited`, not proved.

---

## 6. The preservation engine

> **C15.** Sources: `Willard2005` §§3, 5 (fullest — `Normed(a,b)`,
> `App∀`/`App∃`, `θ`-Compactification, Lemmas 1–2 with the nine-subcase
> `Probe(a,b,T)` induction, Theorems 1–5); `Willard2011` §4 (`Scope_E`,
> `Good(N)`, `♯` — the compressed reformulation); `Willard2002c` §1.2 +
> Lemmas 2–3 (the `(L,M)`-Conservative Branch and PROBE, with the `½`);
> `Willard2004` §6 (`G-good`, `ℑ(H) = Σ χ(p_i)`, the `⅓`); `Willard2001`
> Thm 3.4 (Hilbert-line engine); `Willard2006a` §4 (`ISCE`); `Willard2016` §6
> (`IQFS`, conditional).
>
> Must carry: the three constants kept separate (**O2**); density bounds `θ`
> from **below**, stated as the window `⅕ ≤ θ < ⅓` with `θ = ¼` proved
> (**O3**, drift **D6**); the `m ≥ 3` side condition (**O1**); the three
> inequivalent "Consistency Preserving" hypotheses (**D55**, §Axis 5); the
> negation delicacy (**O27**); `Willard2016`'s conditionality on Conjecture 6.6
> wherever `IQFS` is used (**O66**); the five-vocabulary envelope lineage
> (replication map §5); the four names for the branch that stays open
> (**D56**).

## 7. The boundary and the negative side

> **C15.** Sources: `Willard2002a` (Thms 3.4, 3.5, 6.4 — the Paris–Wilkie
> answer); `Willard2000-TAB`; `Willard2007-APAL` (Thms 5–6 for `T-1`/`T-2`);
> `Willard2004` (Thms 1–2, the Level dial); `Willard2005` §6 (`NS` inconsistent);
> `Willard2006a` (Thms 4, 4\*, the multiplicative convention);
> `Willard2020` Thm 4.5 (`Xtab` fails); `Willard2009` Thm 5 + App. B.
>
> Must carry: §7.4 on `Ax-3` — one theory, three axiomatizations, **opposite**
> incompleteness properties, because the systems cannot prove their own
> equivalence (**O61**); Theorem 5 as the counterweight that stops Theorem 4
> being over-read (**O64**); the `Diag(1)`/`Diag(2)` invalidity contrast, which
> survives only in the conference witness (**O69**, drift **D47**); the
> Paris–Wilkie question answered in two directions (**O62**); the fact that the
> negative results are non-constructive while the positive ones are not
> (**O37**); the deferred "longer version" proofs recorded as open, not
> discharged (**O93**, gaps **G30**, **G35**).

## 8. The result matrix

> **C15.** Composed from
> [`concordance/result-matrix.md`](concordance/result-matrix.md), which is
> already source-anchored and status-carrying. Must state the boundary once and
> then give its **five** coordinatisations — `ℜ`, Level(n), Hybrid(H),
> `Herb−k`, `Z` — rather than presenting them as five results (**O49**,
> **O60**, drift **D38**), and must not merge `Willard2004`'s `ℜ` dial with
> `Willard2020`'s `Z` dial (**O46**, drift **D34**).

## 9. Frontier and open problems

> **C15.** Two frontier problems, not one (**O53**): `Willard2020` §7's `Π*₁`
> conjecture — sharpened by drift **D34** into the claim that `Willard2004`'s
> proved `Π*₁ ∪ Σ*₁` result **transfers from the `ℜ` dial to the `Z` dial** —
> and `Willard2016` Conjecture 6.6, the corpus's one unproved load-bearing
> lemma, on which `IQFS`'s entire result is conditional (**O66**).
>
> Also: `Willard2002a` §7's explicit open question (the shortest `Π₁` sentence
> `V` making `Q + V` a threshold); the nachlass evidence from C12 (**O95**);
> the unwitnessed items — `Willard1997` (**G2**), `Willard2021` (**G1**), the
> ASL-2005 floating-point technical report (**G36**).

## 10. Provenance apparatus

Carried inline throughout rather than gathered here: every claim names its
source and page, every proof status is the registry's, and every unresolved
divergence cites its drift entry. The registries are the machine-checked index
behind it; `audit.sh` is green as a merge condition.
