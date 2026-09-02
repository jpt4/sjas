# The Refined SJAS

*The essential idea of Willard's self-justifying axiom systems, identified and
explicated — departing from his presentation where the idea is clearer without
it.*

> **Relation to the Codification.** [`../codification/codified-sjas.md`](../codification/codified-sjas.md)
> states what Willard proved, in his vocabulary, with his provenance. This
> document asks what he *found*. It may reformulate freely; where it does, it
> says so, and every claim it makes about the literature is anchored in the
> codification's registries rather than restated from memory.

---

## 1. The claim

Thirty years of this literature can be compressed to one sentence.

> **A system can consistently assert its own consistency when, for every proof
> it can write, it can construct within its own means a bounded semantic witness
> that the proof is not a refutation.**

In Willard's arithmetic setting that condition takes one specific form, and it
is this form that the rest of the document develops:

> **naming an integer must not be cheaper than proving something about it.**

Everything else in the corpus — the four axiom groups, the five apparatus
families, the eleven formula classes, the five boundary dials — is machinery for
making the arithmetic form precise in one setting or another, or for locating
the point at which it stops being true.

**The arithmetic form is not the whole idea, and R1's first draft wrongly said
it was.** Pakhomov's `H_{<ω}` (2019) proves its own consistency while proving
the existence of every hereditarily finite set — so it is not growth-restricted
in Willard's sense — and satisfies the general condition by a different route:
every finite subtheory has a **finite model**, bounded by `2⁰_p` in the proof `p`.
Willard bounds a *partial valuation on a tableau branch*; Pakhomov bounds a
*full finite model of the axioms used*. See [`R1-review.md`](R1-review.md) §2.

The rest of this document argues that this is the right sentence: §§2–4 say why
the usual reading of Gödel's Second Theorem misidentifies the obstacle, §5
gives the criterion its sharp form, §6 says what it costs, and §7 shows the same
phenomenon outside arithmetic.

---

## 2. Where the breach goes

The standard reading of G2 is that self-reference is the problem: a system
strong enough to talk about itself can build "I am not provable", and that
sentence destroys any attempt at self-certification.

That reading is wrong, and Willard's systems are the counterexample. **They
contain the fixed point as an axiom and are consistent.**

### 2.1 The fixed point is free; the derivability conditions are not

Lawvere's Fixed Point Theorem makes the point structurally. In a cartesian
closed category, if there is a weakly point-surjective `f : A × A → B`, then
**every** endomorphism `t : B → B` has a fixed point. Cantor, Russell, Tarski,
Turing and Gödel's *First* theorem are the contrapositive: exhibit a
fixed-point-free `t`, conclude no such `f` exists. (Yanofsky's treatment, held
at `lit/…Working Category Theorist…pdf`, develops the family.)

What Lawvere gives is **existence**, and it costs almost no theory. Kleene's
recursion theorem is the arithmetic form, and Willard uses it directly: every
affirmative system in the corpus builds its Group-3 axiom by exactly this
construction (codified §5.1). Willard2001's own footnote makes the division
explicit — the fixed-point identity "is the **only** aspect of the proof of the
Hilbert-Bernays Theorem that needs Peano Arithmetic for justifying it", and
"the remainder of the proof rests solely on" the derivability conditions.

So the paradox is not in the diagonal. It is in what must hold *around* it.

### 2.2 Willard's own frame: the breach is obligatory, its location is not

`Willard1993-TR` states the governing fact three times, and it is the frame the
whole corpus is built in:

> "Gödel's Incompleteness Theorem requires that **every self verifying system
> must breach in some way one of the three fundamental Hilbert-Bernays
> conditions**. **The breach, which can be very subtle, may arise because of
> either the axiom system employed or the choice of deduction method, but not
> necessarily due to both.**"

`Willard2001` Appendix A sharpens the theorem being breached so that it applies
to systems far below PA — **Theorem A.1**: if `α` proves all of PA's `Π⁻₁`
theorems and `Der` satisfies

> **(1)** `α ⊢ Φ ⟹ α ⊢ Der(⌜Φ⌝)` · **(2)** `α ⊢ {Der(⌜Φ⌝) ∧ Der(⌜Φ ⊃ Ψ⌝)} ⊃ Der(⌜Ψ⌝)` · **(3)** `α ⊢ Der(⌜Φ⌝) ⊃ Der(⌜Der(⌜Φ⌝)⌝)`

then consistent `α` cannot prove `¬Der(⌜0=1⌝)`. Willard notes this "differs from
the classic Hilbert-Bernays theorem only by not requiring `α` to be an extension
of Peano Arithmetic", and that proving PA's `Π⁻₁` theorems "is actually the core
fact that is needed by the classic proof". `Willard2011` puts the consequence
plainly: "**Conventional** generic configurations `ξ` will satisfy the
Hilbert-Bernays derivability conditions… Their `G^ξ_k(θ)` will thus be
**automatically inconsistent**."

Willard's systems prove all of PA's `Π⁻₁` theorems. They therefore *must* breach
one of (1)–(3). **The research programme is the study of where to put the
breach**, and that is why its results trade design parameters against one
another rather than converging on a single system.

Willard contrasts his choice with Feferman's: `[Fe60]` "illustrates a
self-verifying system which employs **all the axioms of Peano Arithmetic**, but
which draws upon a **deduction method** which captures the numerical but not
intensional definition of classic deduction", whereas `IS(A)` keeps "a deduction
method as natural as semantic tableaux" and instead weakens "several of the
axioms of Peano Arithmetic, **including the axiom that the multiplication
function is total**". Same obligatory breach; opposite placement.

### 2.3 The two branches are two placements — an inference, with its evidence

Willard says the breach is one of the three; he does not, anywhere located in
this corpus, say **which**. The following assignment is therefore this
Refinement's inference, and it is stated as one. Its support is that each half
predicts a known negative result.

| Branch | Profile | Apparatus | Breached | Why |
| --- | --- | --- | --- | --- |
| **Tableaux line** (`IS^λ(A)`, `IS_D(A)`, …) | **Type-A** — successor **and addition total**; multiplication a 3-way relation | cut-free | **(2)** | Internalised modus ponens *is* cut. `Willard2002a` Thm 2.2: cut elimination guarantees the combined proof **exists** while its length "can certainly be **super-exponentially longer**". The system cannot assert the existence of a proof it cannot name |
| **Hilbert line** (`ISREF`, `ISCE`, `IQFS`) | **Type-NS** — nothing total | modus ponens intact | **(1)** | Condition (2) is unproblematic where proofs concatenate. But asserting `Der(⌜Φ⌝)` means asserting a proof **exists**, which means naming it — and with no totality axiom the naming convention is the only route to large integers |

**Each half is confirmed by the control that fails.** Restore cut to the
tableaux line — that is exactly what `Xtab` does, admitting excluded middle as
logical axioms — and condition (2) returns; `Willard2020` Theorem 4.5 says
`IS_Xtab(β)` is then **automatically inconsistent**. Make naming cheap on the
Hilbert line — the multiplicative convention — and condition (1) returns;
`Willard2006a` Theorem 4 says the system is then **unable** to prove its own
consistency.

This also corrects a claim it is easy to make and which an earlier draft of this
section made: **it is not true that Willard's systems deny successor totality.**
Only the Hilbert line does. The tableaux line proves successor *and* addition
total and gives up multiplication alone. Which totality is surrendered is one of
the design parameters, not a fixed feature of the programme.

### 2.4 What each condition licenses, and what contraction licenses

The three conditions are not three versions of one thing, and an earlier draft
of this section flattened them by calling (1) and (2) "permissions to use a proof
again". They are not. Taken one at a time:

| | Licenses | |
| --- | --- | --- |
| **(1)** | **internalisation**: from *having* a proof, assert that one exists | once |
| **(2)** | **composition**: combine proofs of `Φ` and `Φ ⊃ Ψ` into a proof of `Ψ` | two *different* proofs |
| **(3)** | **iteration**: from the assertion `Der(⌜Φ⌝)`, assert `Der(⌜Der(⌜Φ⌝)⌝)` | **the same proof, one level up** |

**Reuse is (3).** It is the condition that takes a statement already *about* a
proof and makes that statement itself a proved object — the proof is used again,
reflected upward. (1) internalises once; (2) composes two distinct derivations.

(3) is also the strongest of the three in a way worth recording: proving
`Der(⌜Der(⌜Φ⌝)⌝)` requires internalising (1), so **a system that fails (1) fails
(3) a fortiori**. §2.3's assignment is therefore a claim about which of the
*weaker* conditions already gives way — which is what the design parameter
controls — not a claim that (3) survives.

### 2.5 Contraction is a structural rule, not one of the three

It is tempting, and this Refinement earlier yielded to the temptation, to say
that Beklemishev–Shamkanov's `□`-contraction is one of the derivability
conditions restated. It is not, and their proof says where it is used.

Contraction is `Γ, φ, φ ⊢ ψ ⟹ Γ, φ ⊢ ψ` — a rule of the ambient consequence
relation about reusing a **hypothesis**, not about reusing a proof. In their
Proposition 3.8 it is consumed at one specific step, establishing their condition
**C3**:

> "By Lemma 3.3(i) we have: `φ, ¬φ, ⊤ ⊢ ⊥`. Hence, `φ, ¬φ ⊢ ⊤ → ⊥`, therefore
> `φ, ¬φ ⊢ ¬⊤` by Condition 1. **The rules of transitivity and contraction imply
> that, if `Γ ⊢ φ` and `Γ ⊢ ¬φ`, then `Γ ⊢ ¬⊤`.**"

So what contraction licenses is the step from *two derivations off the same
context* — one of `φ`, one of `¬φ` — to a single derivation of absurdity. Without
it, a context that proves both a formula and its negation is not thereby
inconsistent. That is more elementary than any of Löb's conditions and sits
underneath all of them.

The correct picture is therefore that Gödel's argument needs **three separate
things**, and the literature now contains a way of removing each:

| Ingredient | Removed by |
| --- | --- |
| the **fixed point** | nobody — Lawvere gives it away, and every system here keeps it |
| the **derivability conditions** | **Willard** — (2) on the tableaux line, (1) on the Hilbert line (§2.3); and **Pakhomov**, whose finite models block the argument semantically |
| the **structural licence** to use a context twice | **Beklemishev–Shamkanov** — contraction-free K4 |

These are three different steps of one argument, not one condition under three
descriptions. The unification claimed in an earlier draft was too strong, and
what survives it is weaker but still worth stating: **G2 is over-determined**,
and a system escapes by failing any one of its several independent
prerequisites — which is why the corpus reads as a study of design parameters
rather than a search for a single trick.

> **Refined reading of G2.** Gödel's Second Theorem is not a theorem about
> self-reference. It is a theorem about systems that meet *all* of its several
> prerequisites at once — and the corpus is the catalogue of ways to fail one.

## 3. What the cost actually is

Take a proof `p` of length `L`. For the system to certify "`p` is a proof", it
must exhibit an object that encodes the whole verification. The classical
bound (`Willard2007-APAL` §4) is that this object has Gödel number below
`ω₁^K(p)` for some fixed `K`, where `ω₁(x) = x^{Log(x)}` and `ω₁^K` is `K`
iterations of it.

So the system needs to **name a number substantially larger than `p`**. If it
can, the diagonalisation closes and G2 applies. If it cannot, the argument
stalls — not because the fixed point is absent, but because the step from the
fixed point to the contradiction cannot be taken inside the system.

Willard's whole corpus is the exploration of systems too weak to take that step
while still strong enough to be interesting.

---

## 4. Naming rates, and why exactly two of them matter

The corpus's single most repeated object (codified §5.3; ten occurrences catalogued
in [`../codification/concordance/replication-map.md`](../codification/concordance/replication-map.md) §1)
is a pair of sequences from the same start:

| | recurrence | value | **encoding length** |
| --- | --- | --- | --- |
| additive | `x_i = x_{i−1} + x_{i−1}` | `x_n = 2^{n+1}` | `Log(x_n) = n + 1` |
| multiplicative | `y_i = y_{i−1} · y_{i−1}` | `y_n = 2^{2^n}` | `Log(y_n) = 2^n` |

Both are built in `n` steps. The additive sequence names a number whose binary
encoding is `n + 1` bits — **linear in the work done**. The multiplicative
sequence names a number whose encoding is `2^n` bits — **exponential in the
work done**.

That second fact is the whole game. `Willard2009` Lemma 5 states it in the
cleanest available form: `n` rounds of tableau elimination build
`U₀ = 2`, `U_{i+1} = U_i · U_i` along one branch of a fragment with **`O(n)`
nodes**, establishing `U_n = 2^{2^n}`, "whose binary encoding has a `2^n`
length that is much larger than `F`'s length."

A short proof has named a long object. Once that is possible, the system can
assert — cheaply — the existence of something big enough to encode a refutation
of itself, and G2 bites.

**This is why the boundary sits where it does**, and Willard says so three
times, in three papers, each naming the specific lemma that dies when
multiplication stops being total (`Willard2000-TAB` §5, `Willard2002a` §7,
`Willard2004` p. 4; codified §7.2). The affirmative and negative halves of the
corpus are two readings of one observation: the same construction that lets a
system *be* self-justifying is what the incompleteness proofs *consume*.

---

## 5. The criterion

> **Refined criterion.** A system is self-justifying precisely when its naming
> is **non-compressive**: no proof of length `L` may denote an integer whose
> binary encoding exceeds `O(L)`.

Checked against the corpus:

| Naming convention | `n` steps reach | encoding | compressive? | verdict |
| --- | --- | --- | --- | --- |
| incremental `C_{i−1}+1` | `n` | `Log n` | no | works, but **not Continuously Expanding** — it reaches every integer, just too slowly for the `K_i` sequence `Willard2006a` p. 7 requires |
| **additive** `C_{i−1}+C_{i−1}` | `2^n` | `n` | **no** | **works** |
| Hybrid(H), `H = 1` | — | — | no | works |
| Hybrid(H), `H > 1` | — | — | yes | fails |
| multiplicative `C_{i−1}·C_{i−1}` | `2^{2^n}` | `2^n` | **yes** | fails |

`Willard2006a` pp. 7–8 gives the same ordering in his own vocabulary — the
incremental convention "grows too slowly" to be Continuously Expanding, the
multiplicative is too fast to be self-justifying, and the additive is
"simultaneously *sufficiently slow* … while also *sufficiently fast*". The
criterion above says what "sufficiently" means: **fast enough to enumerate ℕ,
slow enough that naming never outruns proving.**

Willard states the inequality exactly once, and it is the sharpest formulation
in the corpus. `Willard2011` Definition 4.5 calls a configuration **Tight**
when every inconsistent `β` above the base satisfies

> `Log(q_β) ≥ ♯(β) + 2`

— proof length at least envelope plus two. That is the criterion above, written
as arithmetic.

### 5.1 The five dials are one dial

The codification records five parameterisations of the boundary that no paper
relates to any other (drift **D38**; codified §8.1): `ℜ`, Level(n), Hybrid(H),
`Herb−k`, and `Z`. On the present reading they are one quantity measured five
ways — **how much compression the system is permitted**:

- **Hybrid(H)** measures it directly, in the naming rate.
- **`ℜ`** and **`Herb−k`** measure it as *how much cut* is allowed, and cut is
  proof compression (`Willard2002a` Theorem 2.2: cut elimination preserves
  theoremhood while the proof "can certainly be super-exponentially longer").
- **`Z`** measures it as *how much excluded middle* is admitted as a logical
  axiom — and LEM-as-axioms is a cut (codified §4.5).
- **Level(n)** measures it as *how much the consistency statement quantifies
  over*, which fixes how large a witness the statement can be asked about.

Each dial has a positive and a negative setting, and in every case the negative
setting is the one that lets a short derivation reach a long object. That the
five agree is the strongest evidence available that the criterion in §5 is the
right invariant.

---

## 6. The Rosser analogue

Rosser's improvement to Gödel replaces "I am not provable" with

> `∀y ( Prf(y, ⌜R⌝) → ∃z < y  Prf(z, ⌜¬R⌝) )`

— a sentence that does not assert unprovability absolutely, but **relative to an
ordering by proof size**. That single change buys the theorem for merely
consistent systems.

Willard's construction makes the same move, in the other direction.

**The sentence is bounded.** The working form of the diagonal throughout the
corpus is not `Prf` but `SemPrf^K_α(x, y, z)` — "`y` is a proof of `x` **and**
`y` is bounded by the `K`-fold iterated logarithm of `z`". The self-reference is
to the non-existence of a **short** proof, not of any proof.

The two definitions differ in strictness, which no paper notes: `Willard2000-TAB`
Definition 1 requires **`y < Log(z, K)`** and `Willard2007-APAL` Definition 2
requires **`y ≤ Log(z, K)`** (drift **D81**). Nothing in either argument turns on
it, but a refinement that unified them silently would be doing the thing this
project exists to avoid.

**The argument is a least-number principle on size.** Every consistency-
preservation proof in the corpus runs the same way: assume a proof `P` of `⊥`,
take the one minimising the Gödel number `Θ(P)`, and construct from it a
smaller `R` with `Θ(R) < B < Θ(P)`, contradicting minimality (`Willard2009`
Theorem 4; `Willard2001` Theorem 3.4; `Willard2005` Theorem 1).

So:

> **Rosser bounds the diagonal by comparison; Willard bounds it by magnitude.**
> Both replace an absolute self-referential claim with a size-relative one.
> Rosser buys G2 for consistent systems. Willard buys self-justification for
> systems that cannot count high enough to refute themselves.

The recorded hook asked whether the consistency-preservation results could be
*derived from* a Rosser analogue. The honest answer from the codified material
is that they already **are** one — the trick is present, undeclared, in the
`SemPrf^K` predicate and in the minimal-`Θ` argument. What the Refinement adds
is the observation that this is what they are.

---

## 7. What the idea costs

A refinement that stated only the mechanism would be a misrepresentation. Four
things are given up, and Willard says all four himself (codified §1.4).

1. **Ordinary arithmetic is gone.** Every affirmative system is Type-A (no
   multiplication as a total function) or Type-NS (nothing total). This is not
   a technicality: `Willard2005` Remark 4 shows the engine is *impossible* once
   multiplication is admitted, "under any possible deduction method `D`,
   whether cut-free or otherwise."
2. **The proof is one line.** `Willard2011` Remark 6.16b: self-justification
   from a SelfCons axiom yields "essentially a **1-line proof**" — instinctive
   faith, not proof-justification.
3. **The programme's own verdict.** `Willard2016` §9: the evasions rest on
   arithmetics "**weaker than traditional arithmetics**", and G2's refutation of
   Hilbert's original objectives is "simply, **undeniable**."
4. **The objection stands.** `Willard2016` §8 asks whether this is "**almost
   cheating**", answering only that a thinker needs no more than *Platonic
   Stability* — a formalism in which presuming one's own consistency provably
   will not spin one into inconsistency.

5. **The axioms are not natural.** Pakhomov's assessment, from outside the
   corpus: Willard's theories "are not completely natural in the sense that
   **some of axioms are constructed using Diagonal Lemma**". His `H_{<ω}` is
   offered explicitly as "a more natural example of this kind". That is a fair
   charge — the Group-3 axiom *is* a fixed point, by construction (§2) — and it
   is the sharpest external criticism of the programme on record.

The refined claim is therefore **not** that Hilbert's programme survives. It is
that the reason it fails is narrower and more interesting than "self-reference
is paradoxical": it fails because **checking your own work costs more than
doing it**, and any system cheap enough to escape that cost is too weak to be
the arithmetic Hilbert wanted.

---

## 8. The same phenomenon outside arithmetic

The synthesis names computational analogues as the near-term purpose of this
work. The criterion of §5 transfers directly, and there is a worked instance in
the held literature.

**Self-interpreters for total languages.** Folklore holds that a strongly
normalizing language cannot have a self-interpreter: a total `eval` of the
obvious type permits a diagonal argument. Brown and Palsberg
(`lit/brownpalsberg2016self-interpreter-f-omega.pdf`, and the System U and
typed-self-evaluation papers alongside it) nevertheless construct one for
**F_ω**.

The mechanism is the same as Willard's. They do not block the diagonal — the
self-representation exists and is total. They arrange that the **type** of the
representation does not permit the composite the paradox needs. The fixed point
survives; the derivation does not.

In the vocabulary of §2: Lawvere's existence half is kept, and the
derivability-condition half is denied — there by typing discipline, in Willard
by growth restriction. Both are instances of

> keep the self-reference; make the *use* of it unaffordable.

That **analogy** — it is not yet a correspondence, since no formal mapping has
been established — is the concrete form of the "computational analogue" the
synthesis is aiming at, and it suggests the transfer to test first: **the
naming-rate criterion of §5 should have a type-theoretic reading, in which
"non-compressive naming" becomes a bound on the size of the type a term can
denote relative to the term's own size.** That is a conjecture of this
Refinement, not a result.

---

## 9. What is not settled

**The Beklemishev comparison is blocked.** The synthesis takes its inspiration
from "Lev Beklemishev's claim of a simplified presentation of SJAS". That
survey is **gap G7**, `refinement-prep`, and is not held. Everything above is an
independent identification of the essential idea; whether it *is* Beklemishev's
simplification, refines it, or misses it, cannot be determined without the
paper. Acquiring it is the first action of any continuation.

**The Lawvere framing is a framing, not a theorem.** §2 uses Lawvere to
separate *existence of a fixed point* from *derivability of its consequences*,
which is correct and clarifying. It does not give a categorical proof of any
Willard result, and the claim that resource-bounded internal homs model the
SJAS situation is a research direction, not a construction.

**The criterion of §5 is stated, not proved.** It is an invariant read off five
independently-parameterised boundaries plus one explicit inequality
(`Willard2011` Def. 4.5). Turning "non-compressive naming" into a definition
general enough to prove the five dials are instances of it is the obvious next
piece of work, and would be the Refinement's first genuinely new theorem.

**Two corpus results the refinement leans on are `sketch`.**
`Willard2004` Theorems 2 and 3 (gap **G35**, permanently sketch) supply two of
the five dials' settings, and `Willard2016` Theorem 6.7 is conditional on
Conjecture 6.6. The §5.1 claim that the dials agree is therefore as strong as
those statuses allow, and no stronger.
