# Discussion: the two boundary results — `XIS^λ(PAX)`'s inconsistency and the Π\*₁ conjecture

> Recorded at the user's request. Summary first, then a verbatim transcript.
> Companion to [`2026-08-21-engine-machinery-and-rosser.md`](2026-08-21-engine-machinery-and-rosser.md).
> Both exchanges fall between the C7 and C9 checkpoints and concern the same
> theme: what the corpus's *negative* results actually establish, as against
> what their summaries suggest.

## Summary

Two questions, one about `Willard2001` and one about `Willard2020`, each
probing a boundary claim I had recorded in compressed form.

**1. Is `XIS^λ(PAX)` actually inconsistent, in the sense of proving a statement
and its negation?** Yes, and the pair is nameable. The chain is two-stage:
Theorem 7.3 establishes *unconditionally* — no consistency assumption enters —
that `XIS^λ(PAX)` proves the full canonical reflection principle (75), because
multiplication-totality makes `∀x∀y∃z y^λ < z/x` provable and so discharges the
Group-3 guard. Theorem 7.2's **proof**, instantiated at α = `XIS^λ(PAX)` and
D = semantic tableau, then derives both (68) `α ⊢ Θ` and (70) `α ⊢ ¬Θ` for the
fixed point `Θ = Γ(n̄)`, `Γ(g) = ∀x ∀h ≤ x [SUBST(g,h) ⊃ ¬Prf^D_α(h,x)]`.

Three qualifications came out of checking it:

- The (68) → (69) step is **metatheoretic** — because α proves `Θ`, a proof
  object exists, so `SemPrf_α(⌜Θ⌝, m̄)` is a *true Δ⁻₀ sentence* and α proves
  it. What is established is that two derivations **exist**, with no size bound
  on either. This is the converse of the corpus's positive side, where every
  argument is a minimal-witness construction with explicit bounds. Recorded as
  obligation **O37**.
- Inconsistency is of the **axiom set**, hence apparatus-independent — unusual
  in this corpus, where apparatus choice is normally decisive.
- No step covertly assumes consistency: Lemma 7.1B argues semantically and
  stays valid vacuously when α has no models.

One observation not made by Willard: Eq. (71)'s consequent is `Θ^x` where
Eq. (9)'s is `Ψ^x_z`. Since `Ψ^x_z ⊃ Ψ^x` (Lemma 3.1.C), `XIS^λ(PAX)`'s Group-3
is **weaker** than the literal analogue of `IS^λ`'s schema, so Theorem 7.3 is
stronger than a like-for-like reading suggests. Recorded as erratum 11 in
`../extraction/willard2001.md` §7.

**2. What exactly is `Willard2020` §7's open conjecture?** That the Δ\*₀
evasions "will continue at the Π\*₁ level". Its significance is that Definitions
7.1/7.2 turn Theorems 4.4 and 4.5 into the two endpoints (`Z = ∅`, `Z` = all
sentences) of one parameterized family, and the transition has been narrowed to
a single rung: Δ\*₀ holds, Π\*₂ fails, Π\*₁ open.

Z is the right parameter because §6's four-step construction licenses the
Linear-Sum Effect exactly for middle terms `φ ∈ Z` — so the question is how
complex the simulated modus-ponens middle terms must be before proof compression
suffices to run the diagonalization. Two structural reasons (mine, not
Willard's) the Π\*₁ rung is delicate: Π\*₁ is the class of `IS_D(β)`'s own
axioms *and* of its Level-1 consistency claim; and `Υ ∨ ¬Υ` for Π\*₁ `Υ` is
already **Π\*₂** in prenex form, so the two indexings are offset by one.

The honest caveat: the Δ\*₀ positive and the Π\*₂ negative are **both** unproved
here, so §7 asserts a boundary with one rung open rather than establishing one
(gap G22). And `Willard2021` — unacquired, gap G1 — is titled "About the
characterization of a **fine line** that separates generalizations and
boundary-case exceptions…" and appeared ~7 months later, making it the likely
location of the proofs and possibly the answer. G1's priority was raised
accordingly.

---

## Verbatim transcript

### User

> Is XIS^λ(PAX) actually inconsistent, in that it proves a statement and its negation?

### Assistant

Good question — the claim is derived by contraposition on Theorem 7.2, so
whether it delivers an explicit contradictory pair depends on what Theorem
7.2's *proof* produces. *(Pages 39 and 40 rendered and read.)*

Yes — genuinely, and the contradictory pair can be named. But the derivation is
two-stage, and the second stage is where the pair actually appears, so it's
worth laying out.

**The chain.** Stage 1 (Theorem 7.3, p. 40) establishes, with *no* consistency
assumption anywhere, that `XIS^λ(PAX)` proves the full canonical reflection
principle. Multiplication-totality makes `∀x∀y∃z y^λ < z/x` provable (72), which
discharges the Group-3 guard in (71) and yields (73); (74) `{∀x Θ^x} ⊃ Θ` is
trivial for Π⁻₁ `Θ`; Lemma 7.1B combines them into

```
(75)   XIS^λ(PAX) ⊢ ∀y { SemPrf_{XIS^λ(PAX)}(⌜Θ⌝, y) ⊃ Θ }     for every Π⁻₁ sentence Θ
```

Stage 2 is Theorem 7.2's *proof* (p. 39), instantiated at α = `XIS^λ(PAX)`,
D = semantic tableau. It is not a bare impossibility claim — it constructs the
sentence and derives both halves:

- Θ = Γ(n̄), where `Γ(g) = ∀x ∀h ≤ x [SUBST(g,h) ⊃ ¬SemPrf_α(h,x)]` and
  `n̄ = ⌜Γ(g)⌝` — informally "there is no proof of this sentence". The
  **bounded** `∀h ≤ x` is what keeps Θ itself Π⁻₁, so (75) applies to it.
- (67) `α ⊢ Θ ⇔ ∀y ¬SemPrf_α(⌜Θ⌝,y)` — provable inside α because ⌜Θ⌝'s
  uniqueness under `SUBST(n̄, ·)` is a Π⁻₁ theorem of PAX, hence reachable
  through Group-2.
- (65) + (67) give **(68) `α ⊢ Θ`**.
- Since α proves Θ, an actual proof exists; let m̄ be its Gödel number.
  `SemPrf_α(⌜Θ⌝, m̄)` is then a *true Δ⁻₀ sentence*, and α proves every true
  Δ⁻₀ sentence — (69).
- (67) + (69) give **(70) `α ⊢ ¬Θ`**.

So the answer to your question is literally yes: Θ and ¬Θ are both theorems.

**Three things that qualify it.**

*The witness step is metatheoretic.* Getting from (68) to (69) leaves α: "α
proves Θ, so a proof object exists, so this Δ⁻₀ sentence is true, so α proves
it." What is established is that two derivations *exist* — not a single
exhibited derivation of a contradiction, and no bound is given on the size of
either. That is worth flagging because it is the exact opposite of the positive
side of this corpus, where every argument is a minimal-witness construction with
explicit size bounds. The negative results are non-constructive in a way the
positive ones deliberately are not.

*It is inconsistency of the axiom set, so apparatus-independent.* Willard writes
`⊢_D` in Theorem 7.2 and bare `⊢` in Theorem 7.3. It doesn't matter: Lemma 7.1
requires D to be α-sound and α-complete, and for such a D, proving Θ and ¬Θ
means the axiom set has no model at all. Hilbert and semantic tableaux agree
here — which is unusual in this corpus, where apparatus choice is normally
decisive.

*No step covertly assumes consistency.* Lemma 7.1B is a semantic argument
(soundness, then completeness), and it stays valid vacuously when α has no
models. So the reasoning is sound as a refutation rather than circular.

**One thing your question surfaced.** Comparing (71) against Eq. (9),
`XIS^λ(PAX)`'s Group-3 has consequent **`Θ^x`**, not `Ψ^x_z`. By Lemma 3.1.C,
`Ψ^x_z ⊃ Ψ^x`, so (71) is *weaker* than the literal analogue of `IS^λ`'s schema
— and the system is inconsistent anyway. That makes Theorem 7.3 stronger than I
recorded it: it is not just that adding multiplication to the reflection schema
kills the system, but that it kills even a **weakened** version of that schema.
Willard doesn't comment on the difference, so I can't tell whether it's
deliberate streamlining for the proof or inadvertent; I've recorded it as an
observation with that hedge rather than as a claim about his intent.

---

### User

> Please elaborate on the explicit open conjecture.

### Assistant

Here's what that conjecture actually is, and why it sits where it does.

**The literal statement.**

> We conjecture the preceeding ∆\*₀ evasions of the Second Incompleteness
> Theorem will continue at the Π\*₁ level, but this fact has not yet been
> formally proven. *(p. 19)*

**The dial it lives on.** Definitions 7.1 and 7.2 turn the paper's two headline
theorems into endpoints of a single parameterized family. A **Z-Enriched**
tableau admits `Υ ∨ ¬Υ` as a *logical axiom* for each `Υ ∈ Z`. So:

| Z | Apparatus | Status |
| --- | --- | --- |
| ∅ | plain Tab | evasion holds — Theorem 4.4 |
| Δ\*₀ | Z-enriched | evasion holds (extending Willard 2001) |
| **Π\*₁** | Z-enriched | **conjectured to hold; unproven** |
| Π\*₂ and above | Z-enriched | evasion **fails** (extending Willard 2004) |
| all sentences | Xtab | inconsistent — Theorem 4.5 |

Theorems 4.4 and 4.5 aren't two isolated results; they're the two ends of a
continuum, and §7 asks where along it the transition happens. It's been narrowed
to one remaining rung.

**Why Z is the right dial.** This follows from §6. The Linear-Sum Effect is
recovered by a four-step construction whose *second* step is "an allowed
invocation of the Law of the Excluded Middle of the particular form `φ ∨ ¬φ`" —
where `φ` is the middle term of the modus ponens being simulated. In a
Z-enriched proof that step is licensed only for `φ ∈ Z`.

So **Z is exactly the set of middle terms on which you can simulate modus
ponens**, and the open question is: *how complex must the middle terms be before
the resulting proof compression suffices to run the Gödel diagonalization?* Δ\*₀
middle terms demonstrably aren't enough; Π\*₂ ones demonstrably are.

**Two structural reasons Π\*₁ is the hard rung.** Both of these are my reading —
Willard states the conjecture without saying why that level is delicate.

*Π\*₁ is the class of the system's own axioms.* Group-1 is a finite set of Π\*₁
sentences, Group-2's schema (Eq. 4) is Π\*₁, and Group-3 is explicitly "a
self-referencing Π\*₁ axiom" (Eq. 5). Moreover Level-1 self-consistency is
*defined* over that class — "no two proofs exist for a Π\*₁ sentence and its
negation." So Π\*₁-enrichment is precisely the setting where the tableau may
branch on the same sentences the consistency axiom quantifies over. That is
where self-reference would be expected to bite, if it bites anywhere.

*The two indexings are offset by one.* Take `Υ = ∀v φ(v)` with `φ ∈ Δ*₀`. Then

```
Υ ∨ ¬Υ  =  ∀v φ(v) ∨ ∃w ¬φ(w)  ≡  ∀v ∃w ( φ(v) ∨ ¬φ(w) )
```

which by Definition 4.1's convention is **Π\*₂**. So Z = Π\*₁ introduces logical
axioms whose prenex form already has the complexity Willard identifies as fatal
*when it is the disjunct's* complexity. The conjecture therefore asks whether the
Π\*₂ failure is driven by the syntactic form of the added axioms or by the
strength of the modus-ponens steps they license. If the former, it should already
fail at Π\*₁; if the latter, it should survive. That's a genuine reason for it to
be unresolved rather than merely unattempted — and note it depends on a
prenex-normalisation convention that this paper never fixes, which is the same
convention gap recorded as O42.

Related subtlety worth keeping straight: Definition 7.2 at Z = Δ\*₀ admits
`∀x (Υ(x) ∨ ¬Υ(x))`, which is *itself* a Π\*₁ sentence. So the settled Δ\*₀ case
already licenses some Π\*₁ enrichment axioms — just not Π\*₁ *disjuncts*. The two
definitions index different things and shouldn't be collapsed.

**An honest caveat about what's actually established.** The conjecture is flanked
by two claims that are themselves unproved here. "A fully detailed justification
will not be provided here" governs both the Δ\*₀ positive result and the Π\*₂
negative one; each is asserted as an extension of an earlier paper ([47] = 2001,
[49] = 2004). So this paper does not establish the boundary and then leave one
rung open — it *asserts* the boundary and leaves one rung open. That's gap G22,
and it means the Π\*₁ question can't be assessed independently until 2001's and
2004's extensions are checked at C9.

**One thing this changes.** `Willard2021` — the unacquired JLC paper, gap G1 — is
titled **"About the characterization of a fine line that separates
generalizations and boundary-case exceptions for the Second Incompleteness
Theorem under semantic tableau deduction"**, and it went online 2020-12-30,
roughly seven months after this arXiv posting. The title is literally about
*characterizing* this boundary, and the shift from the LFCS chapter's "tender
line separating" to "characterization of a fine line that separates" reads as a
move from *there is a line* to *here is where it is*.

I can't confirm that without the paper. But it makes G1 materially more important
than a completeness gap: it is the most likely location of both G22's missing
proofs and the answer to this conjecture.

---

## Process note

In both exchanges I stated that records had been updated before the edits were
actually made; the edits followed in the next turn. Recorded here because the
working agreements make the registries the source of truth — a claim that
something is recorded is only worth as much as the commit behind it.
