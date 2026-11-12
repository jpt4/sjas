# The essence of self-justification, at Lawvere's level of generality

Date: 2026-10-28

> Revised 2026-10-29 in the light of Gross, Gallagher and Fallenstein's Löb
> paper: see [the revision](2026-10-29-essence-of-self-justification-after-lob.md).
> The theorem below is the arithmetic instance of the revision's
> guarded-explanation theorem. The corollary's type-theoretic form is now
> checked in Lean, and its arithmetic form still is not.

## Request (verbatim)

> In your own words, try to articulate the essence of self-justification, in
> a way as generally applicable as Lawvere's Fixed Point Theorem.

## Sources consulted

- `nachlass/codification/codified-sjas.md` (branch `sjas-codification`):
  §2.2 (Definition 3.4), §2.4 (Solovay's two branches), §5.3 (growth
  restriction), §6.2 (the envelope, "Tight"), §6.7 (Remark 4 and its
  dependence on `Willard2002a` Thm 6.4), §6.9 (minimal-counterexample method).
- `nachlass/codification/extraction/willard2001.md`: Lemmas 4.6–4.8 and
  Theorem 4.3, with the six-bit density argument (Case 11, p. 28).
- `nachlass/codification/registry/results.md`: statuses of `Willard2001`
  Thm 4.3, `Willard2005` Thms 1–5, `Willard2002a` Thm 6.4.
- `prose/self-justification-ex-machina/self-justification-ex-machina.tex`
  (branch `sjexm-essay`): "Why the genius survives", "Transcripts Are Bigger
  Than Runs", "The Price Of Faith", "Refactoring The Incompleteness Theorems,
  Again".
- `nachlass/codification/discussion/2026-08-21-engine-machinery-and-rosser.md`
  (the Rosser comparison and obligation O26).
- [2026-10-06 λᶜᵉʳᵗ note](2026-10-06-artemov-lp-and-lambda-cert.md) and
  [2026-10-27 Chaitin note](2026-10-27-chaitin-and-self-justification.md).

No page images were consulted for this note; every Willard claim below is
taken from the codification, which was image-verified.

## The articulation

**Self-justification is infinite descent, with the trust axiom as the step.**

Give a system true axioms plus T, "nothing refutes me". A sound refutation
has to expose something false, and T is the only candidate; exposing T's
falsity means exhibiting a refutation. If no proof can see past itself, the
refutation it exhibits is smaller than itself. So every refutation yields a
smaller one, and there are none. Naming itself costs the system nothing (that
part is Lawvere's). What it must lack is any way to see past its own proofs.

## The theorem

Grade truth along a well-founded order of sizes. In arithmetic, *σ holds below
n* when σ's unbounded universals hold for all values below n and its unbounded
existentials have witnesses below n (Willard's "true up to" and "witnessed
below"; bounded quantifiers are left alone). Proofs have sizes in the same
order: Gödel numbers, budgets, steps. Call a configuration (language, deduction
method, encoding) **guarded** when a proof cannot refute any set of universal
axioms that all hold below it.

> **Theorem.** Let S be true universal axioms plus a fixed point T saying
> "nothing refutes S", so that every counterexample to T is a refutation. If
> the configuration is guarded, S is consistent and T is true.
>
> *Proof.* Take a least refutation r. The true axioms hold below r. T holds
> below r too: a counterexample there would be a smaller refutation. So r
> refutes axioms that all hold below it, which guardedness forbids. ∎

As with Lawvere's theorem, the proof is three lines and the content is in the
hypothesis. Willard's consistency proofs are this argument with the arithmetic
left in:

- `Willard2001` Theorem 4.3. With y the least tableau proof of 0 = 1, Group-3
  holds ⇓(y−1), and Lemmas 4.6–4.8 are the guard.
- `Willard2005` Theorem 1. The guard is θ-compactification (Definition 5) plus
  the collision (m−1)/m^{1/4} > m^{1/4}; Theorems 2 and 4 establish it for
  tableaux and tableau lists.

## Where Lawvere and Gödel come in

Lawvere's theorem is about **names**: if codes name every way of reading codes,
every map on values has a fixed point. Copying and evaluation are free in its
setting, so it never sees evidence or its size. Willard's systems satisfy it;
T is such a fixed point.

G2 needs a second diagonal, on **evidence**: from a variable proof p, the
system must build the proof that checks p. That proof is bigger than p, since
transcripts are bigger than runs, so building it for an arbitrary p means
reasoning about a proof as large as the one doing the reasoning. The two
diagonals are independent: the Chaitin note found that the Kritchman–Raz route
to G2 drops the first and keeps the second.

The theorem makes the absence of the second forced rather than incidental:

> **Corollary.** In a guarded system containing T whose base proves every true
> Σ₁ sentence, D1 holds as a rule and the fixed point is free. So D2 and D3
> cannot both hold internally — Löb's derivation would make S inconsistent —
> and one of them fails at an instance about the Gödel sentence.

So the failures already on record are consequences of the guard, not accidents
of particular systems: IS(A) proves modus-ponens closure only "in the
degenerate case where x and y are fixed constants" (TR 93-10, p. 12), and the
essay's 7b holds only at true Σ₁ sentences.

In modal terms: G2 is Löb's rule for the system's own provability, run inside
the system; the theorem is Löb's rule for "earlier" (well-founded induction),
run outside it. The guard keeps provability, as any single proof can use it,
no stronger than "earlier".

## What the guard is, in arithmetic

An exchange rate in bits: what a proof step adds to the values the proof can
reach, against what the encoding charges to write that step down. A step adds
at most one bit, because it at most doubles. Willard's arguments need a fixed
number of bits per doubling (six in 2001; more than four in 2005), and the
encodings pay more ("substantially more than six" in 2001; more than five in
2005).

The guard fails wherever reach can be had without paying for it, and each such
place is one of the coordinates every claim must name:

- **base**: a true existential with a huge witness gives short proofs reach
  they never paid for (the essay's One Big Number);
- **language**: x·x doubles bit-length in one step (Squaring);
- **deduction method**: cut reuses a lemma's reach without paying for it again
  (Subroutines);
- **encoding**: a notation spending four or fewer bits per function symbol
  breaks 2005's argument.

The other two coordinates set the terms of the guard rather than the rate. The
**consistency level** fixes which refutations must need room. The **proof
predicate** is where a guard can be faked: Feferman's admission condition
writes it into the predicate, which is consistent but is the intensionally
incorrect route Willard sets aside. So claim granularity isn't bookkeeping. It
lists every place the guard can break.

## Status

- **Theorem and corollary.** Mine, with the proofs above. They hold relative to
  a metatheory in which sizes are well-founded and the base is true. Not
  machine-checked. The corollary inherits Löb's hypotheses: D1 as a rule, and D2
  and D3 at the instances Löb's derivation uses.
- **The hypotheses in Willard's systems.** These rest on Willard's own proofs,
  all `full` (complete proof printed): `Willard2001` Thm 4.3; `Willard2005`
  Thms 1, 2 and 4.
- **Open: the converse.** It says every self-justifying configuration with an
  honest proof predicate is consistent because of a guard. The corpus's nearest
  claim is `Willard2005` Remark 4's "under any possible deduction method",
  which rests on `Willard2002a` Thm 6.4, a `sketch`.

## For the builder

Self-reference, a checker, a true base and T are all free. The one thing to
build is the guard: no axiom, constructor, rule or encoding may create more
reach than it costs, checked locally. λᶜᵉʳᵗ₀'s `reflect` runs a certificate only
at a budget below the certificate's size, so it is this inequality in a
calculus. The corollary is the general form of the λᶜᵉʳᵗ₀ draft's informal
argument that free `·` and `!` cannot coexist with uniform reflection. Its
recommended "no certified Ω" theorem says exactly that the evidence diagonal is
blocked.

## Possible uses

- SJexM: the "Refactoring" section already has "self-reference is not where
  the trouble lies". The theorem and corollary could give that sentence a
  statement and a proof.
- λᶜᵉʳᵗ₀: the corollary could replace the draft's informal §8.3 argument,
  once Löb's derivation is checked to be expressible in the calculus.
