# Extraction: Willard1993

> Origin extraction (component C5), companion to
> [`willard1993-tr.md`](willard1993-tr.md). This record is **delta-focused**:
> the full-length original is the TR, and the material shared with it is
> recorded there. What follows is this witness's own identity, what it states
> versus proves, and where it differs from the TR. Anchors are the printed
> LNCS page numbers (325–336); PDF page *n* = LNCS page 324 + *n*.
> All content below was read from the page images.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard1993` |
| Title | Self-Verifying Axiom Systems |
| Venue | Computational Logic and Proof Theory: Third Kurt Gödel Colloquium (KGC 3), Springer LNCS 713 (1993), pp. 325–336 |
| Presented | 25 August 1993 (per the TR's preface page) |
| Witness | `nachlass/papers/willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf` → `lit/danwillard1993.pdf` (12 pp.) |
| Text aid | `../sources-text/willard1993.txt` (6,038 words) |
| Funding | NSF 9060509 |
| Structure | §1 Introduction (325); §2 General Perspectives (326–330); §3 Formal Summary of Main Results (330); §4 Intuition Behind Main Theorem (331); §5 Details of Main Proof (332–334); §6 Philosophical Implications and Main Conjecture (334–335); Acknowledgements (335); References (336) |

## 2. Role in corpus

The **published** origin: an "Extended Abstract" (its own term, p. 330) of the
TR. Its abstract states the programme in one sentence: "We introduce a class of
First Order axiom systems which can *simultaneously* verify their own
consistency and prove more Π₁ theorems than Peano Arithmetic. Despite these
strengths, our axiom systems do not violate Gödel's Incompleteness Theorem
because they treat multiplication as a partial function."

Willard's positioning (p. 325): "Our axiom system is the first example of a
First Order self-verifying system that can prove the Π₁ theorems of Peano
Arithmetic", distinguished from Kreisel–Takeuti [Ta53, KT74], whose second-order
cut-free `CFA` route "requires no translation into Second Order logic" in his
case and "instead treats multiplication as a partial function". He also
excludes the Feferman [Fe60] escape route explicitly: there exist "unnatural
deduction methods (which are *numerically correct* but *intensionally
incorrect*) that support very strong self-verification… Our research studies a
very different type of problem, where the rule of inference is required to be
an *intensionally correct*, very natural method."

`Willard2001` reference [67] describes this witness as summarizing "the
intuition behind the IS(A) axiom system, without formal proofs" — accurate for
Propositions 2–4 but not for Proposition 1, which is proved here in §5.

## 3. What this witness states versus proves

| Item | Page | Status **here** | Status in the TR |
| --- | --- | --- | --- |
| Proposition 1 (`IS(A)` consistent for nice `A`) | 330 | **Proved** (§5, via Lemmas 1–2) | proved (§6) |
| Proposition 2 (reflection for Δ₀ and decidable Π₁) | 330 | **stated-only** — "Three generalizations of Proposition 1 that are not proven in this 12-page Extended Abstract" | proved (§7); finer-grained, three clauses (a)(b)(c) |
| Proposition 3 (`IS*(A)` consistent) | 330 | **stated-only** | proved (§8), named `IS^{Σ₁}(A)` |
| Proposition 4 (`ISVALID`; `FORBIDDEN(A)` inconsistent) | 330 | **stated-only** | proved (§9) |
| Lemma 1 (5.1 not a subcomponent of Group-1/2 axioms) | 333 | **Proved** | = Lemma 6.1 |
| Lemma 2 (inconsistency proof must construct witness `p*`) | 333 | **Proved** | = Lemma 6.2 |
| Main Conjecture (`IS₊`, `IS*₊` self-verifying) | 335 | **conjecture** | Propositions 5–7 and Solovay's Theorem carry the corresponding negative results |
| Propositions 5–9, Remarks 1–7, Appendix A | — | **absent** | present |

Additional Proposition-3 note recorded here and not in the chapter's siblings:
"It is also impossible to strengthen this result because `IS(A)` becomes
inconsistent when it includes Π₁ Reflection" (p. 330) — the chapter's
one-sentence form of the TR's Proposition 5.

## 4. The Main Conjecture — and its later refutation

§6 (pp. 334–335) defines `IS₊(A)` and `IS*₊(A)` as "the natural generalizations
of `IS(A)` and `IS*(A)` that **permit cuts**", and states:

> A central open question is whether or not these two systems also satisfy part
> (ii) of the definition of self-verification. **We conjecture that the answer
> to both open questions is "yes"** because it would be hard to visualize
> otherwise how humans can instinctively recognize their own self-correctness.

Willard notes the consequence: "if this conjecture is correct, then `IS(A)`
must be substantially different from [Ta53, KT74]'s *CFA* system, since Kreisel
and Takeuti have shown the latter supports self-verification *only when cuts
are absent*!"

**The conjecture is false as stated, and Willard's own later work refutes it.**
The TR already contains the counter-machinery — Proposition 7 gives consistency
with cuts only after **dropping addition-totality**, and Solovay's Theorem
kills the successor-carrying case. `Willard2004` then shows no consistent
system recognizing addition as total can verify its consistency under
Tab−Σ\*₂ or Tab−Π\*₂ deduction, and `Willard2005` Theorem 5 recovers only a
**Level-1** restricted modus ponens, proven near-maximal. The mature answer:
cuts cannot be admitted freely; a strictly limited modus ponens can.

He also anticipates the alternative outcome (p. 335): that no set-theoretic
proof of Proposition 1's analogue exists for `IS₊(A)` "although these systems
are in fact self-verifying", in which case they "could explain how human beings
can stubbornly recognize… their own consistency, but yet they may be eternally
unable to ever formally justify this necessary epistemological assumption."

**Composition obligation O8**: the codified statement must record this arc —
the origin paper's central conjecture, its refutation, and the precise sense in
which the mature result is the conjecture's surviving fragment.

## 5. Passages of independent value

These appear in this witness in a sharper form than anywhere else in the
corpus, and the codified statement should draw its motivation layer from them.

**5.1 The growth argument, informally (p. 329).** The philosopher's answer —
"multiplication is a function in theory but not always in practice" —
formalized as a bit-counting contrast: for `b_{i+1} = b_i + b_i`, proofs of
`b_n`'s existence require *more* bits than `b_n`'s binary encoding, so "there
will always be adequate memory to write down the binary representation of `b_n`
when there is sufficient memory for storing the proof of its existence
(whereas the same is plainly not true when multiplication is assumed to be a
function, because then `a_n`'s existence proof is exponentially shorter than
its binary encoding!)". Illustration: "multiplication allows one to prove the
existence of a number `a_400` within the 400-line length of this paper, whose
binary encoding requires more digits than the number of atoms in the universe.
In sharp contrast, even the full cardinality of the universe *is insufficient*
for the Addition Axioms to prove the existence of a number whose bit length is
as large as the universe!"

This is the informal ancestor of `Willard2005` Definition 5 (θ-compactification)
and Eq. (20): proof size versus reachable magnitude, on one scale.

**5.2 Variable duplication as the mechanism (p. 328).** The finite-state-machine
reading: for a `b`-bit machine, `∀x∀y∃z : z = x+y` (2.3) is acceptable because
"since the symbol '+' must require at least one bit of storage, the variables
`x` and `y` must require fewer than `b−1` bits, implying there is sufficient
space in a `b`-bit memory to store their sum `z`". But `∀x∃z : z = x*x` (2.4)
fails: "if `x = 2^{3b/4}`, the right hand side of (2.4) can be stored in fewer
than `b` bits of memory while `z > 2^b` can not be!" Willard then draws the
diagonalization connection explicitly:

> At first, it may appear that this counterexample is artificial because it
> requires at least **two occurrences of the same variable `x`** on the right
> sides of (2.4). However, **the proof of Gödel's Incompleteness Theorem used
> essentially the same double appearance of a variable** when it provided a
> counterexample to self-verification via a diagonalization argument.

**Provenance note.** The affine-tree design (`docs/design/affine-tree-sjas.md`
§4, and ADR-0001's "Decisive Finding") identifies the addition/multiplication
boundary with an **affinity/duplication** boundary — a variable occurring twice
in a term — and presents this as a finding of its tree transposition. The
identification is Willard's own, stated here in 1993 in the arithmetic setting,
and the affine-tree documents do not cite it (they cite 2005 and 2001/2020).
The insight is therefore *corroborated* rather than novel, which strengthens
it; the codified statement should attribute it to this passage. Recorded as
drift **D13** and composition obligation **O9**.

**5.3 The 1993 proof skeleton (pp. 331–334).** Proposition 1's proof, whose
lineage into 2005 is direct:

1. let `p` be the **minimal** integer encoding an inconsistency proof of `IS(A)`;
2. Lemma 1: the inconsistency assertion (5.1) is not a subcomponent of any
   Group-1 or Group-2 axiom;
3. Lemma 2: therefore `p` must construct **within itself** a witness `p*` with
   a node `¬Prf_{IS(A)}(⊥, p*)`; by minimality `p* ≥ p`;
4. `INT` interprets the branch's parameter symbols; `β` is s-consistent to
   depth `d−1`;
5. bit-counting `m̄_max < p·2^{−j}` (5.2) and the doubling bound
   **`INT(c_j) ≤ 2^j · m̄_max`** (5.3) — the latter "rests on the fact that
   **Addition is the only increasing function** defined by `IS(A)`… if `c_i` is
   the largest constant before the addition function defines a new parameter
   `c_{i+1}` then `INT(c_{i+1}) ≤ 2 INT(c_i)`";
6. hence `INT(p*) ≤ MAX(INT(c_j), m̄_max) < p` (5.4), contradicting minimality.

Step 5 is `Willard2005` Eq. (19) `Val(u) ≤ b·2^{Δ(s)}` in embryo (`INT`↔`VAL`,
`j`↔`Δ(s)`, `m̄_max`↔`b`); step 1 is the minimal-pair opening of 2005's
Theorem 1. What 2005 adds is the **abstraction barrier**: Definition 5 separates
the engine (Theorem 1) from the per-apparatus analysis (Theorem 2 via Lemmas
1–2), which is exactly why Willard says Definition 5 "did not exist in our prior
papers."

**5.4 Substitution as a relation (p. 332).** "Gödel's substitution function can
be treated only as a relation by the axiom system IS… although `IS(A)` can not
prove `∀x ∃y : SUBST(x,y)`, it can verify for any fixed `k` that
`∃y : SUBST(k,y)`. This implies that `IS(A)` satisfies part (i) of the
definition of self-verification (and that `Prf_{IS(A)}(x,y)` is a Δ₀ formula)."
The demote-to-relation / totality-unprovable / instance-verifiable pattern, at
the origin, applied to substitution itself.

**5.5 The cut-free property, defined structurally (p. 332).** "every semantic
tableaux or resolution proof of the inconsistency of an axiom system `A` must
have every sentence of that proof constituting a **subcomponent** of some axiom
of `A`. This characteristic … will be called their 'cut-free' property. It is
closely analogous to Gentzen's notion of a cut-free sequent calculus proof."
Resolution and cut-free sequent calculus are named as covered by the same
analysis, here and at TR pp. 4 and 10 — the earliest witness for the
apparatus-generality claim that the 2026-07-28 audit tracks, and **stated-only
in every one of those places**.

**5.6 ZF (p. 331).** "While Zermelo-Frankel set theory (ZF) can not prove its
own consistency [Go31], section 5 shows surprisingly it can prove Proposition 1.
This is exciting because it means ZF **CAN PROVE** its consistency equivalent to
that of an *alternate* system IS(ZF), which is self-verifying and affirms the
consistency of ZF!!" — and (p. 335) "This raises the philosophical question of
whether when humans *think* they are using ZF to reason, they may be actually
relying upon IS(ZF) *unconsciously*???" The origin of the set-theory thread
that reappears in the 2008 ZCF nachlass drafts.

**5.7 The induction weakness (p. 331).** "The one sharp disadvantage of IS is
that it is clearly too weak to prove the validity of the Principle of
Induction." Willard's response — that induction is "*solely* an epistemological
means towards the two ends of proving and of shortening the proofs of Π₁
theorems, *rather than a final end unto itself*" — anticipates the passive
induction of `Willard2007-APAL`.

**5.8 Proof-length claim (p. 331).** `IS(Peano)`'s proofs of Π₁ theorems are
"sometimes dramatically shorter and never more than a small polynomial factor
longer than the analogous Peano proofs."

## 6. Notation and errata specific to this witness

- `IS*(A)` here = `IS^{Σ₁}(A)` in the TR (drift D8).
- The Π₁/Σ₁/Δ₀ definitions differ in statement from the TR's (drift D9).
- The Group-1 function list has fourteen members against the TR's eight
  (drift D10).
- Proof-compression allowance (p. 327): "our results are slightly
  strengthened if we assume one is allowed to slightly compress a proof `p` by
  physically writing the bit representation of any 'long constant' `k` only
  once and storing its other appearances as **pointers** to this long
  representation." Willard permits a limited pointer-sharing and reports that
  it *strengthens* the result — worth contrasting with the NC-SJAS plan's
  blanket rule that "no DAG sharing, memoization, normalization, or host
  callback may count as free proof compression". Recorded as drift **D14**.
- Acknowledgements (p. 335): conceived during a 1992 sabbatical hosted by the
  University of Tel Aviv; thanks Sam Buss, Paliath Narendran, Robert Paige,
  Dan Rosenkrantz, Moshe Vardi.

## 7. Saturation record

| Pass | Date | Method | Items | Result |
| --- | --- | --- | --- | --- |
| 1 | 2026-08-21 | **Full visual read of all 12 pages** plus text-layer sweep | Propositions 1–4, Lemmas 1–2, Main Conjecture | Complete |
| 2 | *pending* | Re-pass for saturation closure | — | — |


## Verification pass, 2026-08-27

Read to completion (pp. 1–12 = printed 325–336) as part of the C4–C6 coverage
verification. The inventory was confirmed complete by an uncapped,
case-insensitive item sweep. Six findings not previously recorded:

1. **The Group-1 functions are eleven bit-manipulation primitives** — Count,
   Shift, Remove, Extract, Compress, Andreverse, Andmacro, Andmultiply,
   Andexpand, Address, Width — wholly unlike the six or seven grounding
   functions of every later paper. Willard notes `Turing(x,y,z)` is expressible,
   so the language is strictly stronger than Presburger Arithmetic.
2. **The class definitions differ substantively.** A prenex sentence is `Π₁`
   here iff every existentially quantified variable is bounded by *the maximum
   of the enclosing universally quantified variables* or by a constant — not
   "all quantifiers bounded". This makes D9/O12 a **definitional** drift, not
   merely a notational one.
3. **The corpus's founding statement of the growth device** (p. 329). With
   `a₀ ≥ 2`, `a_{i+1} = (a_i)²` needing `2ⁿ` bits, against `b_{i+1} = b_i + b_i`:
   the additive sequence "is characterized by proofs of the existence of `b_n`
   requiring **more bits than `b_n`'s binary encoding**", so "there will always
   be adequate memory to write down the binary representation of `b_n` when
   there is sufficient memory for storing the proof of its existence (whereas
   the same is plainly not true when multiplication is assumed to be a function
   because then `a_n`'s existence proof is **exponentially shorter than its
   binary encoding**!)" — with the illustration that multiplication proves `a₁₀`
   exists "within the 400-line length of this paper, whose binary encoding
   requires more digits than the number of atoms in the universe", while "even
   the full cardinality of the universe is insufficient for the Addition Axioms
   to prove the existence of a number whose bit length is as large as the
   universe". **This is `Willard2006a` Remark 3 and `Willard2020` §5, thirteen
   and twenty-seven years early** (obligations O44, O50).
4. **"Cut" is defined via the Law of the Excluded Middle** (p. 335): "a detour
   in a theorem proof (**following from an unnecessary application of the Law of
   the Excluded Middle**) that is considered redundant". `Willard2020`'s result
   is the formalisation of this 1993 definition. Immediately after it Willard
   states the **apparatus-identity thesis** (O38): proof systems with and
   without cuts "are known to prove the **identical set of theorems**", yet a
   system employing cuts "has **sharply different properties**".
5. **`Willard2011`'s generic configuration is foreshadowed here** (p. 334):
   "`IS(A,g,d)` and `IS*(A,g,d)` … where **g** is the set of proposed Group-1
   Axioms and **d** is the deduction method … **IS-like systems**". Two of the
   five components of Definition 3.4's `ξ = (L, Δ₀, B, d, g)`, in 1993 notation.
6. **The ZF result** (pp. 331, 335) is the 1993 root of the 2008 ZCF/ZF nachlass
   material: ZF cannot prove its own consistency but *can* prove Proposition 1,
   so "ZF can prove its own consistency is equivalent to an alternative
   `IS(ZF)`, which is self-verifying and affirms the consistency of ZF. Indeed,
   `IS(ZF)` confirms ZF's `Π₁` validity" — with the question "whether when
   humans think they are using ZF to reason, they may be actually relying upon
   `IS(ZF)` unconsciously???"

Also noted: `nice` is the 1993 admission condition (a fifth member of D24's
family); the **subcomponent** relation and the definition of the **cut-free**
property (p. 332); the claim that `IS(Peano)`'s `Π₁` proofs are "sometimes
dramatically shorter and never more than a small polynomial factor longer" than
PA's (p. 331); and the "eternally unable to formally justify" scenario (p. 335),
a companion to O24.
