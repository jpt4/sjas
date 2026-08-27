# Extraction: Willard1993-TR

> Origin extraction (component C5). Template: charter ADR-0001 §Decision.
> **Anchoring convention**: this witness is a collated scan with an OCR text
> layer produced in C1. Anchors are given as *printed page* numbers; the
> collated PDF page is **printed + 2**. The OCR layer is a search aid only —
> every statement recorded here from the formal core was read from the page
> image (see §8).

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard1993-TR` |
| Title | Self-Verifying Axiom Systems and the Incompleteness Theorem |
| Venue | SUNY-Albany Computer Science Department, Technical Report 93-10 |
| Witness | `nachlass/papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf` (61 collated pp.; title + preface + printed pp. 1–55 + Figures 1–4) |
| Text aid | `../sources-text/willard1993-tr.txt` (24,221 words, OCR — **not authoritative**) |
| Relation to `Willard1993` | The full-length original. Its preface states the published KGC chapter is "a 12-page abbreviated version of this paper"; its own reference list cites the chapter as `[Wi93]`, so the TR postdates it |
| **Actual date** | **No earlier than April 1994** — see §7 D1-resolution |
| Structure | §1 Introduction (pp. 1–2); §2 General Perspectives (pp. 2–7); §3 Formal Summary of Main Results (pp. 8–12); §4 Intuition Behind the Proof of Proposition 1 (p. 13); §5 Semantic Tableaux Formalism (pp. 14–18); §6 Proof of Proposition 1 (pp. 19–22); §7 Proof of Proposition 2 (pp. 23–25); §8 Proof of Proposition 3 (pp. 26–27); §9 Proof of Proposition 4 and Generalizations (pp. 28–29); §10 Philosophical Implications of Propositions 2c, 4, 8, 9 (p. 30); §11 Proof of Proposition 5 (p. 31); §12 Justification (p. 32); §13 Proof of Proposition 7 and Added Remarks (p. 33); §14 Philosophical Speculations (p. 34); **Appendix A (pp. 37–52)**; **Appendix B (pp. 53–54)**; References (p. 55); Figures 1–4 |

## 2. Role in corpus

**This is the origin document of the entire SJAS programme.** It introduces:

- the term **"Introspective Semantics (IS)"** for the family (p. 2 of the
  chapter; the TR uses `IS(A)` throughout) — the ancestor of every later
  `IS_D(A)`, `IS#_D(β)`, `ISRes` name in the corpus;
- the **definition of self-verifying** (see §3.1 below), which by 2005 has
  split into "Introspectively Unified Logic" (the pair) and "Self-Justifying"
  (the system);
- the **three-category growth/deduction trade-off** that the whole later
  corpus refines (§3.2);
- the **matched positive/negative discipline** — affirmative systems paired
  with minimally-different inconsistent controls — that recurs through 2005's
  `NS^{k,m}_D(A)` and 2020's `Tab`/`Xtab`;
- the **informal growth argument** that Willard 2005 Definition 5 formalizes
  as θ-compactification (§3.3).

**Genesis of the proofs** (printed p. 14): "The formal proofs of
Propositions 1-7 appear in Sections 6-13 (essentially one proof per section).
**The first version of our proofs was conceived using Gentzen's cut-free
sequent calculus notation.** Later, we observed that our proofs could be
simplified significantly, if we instead employed the semantic tableaux
notation… because it mixes exceptionally well with Section 6's notation."
This is stronger evidence for the apparatus-generality claim than the bare
"trivially generalizes" assertions elsewhere: the argument was *born* in
cut-free sequent calculus and transposed to tableaux for readability.

Willard's own framing (p. 2, chapter p. 326): "when human beings normally
think, they implicitly presume their own consistency… Also in the distant
future computers should be able to imitate such human self-confidence." The
epistemological thread of `Willard2013`/`Willard2014` starts here.

## 3. Systems defined

### 3.1 The definition of self-verifying (chapter p. 325; TR §1)

> Define an axiom system `A` to be **self-verifying** iff (i) one of the
> theorems implied by `A` is the statement of its own consistency, and (ii)
> the axiom system is in fact consistent.

**No deduction-method parameter appears in the definition.** The parameter
exists in the TR's *notation* — systems are written `IS^s_d(A,G)` where `d`
is the deduction method and `G` the Group-1 axiom set, "the subscript `d`…
was omitted, indicating the deduction corresponded by default to semantic
tableaux" (p. 10) — and the chapter's §6 proposes `IS(A,g,d)` for "IS-like
systems". By `Willard2005` the pair `(α, D)` is definitional. See drift D7.

### 3.2 `IS(A)` and the three axiom groups

`A` is the **inner axiom system**; `IS(A)` wraps it. **Three** groups (not
four as in 2005):

| Group | Content |
| --- | --- |
| Group-1 | The function/relation definitions — the set `G₀` (see below), plus equality, greater-than, the integer constants `k̄` with axioms `k̄ = k−1 + 1̄` and `∀x x<k̄ ⊃ x=0̄ ∨ … ∨ x=k−1`, and `∀x : x+1 ≠ x`. **Addition-totality is Π₂**; all remaining axioms are Π₁ (p. 4) |
| Group-2 | Schema: for each Π₁ sentence Φ, the reflection axiom `Pr_A(⌜Φ⌝) ⊃ Φ` (Eq. 2.1), Π₁ in prenex normal form because `Prf_A(x,y)` is Δ₀ |
| Group-3 | A single sentence asserting "The union of the first two groups of axioms *with this very sentence* forms a consistent set of axioms", well defined via fixed-point operators [Ro67, Je71]. Formally `∀y ¬Prf_{IS(A)}(⊥, y)` (Eq. 2.2) |

**`G₀` — the eight functions** (p. 3): Addition, Subtraction, Division, plus
(i) `StringCount(x,y,j)`, (ii) `Shift(x,y) = x/2^y`, (iii) `Extract(x,y,z)`,
(iv) `Andreverse(x,y,i,j)`, (v) `Address(x,y,z,i,j)`. Subtraction is truncated
(`x−y = 0` when `x ≤ y`) and `x/0 = 0`, so both are total.

**`nice`** (p. 4): `A` is nice iff `A` is consistent with the Group-1 axioms of
IS and there exists a Δ₀ formula `Ax_A(y)` expressing that `y` is the Gödel
number of an axiom of `A`. "Every r.e. axiom system, which is consistent with
Group-1, can be mapped onto a nice system that proves all its theorems."

### 3.3 The Group-1 variants — the boundary in its origin form

The TR's systems differ by which Group-1 set they carry, and this is where the
boundary lives:

| Set | Definition | Consequence |
| --- | --- | --- |
| `G₀` | The eight functions above; **Addition total**, multiplication only a relation | Props. 1–4: consistent under **cut-free** deduction |
| `G₋` | `G₀` **minus** the addition-totality axiom | Prop. 7: `IS₊(A, G₋)` consistent **even with cuts permitted** |
| `G_s` | Like `G₀`/`G₋` but **Successor is the only growth function** | Solovay's Theorem ⇒ `IS₊(PA+, G_s)` **inconsistent** |
| `G_m` | `G₀` **plus** the axiom that multiplication is total | Prop. 6: `IS^{UΔ₀}(A, G_m)` **inconsistent** for some nice `A` |

Willard's own tabulation of the trade-off (p. 3): systems "fall into three
categories depending on whether they have no growth functions, permit Addition
as a function, or recognize both Multiplication and Addition as total
functions", where only the first "will be capable of permitting Gentzen-style
deductive cuts without becoming inconsistent."

The sharpest statement of the boundary (p. 11): Solovay's inconsistent
`IS₊(PA+, G_s)` "would be transformed into a consistent system if **either**
(a) deductive cuts were removed… **or** (b) the successor function was removed
from `G_s`." One inconsistent system, two independent repairs — growth or
deduction, pick one.

### 3.4 The reflection-strength family

| System | Group-3 asserts | Result |
| --- | --- | --- |
| `IS(A)` | own consistency (Eq. 2.2) | Prop. 1: consistent for nice `A` |
| `IS^{Σ₁}(A)` | strong-uniform reflection for all Σ₁ formulae | Prop. 3: consistent for nice `A` |
| `IS^{LΔ₀}`, `IS^{LΣ₁}`, `IS^{UΣ₁}`, `IS^{UΔ₀}` | local Δ₀ / local Σ₁ / second-uniform Σ₁ / second-uniform Δ₀ reflection | Remark 1: all weaker than `IS^{Σ₁}(A)`, so Prop. 3 gives consistency |
| `IS^{LΠ₁}(A)` | local reflection for all **Π₁** sentences | **Prop. 5: inconsistent** for some nice `A` (e.g. PA+) |
| `ISVALID`, `FORBIDDEN(A)`, `ISFORBIDDEN` | `Bool_i` functions added; FORBIDDEN adds multiplication | Prop. 4 / chapter: Props. 1–3 generalize to ISVALID; FORBIDDEN and ISFORBIDDEN inconsistent for some nice `A` |
| `IS₊(A, G)` | subscript `+` = deduction permitting cuts (Hilbert-style) | Prop. 7 positive for `G₋`; Solovay negative for `G_s` |

The chapter calls `IS^{Σ₁}(A)` by the name `IS*(A)` — a notation difference
**between the two 1993 witnesses** (drift D8).

### 3.5 The encoding, and why it uses pointers

Printed pp. 14–15. A **byte is six bits** — identical to `Willard2005`
Appendix A's convention, so the coding density is stable across the corpus.
6-bit codes are assigned to the six connectives, three kinds of parentheses
plus comma, five symbols (`Â`, `v̂`, `ĉ`, `û`, `f̂`) for atomic formula /
variable / fixed constant / proof-parameter / function, and **two special
symbols `u#` and `v#`** that "allow us to use the `u` and `v` symbols to act
as **pointers to constants**". Integers cost `⌊log₃₂ i⌋ + 3` bytes.

Two consequences worth carrying forward:

- **Linear equivalence.** Two encodings are *linearly equivalent* iff each
  translates into the other with at most constant-factor length increase.
  "Lemma 5.1 implies that the set of Δ₀ formulae for two distinct but linearly
  equivalent encodings must be essentially isomorphic… Lemma 5.1 assures that
  the absence of a formal multiplication function in `IS(A)` does not raise any
  new complications, with regards to linearly equivalent encodings." This is
  the corpus's earliest coding-invariance statement, and the ancestor of
  `Willard2011` Definition D.1(iv)'s "essentially any natural method".
- **Pointers are a totality device, not an optimisation.** Lemma 5.2's
  parenthetical is explicit: the `v#` notation is needed because "the two
  analogous mappings *onto* `⌜Φ(ĉ[j])⌝` **are unknown by `IS(PA+)` to be total
  functions**". Expanding a long constant in place is exactly the operation the
  system cannot prove total; the pointer form keeps substitution inside the
  budget. See drift D14.

### 3.6 Terse proofs and the internalization step

Printed p. 23 (§7). A **terse proof** of Ω is a tree in which (i) every node is
an axiom or a deduction from a higher node — the root does *not* store `¬Ω`,
so proof by contradiction is precluded — and (ii) exactly one branch, the
**pivotal branch**, may be open, its leaf holding `Positive(Ω)`.

If `t` is a terse proof of `¬Φ` and `p` a tableaux proof of `Φ`, then
`Glue(t,p)` — rooted at `¬⊥`, with `t` below the root and `p` hanging from the
bottom of `t`'s pivotal branch — is a tableaux proof of `⊥`. Hence no
consistent system has both. Willard's point is that this is "**not merely a
meta-theorem**": Lemma 7.1 shows `IS(PA+)` proves a self-reflexive form of it.
That internalization is how Proposition 2's reflection is obtained, and it has
no direct counterpart in `Willard2005`, where Level-1 reflection instead comes
from the Tab−U\*₁−List modus ponens.

### 3.7 Why multiplication is fatal — the 1993 mechanism

Printed p. 32 (§12). Willard abbreviates Proposition 6's system
`IS^{UΔ₀}(PA+,G_m)` as **INVALID.2** (Proposition 5's is INVALID.1; the TR's
Figures 1–4 illustrate their inconsistency proofs). Lemma 12.1's route:

1. for each Δ₀ `Ψ(v)` there is λ with PA+ ⊢ (12.1), by Lemma 5.3's
   T-transformation;
2. (12.1) is Π₁, so Group-2 makes it a theorem of INVALID.2;
3. **because INVALID.2 recognizes multiplication as total**, it also proves
   `∀a∀b∃c c=(ab)^λ`, and the two together give (12.2);
4. its `UΔ₀` reflection gives (12.3); the **Cut Elimination Theorem** (via
   Remark 2, p. 22) then yields (12.4);
5. so INVALID.2 proves *all* its local Π₁ reflection statements — which
   Proposition 5 has already shown to be fatal.

**Multiplication kills the system by making local Π₁ reflection derivable.**
This is a materially different explanation from `Willard2005` Remark 4, where
multiplication is fatal because its growth "overwhelms Definition 5's
constraints". Both are Willard's; the codified statement should present them as
two routes to one boundary rather than paraphrasing either as *the* reason.

### 3.7b Two methodological caveats Willard states about his own proofs

**Meta-Logic (Remark 2, printed p. 24).** Many proofs in the TR do *not*
construct cut-free proofs; they establish their existence at the meta level:

> our proofs shall *apply a cut rule at the meta-theoretical level*. That is,
> suppose it is known that a *cut-free* `IS(PA+)` system can prove the two
> statements `α` and `α ⊃ β`. Then by the Cut Elimination Theorem … `IS(PA+)`
> will also support a cut-free proof of `β`. For the sake of shortening many of
> the proofs in this paper, we will *often omit constructing formally the
> cut-free proofs* of such theorems `β`: Instead, *their theoretical existence*
> will be established implicitly …

This is a global qualification on the document's proof-status profile, and it
sits in a pointed relation to the object-level story: the *systems* are
cut-free, while the *reasoning about them* freely uses cut. It also explains
several of the sketch-grade labels. Note the interaction with drift D16: the
axiom-versus-theorem distinction matters *inside* the system precisely because
cut is unavailable there — but Willard helps himself to cut outside it.

**The `u#` notation is necessary, not convenient (printed p. 18).** Lemma 5.3's
proof is followed by:

> It should be noted that **Lemma 5.3 would not follow, had this section
> employed slightly different definitions (such as omitting the symbol `u#`)**,
> because `T(p,k) ≤ L_k p` could then fail for all constants `L_k`.

So the constant-pointer compression of §3.5 is load-bearing for a lemma, not an
optional strengthening. Willard then scopes the damage himself: "even without
such a formalism supporting Lemma 5.3, most of our seven main theorems,
**including Propositions 1, 2a, 3, 5, & 7 will remain valid**" — so only the
results routed through Lemma 5.3 (Propositions 2b, 2c, 4 and 6) depend on it.
This settles drift **D14**: any transposition that forbids sharing outright is
not merely stylistically at odds with Willard, it loses these four results
unless it supplies a replacement.

**`IS2`: the trace-carrying fallback (Lemma 5.4, printed p. 19).** `Prf2_B(x,
y₁, y₂)` holds when `y₁` is a tableaux proof of `x` from `B` **and `y₂`
describes the successive states of a Turing Machine verifying that `y₁` is well
defined**; `IS2^s_d(A)` is the analogue built on it. Willard's scoping is
explicit:

> Appendix A is in some sense **unnecessary** because each theorem in Sections
> 6–14, about IS, retains meaning, even without Appendix A, if Lemma 5.4's
> slightly weaker `IS2` formalism is used to replace `IS`. That is, each
> theorem about `IS` can be translated into an analogous result about `IS2` by
> simply replacing `y` with the ordered pair `(y₁, y₂)`.

The published chapter offers the same escape hatch under the name `Prf*_A(x,
y₁, y₂)` (p. 327). Two consequences: the corpus's *results* do not depend on
Appendix A's 16-page arithmetization (though its *constructs* are what 2005
inherits — §3.8); and **a proof predicate that carries an explicit computation
trace is Willard's own device**, from 1993 — the ancestor of the trace-based
checking (`Step`/`TraceOK`, `EvalTraceOK`) that the NC-SJAS plan and the
affine-tree design treat as a native-computational innovation.

### 3.7c Which derivability condition fails, and Willard on the delicacy

Two passages from the close of §3 (printed p. 12) that the codified statement
should carry.

**The Hilbert–Bernays condition IS(A) violates.** "A well-known corollary to
the Gödel Incompleteness Theorem is that every self-verifying system must
violate one of the Hilbert-Bernays conditions [HB39, Me87]. For the case of
`IS(A)`, the difficulty is that **only in the *degenerate case* where `x` and
`y` are *fixed constants*** can `IS(A)` prove the Hilbert-Bernays condition
that *if `x` is a proof of `α` and `y` is a proof of `α ⊃ β` then there exists
some `z` that proves `β`*."

This names the exact failure point — the second derivability condition
(provable modus ponens) — and does so in the *same* shape as every other
restriction in the document: it holds for **fixed parameters** and fails
uniformly. The pattern recurs at `SUBST_i(k̄,y)` for fixed `k̄` (§3.8), at
Lemma 5.3's `T(p,k)` for fixed `k` or fixed `p` (§3.7b), and at `Scalar_k` for
fixed `k`. The codified statement should present *fixed-parameter totality
versus uniform totality* as the single recurring device, of which the
multiplication restriction is one instance.

**Willard states the delicacy thesis himself.** "Or at least in a context where
a **modest change in a formalism's definition will cause Propositions 1 thru 7
and Solovay's Theorem to produce starkly different results**, it appears
premature to develop hasty conclusions, whether positive or negative, about
self-verifying systems."

This is the premise of `prose/sjas_synthesis.txt` — "the properties of a formal
system can be delicately sensitive to its precise definition" — asserted by
Willard in 1993 about his own results. The Codification's governing caution is
therefore not an external methodological choice imposed on the corpus; it is
the corpus's own stated lesson, and the codified statement should quote it
where it explains why variants are tracked rather than merged. Recorded as
composition obligation **O21**.

Willard's surrounding position is pluralist rather than triumphal: `IS(A)`
"can not fully characterize the complexity of human thought", yet it is
"simplistic to fully ignore self-verifying systems because humans appear to
implicitly presume their consistency (at some type? of weak level)"; "the truth
(?) may possibly lie between these two extremes."

### 3.8 Appendix A — the arithmetization (printed pp. 37–52)

**Purpose** (p. 37): "for each nice `A`, it is possible to encode
`Prf_{IS(A)}(x,y)` as a Δ₀ formula", generalizing to `IS^s_d(A,G)` for any
`G ⊇ G₀`, hence covering Propositions 3–9.

**The vocabulary is already 2005's.** Three constructs are defined here and
survive verbatim into `Willard2005` §3:

| 1993 (p. 37) | 2005 (pp. 9–10) |
| --- | --- |
| `UNION(A)` = union of IS(A)'s Group-1 and Group-2 axioms | `UNION(A)` = Groups 0+1+2 (2005 splits 1993's Group-1) |
| `ExPrf_B(x,y,z)` = `y` proves `x` from `B` **plus the added axiom with Gödel number `z`** | `ExPrf^D_{UNION(A)}(h,t,p)` |
| `SUBST_i(x,y)` — Gödel substitution as a **relation** | `Subst(g,h)` |

**The fixed point** (p. 38). With `J` the Gödel number of (A.1):

- (A.2), the Group-3 axiom: `∀y ¬{ Prf_{UNION(A)}(⊥,y) ∨ [∃z<y SUBST_i(J,z) ∧ ExPrf_{UNION(A)}(⊥,y,z)] }`
- (A.3), the definition of `Prf_{IS(A)}(x,y)`: `Prf_{UNION(A)}(x,y) ∨ { ∃z<y SUBST_i(J,z) ∧ ExPrf_{UNION(A)}(x,y,z) }`

Compare `Willard2005` Eq. (7):
`SubstPrf^D_{UNION(A)}(g,t,p) ≡ Prf^D_{UNION(A)}(t,p) ∨ ∃h ≤ p [ Subst(g,h) ∧ ExPrf^D_{UNION(A)}(h,t,p) ]`.
**These are the same formula**, differing only in that 1993 fixes the first
argument to the constant `J` while 2005 abstracts it to a parameter `g` (and
`z<y` becomes `h ≤ p`). 2005's `SubstPrf` is this construction with the
diagonal argument promoted to a variable — a precise, checkable lineage claim.

*Clarifying Comment 1* explains the two disjuncts: with `K` the unique number
satisfying `SUBST_i(J,K)`, proofs not using Group-3 have Gödel number `< K` and
are caught by the first disjunct; proofs using it exceed `K` and are caught by
the second.

**The demote-to-relation pattern, stated exactly** (p. 37, "Point of
Clarification"): "Since it does not recognize multiplications as a function,
`IS(A)` will clearly be too weak to prove that `∀x∃y SUBST_i(x,y)`. However,
`IS(A)` will be able to prove for **any fixed integer `k̄`** that
`∃y SUBST_i(k̄,y)`. It will turn out that the latter will be sufficient for
`IS(A)` to formally define **the particular instance of the reflection
principle** needed to define `H`."

**Theorem A.1** has four parts: (a) `SUBST_i` is majorized; (b) axiomhood for
**PA, PA+ and ZF** is majorized; (c) for any majorized-encodable `B`, `Prf_B`
is majorized; (d) for any nice `A`, `ExPrf_{UNION(A)}` and `Prf_{UNION(A)}` are
majorized. Part (b) is what licenses `IS(PA)`, `IS(PA+)` and **`IS(ZF)`** (p. 39).

**The eighteen items** (`Willard` calls A.5–A.18 "the next fourteen
propositions", p. 43 — a count that checks):

| Item | Page | Content | Proof |
| --- | --- | --- | --- |
| Theorem A.1 | 37 | Parts (a)–(d) above | via A.9, A.12, A.15, A.18 |
| Lemma A.2 | 41 | `TreeCheck(s)` — `s` is a parenthesis tree `{ P {α₁}…{αₙ} }`; five conditions | full |
| Lemma A.3 | 42 | `BinaryTreeCheck(s)` (internal nodes have one or two children) | full |
| Lemma A.4 | 42 | `NODE`, `INDEX`, `PRINCIPLE`, `ANCESTOR(i,L,s)`, `PARENT` over tree encodings | full |
| Lemma A.5 | 43 | `FormulaCheck(x)`; well-formed formulae are a **context-free language** [ASU86, LRS76], checked by an explicit parse tree | **sketch** ("Proof Sketch") |
| Lemma A.6 | 44 | `SentenceCheck(x)` | full |
| Lemma A.7 | 45 | `ReplaceCheck(x₁,s₁,x₂,s₂)` via double encoding | full |
| Observation A.8 | 45 | Two integer encodings: the **natural** `i` (base 2) and the **godelized** `i*` (`⌊log₃₂ i⌋+3` bytes, base-32 digit `b` as the 6-bit code `32+b`) | n/a |
| Corollary A.9 | 46 | **Part (a)** of Theorem A.1 | full |
| Observation A.10 | 46 | `ConcatenateCheck_m`. "**While `IS(A)` is too weak to prove that concatenation is a total function**… it certainly can represent `ConcatenateCheck_m` as a Δ₀ formula" | n/a |
| Lemma A.11 | 46 | `InductAx(s)` — PA's induction axioms | full |
| Corollary A.12 | 47 | **Part (b)**: PA, PA+, ZF. Each has "only a finite number of axioms outside an easily recognized infinite schema" | full |
| Lemma A.13 | 47 | `PositiveCheck(g,h)` — `h` is the Positive Rewrite of `g`; uses `Parity`, `Prefix`, `Suffix` | **sketch** ("Proof Outline") |
| Lemma A.14 | 49 | `DeductionCheck(i,t)` — the main component of Part (c). **Figure 1 lists twelve permissible deduction rules**; rule 10 carries the eigenvariable condition ("the parameter `u` must not be also employed by any of `P(u)`'s ancestors") | full |
| Corollary A.15 | 50 | **Part (c)**: `Prf_B(x,y)` majorized, via three constraints — (I) root is the positive rewrite of the negated target, (II) every other node is an axiom or a `DeductionCheck` deduction, (III) every branch is closed | full |
| Lemma A.16 | 51 | `AxGr₁(x)` — Group-1 axiomhood. Notes `G₀` was "defined only informally in Section 2"; formally it is finitely many axioms **plus two infinite schemata** (A.31) `ĉ[k] = ĉ[k−1] + ĉ[1]` and (A.32) `∀v (v<ĉ[k] ⊃ ⋁_{j<k} v=ĉ[j])` | **sketch** ("Proof Sketch") |
| Lemma A.17 | 51 | `AxGr₂(x)` — Group-2 axiomhood, via concatenation of five substrings | full |
| Corollary A.18 | 52 | **Part (d)** | full |

**Corollary A.15 is the arithmetized proof-checker in origin form**: root check,
node-justification check, closure check. It is the direct ancestor of the
Proflog `tableau-proof` predicate and of the affine-tree design's
`Deriv(s,p,φ)`, and it is worth quoting when the codified statement reaches the
proof predicate.

**Added Comment (p. 52) — the minimal signature.** "our discussion technically
only needed `G₀`'s atomic functions of **Addition, StringCount, Shift and
Extract** to encode `Prf_{IS(A)}(x,y)` as a Δ₀ formula. The sole purpose of
`G₀`'s functions of Andreverse, Address, Subtraction and Division… was to
simplify the presentation." Only **four** of the eight are load-bearing for the
arithmetization. Together with Remark 6 this all but dissolves the apparent
signature drift (D10/D11).

**Eighteen compound functions** are derived from the atomic ones (pp. 39–41),
among them `Scalar_k` — "**Unlike normal multiplication, scalar multiplication
is a function under `IS(A)`** because it is defined by `Scalar₀(x)=0` and
`Scalar_{k+1}(x) = Scalar_k(x)+x`" — with "`2x, 3x, 4x, …`" abbreviating
`Scalar₂, Scalar₃, Scalar₄`. So **`Willard2005`'s `Double` is 1993's
`Scalar₂`**, and multiplication by any *fixed* constant was always total;
"multiplication is not total" concerns *variable* multiplication only.
`Maximum` and `Minimum`, primitive in 2005, are here **derived**
(`Maximum(x,y) = x + (y−x)` on truncated subtraction).

**Where the density constant comes from.** Observation A.8 encodes a base-32
digit in a **6-bit** byte, so the godelized form costs `6/5` of the natural
form; p. 46 uses exactly this — "Since `i* > i^{6/5}`, `IS(A)` will be
incapable of recognizing that the mapping of `i` onto `i*` constitutes a total
function. However, it will be able to recognize **as a relation**…
`ConvertCheck(i,i*)`". This is the origin of the `5`-versus-`6` pair that
`Willard2005` Appendix A (6-bit bytes) and Eq. (20) (the constant 5) inherit,
and that `Willard2011` D.1(iv) states as "≥ 5J bits… Gödel number ≥ 32^J" —
where the `32` is the base-32 alphabet. See composition obligation O2.

### 3.9 Appendix B — the worked inconsistency derivation (printed pp. 53–54)

Appendix B completes Proposition 6 by exhibiting the tableaux proof of `Ψ` from
INVALID.2 that **Figure 4** draws. With `Φ ≡ {∀x₂∀x₃ [SUBST(x₁,x₂) ⊃
¬Prf_{INVALID.2}(x₂,x₃)]}` (B.1), `H` the Gödel number of `Φ(⌜Φ⌝)`, and
`Ψ ≡ {∀x ¬Prf_{INVALID.2}(H,x)}` (B.2), the proof runs seven levels; the
footnote records that **only one of its four branches uses multiplication
totality**.

Its **Summarizing Comment** is the conceptually important part:

> The complication in formally proving Proposition 6 was that semantic tableaux
> proofs allow only axioms (and their "cut-free" deductions) to appear in the
> intermediate stages of a proof (i.e. provable theorems are disallowed).
> Therefore *unlike* Figure 3, the third line in Figure 4's proof tree **cannot
> explicitly specify an LΠ₁ reflection sentence**. Instead, Levels 4 thru 7 …
> had **implicitly exploited LΠ₁ reflection by constructing its essential
> cut-free implications**.

So the difference between INVALID.1 and INVALID.2 is precisely whether a
principle is available **as an axiom** or only **as a theorem** — and under a
cut-free apparatus that difference is not cosmetic, because a cut-free proof
cannot cite a theorem as an intermediate step. This is the same device that
drives `Willard2005`'s Theorem 5/6 contrast, where `IS_D(A)` *proves* `Υ(k,m)`
while `NS^{k,m}_D(A)` has it *as an axiom*. Recorded as drift **D16**.

## 4. Numbered-item inventory

Verification column: `V` = read from the page image; `O` = OCR text only,
visual verification deferred to pass 2.

### Propositions

| Label | Page | Statement digest | Proof | Ver. |
| --- | --- | --- | --- | --- |
| Proposition 1 | 8 | **The main theorem.** For each nice axiom system `A`, `IS(A)` is consistent | full (§6) | V |
| Proposition 2 | 9 | `IS(PA+)` can prove (a) its local reflection statements for **every** Δ₀ sentence; (b) its local reflection statement for the subset of Π₁ sentences Ψ which are **decidable**; (c) its semi-uniform reflection statement whenever `Φ_i` in Eq. (3.4) is Δ₀. Contrasted with Löb's Theorem, which blocks this for conventional systems | full (§7) | V |
| Proposition 3 | 9 | `IS^{Σ₁}(A)` — Group-1 and Group-2 as in `IS(A)`, Group-3 strengthened to assert strong-uniform reflection for all Σ₁ formulae — is consistent for any nice `A` | full (§8) | V |
| Proposition 4 | 9 | `IS^{Σ₁}(PA+)`, `IS^{LΣ₁}(PA+)` and `IS^{UΣ₁}(PA+)` can prove Φ's semi-uniform reflection statements when `Φ_i` is Σ₁ | full (§9) | V |
| Proposition 5 | 10 | **Negative.** `IS^{LΠ₁}(A)` (Group-3 asserts local reflection for all Π₁ sentences) is **inconsistent** for some nice `A` — PA+ or any extension | full (§11) | V |
| Proposition 6 | 10 | **Negative.** With `G_m` = `G₀` plus multiplication-totality: `IS^{UΔ₀}(A,G₀)` is consistent for all nice `A`, but there exist nice `A` where `IS^{UΔ₀}(A,G_m)` is **inconsistent**; likewise `IS^{UΣ₁}(A,G_m)` and `IS^{Σ₁}(A,G_m)` | full (§12) | V |
| Proposition 7 | 10 | With `G₋` = `G₀` minus addition-totality: for any nice `A`, **`IS₊(A,G₋)` is consistent** — i.e. self-verification survives *cuts* when addition-totality is dropped | full (§13) | V |
| Proposition 8 | 29 | For each nice `A` there exists a consistent **`IS^{ETR}(A)`** whose Group-3 axioms verify its **E-Tree Reflection Principle** (Eq. 9.5) for every Ψ and every tuple (I₁…I_m) | **deferred** — "postponed until [Wi94] because of its length" | V |
| Proposition 9 | 29 | `IS^{Σ₁}(A)` (for `A` = PA+ or any extension) proves its **Bounded** E-Tree Reflection Principle (Eq. 9.6) for every Ψ and tuple, with λ = m | **sketch**, labeled "**Very Informal Proof Sketch**"; fuller justification in [Wi94] | V |
| Solovay's Theorem | 10 | **Cited, unpublished.** There exists a finite set `F` of the Π₁ theorems of Peano Arithmetic such that no consistent Gentzen-style sequent calculus system **employing cuts** can simultaneously prove all theorems of `F`, recognize Subtraction, non-zero Division and Successor as total functions, and prove its own consistency. Discovered "shortly after learning of our results" | cited [So94] | V |

### Lemmas, remarks, appendix

| Label | Page | Content | Proof | Ver. |
| --- | --- | --- | --- | --- |
| Lemma 5.1 | 14 | A formula is **majorized with exponent λ** iff every quantified variable is bounded by `x_i^λ` for some `i`; every majorized Ψ has an "essentially equivalent" **Δ₀** Ψ\* agreeing on all L-tuples. Construction: replace `∃v < x^λ` by `∃v₁<x … ∃v_{2λ}<x` (2λ, not λ, to avoid round-off) | **sketch** — labeled "Informal Proof Sketch" | V |
| Lemma 5.2 | 15 | For fixed `j`, variable Φ: `IS(PA+)` proves a map from `⌜Φ(v̂[1])⌝` onto `⌜v#[1,j] Φ(v̂[1])⌝`. **Rationale**: "the two analogous mappings *onto* `⌜Φ(ĉ[j])⌝` **are unknown by `IS(PA+)` to be total functions**" — the pointer notation exists to stay inside the provable-totality budget | full (statement V) | V |
| Lemma 5.3 | 17–18 | With `T(p,k)` the **T-transformation** of a proof `p` of `∀v̂[j]Φ(v̂[j])` into a proof of `Φ(ĉ[k])`: for any **fixed** `k` the map `p ↦ T(p,k)` is recognized total by `IS(PA+)`; likewise for **fixed** `p` the map `ĉ[k] ↦ T(p,k)`. Neither joint totality is claimed | **sketch** — "We will therefore sketch only the proof of the first claim" | **V** |
| Lemma 5.4 | 19 | Defines `Prf2_B(x,y₁,y₂)` — `y₁` is a tableaux proof of `x` from `B` **and `y₂` is a Turing-machine trace verifying `y₁` is well defined** — and `IS2^s_d(A)`, the analogue of `IS^s_d(A)` built on it. For all nice `A` these have Δ₀ encodings | **sketch** — labeled "**Quite Informal Proof Sketch**", the weakest label in the document; argues from Kleene's Fixed Point Theorem [Ke38, Ro67] | **V** |
| Lemma 6.1 | 20 | Sentence (6.1) `¬∀y ¬Prf_{IS(A)}(⊥,y)` is not a subcomponent of any Group-1 or Group-2 axiom | **sketch** — labeled "Proof Sketch", where the chapter's identical Lemma 1 carries a full "Proof" | V |
| Lemma 6.2 | 20 | A tableaux proof of `IS(A)`'s inconsistency is impossible without formally constructing **a parameter or constant element** `p*` with a node `¬Prf_{IS(A)}(⊥,p*)` (chapter Lemma 2 says only "an element") | full | V |
| Lemma 7.1 | 23 | If a **terse** proof tree for `¬Φ` exists from `IS(PA+)`, then `IS(PA+)` **can prove** it will be unable to construct a semantic tableaux proof of `Φ`. Willard stresses this is "**not merely a meta-theorem**" — the internalization step behind Proposition 2 | full (statement V) | V |
| Lemma 12.1 | 32 | All of **INVALID.2**'s (= `IS^{UΔ₀}(PA+,G_m)`) local Π₁ reflection statements are its theorems — so Proposition 5 makes it inconsistent | **sketch** — labeled "Proof Sketch" | V |
| Remark 1 | 9 | The four alternate reflection systems, all weaker than `IS^{Σ₁}(A)` | n/a | V |
| Remark 2 | 24 | **Meta-Logic.** "our proofs shall *apply a cut rule at the meta-theoretical level*": if a cut-free `IS(PA+)` proves `α` and `α ⊃ β`, the Cut Elimination Theorem gives a cut-free proof of `β`, so "we will *often omit constructing formally the cut-free proofs*" and establish only their *theoretical existence*. "Meta-Logic will be used frequently in the remainder of this paper" | n/a | **V** |
| Remark 3 | 28 | **`GΣ₁`** formulae — every quantifier existential *or bounded universal* over a Δ₀ matrix — and the claim that Proposition 3 strengthens to `IS^{GΣ₁}(A)`. The footnote supplies only the intuition, saying the formal proof "requires some tedious notation" | **stated-only** | V |
| Remark 4 | 28 | Propositions 1 and 3 hold whether a total function is given by an **m-ary function symbol** or by an **(m+1)-ary relation symbol plus a totality axiom** `∀x̄∃w A_f(x̄,w)`. §6's growth analysis assumed the latter only to simplify notation | stated-only | V |
| Remark 5 | 34 | Cut-permitting hybrids over `G₋` — `IS^{Σ₁}₊`, `IS^{UΣ₁}₊`, `IS^{LΣ₁}₊`, `IS^{UΔ₀}₊` — are consistent for nice `A`, "once again" with `IS^{LΠ₁}₊(A,G₋)` the exception. "Follow from the techniques of Sections 8–11" | **stated-only** | V |
| Remark 6 | 34 | **`G₀`'s definition is "quite arbitrary".** Call `f̂[i,j]` **slowly growing** iff `f̂[i,j](x̄) ≤ i·2^j · Max(x̄)`. Propositions 1–4, 8 and 9 hold when **any** set of slowly growing functions is added to `G₀`; Proposition 7 holds when any set of non-growth functions is added to `G₋` | **stated-only** | V |
| Remark 7 | 34 | For any r.e. `R` in IS's language consistent with `G₀`, the wrapper `A_R = G₀ ∪ {∀y(Deriv(⌜Φ⌝,y) ⊃ Φ)}` is nice, so `IS(A_R)`, `IS^{Σ₁}(A_R)` and `IS₊(A_R,G₋)` are consistent and **prove all of `R`'s Π₁ theorems** | stated-only | V |
| Appendix A items (18) | 37–52 | Full inventory in §3.8 below | mixed | **V** |
| Appendix B | 53–54 | The explicit tableaux inconsistency derivation for INVALID.2 (§3.9) | full | **V** |

## 5. Notation table

| Symbol | Meaning | Anchor | Note |
| --- | --- | --- | --- |
| `IS(A)` | Introspective Semantics over inner system `A` | §2 | Family name "Introspective Semantics" |
| `IS^s_d(A,G)` | Full notation: reflection strength `s`, deduction method `d`, Group-1 set `G` | p. 10 | `d` omitted ⇒ semantic tableaux |
| `IS₊` | Subscript `+` = deduction permitting cuts (Hilbert-style) | p. 10 | |
| `G₀`, `G₋`, `G_s`, `G_m` | Group-1 variants: eight functions / minus addition-totality / successor-only growth / plus multiplication-totality | pp. 3, 10, 11 | The boundary parameters |
| non-growth function | `f(x₁…xₙ) ≤ Max(x₁…xₙ)`; growth function otherwise | p. 3 | **Identical to `Willard2005` §2** |
| `Δ₀`, `Π₁`, `Σ₁`, `Π₂` | Bounded-quantifier class and its prefix extensions | p. 4 | **Unstarred here**; these are `Willard2005`'s `Δ*₀`/`Π*₁`/`Σ*₁`. Drift D9 |
| `nice` | `A` consistent with Group-1 and with Δ₀ axiom-recognition `Ax_A(y)` | p. 4 | Admission condition; ancestor of 2005's "Π\*₁ theorems true in the standard model" |
| `Prf_A(x,y)`, `Pr_A(x)`, `⌜Φ⌝` | Δ₀ proof predicate; `∃y Prf_A(x,y)`; Gödel number | p. 4 | |
| `SUBST(a,b)` | Gödel substitution **as a Δ₀ relation** | chapter p. 332 | `IS(A)` cannot prove `∀x∃y SUBST(x,y)` but verifies each instance `∃y SUBST(k,y)` |
| subcomponent `⟨X,Y⟩` | Five-clause structural relation | chapter p. 332 | Defines the **cut-free property**; absent from 2005 |
| `INT`, `LIST(β,d)`, s-consistent | Interpretation function assigning integers to parameters; depth-`d` sentence list; validity under INT | chapter p. 333 | **Ancestor of 2005's `VAL` and `Positive(a,b)`** |
| `⊥` | The empty sentence | chapter p. 332 | |
| `Turing(x,y,z)` | Recursive predicate: `x` encodes the first `y` states of a machine with initial state `z` | p. 3 | Expressible ⇒ language stronger than Presburger |

## 6. Replicated context

| Block | Relation |
| --- | --- |
| Non-growth function definition | Verbatim in `Willard2005` §2 |
| PA\* / PA′ counterexample (Rogers) | Recurs in `Willard2005` §1 |
| `nice` admission condition | Becomes "`A`'s Π\*₁ theorems valid in the standard model" (2005 Def. 1) |
| Group-2 reflection schema | Same shape in 2005 (Eq. 5), with Π\*₁ replacing Π₁ |
| Group-3 Kleene fixed point | Same construction; 2005 adds the explicit `SubstPrf` Δ\*₀ encoding (Eqs. 7–8) |
| Resolution and cut-free sequent calculus as alternative `d` | Asserted here (pp. 4, 10) and again in 2005 Remark 1 — **stated-only in both** |
| Appendix A encoding machinery | Precursor to 2005's Appendix A (B-adic) and to `Willard2001`'s Appendices C–D |

## 7. Discrepancies and errata

### D1 resolved — the TR is a 1994 document

The TR's own reference list (p. 55, read from the page image) contains:

- **`[So94]` R. Solovay, private communications (April 1994).**
- `[Wi93]` D. Willard, Self-Verifying Axiom Systems, … LNCS #713 (1993), pp. 325–336.
- `[Wi94]` D. Willard, **The ETR Reflection Principles for Self-Verifying Axiom Systems, forthcoming report.**

A report numbered 93-10 that cites April-1994 private communications and the
already-published 1993 chapter **cannot have been finalized before April
1994**. This corroborates `Willard2001` reference [41], which cites the same
document as "SUNY-Albany Technical Report, **March 1994**". The working
identification in drift D1 therefore holds, and the apparent date conflict is
explained: a 1993-series report number carrying a document finalized in 1994.
Residual minor discrepancies: March (per JSL 2001) vs. April-or-later (per the
content); "50-page" (per JSL 2001) vs. 55 printed pages plus four figure
pages.

### New unlocated work

**`[Wi94]` "The ETR Reflection Principles for Self-Verifying Axiom Systems",
forthcoming report** — cited as containing the proof of Proposition 8 and a
fuller justification of Propositions 8 and 9. Not in `paperlist`, not found by
the C2 coverage pass, not in DBLP, arXiv, or Willard's own résumé. Its title
strongly resembles the KGC-5 chapter "The Tangibility Reflection Principle for
Self-Verifying Axiom Systems" (`Willard1997`, gap **G2**), which is the
likeliest published outcome. Recorded as gap **G14**; it raises G2's priority,
since Proposition 8's proof is otherwise unavailable in the corpus.

### Between the two 1993 witnesses

- **D8 — system naming.** The TR's `IS^{Σ₁}(A)` is the chapter's `IS*(A)`.
- **D9 — formula-class definitions differ.** The TR (p. 4) defines Δ₀ by
  bounded quantifiers `∃v ≤ z` / `∀v ≤ z` with the Π₁/Σ₁ canonical forms as
  prefixed Δ₀ — essentially 2005's Π\*₁/Σ\*₁. The **chapter** (p. 327) instead
  defines Π₁ so that "each variable `x` introduced by an existential
  quantifier … is required to either have a value less than or equal to the
  maximum of the universally quantified variables enclosing it, or … less
  than a prespecified constant" — a majorization-style condition. The two
  formulations are close but not identically stated, and the paper's own
  results are sensitive to exactly this class.
- **D10 — the Group-1 function list.** The **chapter** (p. 326) lists
  addition, subtraction, division plus **eleven** further functions (Count,
  Shift, Remove, Extract, Compress, Andreverse, Andmacro, Andmultiply,
  Andexpand, Address, Width) — fourteen in all. The **TR** (p. 3) fixes `G₀`
  at **eight**: Addition, Subtraction, Division, StringCount, Shift, Extract,
  Andreverse, Address. The later, refined witness has the shorter list.
  Note the chapter's `Andmultiply(x,y,z)` — multiply then mask with `z`, hence
  non-growth — shows a *bounded* multiplication was admissible from the start.

### Remark 6 relativizes the signature drift

Before reading D10–D11 as a serious divergence, note **Remark 6** (printed
p. 34): `G₀`'s definition "was **quite arbitrary**, and our main theorems
trivially extend for more general definitions". Call `f̂[i,j]` **slowly
growing** iff `f̂[i,j](x̄) ≤ i·2^j · Max(x̄)`. Then Propositions 1–4, 8 and 9
hold when **any** set of slowly growing functions is added to `G₀`, and
Proposition 7 when any set of non-growth functions is added to `G₋`.

So the differing function lists (D10, D11) are **not** competing claims about
what the system is: the load-bearing invariant is the **growth class**, not the
signature. This is the earliest corpus statement of that principle, it is what
licenses 1993's bit-string eight and 2005's arithmetic eight to be the same
theory, and it independently corroborates the affine-tree design's decision to
abstract to a growth discipline. Recorded as composition obligation **O13**.

### Against later corpus members

- **D11 — the signature is not the U-Grounding eight.** Both 1993 lists are
  bit-string manipulation functions. `Willard2005`'s eight U-Grounding
  functions are Subtraction, Division, Root, Maximum, Logarithm, Count,
  Addition, Double. **Only Addition, Subtraction and Division are common to
  both** (and 1993's `StringCount` differs from 2005's `Count`). Both
  signatures have exactly eight members and both satisfy the same non-growth
  discipline, but they are different languages. Any claim of the form
  "Willard's system recognizes exactly these functions" must name its source.
- **D12 — addition-totality changes logical form.** In 1993 it is an explicit
  **Π₂** axiom (p. 4). In 2005 addition and `Double` are *function symbols* of
  the U-Grounding language, so the corresponding axioms are Π\*₁ — which is
  what allows Definition 4's Normed class to require every axiom to be
  Π\*₁/Σ\*₁. The U-Grounding language exists to remove this Π₂ axiom.
  **Sharpened by Remark 4** (printed p. 28): in 1993 Willard explicitly says
  Propositions 1 and 3 hold *either* with a function symbol *or* with a
  relation symbol plus the totality axiom `∀x̄∃w A_f(x̄,w)` — the two
  presentations are interchangeable. By 2005 they are **not**: a Π₂ totality
  axiom is inadmissible in a Normed(a,b) system, so the function-symbol
  presentation becomes obligatory. A genuine tightening across the corpus, and
  a case where 1993's greater permissiveness is easy to misread as agreement.

### Errata

- **E1993-1 — resolved, not an erratum.** §4 *does* carry a display heading,
  "Intuition Behind the Proof of Proposition 1" (printed p. 13, mirroring the
  chapter's §4). The OCR sweep missed it because the running page number ran
  into the heading text, producing `-134. Intuition Behind…`. A reminder that
  the heading sweep's silence is evidence of nothing.

## 8. Saturation record

| Pass | Date | Method | Items | Result |
| --- | --- | --- | --- | --- |
| 1 | 2026-08-21 | OCR sweep of the 61-page collated text for item headings, plus **visual reading** of the formal core: TR printed pp. 3, 4, 9, 10, 11, 55 (references) and the whole 12-page published chapter | 9 Propositions, Solovay's Theorem, 7+ Lemmas, 7 Remarks, Appendix A block | Formal core complete and visually verified; Appendix A inventoried at heading level only |
| 1v | 2026-08-21 | **Visual control pass over the main body**: printed pp. 14, 15, 20, 23, 28, 29, 32, 34 | 0 new items; **8 items re-graded** | See below |
| 1a | 2026-08-21 | **Visual pass over the appendices**: every page of Appendix A and B read (printed pp. 37–54) | **19 items added** (Theorem A.1, Lemmas A.2–A.7, Observations A.8/A.10, Corollaries A.9/A.12/A.15/A.18, Lemmas A.11/A.13/A.14/A.16/A.17, Appendix B) | Structure corrected — **Appendix B had been missed entirely**; inventory complete |
| 1b | 2026-08-21 | **Completion pass**: printed pp. 12, 13, 17, 18, 19, 24 read | **2 items added** (Lemma 5.4; Remark 2 relocated to p. 24), 1 re-graded (Lemma 5.3 → sketch) | All numbered items now visually verified; E1993-1 resolved |
| 2 | *pending* | Saturation re-pass (charter A1): re-sweep for any item the heading passes missed, as Lemma 5.4 and Appendix B both were | — | — |

**Outcome of pass 1a (appendices).** Reading the appendices was not optional
housekeeping: it recovered an entire **Appendix B** that the OCR-based structure
note had missed, the three constructs (`UNION(A)`, `ExPrf`, `SUBST`) that
`Willard2005` reuses verbatim, the fixed-point equations (A.1)–(A.3) of which
2005's Eq. (7) is the parameterized form, the arithmetized proof-checker
(Corollary A.15), the explanation of the 5-versus-6 density constants
(Observation A.8), the statement that **concatenation is not provably total**
(Observation A.10), the fact that **scalar multiplication always was** total,
and the Added Comment reducing the working signature to four functions. Three
further items are sketch-grade (A.5, A.13, A.16).

**Outcome of pass 1v.** Eight items were re-graded against the page images,
and **three proof-status judgements taken from the OCR layer were wrong** —
Lemmas 5.1, 6.1 and 12.1 are labeled *Proof Sketch* (or "Informal Proof
Sketch"), not full proofs, and Proposition 9's is a "**Very Informal Proof
Sketch**". Since all three sketched lemmas are load-bearing (5.1 underwrites
the Δ₀ encoding, 6.1 is half of Proposition 1's proof, 12.1 is the whole
mechanism of Proposition 6), the TR's real proof-status profile is
appreciably weaker than the OCR sweep suggested. Newly verified: Proposition
8's system is `IS^{ETR}(A)` and its principle is **E-Tree Reflection**, which
decodes the title of the unlocated `[Wi94]` (gap G14); Remarks 2–7 are now
individually inventoried.

**Visual control.** The OCR layer of this witness is materially worse than a
publisher text layer — it renders `IS^{Σ₁}(A)` as `is™ (A)`, `G₀` as `Gg`,
`Lemma 6.2` as `Lemma 6,2`, and drops most superscripts. Every statement in
§§3–4 above marked `V` was read from the page image; nothing marked `O` may be
quoted in the codified statement until pass 2 verifies it. Rendering:

```bash
pdftoppm -f <collated> -l <collated> -r 135 -png \
  nachlass/papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf out/c
# collated page = printed page + 2
```


## Visual pass over the remaining pages, 2026-08-27 (closing gap G29)

Every page of the witness has now been read as an image — the 23 that had never
been rendered were PDF 1–4, 7–10, 18, 23–24, 27–29, 32–33, 35, 37–38 and the
four unnumbered **Figure pages** 58–61, which no earlier pass had covered.
The pass was not confirmatory; it produced the following.

**Drift D1 settled.** PDF 2's front matter: "A 12-page abbreviated version of
this paper, entitled *Self-Verifying Axiom Systems*, was published on **25
August 1993** in the Proceedings of the Third Kurt Gödel Symposium pp. 325–336 …
**This new longer draft has been issued as SUNY-Albany Comp. Science TR 93-10.**"
With printed p. 1's "Robert Solovay [So94] recently proved a new version of
Gödel's Incompleteness Theorem, **shortly after learning of our work**", the
order is fixed: chapter August 1993 → Solovay April 1994 → this report. The same
page gives the structure ("The formal theorem proving is only 20 pages long
(pp. 13–32)") and two grants, NSF CCR-90-60509 **and** CCR-93-02920.

**`Xtab` is defined here** (printed p. 33): "Define a **tableaux proof with
cuts** to be a deduction formalism identical to semantic tableaux, except that
it is permissible to include the sentence `Φ ∨ ¬Φ`, for any sentence `Φ`, in any
node of a proof tree" — verbatim `Willard2020`'s `Xtab`. Printed p. 36 adds a
parenthetical remark that **ZF** "can be viewed as performing deduction *with
Gentzen-style cuts*, if ZF is defined to include the axiom `Φ ∨ ¬Φ` for each
sentence `Φ`" — corroborating, but hedged and system-specific, and **not** a
stated identification of LEM-as-axioms with cuts. Drift **D42**, obligation
**O55**.

**The `Δ₀` end of `Willard2020`'s Z dial is justified here** (printed p. 35,
footnote), by adding `Φ ∨ ¬Φ` as an axiom for each `Δ₀` `Φ` and observing that
`IS(A)` already "can simulate a `Δ₀` cut rule with only a polynomial increase in
proof length", because Group-2 supplies `Prf_A(⌜Φ ∨ ¬Φ⌝) ⊃ Φ ∨ ¬Φ`. Drift
**D43**, obligation **O56**; improves gap **G22**.

**The two branches are derived on printed p. 2** from Solovay's (a)/(b) pair —
Proposition 1 relaxes the cut rule, Proposition 7 relaxes Addition-totality.
Drift **D44**, obligation **O57**.

**Further machinery recovered:**

- `IS^s_d(A,G)` (printed p. 8) parameterises by deduction method `d`, Group-1
  axioms `G` **and** the Group-3 self-verification class `s` — three of `ξ`'s
  five components, with `s` the ancestor of `SelfCons_k`'s level index.
- A **five-way reflection taxonomy** (Eqs. 3.1–3.5): local, first uniform,
  second uniform, semi-uniform, and **strong-uniform `Σ₁`**, the last written
  `∀v∀y {Prf_B(⌜Φ_v⌝,y) ⊃ Φ^y_v}` — whose bounded `Φ^y_v` is the ancestor of
  `Willard2001` Definition 1.1's `Φ^i_j`.
- **Positive Normal Form** and the **Positive(Ω)** rewrite, over **broad**
  quantifiers (printed p. 16) — the origin of 2001/2005's prenex\* convention,
  bearing on obligations O27 and O42.
- **ANCESTOR(N)**, **ancestor-consistent**, **maximal node** (printed p. 21) and
  the refined **ANCESTOR\*(N)** (printed p. 27) — the root of the
  (k,m)-Affirmative / G-good / `Normed` lineage.
- **Δ₀ cuts are safe, unbounded-quantifier cuts fatal** (printed pp. 7 and 35),
  the K-deduction threshold avant la lettre; and printed p. 7's reason cuts
  change everything — "Addition can then be employed to **implicitly define** a
  sequence that grows actually faster than `a₀, a₁, a₂ …`".
- **Every self-verifying system must breach a Hilbert–Bernays condition**, and
  "the breach … may arise because of **either** the axiom system employed **or**
  the choice of deduction method" (printed p. 1).
- **"Tangible"** already carries its philosophical sense (printed p. 30: "only
  very tangible `Δ₀` sentences"), and the same page cites **[Wi94]** for
  Propositions 8 and 9, confirming gap G14. Obligation **O58**.
- **Primary / secondary deductions** and **sharp / intermediate / `Π₁`-normalized
  contradictions** are defined on printed p. 26 — the source of 2001's
  Primary/Secondary/Tertiary terminology.
- **Figure 1** lists **twelve** tableaux rules (8 in 2001/2005, 6 elsewhere);
  **Figure 2** names the **terse proof** and the **pivotal branch**; **Figures 3
  and 4** give `INVALID.1`'s and `INVALID.2`'s inconsistency proofs. All four
  are instances of the branching-axiom construction, moving drift **D30**'s
  origin from `Willard2002a` back to this report.
- Floating-point multiplication as a total function is already noted (printed
  p. 6 footnote), so it does not originate in the unlocated ASL-2005 companion.
- `PA+` is said to carry **eight** atomic Group-1 functions where the published
  chapter lists **eleven** — a count drift; and the **transitive version of the
  Cut Elimination Theorem** (printed p. 25) is this report's form of
  `Willard2001` Lemma 7.1 / `Willard2002a` Theorem 2.2.
