# Chaitin's theorem, the Chaitin→Gödel proofs, and self-justification

Date: 2026-10-27

## Request (verbatim)

> Consider the proof of Chaitin's Incompleteness Theorem, and proofs of Goedel's
> Second Incompleteness Theorem as a consequence of Chaitin (e.g.
> https://arxiv.org/pdf/2302.08619) How might self-justification be
> characterized in Chaitin terms? Where does the Chaitin->Goedel proof break?

> This might also be useful:
> https://calude.net/cristianscalude/cristianAssets/pdf/IncompletenessTheHaltingProb.pdf

## Sources and reading record

All four were downloaded to the session scratchpad, not to the repository.
Each was read **as a text layer only** (`pdftotext -layout`), not as page
images. Quotations below should be checked against images before they go into
SJexM.

| Source | Pages read | Role |
| --- | --- | --- |
| D. O. Zisselman, *A proof of Gödel's incompleteness theorems using Chaitin's incompleteness theorem*, arXiv:2302.08619v1 (2023) | 1–12 (all) | the user's example; a model-theoretic Chaitin→G2 proof for T ⊇ ZFC |
| C. S. Calude, *Incompleteness and the Halting Problem*, Studia Logica (2021), doi 10.1007/s11225-021-09945-2 | all of the text layer | abstract G1/G2 from the halting problem; Kritchman–Raz as a corollary |
| S. Kritchman, R. Raz, *The Surprise Examination Paradox and the Second Incompleteness Theorem*, arXiv:1011.4974 (Notices AMS 57(11), 2010) | 1–8 (all) | the original Chaitin→G2 proof; formal version cites Kikuchi 1997 |
| T. Coquand, *Auto formalisation of Chaitin and of the surprise incompleteness Theorem*, arXiv:2606.12462 (June 2026) | 1–9 (all) | an Agda proof of KR in Basic Recursive Arithmetic; it makes the internal steps explicit |

Kikuchi 1997 (Arch. Math. Logic 36), which KR cite for their equations (1)
and (2), was **not read**.

## 1. What the Chaitin→Gödel proofs actually use

Notation: L is Chaitin's constant for T. g_L is Chaitin's program: "enumerate
T-proofs and print the x of the first proof of a sentence `K(x) > L`". m is the
number of x ≤ 2^{L+1} with K(x) > L.

KR's formal section (arXiv p. 4) is explicit. It assumes the Hilbert–Bernays
derivability conditions and two internal facts:

- **(1) internal Chaitin:** T ⊢ Con(T) → ∀x ≤ 2^{L+1} ¬Pr(⌜K(x) > L⌝).
- **(2) internal Σ₁-completeness for upper bounds:**
  T ⊢ ∀y ≤ 2^{L+1} (K(y) ≤ L → Pr(⌜K(y) ≤ L⌝)).

The induction step (items 1–7 on p. 5) also uses:

- **D1** at "m ≥ i".
- An **internal assembly** (item 3): from Pr(m ≥ i) and the r proofs
  Pr(K(y_j) ≤ L), conclude Pr(K(x) > L). That is D2/modus ponens under Pr.
- The **pigeonhole base** T ⊢ m ≥ 1.

Coquand's Agda version of KR makes the same steps concrete. It runs in Guard's
Basic Recursive Arithmetic and replaces counting with a sorites descent S(0) →
… → S(N). The introduction says "Skolem arithmetic". From context that
presumably means Skolem's 1923 free-variable recursive arithmetic, not the
decidable theory of multiplication.

- Chaitin becomes an object-level proof transformer G with no consistency
  premise, (C): th(w) = code(K(out(w)) > L*) ⊃ th(G·w) = code(0 = 1).
- Con_T is used exactly once per grain (Step 6).
- **The one non-classical piece** is a Gandy–Howard majorant: a closed term
  `fuel(f)` that bounds the run length of the internal interpreter. Coquand
  writes: "Without this majorisation there would simply be no term to put
  there" (§7.3).
- §2 adds that g_L is short only because "the length of the computation is not
  taken into account in its size … program size and running time are
  independent complexity measures."

Zisselman relocates the same steps into model theory:

- The proof takes a model M₀ ⊨ T. In each M_i it applies Chaitin internally, then uses
  M_i ⊨ Con(T + L ≥ Kol(x_i)) to build M_{i+1} as a set of M_i. That step is the
  arithmetized completeness theorem.
- It pushes the earlier witnesses forward along initial-segment embeddings
  (§4.1–4.1.2).
- At depth L + 2 it gets L + 2 distinct numbers below L.
- Two internal steps are left implicit:
  - **Chaitin's theorem holding inside M_i.** The paper's proof of Chaitin (§3.1.2)
    argues by *soundness* ("as L ≤ Kol(x′) it follows …"). M_i has no access to
    that. Inside M_i the step must use Con(T) plus formalized Σ₁-completeness,
    which is KR's (1) again.
  - **The Henkin construction inside M_i.**
- The paper's own Remark 3 after §4.1.1 notes that absoluteness of computation needs
  "induction on ω(V₁)": an induction over run length.
- The paper's claim of "no fixed point theorem" is true. Read as "no
  derivability conditions", it would be false.

Calude's abstract Theorem 5 has the hypotheses: consistent, sound and
*reciprocally sound* for True, Prov₁ not computable.

- Reciprocal soundness is external Σ₁-completeness on the class P₂. As printed,
  Definition 3 reads "for every S ∈ True we have ⊢ ¬S", which is evidently a
  misprint for "for every S ∈ P₁ with ¬S ∈ True".
- Every listed hypothesis is **external**. The proof's last paragraph carries
  the internal step: "Using the consistency of T and the fact that T is rich
  enough, we deduce that T proves that M_n has at least 2 elements".
- Footnote 2 promises that "the minimal amount of arithmetic required will be
  clear in each case". For Theorem 5 it never is.

**Summary.** Removing the fixed point from the proof of G2 does not remove the
derivability conditions. KR, Coquand and Zisselman all need the theory to
*internally* turn a halting run into a proof (the "knowing" step). Calude hides
it in "rich enough".

## 2. Self-justification in Chaitin terms (ours)

Split Chaitin's situation into four capacities of T at the constant L:

| | external (T does it) | internal (T proves it does it) |
| --- | --- | --- |
| **blindness** | T proves no `K(x) > L` (Chaitin's theorem) | T ⊢ ∀x ¬Pr(⌜K(x) > L⌝), or Con(T) where the argument consumes it |
| **sight** | T proves every true `K(y) ≤ L` (exhibit the program and run it) | KR's (2): T ⊢ K(y) ≤ L → Pr(⌜K(y) ≤ L⌝) |

- **External blindness** follows from consistency plus external sight. It is a
  G1-type fact.
- **KR as a theorem about these capacities:** internal blindness + internal
  sight + pigeonhole + internal assembly ⇒ inconsistency. The surprise
  examination (or the sorites) is the engine that turns them into a
  contradiction.
- **Self-justification in Willard's sense**, characterized in these terms, is a
  system that has external blindness, external sight and internal blindness,
  and lacks internal sight (or assembly). It knows that it cannot certify
  complexity. It does not know that it can certify simplicity. This is the
  SJexM split between *watching* and *knowing that one watches*, transposed
  into Chaitin's setting.

**Chaitin-reflexivity.** Call T *Chaitin-reflexive at L* if
T ⊢ ∀x ¬Pr_T(⌜K(x) > L⌝).

- Over a base with D1–D3, Chaitin-reflexivity at L_T is equivalent to
  T ⊢ Con(T):
  - (→) by explosion;
  - (←) by (1).
- In strong theories, then, "self-justification" and "T proves its own Chaitin
  bound" are the same thing. KR says that no such theory exists.
- In Willard's systems the two notions come apart:
  - IS_D(A) proves Level(1) consistency. At the Π₁ class {`K(x) > L`} that is
    ∀x ¬(Pr(K(x) > L) ∧ Pr(K(x) ≤ L)): "I never both certify and refute a
    complexity claim".
  - That is weaker than Chaitin-reflexivity.
  - The gap between them is exactly internal sight at the run of g_L.

**Information content.**

- Con(T) is a single Π₁ sentence of complexity ≤ |T| + c. Chaitin's bound
  therefore gives no reason why T cannot prove it.
- In Ω terms, Con(T) is the halting bit of one program of length ≈ |T| + c. It
  is not a run of Ω's bits.
- G2 is not information-theoretic in Chaitin's sense. What KR shows is that
  Con(T) *amplifies* when it is paired with self-simulation. The descent
  converts one trusted bit into certified incompressibility, one grain at a
  time.
- Without self-simulation, Con(T) is inert: it is one more true Π₁ axiom.
  SJexM §"The Price Of Faith" puts this as "A universal axiom constrains. An
  existential axiom supplies a number."

**Calude's Lemma 1** (knowing how many inputs halt decides which ones halt) is
KR's step "m = i ⇒ T can identify the incompressible strings". Internalizing
Lemma 1 means T proving that waiting for the others to show up *terminates
with proofs in hand*. That is internal sight again.

## 3. Where the proof breaks for a self-justifying system

Configuration: Willard 2005 `IS_D(A)`.

- **Language:** U-Grounding with `+` and `Double`.
- **D:** tableaux / Tab-U\*₁-List.
- **Consistency:** Level(1).
- **Proof predicate:** the Group-3 `Prf_D`.
- **Encoding:** Appendix A (Eq. 20).
- **Base hypothesis:** A's Π\*₁ theorems are true.

This is SJexM's Theorem D(b) configuration. "Consistent" below means
consistent relative to the metatheory, by Willard 2005 Thms 3/5 (registry
status full).

1. **Not at Chaitin's theorem.** It is external. IS_D(A) watches every halting
   computation, so it has external sight. It is consistent. It therefore proves
   no `K(x) > L` for large L.
2. **Not at self-reference.** KR's proof has no fixed point. In
   self-justification the self-reference sits in the *axiom* (Group-3), not in
   the proof, and the Chaitin route offers no way around it. This corroborates
   SJexM's "Taking Off The Glasses": self-reference is cheap and
   self-simulation is the cost. Remove the fixed point entirely and G2 still
   stands or falls with self-simulation.
3. **Not at internal blindness.** At Level(1), Group-3 applies directly to the
   pair (`K(x′) > L`, its negation). KR's (1) then reduces to one instance of
   (2), at the output x′ of g_L. One caveat: the code-sensitivity issue in
   Willard's `Pair` (memory: sjas-claim-granularity) recurs. The sentence that
   watching proves must be the one `Pair` matches with `K(x′) > L`.
4. **It breaks at internal sight**, and harder than the Gödel route does.
   - To prove (1), T must show: for every proof w′ of some `K(x′) > L`, there
     is a proof of `K(x′) ≤ L`.
   - That proof certifies the run of g_L, which enumerated all w′ candidates
     before finding w′.
   - With trace-based K (the natural reading, since K quantifies over all
     programs through a universal machine), the trace has at least w′ steps.
     Its code is therefore ≥ 2^{w′}, assuming at least one bit per step.
   - By SJexM's witness bound and transcript lemma, any tableau proof of that
     existential has code ≥ (trace)^4. So the faith step is
     **w′ ↦ ~2^{4w′}: exponential**.
   - The Gödel route's step (2) exhibits s and checks it. Its cost is
     **s ↦ > s⁵: polynomial**. Willard 2005 Lemma 3 (status: sketch) says that
     checking a proof is cheap.
   - On this reading, the Chaitin route needs *strictly more* faith than the
     Gödel route.
   - Coquand's Gandy–Howard `fuel` term marks the spot, but it is not the
     whole obstacle (refined 2026-10-28):
     - For g_L the step count is only mildly superlinear in w′.
     - The cost lies in what the fuel indexes. BRA has the iterate stepⁿ as a
       primitive recursive term. Willard's language has no iteration and must
       code a run as a trace, which is exponential in the run's length.
     - Closed terms in Willard's language are linear in their arguments
       (SJexM, linear witnesses). So it has neither a general fuel majorant
       nor the iterate.
   - Willard 2005 Thm 6 is the faith bought. There, Υ(k,m) supplies one large
     number, not a uniform function.
5. **It also breaks at internal assembly** (KR item 3). Combining given proofs
   into a proof of `K(x) > L` costs product-sized codes on a Hilbert line, and
   cut on a tableau line. Willard proves neither total.
6. **Zisselman's route breaks earlier still.** It needs the arithmetized
   completeness theorem inside M_i. Building a Henkin model presupposes growth
   that Willard's systems give up. This is our estimate, not a cited
   theorem.
7. **Calude's Theorem 5 as printed.** IS_D(A) appears to satisfy every external
   hypothesis:
   - consistent;
   - sound for Π₁ sentences, since its axioms are true;
   - reciprocally sound for N(P,v), by SJexM's watching lemma;
   - Prov₁ not computable, by the standard recursive-inseparability argument.
     *That argument was not checked at Willard's granularity.*

   IS_D(A) also proves the Level(1) consistency that Calude's argument uses
   (only P₁ sentences enter, through Lemma 3). So Theorem 5 is correct only if
   "rich enough" includes *internal* reciprocal soundness. Willard's systems
   show the gap: the stated hypotheses are not sufficient.

**Precision caveat.** SJexM's Proposition (linear witnesses) shows that IS_D(A)
cannot prove *uniform* formalized Σ₁-completeness for runs. Its antecedents
hold in ℕ for unboundedly long runs, and transcripts outgrow every linear
bound. It does **not** directly show that IS_D(A) fails to prove KR's (2) or
(1):

- (2) concerns the finitely many programs shorter than L. For standard runs its
  witnesses are standard numbers.
- (1)'s antecedent is never satisfied in ℕ.

So the established claim is about the *natural proofs* of (1) and (2). The
claim about the sentences themselves is open (item (i) below). What does
follow is the contrapositive:

**KR run backwards** (ours; the analogue of SJexM Prop. E). Under the
configuration above, IS_D(A) is consistent and proves its Level(1)
consistency. It therefore cannot prove *all* of the following:

- (2), in the `Pair`-compatible form;
- the item-3 assembly;
- D1 at "m ≥ i";
- the pigeonhole base.

Status: a paper argument at G2 granularity. It has not been checked against
Willard's Δ\*₀ machinery or the encoding.

**The surprise examination in a self-justifying world** (ours). KR resolve the
paradox by noting that the students cannot prove Con(T, S). Willard's students
*can*: it is an axiom. The exam can still fall on any day. The reason is that
the students cannot afford to watch themselves deduce the previous night's
conclusion. They trust themselves, but they cannot simulate themselves.

## 4. Open items and conjectures

- (i) **Does IS_D(A) prove KR's (2)?** A sketch for "no":
  - Take a model M of PA + true Π₁ + Level(1)-Con(IS_D(A)) in which a short,
    truly non-halting program halts at a nonstandard time s.
  - Cut M at an initial segment that is closed under Willard's function
    symbols, contains the trace, and excludes its transcript.
  - Groups 0–3 are Π₁ and survive the cut. (2) fails in the segment.

  Unverified points: the formalization of halting (trace versus
  search-success; SJexM analysis note, point 4), and whether the transcript
  bound holds inside M.
- (ii) **A Chaitin-form Group-3.** Take the fixed point σ ↔ "no D-proof from
  T₀ + σ of any `K(x) > L̄`".
  - Externally, given watching, σ is true iff T₀ + σ is consistent: the same
    status as Willard's Group-3.
  - Over bases containing PA as a theory, T₀ + σ proves Con(T₀ + σ) by
    explosion, so G2 makes it inconsistent.
  - Over Willard bases the question is open. Willard's minimality argument does
    not transfer directly: a shorter proof of `K(x) > L` is not a shorter proof
    of 0 = 1, and the conversion G is exponential.
- (iii) **A self-reference-free proof of Willard 2005 Thm 6.** In
  NS^{k,m}_D(PA+), Υ(k,m) supplies the fuel. Bounded internal Σ₁-completeness
  is a Π₁ theorem of PA+, inherited through Group-2. The KR descent might then
  run for k large without ℧'s "no short proof" self-reference.
  - Rough sizes: the descent has 2^{L+1} grains. Proof codes grow about like
    c_{i+1} ≥ c_i⁵, from D1 numerals. The total stays under a tower of fixed
    height in L.
  - Worth checking, because SJexM counts Thm 6 as a sketch: its third step
    pairs the wrong sentence. The KR route has the same `Pair` exposure, but at
    a different sentence.
- (iv) **The formalization of K.** Trace-based K makes internal Chaitin
  exponential. A Σ₁-definitional (Berry/Boolos-style) complexity would certify
  g_L's output by the found proof w′ itself. The cost would then move into
  certifying *minimality* (∀v < w′, a bounded universal over w′ cases, or a Π₁
  lemma). Which formalization is cheapest is a configuration question in the
  sense of sjas-claim-granularity.
- (v) **The lemma dial.** Syntactically, "m ≥ i" is ∃(bounded)∀: Σ₂\*-shaped.
  It is Π₁ only after collection. Using it as a lemma in a Tab-List proof may
  move the argument onto the rung where Willard 2004 Thm 2 (status: sketch)
  defeats Type-A systems. The class must be fixed before any claim is made.
- (vi) **Essay.** If wanted, SJexM's "Refactoring" section could take a short
  "Chaitin's machine" paragraph. Its point would be that g_L's transcript is
  exponential where P's is polynomial, so Chaitin→Gödel is the more expensive
  faith.
