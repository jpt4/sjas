# Beklemishev–Shamkanov 2016, and what it does to the Refinement

*Gap **G38** acquired 2026-09-01:
`lit/beklemishev-shamkanov2016_abstract_g2_nonclassical_arxiv_1602.05728.pdf`
(arXiv:1602.05728v1, 18 Feb 2016; 16 pp.), the Liber Amicorum Alberti
contribution. Not the unpublished Beklemishev simplification of G7 — that one
is still unpublished — but the nearest published thing, and more useful.*

---

## 1. What the paper establishes

Beklemishev and Shamkanov build **abstract provability structures**: a
consequence relation with a `□` operator, and generalisations of Löb's
derivability conditions that make sense for logics weaker than classical. Two
results matter here.

**Gödel's argument implicitly consumes contraction.** Definition 3.7 (p. 8,
image-verified) gives the *plain* rules:

> - satisfies **contraction** if `Γ, φ, φ ⊢ ψ` implies `Γ, φ ⊢ ψ`;
> - satisfies **weakening** if `Γ ⊢ ψ` implies `Γ, φ ⊢ ψ`, for any `φ`.

with the gloss that contraction "intuitively means that any hypothesis can be
used several times in a derivation", and the verdict: "**a certain amount of
contraction is essential for the proof of G2.**"

Two hypotheses, not one, and they do different work:

- **Theorem 3** (their G2): `S` satisfies **contraction** and `□` satisfies
  Löb's conditions.
- **Theorem 4** (uniqueness of the Gödelian fixed point): `S` satisfies
  **contraction *and* weakening** and `□` satisfies Löb's conditions; then all
  Gödelian fixed points are equivalent to `⊠⊤ =_S □⊥`.

Weakening is what uniqueness needs *in addition*; contraction is required by
both theorems. The **Introduction**, p. 2 — not the abstract, which mentions
neither uniqueness nor weakening — says "Moreover, the uniqueness of Gödelian
fixed point is based on the similarly restricted form of weakening": its
"Moreover" names an increment, not an exclusive. Note also that Theorem 4's
conclusion is `⊠⊤ =_S □⊥`; the text layer strips the `□`.

**Plain versus `□`-restricted.** Theorems 3 and 4 are stated with the *plain*
rules. **Remark 3.9** then weakens them: plain contraction and weakening "are
somewhat excessive requirements", and

> - satisfies **`□`-contraction** if `Γ, □φ, □φ ⊢ ψ` implies `Γ, □φ ⊢ ψ`;
> - satisfies **`□`-weakening** if `Γ ⊢ φ` implies `Γ, □ψ ⊢ φ`, for any `ψ`.

together with `C3′` and `C5′`, suffice for more general versions of both. Note
the asymmetry the plain forms do not have: `□`-weakening may adjoin only a
**boxed** formula. Earlier drafts of this document used "contraction" and
"`□`-contraction" interchangeably; the distinction is exactly what §3's caution
turns on, so it is kept sharp from here.

**Dropping it invalidates G2 while keeping the fixed point.** They exhibit a
modal K4 over the **contraction-free** fragment of classical logic, extended by
fixed-point operators, and prove by cut-elimination that the **formalized** G2
fails in it — with Gödelian and Henkinian fixed points existing, and existing in
infinite supply rather than uniquely.

**But `S` is not self-verifying, and the paper says so.** §6, p. 14
(image-verified at 200 dpi):

> "We remark that the system `S` does not provide a counterexample to the
> non-formalized version of G2, since `⇒ ¬□⊥` is not provable."

and, earlier in the same section:

> "Thus, we are still missing convincing examples of mathematical theories based
> on weak logics for which G2 would fail."

So this route **breaks the argument without obtaining the object**. That is a
real result — it isolates a prerequisite nobody had isolated — but it is not a
third self-verifying system, and §2.3 and `R3-the-margin.md` §5 are corrected
where they treated it as one.

They call the example a "(toy)" system, and that qualification is theirs, not a
softening added here.

---

## 2. The effect on R1: one correction and one confirmation

### 2.1 It corrects `refined-sjas.md` §2 — and so does Pakhomov

R1's §2 said Willard's systems "keep the diagonal and lose the derivability
conditions, and in particular `□φ → □□φ`", i.e. **D3**.

The numbering was right — D3 *is* `□φ → □□φ`, exactly as Pakhomov lists the
three conditions, and as Willard himself lists them as `Willard2001`
Theorem A.1's conditions (1)–(3). What was wrong was the attribution, and then
the first attempt to repair it:

- **The condition named was wrong, but so was the correction.** A first repair
  claimed "no derivability condition is the pivot" and pointed at Pudlák's
  cut-shortening instead. That is also wrong. **Willard frames his own work in
  HBL terms throughout**: `Willard1993-TR` states the point on **four** pages —
  printed pp. 1, 2, 6 and 12, checked page by page — most fully at p. 1, that
  "every self
  verifying system must breach in some way one of the three fundamental
  Hilbert-Bernays conditions"; `Willard2001` Appendix A **states** (does not prove — the row is
  `stated-only`; "no formal proof given", footnote 16 arguing only that the
  classic proof needs no more than `Π^-₁` capacity) **Theorem A.1**, a version
  of the theorem sharpened to apply below PA; and `Willard2011` notes
  that *conventional* configurations satisfy the conditions and are "thus
  automatically inconsistent". HBL is exactly the operative frame.
- **The real content is that the breach's *location* is a design choice.**
  `Willard1993-TR`: the breach "may arise because of either the axiom system
  employed or the choice of deduction method, **but not necessarily due to
  both**" — Feferman keeps PA's axioms and breaches via the apparatus; Willard
  keeps a natural apparatus and breaches via the axioms.
- **Willard names the condition himself, for one branch.** `Willard1993-TR`
  printed p. 12 (image-verified) identifies **condition (2)** as what `IS(A)`
  breaches, and does so as a *uniformity* failure — the implication holds "only
  in the *degenerate case* where `x` and `y` are *fixed constants*". This is
  recorded at `../codification/extraction/willard1993-tr.md` §3.7c. The Hilbert
  line's condition is **not** stated in the corpus, and is not (1). See
  `refined-sjas.md` §§2.3–2.3a.
- **Willard also has a route that bypasses HBL entirely**, which bears on how
  much weight this frame can carry: `Willard2001` Lemma 7.1 "replaces the
  Hilbert-Bernays derivability conditions with a semantic argument", which is
  why that paper's §7 theorems need no arithmetic inside `α`.
- **`□`-contraction is not a restatement of a derivability condition**, and
  conditions (1) and (2) are not both "permissions to use a proof again".
  (1) internalises a proof once; (2) composes two
  *different* proofs; **reuse is (3)**, `Der(⌜Φ⌝) ⊃ Der(⌜Der(⌜Φ⌝)⌝)`.
  Contraction is a rule of the *ambient consequence relation*, lying underneath
  all three, and Proposition 3.8 consumes it on the **context** `Γ`, not on a
  proof. It is a distinct prerequisite, which is the whole point of §2.3.

§2 is rewritten around Willard's own frame rather than around whichever paper
was read most recently.

### 2.2 It confirms R1's central thesis, from an independent direction

R1's §2 claim is that **the diagonal is not the obstacle** — that G2 needs
something beyond the fixed point, and that the something is a resource.

Beklemishev–Shamkanov establish exactly the first half by construction, at the
level of abstract consequence relations, with no arithmetic at all: a system
with Gödelian fixed points and no G2. That is the strongest available
confirmation that self-reference is not what does the work, and it was arrived
at along a route with nothing in common with Willard's.

### 2.3 It supplies the unification R1 was missing

R1's revised claim (after Pakhomov) is that a system is self-verifying when it
can construct, within its own means, a **bounded semantic witness** that a given
proof is not a refutation. That is a statement about *cost*.

Beklemishev–Shamkanov's is a statement about *structure*: the argument needs
`□`-contraction.

**They are not "the same condition described twice"**, and their own proof
shows why. Contraction is consumed at one identifiable step of Proposition 3.8
— the step from **`Γ ⊢ □φ` and `Γ ⊢ □¬φ`** to **`Γ ⊢ □¬⊤`**, which reuses the
**context**, not a proof. Under `⊠φ := □(φ → ⊥)`, `□¬⊤` *is* `⊠⊤`, the
**formalized** inconsistency assertion — which is why dropping contraction
costs only *formalized* G2. It is a rule of the ambient consequence relation,
more elementary than Löb's conditions and lying underneath them. Nor is it a
restatement of any single condition: **reuse, among the three, is condition
(3)** — `Der(⌜Φ⌝) ⊃ Der(⌜Der(⌜Φ⌝)⌝)`, the one that reflects a proof upward.
(1) internalises once; (2) composes two *different* proofs.

What survives is weaker and still worth having: **G2's argument is
over-determined**, and the literature now removes its prerequisites
independently.

| Route | Which prerequisite is removed | Self-verifying theory obtained? |
| --- | --- | --- |
| **Willard, tableaux line** | a **derivability condition** — Willard names **(2)** for `IS(A)` at TR p. 12, as a uniformity failure: the apparatus is cut-free, so the implication holds only for fixed constants | **yes** |
| **Willard, Hilbert line** | a derivability condition, **not identified in the corpus**. With no totality axiom a long proof cannot be named; whether that lands on (1), (2) or (3) is this Refinement's open question, not Willard's statement | **yes** |
| **Pakhomov** | the **semantic** step — every finite subtheory has a finite model. Note `H_{<ω}` *also* cannot prove successor totality, so it shares Willard's evasion of Pudlák | **yes** |
| **Beklemishev–Shamkanov** | the **structural** licence — no `□`-contraction, so a context proving `φ` and `¬φ` is not thereby inconsistent | **no** — `⇒ ¬□⊥` is not provable in `S` (§6, p. 14) |

Two things follow.

**Breaking the argument is necessary, not sufficient.** The fourth row is a case
where G2's argument fails and self-verification does not follow. A theory must
additionally *prove* `Con`, which is a positive obligation on its own axioms.
"Self-verification = failure of G2's argument" is therefore a biconditional this
document does not assert.

**The catalogue is not a criterion.** These are independent failure points, not
values of one parameter. Whether a single condition subsumes them is R3's
problem, and `R3-the-margin.md` §5 answers it in the negative — with the roster
above, corrected.

---

## 3. The caution this paper carries for this repository

The paper contains a warning that applies directly to work already under way
here. This repository has an affine-tree SJAS line, and affine logic is
contraction-free.

*Provenance note.* That line was previously identified with
`docs/design/affine-tree-sjas.md`, which **does not exist** — the design
document was an untracked working-tree file, recorded as such in the
Codification plan's hygiene flag, and is not present now. It also cited
`code/alsjas/`, which exists but is **untracked**, so it is not part of any
committed state this document can point a reader at. The caution below
therefore applies to a line of work the repository does not currently hold in
tracked form; it is recorded against **RO1** so that whoever revives that line
meets it. The obvious inference — that going affine buys the failure of G2 —
**is not valid**:

> "The property of `□`-contraction actually holds for some meaningful
> arithmetical systems **lacking general contraction rule**, for example, for a
> version of Peano arithmetic based on **affine predicate logic** considered by
> the second author of this paper (as yet, unpublished)."

So a system may be affine at the object level and still admit contraction **on
boxed formulas**, which is all G2 requires. Affineness is not sufficient; what
must be checked is the restricted rule. Any claim in the affine-tree line that
dropping contraction evades G2 needs to establish `□`-contraction fails, not
merely that general contraction does.

Recorded as obligation **RO1** for the computational-analogue component (R4).

---

## 4. What remains open

**G7 stays closed and unacquirable.** This paper is Beklemishev on abstract G2;
it is *not* the simplification of Willard's construction, which its own
co-author's later collaborator (Pakhomov, 2019) still describes as unpublished.

**The toy status is real.** The contraction-free K4 system is offered as a toy.
It shows the *possibility* of a fixed-point-carrying system without G2; it does
not exhibit an arithmetic of independent interest. Willard's and Pakhomov's
systems do the latter and are correspondingly harder.

**R3 is now the right next piece and is better specified again.** A definition of
self-verification must cover the two witness kinds that actually deliver
self-verifying theories — a bounded valuation (Willard) and a bounded finite
model (Pakhomov) — while explaining why the third route, which removes a
prerequisite with **no size parameter at all**, does not deliver one. Whether
"bounded witness" and "no reuse" are one idea or two that coincide in the
arithmetic case is the open question of this stage.

*Answered by R3, 2026-09-02:* they are two. `R3-the-margin.md` §5 gives the
margin as the invariant for Willard's cell alone, shows Pakhomov shares his
evasion of Pudlák by a different witness kind, and records that
Beklemishev–Shamkanov's cell is **empty of a theory** — which their own §6
names as the open problem.

---

## Sources

- L. D. Beklemishev and D. S. Shamkanov, *Some abstract versions of Gödel's
  second incompleteness theorem based on non-classical logics*,
  [arXiv:1602.05728](https://arxiv.org/abs/1602.05728) — **acquired**.
- F. Pakhomov, *A weak set theory that proves its own consistency*,
  [arXiv:1907.00877](https://arxiv.org/abs/1907.00877) — held.
