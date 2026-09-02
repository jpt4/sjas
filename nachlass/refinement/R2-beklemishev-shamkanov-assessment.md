# Beklemishev–Shamkanov 2016, and what it does to the Refinement

*Gap **G38** acquired 2026-09-01:
`lit/beklemishev-shamkanov2016_abstract_g2_nonclassical_arxiv_1602.05728.pdf`
(arXiv:1602.05728v1, 18 Feb 2016; 16 pp.), the Liber Amicorum Alberti
contribution. Not the unpublished Beklemishev simplification of G7 — that one is
still unpublished — but the nearest published thing, and more useful.*

---

## 1. What the paper establishes

Beklemishev and Shamkanov build **abstract provability structures**: a
consequence relation with a `□` operator, and generalisations of Löb's
derivability conditions that make sense for logics weaker than classical. Two
results matter here.

**Gödel's argument presupposes `□`-contraction.** Contraction is
`Γ, φ, φ ⊢ ψ ⟹ Γ, φ ⊢ ψ`, which the paper glosses as "any hypothesis can be
used several times". Restricted to `□`-formulas, its admissibility is what the
G2 argument **implicitly** consumes — the paper's word is that the argument
"presupposes" it, and the point is that nobody had isolated it, not that anyone
concealed it.

A second, independent result about the same system: **uniqueness** of the
Gödelian fixed point fails there, and what uniqueness needs is the restricted
**weakening**, not the restricted contraction. That is a further property of
their toy system, not a further prerequisite of the G2 argument.

**Dropping it invalidates G2 while keeping the fixed point.** They exhibit a
modal K4 over the **contraction-free** fragment of classical logic, extended by
fixed-point operators, and prove by cut-elimination that the formalized G2
**fails** in it — with Gödelian and Henkinian fixed points existing, and
existing in infinite supply rather than uniquely.

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
  HBL terms throughout**: `Willard1993-TR` says three times that "every self
  verifying system must breach in some way one of the three fundamental
  Hilbert-Bernays conditions"; `Willard2001` Appendix A proves **Theorem A.1**, a
  version of the theorem sharpened to apply below PA; and `Willard2011` notes
  that *conventional* configurations satisfy the conditions and are "thus
  automatically inconsistent". HBL is exactly the operative frame.
- **The real content is that the breach's *location* is a design choice.**
  `Willard1993-TR`: the breach "may arise because of either the axiom system
  employed or the choice of deduction method, **but not necessarily due to
  both**" — Feferman keeps PA's axioms and breaches via the apparatus; Willard
  keeps a natural apparatus and breaches via the axioms.
- **And the specific condition differs between Willard's two branches**, which
  is why the earlier drafts kept failing: there is no single answer. The
  tableaux line (Type-A, cut-free) breaches **(2)**; the Hilbert line (Type-NS,
  modus ponens intact) breaches **(1)**. See `refined-sjas.md` §2.3, where the
  assignment is labelled as this Refinement's inference and each half is checked
  against the control that fails.
- **`□`-contraction is the abstract counterpart**, not a competitor: conditions
  (1) and (2) are both permissions to use a proof again, and contraction is that
  permission stated as a structural rule.

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
`□`-contraction. **These are the same condition described twice.** Contraction
*is* reuse; reuse is what costs. Gödel's argument must use `□φ` more than once,
and a system escapes exactly when it will not or cannot pay for the second use:

| Route | How the second use is denied |
| --- | --- |
| **Willard, tableaux line** | condition (2) is denied: the apparatus is cut-free, so combining two proofs is not internally available at bounded length |
| **Willard, Hilbert line** | condition (1) is denied: with no totality axiom, a long proof cannot be **named**, so its existence cannot be asserted |
| **Pakhomov** | it is bounded **semantically** — every finite subtheory has a finite model, so no proof reaches a refutation |
| **Beklemishev–Shamkanov** | it is denied **structurally** — the logic has no `□`-contraction |

This is a materially better statement of the essential idea than R1 had, and it
is the first version of it that is not read off Willard alone.

---

## 3. The caution this paper carries for this repository

The paper contains a warning that applies directly to work already under way
here. This repository has an affine-tree SJAS line (`docs/design/affine-tree-sjas.md`,
the C4 pilot's fidelity target; and the affine-typing phases under
`code/alsjas/`), and affine logic is contraction-free. The obvious inference —
that going affine buys the failure of G2 — **is not valid**:

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
self-verification adequate to all three routes must cover a bounded valuation
(Willard), a bounded finite model (Pakhomov), **and** a structural absence of
`□`-contraction (Beklemishev–Shamkanov) — the last of which has no size
parameter at all. Whether the three are instances of one condition, or whether
"bounded witness" and "no reuse" are genuinely two ideas that coincide in the
arithmetic case, is now the open question of this stage.

---

## Sources

- L. D. Beklemishev and D. S. Shamkanov, *Some abstract versions of Gödel's
  second incompleteness theorem based on non-classical logics*,
  [arXiv:1602.05728](https://arxiv.org/abs/1602.05728) — **acquired**.
- F. Pakhomov, *A weak set theory that proves its own consistency*,
  [arXiv:1907.00877](https://arxiv.org/abs/1907.00877) — held.
