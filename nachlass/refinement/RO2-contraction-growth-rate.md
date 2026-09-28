# RO2 — the linear-sum currency: Beklemishev–Shamkanov, Willard, and the `!` conjecture

*Refinement obligation. Opened 2026-09-07 as a research query; **substantially
revised the same day** after the B–S image verification, which refuted its
opening premise and found that the authors state the target themselves.
Attached to component **R5**. Origin:
[`R6-g2-via-lawvere.md`](R6-g2-via-lawvere.md) §7.4.*

> **Status.** §§1–3 are image-verified against held witnesses. §4's programme is
> **open**, and is B–S's own stated conjecture, not this stage's invention.
> Two light-logic witnesses are now held (§5); Girard–Scedrov–Scott 1992,
> Girard 1998 and Lafont 2004 are **not**, and nothing below depends on them.

---

## 1. What the first draft got wrong

The 2026-09-07 opening draft asserted that Beklemishev–Shamkanov "give the
structural fact" and that "their statement is structural and carries no size
claim", making the connection to growth rates "a research step, not a citation".

**That is false, and the counterexample is the main theorem of their §5.**
Printed p. 11, image-verified — with `‖π‖` defined on the same page as "the
number of nodes in `π`":

> **Theorem 5.** "The cut rule … is admissible for `S`. Moreover, if `π₁` and
> `π₂` are proofs of the premises of (cut), then the conclusion of (cut) has a
> proof with the size being **less than `‖π₁‖ + ‖π₂‖`**."

So B–S's calculus has cut elimination with a **strict linear-sum size bound**.
The quantitative statement RO2 was opened to ask for is already there.

## 2. And it is the same bound Willard names as the danger

`Willard2020` printed p. 279, image-verified:

> "modus ponens assures that a proof of a theorem `ψ` from an axiom system `α`
> has a length **no greater than the sum** of the proof-lengths needed to derive
> `φ` and `φ → ψ` from `α`. This **"Linear-Sum Effect"** does not apply,
> actually, also to *Tab*-deduction because it owns no analog of a modus ponens
> rule"

`Xtab` recovers it via the `φ ∨ ¬φ` node, and Willard's statement ⊙ then makes
any `A` using Xtab deduction "automatically inconsistent" given successor
totality, the ring properties as 3-way relations, and an added self-consistency
theorem.

**The two results are in the same currency after all**, and they point opposite
ways:

| | linear-sum composition | verdict |
| --- | --- | --- |
| **B–S `S`** | Theorem 5: cut elimination is `< ‖π₁‖+‖π₂‖` | formalized G2 **fails**; `S` is consistent |
| **Willard `Xtab`** | p. 279: `Xtab` recovers the Linear-Sum Effect | with I+II+III of ⊙, **inconsistent** |

The difference is arithmetic. ⊙ needs successor totality and the ring
properties; `S` has no arithmetic at all — it is a modal K4 over a
contraction-free base. **So linear-sum composition is not by itself fatal;
linear-sum composition together with enough arithmetic and a self-consistency
axiom is.** That is the sharpest form this comparison has reached, and it
relocates the open question.

## 3. What B–S actually lack, in their own words

Printed p. 14, image-verified:

> "We remark that the system `S` does not provide a counterexample to the
> non-formalized version of G2, since `⇒ ¬□⊥` is not provable."

and, on the same page:

> "For one such system, considered by the second author of this paper, the rule
> of `□`-contraction is admissible, which according to our results still yields
> G2. **Thus, we are still missing convincing examples of mathematical theories
> based on weak logics for which G2 would fail.**"

The first sentence is why `S` has no internal consistency point. The second is
why contraction-freedom *at the object level* is not enough — the second author's
contraction-free arithmetic still admits `□`-contraction. Both were recorded in
this stage before verification and both check out.

## 4. The programme: B–S's own conjecture, which is this stage's target

The closing paragraph of their paper, printed p. 14, image-verified:

> "We believe that such a counterexample can be constructed by **extending the
> language of `S` by an operator similar to `!` from linear logic and adding to
> `S` a fixed point of the form `a = ◇!a`**. However, a confirmation of this
> hypothesis is left for future work."

**This is the objective, stated by the authors.** Three consequences for how it
should be pursued here.

1. **The formulation "adjoin `Con_S` to `S` and ask whether cut elimination
   survives" is the wrong move**, and this document previously implied it.
   Adjoining `⇒ ¬□⊥` as an axiom is not what B–S propose; they propose changing
   the *language* so that a fixed point `a = ◇!a` exists. The distinction
   matters because Theorem 1(i)'s collapse argument (p. 5, image-verified —
   "Assume `⊠⊤ ≤ ⊥` … Therefore, by C2, `⊤ ≤ ⊠⊥ ≤ ⊥`") shows that for an **APS**
   with a Gödelian fixed point, adjoining consistency *is* collapse. `S` escapes
   only because it is not an APS: Proposition 3.8 needs contraction to establish
   condition C3.

2. **So the target is: does `S + !` with `a = ◇!a` have a consistency point and
   remain consistent?** If yes, it is the first "convincing example of a
   mathematical theory based on a weak logic for which G2 fails" — the thing
   their own §6 says the field is missing.

3. **The primary objective for this programme** (user direction, 2026-09-07) is
   a **native formulation in a light/bounded logic capturing `Π₁`-consistency of
   Willard's SJAS or an equivalent**. Note what Willard's own boundary says about
   that target: `Willard2020` §7's `Z`-dial has `Δ*₀` evasions surviving, `Π*₂`
   and above failing, and **`Π*₁` conjectured to survive — "this fact has not yet
   been formally proven"**. So the `Π₁` target is an open problem on the Willard
   side as well as the B–S side, and a light-logic formulation would be
   attacking both at once.

**Deferred, not forgotten** (user direction): the two gradings — Willard's
`Level(n)`, which grades self-consistency by formula complexity, and a
use-budget grading in the manner of BLL's indexed `!` — are independent and may
compose. A `Level(1)` self-consistency axiom carrying a use budget is worth
stating once the native formulation exists. It is not the first objective.

## 5. Witnesses

**Held** (`lit/`, hashes in `SHA256SUMS`):

- Beklemishev and Shamkanov, arXiv:1602.05728 — pp. 5, 7, 8, 11, 14 imaged.
- Dal Lago and Hofmann, *Bounded Linear Logic, Revisited*, LMCS 6(4:7) 2010,
  [arXiv:0904.2675](https://arxiv.org/abs/0904.2675). Held as the accessible
  route to BLL's indexed `!`.
- Asperti and Roversi, *Light Affine Logic (Proof Nets, Programming Notation,
  P-Time Correctness and Completeness)*,
  [arXiv:cs/0006010](https://arxiv.org/abs/cs/0006010). Light **Affine** Logic
  keeps weakening and drops contraction, which is the B–S structural setting,
  and has "a polynomially costing cut elimination".

**Not held, and no claim in this document rests on them:**

- Girard, Scedrov and Scott, *Bounded linear logic: a modular approach to
  polynomial-time computability*, Theoretical Computer Science 97(1) (1992)
  1–66, DOI 10.1016/0304-3975(92)90386-T. A UPenn technical-report version
  (MS-CIS-91-59) is reported to exist; not retrieved.
- Girard, *Light Linear Logic*, Information and Computation 143(2) (1998)
  175–204, DOI 10.1006/inco.1998.2700.
- Lafont, *Soft linear logic and polynomial time*, Theoretical Computer Science
  318 (2004) 163–180, DOI 10.1016/j.tcs.2003.10.018.
- A witness for the non-elementary cut-elimination lower bound (Statman,
  Orevkov). **The first draft of this document leaned on that bound; nothing
  here does now.**
- Grishin's contraction-free set theory and Restall, *An Introduction to
  Substructural Logics*, ch. 11 — both cited by B–S §6 and worth acquiring for
  the contraction-free-arithmetic remark.

## 6. What this must not be taken to claim

- **No claim** that Theorem 5's linear-sum bound and Willard's Linear-Sum Effect
  are the *same theorem*. They are the same bound in two calculi, pointing
  opposite ways, and §2 says why the difference is arithmetic.
- **No claim** that `S + !` with `a = ◇!a` is consistent, or that it has a
  consistency point. That is the open conjecture, and it is B–S's.
- **No claim** about the light-logic systems beyond what the two held witnesses
  state.
- **No claim** that `S` is an arithmetic theory (it is not) or that it evades
  non-formalized G2 (their p. 14 says it does not).
