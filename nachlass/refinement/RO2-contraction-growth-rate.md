# RO2 — a quantitative counterpart to Beklemishev–Shamkanov's contraction result

*Refinement obligation, opened 2026-09-07. Attached to component **R5** (the
secondary-literature pass) because what it needs first is acquisitions. Origin:
the R6 thread on proof-code growth,
[`R6-g2-via-lawvere.md`](R6-g2-via-lawvere.md) §7.4.*

> **Status: a research query, not a finding.** Nothing in §2 is verified in this
> stage. **No light-linear-logic witness is held in this repository**, and the
> cut-elimination lower bound §2 leans on is recalled, not read. The query is
> written down here so that it stops accumulating as unwitnessed conjecture
> inside documents that are making other claims.

---

## 1. Why the query arises

R6 §7.4 established, from page images, that Willard's obstruction to uniform
proof composition is **quantitative**: the glued proof's Gödel code is bounded by
the product of the two input codes, `64·⌜¬⊥⌝·t·p`, and `IS(PA+)` can discharge
that bound only once one factor is a constant, which turns the product into a
scalar multiple `kp = p+p+…+p`.

Beklemishev–Shamkanov block the same arrow by a different route. Their `S`
satisfies L1–L3 and lacks `□`-contraction; the formalized-G2 derivation consumes
contraction at one step, and their Proposition 3.8 is where it is consumed
([`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md)
§§1 and 2.3, which also records that `□`-contraction is a rule of the *ambient
consequence relation* rather than a restatement of a derivability condition).
Their statement is **structural** and carries no size claim.

So two results about the same arrow are stated in incommensurable currencies:
one in bits of Gödel number, one in structural rules.

## 2. The query

Contraction is the only case of cut elimination that **duplicates a
subderivation** — the axiom, logical and commutative cases are size-linear, and
it is the contraction case that forces a cut to be pushed up both branches with
the subderivation copied. That is the standard source of the non-elementary
blow-up, and its absence is what makes normalization polynomial in linear and
affine calculi. If that is right, a structural prerequisite ought to have a size
statement behind it.

> **Q1.** Can Beklemishev–Shamkanov's Proposition 3.8 / Theorem 3 be refined into
> a statement about **derivation size** — that the formalized-G2 arrow would
> require a derivation super-additive in its premises, which an affine
> consequence relation cannot produce?
>
> **Q2.** Dually: does a `□` carrying an explicit resource bound — Bounded Linear
> Logic's `!_{x<p}A`, "usable `p` times", with `p` a term — satisfy the
> derivability conditions **up to its index** and fail them uniformly? That would
> be the fixed-parameter-versus-uniform split at the modality, and the exact
> analogue of Willard's `Scalar_k`-versus-multiplication line.

## 3. Why it is worth doing independently of SJAS

Recorded because the question was raised in that form and should not be filed as
an instrument of this programme.

Beklemishev–Shamkanov's theorem says **that** contraction is required. A size
theorem would say **how much** — converting a structural prerequisite into a
resource bound, and connecting their result to the complexity-theoretic
literature on cut elimination, where the same rule is already the known source of
the blow-up. That is a contribution to their line whether or not it bears on
self-verifying systems, and it is the reason this obligation is worth carrying
even if R4 and R6 were abandoned.

## 4. What it needs

**Acquisitions, none held:**

- Girard, Scedrov and Scott, *Bounded Linear Logic* (1992) — the indexed bang.
- Girard, *Light Linear Logic* (1998); Lafont, *Soft Linear Logic* (2004) — the
  polynomial cut-elimination results.
- A witness for the non-elementary cut-elimination lower bound (Statman 1979 /
  Orevkov 1979, or the survey treatment in the *Handbook of Proof Theory*).

**And one thing that is not an acquisition.** Whether Beklemishev–Shamkanov's own
§3 already contains a size remark has not been checked. R2 read the paper and the
quotation register records no such remark, but the register is not a completeness
claim. This should be re-checked against pp. 7–8 page images at the same time as
the outstanding B–S image verification.

## 5. What this must not be taken to claim

- **No claim** that Beklemishev–Shamkanov's result is a size theorem in disguise.
  Q1 asks whether one exists behind it; it may not.
- **No claim** that light logics are self-verifying or possess a consistency
  point. B–S's `S` does not — their §6, and `lawvere-sjas.md` §0 records it.
- **No claim** that the arithmetic and structural obstructions are the same
  obstruction. R6 §7.4 records that the two multiplications occur at different
  levels: Willard's arises from an exponential encoding applied to an *additive*
  size increase, contraction's from the derivation size itself. They share an
  axis — what a composite costs relative to its parts — and not a mechanism.
- **No claim** about `S` being an arithmetic theory. It is a modal K4 over a
  contraction-free base with fixed-point operators
  ([`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md)
  §1), which is why it blocks *formalized* G2 rather than G2.
