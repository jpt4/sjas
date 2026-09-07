# The affine separation, and what Beklemishev–Shamkanov actually assume

*Draft, 2026-09-06. Sharpens obligation **RO1** (ADR-0002, component R4).
Bears on [`refined-sjas.md`](refined-sjas.md) §2.5 and
[`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md) §3.*

> **Status caveat, discharged 2026-09-07.** Every B–S formula below was
> originally taken from the **text layer** of
> `lit/beklemishev-shamkanov2016_abstract_g2_nonclassical_arxiv_1602.05728.pdf`,
> which **strips every `□`** — R2 §1 records this and quoted Proposition 3.8
> from the page image for that reason. The boxes in L1, L2, L3, C1, C4 and
> Lemma 3.5(ii)–(iii) were **reconstructed from context**, not read.
>
> **Printed pp. 5, 7, 8, 9, 11 and 14 have now been read as page images**
> (register rows in [`VERIFICATION.md`](VERIFICATION.md)), and **every
> reconstruction below checks out**: Definition 3.4's L1–L3 verbatim, including
> that L1 is stated curried and as a sequent; Lemma 3.5(ii)–(iii); Remark 3.6's
> "The latter has a form of built-in contraction that we are not assuming here";
> Proposition 3.8 and the identification of contraction's consumption at the C3
> step, on the context `Γ`; and Remark 3.9's `□`-contraction, `□`-weakening,
> C3′ and C5′. §5's RO1a citation of **§3** for the affine-PA remark is also
> correct — the sentence is at the end of §3, printed p. 9. Status of this
> document's B–S attributions: **`img`**. The affine derivations in §3 are
> independent of that and are `full`.

---

## 1. The conditions B–S use

Their derivability conditions are **Definition 3.4** (p. 7), stated for a
`□ : L_S → L_S` over a consequence relation with implication `S`:

> **L1.** `□(ϕ → ψ) ⊢ □ϕ → □ψ`
> **L2.** `□ϕ ⊢ □□ϕ`
> **L3.** `⊢ ϕ` implies `⊢ □ϕ`

These are the same three as everywhere else, in the modal-logic ordering:

| B–S | Willard / Mendelson | Wikipedia / Smith | name |
| --- | --- | --- | --- |
| **L1** | **(2)** | (3) | K, distribution, **composition** |
| **L2** | (3) | (2) | 4, reflection upward |
| **L3** | (1) | (1) | necessitation |

**L1 is stated curried, and as a sequent** — not as a conjunctive implication.
That matters below.

Distinct from these, and often conflated with them, are the **APS conditions**
of Definition 2.3, which are conditions on an abstract provability structure
rather than on a `□` over a consequence relation:

> **C1.** `x ⩽_S y ⇒ □x ⩽_S □y`, `⊠y ⩽ ⊠x`
> **C2.** `⊤ ⩽_S ⊠⊥`
> **C3.** `x ⩽_S y`, `x ⩽_S ⊠y ⇒ x ⩽_S ⊠⊤`
> **C4.** `⊠x ⩽_S □⊠x`
> **C5.** `x ⩽_S ⊤`

with `⊠ϕ := □(ϕ → ⊥)`. Their gloss (p. 5): C1 is monotonicity of provability
and antitonicity of refutability; C2 says "refutability of `⊥` is provable in
`S`"; C3 says "`⊤` is refutable if some sentence `y` is both provable and
refutable"; C4 says "the refutability of `x` can be formally checked in `S`".
**Remark 3.9** weakens C3 and C5 to `C3′`/`C5′` with `⊠x` in place of `x`.

**Proposition 3.8** is the bridge: *contraction* + L1–L3 ⟹ the structure is an
APS. Contraction is consumed at the C3 step, on the context `Γ`.

## 2. The finding: HBL and contraction are orthogonal in B–S

**Remark 3.6** (p. 7) is the sentence this document exists for. After Lemma 3.5
gives the equivalent rule form of L1–L3 —

> **(iii)** `S` is closed under `□Γ, ∆ ⊢ ϕ  /  □Γ, □∆ ⊢ □ϕ`

— they write:

> "Notice that the last rule is formulated slightly differently from the more
> standard rule for modal logic K4: `□Γ, Γ ⊢ ϕ / □Γ ⊢ □ϕ`. **The latter has a
> form of built-in contraction that we are not assuming here.**"

So B–S **deliberately de-contract the derivability conditions themselves.** K4's
rule duplicates `Γ` in its premise; their (iii) keeps `Γ` and `∆` independent.
Contraction is then reintroduced as a **separate hypothesis** of Theorem 3.

This is sharper than `refined-sjas.md` §2.5's "contraction … sits underneath all
of them." Underneath is right, but the operative fact is that B–S **factored it
out of them**, precisely so it could be denied independently. In their analysis
L1–L3 and contraction are orthogonal, and G2 needs both.

## 3. The separation — and a correction

**Claim, corrected.** An earlier working claim in this line held that Willard's
conjunctive (2) and the curried K form come apart in affine logic, so that the
Mendelson and Smith numberings would name different facts. That is **right only
for additive conjunction, and B–S do not use additive conjunction.**

Their **Definition 3.2** defines conjunction multiplicatively:

> `Γ, ϕ, ψ ⊢ θ ⟺ Γ, ϕ ⊗ ψ ⊢ θ`

which is the `⊗`/comma correspondence. With I3 (`Γ, ϕ ⊢ ψ ⟺ Γ ⊢ ϕ → ψ`) and
multiset exchange, the two forms are interderivable **with no contraction**:

```
    □ϕ ⊗ □(ϕ→ψ) ⊢ □ψ
⟺  □ϕ, □(ϕ→ψ) ⊢ □ψ         Definition 3.2
⟺  □(ϕ→ψ), □ϕ ⊢ □ψ         multiset exchange
⟺  □(ϕ→ψ) ⊢ □ϕ → □ψ        I3
=   L1
```

**In B–S's framework the two forms do not separate.** The earlier claim pointed
at the wrong reference point.

**What does separate.** Write `(W)_&` for the *additive* reading,
`□ϕ & □(ϕ→ψ) ⊸ □ψ`, in affine logic (weakening yes, contraction no):

- **`(W)_& ⊢ L1`** — assume `□(ϕ→ψ)`, assume `□ϕ`; reach `Γ ⊢ □ϕ` and
  `Γ ⊢ □(ϕ→ψ)` for `Γ = □ϕ, □(ϕ→ψ)` by **weakening**; `&R` gives
  `Γ ⊢ □ϕ & □(ϕ→ψ)`; then `⊸E`, `⊸I` twice. **Available affinely.**
- **`L1 ⊢ (W)_&`** — `&L₁` and `&L₂` each consume one copy of
  `□ϕ & □(ϕ→ψ)`. Using both yields `Γ, A&B, A&B ⊢ C`, and descending to
  `Γ, A&B ⊢ C` is **contraction**. **Not available affinely.**

| conjunction | affine verdict |
| --- | --- |
| multiplicative `⊗` | `(W) ⟺ (K)` — the `⊗`/`⊸` adjunction, free |
| additive `&` | `(W)_& ⟹ (K)`, converse costs exactly one contraction |

So the separation is **between readings of the conjunction**, not between the
two numbering conventions. Additive-conjunctive D2 is strictly stronger than
curried D2 affinely; multiplicative-conjunctive D2 is equivalent to it.

## 4. The consequence for transporting Willard

Willard states (2) conjunctively — `sources-text/willard2001.txt:2700`,
`α ⊢ {Der(⌜Φ⌝) ∧ Der(⌜Φ ⊃ Ψ⌝)} ⊃ Der(⌜Ψ⌝)` — in a **classical** arithmetic,
where `∧` is simultaneously additive and multiplicative and §3's distinction is
invisible. **The corpus does not determine which he means, and nothing in his
setting turns on it.** This is a translation obligation, not a defect.

But it means "`IS(A)` breaches condition (2)" **does not transport to a
substructural setting without a choice being made**, and the two choices differ
in strength by one contraction. A candidate affine SJAS could fail additive-(2)
while satisfying L1 — and would then not have reproduced Willard's breach in
B–S's sense at all.

## 5. RO1, restated in three parts

The charter's current form (ADR-0002, R4 row) — "affineness alone does not evade
G2; `□`-contraction can hold in affine PA" — is true and under-specified.
Proposed replacement:

- **RO1a** *(existing)*. Show `□`-contraction fails, not merely general
  contraction. B–S §3: `□`-contraction "actually holds for some meaningful
  arithmetical systems lacking general contraction rule", including an affine
  predicate-logic PA.
- **RO1b** *(new, from §3–§4)*. State which conjunction the transported
  condition (2) uses. Additive and multiplicative readings are inequivalent
  affinely; Willard's text does not decide between them.
- **RO1c** *(new, from §2)*. Give the derivability conditions in a
  contraction-free **rule** form — B–S Lemma 3.5(iii), not K4's
  `□Γ, Γ ⊢ ϕ / □Γ ⊢ □ϕ`. An affine line that writes its box rule in the
  standard K4 form has **assumed contraction inside the very condition it
  claims to deny**, and its result is circular.

RO1c is the one with teeth, and it is checkable by inspection of a candidate
system's box rule.

## 6. What is not claimed

This document does not claim an affine route to a self-verifying system. B–S's
own §6 (p. 14, image-verified in R2) records that `S` is not one — `⇒ ¬□⊥` is
unprovable — and that "we are still missing convincing examples of mathematical
theories based on weak logics for which G2 would fail." The separation here is
about what a transported *condition* means, not about what a transported system
would achieve.

## Open

- **Image-verify pp. 7–8** (Definitions 3.2, 3.4, Lemma 3.5, Remark 3.6) and
  promote or correct the reconstructed boxes. Until then §§1–2 are
  `text-layer, boxes reconstructed`.
- Whether Willard's Hilbert-line systems, whose condition is **not identified in
  the corpus** (`refined-sjas.md` §2.3a), are even in scope for §4's
  transport question.
