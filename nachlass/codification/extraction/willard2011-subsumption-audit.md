# C6 Subsumption Audit: what Willard 2011 actually unifies

Charter ADR-0001 requires that `Willard2011`'s claims to unify, simplify and
extend the earlier results be **audited against the extractions, never
assumed**. This is that audit.

**Scope and its limit.** The paper names four prior paradigms — [59] =
`Willard1993`, [64] = `Willard2005`, [66] = `Willard2006a`, [68] =
`Willard2009`. Two of the four are extracted (C4, C5); **`Willard2006a` and
`Willard2009` are not yet extracted** (tiers A and B, components C9 and C10).
Every claim touching those two is therefore marked **deferred**, with the
check specified so C9/C10 can discharge it. Auditing what cannot yet be
checked would be exactly the assumption the charter forbids.

---

## 1. The claim

> The proofs in our prior papers were challenging primarily because they
> required one to separate the local combinatorial methods employed in
> [59, 64, 66, 68]'s particular applications from the common principles that
> underlied behind all these works. Our **Theorems 5.9, 5.11 and 6.6** will
> rectify this problem by identifying common components that unite these four
> paradigms. (Theorems 6.3, 6.10, 6.12, E.1, G.2 and G.3 will then carry on in
> further directions.) — printed p. 3

Three separate assertions, audited separately: **unification** (§2),
**simplification** (§3), **extension** (§4).

---

## 2. Unification — VERIFIED, with one correction to the correspondence

### 2.1 The device is real

`Willard2005` fixes its language (U-Grounding), its axiom-envelope condition
(`Normed(a,b)`), its deduction methods (tableaux; `Tab−U*₁`), and its coding
(Appendix A's B-adic scheme). `Willard2011` Definition 3.4 makes all five of
these *parameters* of a **generic configuration** `ξ = (L^ξ, Δ^ξ_0, B^ξ, d, g)`.
Prior systems become instances. This is a genuine abstraction, not a
relabelling: the theorems of §5–§6 are stated and proved for arbitrary ξ, and
the per-system work is quarantined in Appendix D.

### 2.2 The correspondence is **not** one paper per configuration

The naive reading — four paradigms, four configurations, in order — is wrong.
What the text actually says:

| Configuration | Willard's own attribution | Corpus key |
| --- | --- | --- |
| ξ\* | "The closest analog of `B*` and ξ\* in our prior work appeared in **Section 5 of [64]**" (p. 37) | `Willard2005` §5 |
| ξ\*\* | replaces tableaux with `Tab−U*₁`; "**Section 5.3 of [64]** noted `Tab−U*₁` has similar self-justification properties" (p. 45) | `Willard2005` §5.3 |
| ξ⁻ | "this result represents **a new way of proving [66]'s Theorem 3**" (p. 46) | `Willard2006a` Thm 3 |
| ξ^R | the `Ax-3` reply to Kolodziejczyk, from **[68]** (p. 48) | `Willard2009` |

So `Willard2005` supplies **two** of the four configurations, and **[59] =
`Willard1993` has no configuration of its own**. This is defensible — 1993 and
2005 §5 are the same paradigm (Type-A, semantic tableaux), so ξ\* covers both —
but it means the sentence "these four paradigms" enumerates *papers* while the
four ξ's enumerate *paradigms*, and the two lists do not align element-wise.
**Recorded as drift D21**; the codified statement must not present a
four-to-four correspondence.

### 2.3 Checks against the extracted papers

| Claim | Check | Verdict |
| --- | --- | --- |
| ξ\*'s language is 2005's | Appendix D recites the six non-growth Grounding functions (Subtraction, Division, `Root`, `Maximum`, `Logarithm`, `Count`) plus addition and `Double` | ✅ **verbatim match** with `willard2005.md` §3/§5 |
| ξ\*'s `Mult` graph | 2011 Eq. (45) versus 2005 Eq. (4) | ✅ **identical formula** |
| ξ\*'s apparatus | 2011's tableau rules 1–8 (printed p. 40) versus 2005 §2's rules 1–8 | ✅ **identical, in the same order** |
| The fixed point | 2011 Eq. (35) versus 2005 Eq. (7); 2011 Eq. (36) versus 2005 Eq. (8) | ✅ **identical**, with `Neg^k` for `Pair` and the level parameter `k` added |
| Group-2 | 2011 Def. 6.5 versus 2005 Eq. (5), 1993-TR Eq. (2.1) | ✅ same schema; Willard says so explicitly |
| ξ⁻ ↔ `Willard2006a` Theorem 3 | — | ⏸ **deferred to C9** |
| ξ^R ↔ `Willard2009`'s `Ax-3` | — | ⏸ **deferred to C10** |

### 2.4 What is *not* carried over

- **`Tab−U*₁` is never defined in 2011** — "The formal definition of `Tab−U*₁`
  deduction had appeared in [64]. It will be unnecessary to repeat here"
  (p. 45). A reader of 2011 alone cannot reconstruct ξ\*\*.
- **`Normed{a,b}` and the Fact D.3 machinery are imported from [64]**, not
  re-derived, for the ξ\* stability proof (Appendix D-2).
- The **1993 apparatus** — the `subcomponent` relation, `INT`/`s-consistent`,
  terse proofs, `Glue`, the `IS2`/`Prf2` trace formalism — has **no 2011
  counterpart at all**. It was already absent from 2005.

**Conclusion.** The unification claim holds, but 2011 is *not* self-contained:
it presupposes 2005 for the apparatus definition and for the per-configuration
stability machinery. The codified statement cannot cite 2011 alone for the
Type-A/`Tab−U*₁` results.

---

## 3. Simplification — VERIFIED, and the mechanism is identifiable

### 3.1 What replaced what

| `Willard2005` | `Willard2011` | Relation |
| --- | --- | --- |
| `App∀(a)` / `App∃(b)` — two envelope conditions with two parameters | `Scope_E(Υ,N)` + **Good(N)** + **`♯(Υ)`** — one operator, one measure, on the log scale | Consolidation; the `a`/`b` pair collapses into the single `♯` |
| **Definition 5** θ-compactification, parts (A) and (B), with `θ < 1/3` free | **Definition 5.1 A-Stable** (the Π part) and **Definition 5.3 E-Stable** (the Σ part), each with the fixed constant **½** | The two-part definition is *split into two named properties*, and the free exponent is *fixed* |
| **Definition 4** `Normed(a,b)` — a condition on the axiom system | **R-View** θ and `RE-Class(ξ)` — quantification over *all* r.e. sets, true or not | Generalization: stability is required against untrue θ too |
| **Theorem 1** (engine) | **Theorem 5.9** | Same skeleton; see §3.2 |
| **Theorem 3** (tableaux preserving) and **Theorem 5** (`Tab−U*₁` preserving) | **Theorem 6.3**, one theorem parameterized by stability class | Two apparatus-specific theorems become one |
| **Definition 1** consistency-preserving map `ℑ` | **Definition 6.2** `G^ξ_k` consistency preserving | Same notion, indexed by `k` and ξ |
| **Lemmas 1–2** (Positive Branch, Non-Closure) — nine-case induction, §5.2 | **Theorem D.4** (ξ\* is EA-stable), Appendix D-2 | The per-apparatus combinatorics is *relocated*, not eliminated |
| **Appendix A** B-adic coding *construction* | **Conventional Tableaux Encoding Requirement**: Gödel number ≥ `32^J` for `J` function symbols; "any other natural mechanism … is equally suitable" | A construction becomes a **condition** |

### 3.2 The engine theorems are the same argument

| Step | `Willard2005` Theorem 1 | `Willard2011` Theorem 5.9 |
| --- | --- | --- |
| Assume | `IS_D(A)` inconsistent | `S = B^ξ + SelfCons_1(B^ξ,d)` inconsistent |
| Minimal witness | `m = max(j₁,j₂)` minimal | `Log{Max[p̄,q̄,x̄,ȳ]} = ♯(Φ)+1` minimal |
| System class | `IS_D(A)` is `Normed(m−1, 1)` | `♯(Φ) < ∞` |
| Both sides bounded | Φ is `App∀((m−1)/m^θ)`-valid; ¬Φ is `App∃(m^θ)`-valid | Υ and ¬Υ both satisfy **Good{½♯(Φ)}** |
| Contradiction | `θ < 1/3` ⇒ `(m−1)/m^θ > m^θ` | a sentence and its negation cannot satisfy the same goodness constraint |

Identical skeleton. The 2011 contradiction is *sharper*: instead of comparing
two envelope sizes and needing an arithmetic inequality on `θ`, both sentences
land on the **same** `Good{½♯(Φ)}` and the contradiction is immediate. This is
why 2005's Theorem 1 needs the small-`m` side condition recorded as
composition obligation **O1** while 2011's Theorem 5.9 needs no analogue —
**O1 is a 2005-only obligation**, which the codified statement can note when it
presents the two forms.

### 3.3 Honest accounting of the simplification

The saving is real but partly **relocation**: 2005's §5.2 nine-case induction
becomes Appendix D-2's four-page proof of Theorem D.4, and the per-example
work must still be done once per configuration. What genuinely disappears is
the need to *redo the engine* for each apparatus. Willard says as much in
Remark 5.12: proofs by contradiction "should be simplified whenever possible.
This has been done mainly through Theorem 5.9's short proof."

---

## 4. Extension — VERIFIED; four genuinely new results

None of these has an antecedent in the extracted 1993 or 2005 material.

1. **Theorem 6.12, the Translational Reflection Principle** (the abstract's
   headline). Löb blocks `∀p[Prf(⌜Ψ⌝,p) ⇒ Ψ]`; 2011 obtains
   `∀p[Prf(⌜Ψ⌝,p) ⇒ Ψ^T]` for *all* `Π^ξ_1` Ψ, where `Ψ^T` is Standard-M-
   equivalent to Ψ and itself `Π^ξ_1`. The evasion mechanism (footnote 15):
   Ψ and `Ψ^T` are **not** equivalent from α's own perspective, so the
   diagonal never closes. Compare `Willard1993` Proposition 2, which obtained
   reflection only for `Δ₀` and *decidable* `Π₁` sentences — 2011 gets all of
   them, at the price of translation.
2. **The Global Simulation Sentence** (§6, Eq. 28): one finite axiom
   simulating the infinite Group-2 schema.
3. **Appendix G's `Braced`/`Size` results**: `B^ξ` plus **exactly three**
   added sentences proves, *purely* rather than in simulation, every `Π^ξ_1`
   theorem of `(B,D)` with at most `c` quantifiers, for any fixed `c`, while
   asserting its own consistency (Theorems G.2, G.3).
4. **Theorem E.1**: the translational reflection principle is *inoperative*
   for conventional logics — it is available only to self-justifying ones.

**A direct answer to a question in this repository.** `prose/to-dw-20201206z.txt`
asks Willard (question 2): *"Group-2 is the only component of `IS_D(beta)` that
defines an infinitary axiom schema… Could the infinitary axiom schema of
Group-2 be replaced by a finite set of rules sufficient to construct all
`PI_*_1` theorems, and constructively prove them?"* Items 2 and 3 above answer
it, and the answer is nuanced: **yes finitely, but not for all `Π^ξ_1`
theorems at once.** A single Global Simulation Sentence replaces the schema but
yields only the Test-relativized form `∀x Test^ξ_j(⌜Ψ⌝,x)` (Remark 6.9);
three sentences yield the theorems *purely*, but only for a quantifier-count
bound `c` fixed in advance (Theorem G.3). Recorded as composition obligation
**O23**.

---

## 5. Findings that change earlier records

| Finding | Effect |
| --- | --- |
| 2011's "Self Justifying" is 2005's "Introspectively Unified Logic" | New drift **D18**; compounds D7. Three different predicates now carry closely-related names across 1993/2005/2011 |
| Solovay's theorem has a **third** formulation | New drift **D19**; extends D4 |
| `Willard2001`'s title is mis-cited by 2005 and by our `paperlist` | New drift **D20**; `paperlist` corrected in this component |
| Coding condition is `Gödel number ≥ 32^J` for `J` function symbols, justified as "≥ 2J logical symbols ⇒ ≥ 5J bits" | Refines **O2**. The 1993 route (base-32 digits in 6-bit bytes) and the 2011 route (2J logical symbols) are *different derivations of the same 5* |
| Addition-totality as `∀x∀y ∃z ≤ x+y (z=x+y)` is `Π*₁` because the function symbol makes the bound expressible | **Discharges the explanatory half of O11** with Willard's own sentence |
| 2005's Theorem 1 needs `m ≥ 3`; 2011's Theorem 5.9 needs no analogue | Scopes **O1** to 2005 |
| `Willard2011` is an arXiv preprint at **v8**, never journal-published | Citation policy note for the codified statement |

---

## 6. Verdict

| Claim | Verdict |
| --- | --- |
| **Unifying** | ✅ verified for the two extracted paradigms; the mechanism (Definition 3.4) is real and load-bearing. ⚠️ the four-paradigms/four-configurations correspondence is not element-wise (D21), and 2011 is not self-contained — it defers `Tab−U*₁` and the `Normed`/Fact D.3 machinery to 2005 |
| **Simplifying** | ✅ verified: the engine proof shrinks to a page and the two-part θ-compactification splits into two fixed-constant stability properties. ⚠️ partly relocation — the per-configuration combinatorics moves to Appendix D-2 |
| **Extending** | ✅ verified: Theorem 6.12, the Global Simulation Sentence, Appendix G's three-axiom result, and Theorem E.1 are new |
| Subsumption of `Willard2006a`, `Willard2009` | ⏸ **deferred to C9 / C10**, with the specific checks named in §2.3 |

The charter's instruction not to assume subsumption was warranted: the naive
four-to-four reading is wrong, and 2011 turns out to depend on 2005 for
material it does not restate.
