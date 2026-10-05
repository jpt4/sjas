# P5's hypotheses: statements, sources, and paper proofs

*ADR-0006, F6.6. Written 2026-10-05.*

Proposition 5 (R4-metatheory.md §6) says that no budget derives
`Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0`. In phase 1 the formalization
proves P5 from named hypotheses (`p5.clj`, F6.5). This document states each
hypothesis exactly, gives its sources, and, for the two that are this
project's own results, gives their proofs on paper. Phase 2 is to prove the
hypotheses mechanically, with Mathlib and FormalizedFormalLogic/Foundation.

**Verification status.** Every citation below is marked.
- *[checked]* — the cited statement was read in the source itself during
  this work.
- *[unverified]* — the source was not accessible here. The citation follows
  the literature, and its exact location should be confirmed against the
  printed text before publication.

## 0. What is assumed, and what is proved

The formal P5 is about one checker: the concrete `Check decCert` of F7
(`check_spec.clj`, `certenc.clj`). So `chk′` denotes a defined, computing
function, and the λᶜᵉʳᵗ₀ side of the argument has no hypotheses.

| | Status in phase 1 |
| --- | --- |
| PA (H_PA, R4 §6.2) and its semantics in ℕ | defined (`pa.clj`) |
| PA is consistent (§6.4, step 4) | **proved** (`pa_consistent`) |
| λᶜᵉʳᵗ₀ is consistent, its checker accepts no refutation (T1, Cor. 3.7) | **proved** (`selfjust.clj`) |
| The translation of PA into λᶜᵉʳᵗ₀, at the meta level (§6.2) | to be proved (F6.3) |
| The default model, without budget induction (§6.3, Lemma 6.4's content) | to be proved (F6.4) |
| S1, Gödel's second theorem for PA | hypothesis, §1 |
| S2, Σ₁-completeness | used only inside the proofs of §§4–5 |
| S3, conservativity of E-PA^ω over PA | hypothesis, §3 |
| Int-6.1, Proposition 6.1 internalized in PA | hypothesis, §4 |
| Int-6.4, Lemma 6.4 and Corollary 6.5 internalized in E-PA^ω | hypothesis, §5 |

The formal deduction of P5 from these is short (§6). Its value is that every
assumption is a named, exactly stated proposition, and the rest is
kernel-checked.

## 1. S1 — Gödel's second incompleteness theorem for PA

**Statement used.** Let `Bew_PA(x)` be a provability predicate for PA that
satisfies the Hilbert–Bernays–Löb derivability conditions, and let
`Con_PA := ¬Bew_PA(⌜0 = S0⌝)`. If PA is consistent, then PA ⊬ `Con_PA`.

The conditions, for sentences φ, ψ:
- **D1:** if PA ⊢ φ then PA ⊢ `Bew(⌜φ⌝)`;
- **D2:** PA ⊢ `Bew(⌜φ → ψ⌝) → Bew(⌜φ⌝) → Bew(⌜ψ⌝)`;
- **D3:** PA ⊢ `Bew(⌜φ⌝) → Bew(⌜Bew(⌜φ⌝)⌝)`.

**Why the conditions are part of the statement.** G2 is intensional: it holds
for a *natural* presentation of provability and fails for some others.
Feferman showed that, for a non-standard presentation of PA's axioms, the
corresponding consistency sentence is provable in PA (Feferman, "Arithmetization
of metamathematics in a general setting", *Fundamenta Mathematicae* 49
(1960), pp. 35–92 *[unverified: theorem number]*). The derivability
conditions are the standard criterion of naturalness; with diagonalization
they suffice for G2.

**Sources.**
- K. Gödel, "Über formal unentscheidbare Sätze der Principia Mathematica und
  verwandter Systeme I", *Monatshefte für Mathematik und Physik* 38 (1931),
  Satz XI: the statement, with a proof only outlined *[unverified: numbering
  from the literature]*.
- D. Hilbert and P. Bernays, *Grundlagen der Mathematik II*, Springer 1939:
  the first complete proof, and the derivability conditions *[unverified]*.
- M. H. Löb, "Solution of a problem of Leon Henkin", *Journal of Symbolic
  Logic* 20 (1955): the conditions in the form D1–D3 *[unverified]*.
- **Mechanized:** S. Saitou and M. Noguchi, "Mechanizing Gödel's
  Incompleteness Theorems and Provability Logic", arXiv:2609.13780 (2026),
  in Lean 4, repository FormalizedFormalLogic/Foundation (sorry-free since
  commit 2da7151e, 2024-09-04) *[checked]*:
  - **Theorem 2.18:** "Let T be a Δ₁-definable, consistent arithmetic theory
    stronger than IΣ₁. Then T ⊬ Con_T." PA is such a theory.
  - **Proposition 2.8,** abstract G2: if a provability predicate 𝔅
    satisfies D2 and D3, over a diagonalizable T₀ ⊆ T with T consistent,
    then T ⊬ ¬𝔅⊥. D1 is part of their definition of a provability predicate
    (Definition 2.5).
- Further mechanizations: L. Paulson, Isabelle/HOL, for hereditarily finite
  set theory, not for arithmetic; A. Popescu and D. Traytel, *Journal of
  Automated Reasoning* (2021), an abstract account (Archive of Formal Proofs,
  `Goedel_Incompleteness`) *[checked: abstracts]*.

**In phase 2.** S1 is Foundation's Theorem 2.18 at T = PA. What remains is to
show that Foundation's PA and `Con_PA` are those used here, or to carry the
argument over to Foundation's.

## 2. S2 — Σ₁-completeness

**Statement.** Every Σ₁ sentence true in ℕ is provable in Robinson's Q, and
hence in PA.

**Source.** The Open Logic Project, *Incompleteness and Computability*,
section "Σ₁-Completeness" of the chapter "Representability in Q": "We shall
show that if φ is a Σ₁ sentence which is true in ℕ, then Q ⊢ φ" (Theorem
inp.7 in the project's source) *[checked]*. Mechanized: in Foundation, the
derivability condition D1 for the standard predicate is formalized
Σ₁-completeness (Saitou–Noguchi §2.4) *[checked]*.

**R4's form.** R4 states S2 as "PA proves every true closed equation between
primitive recursive terms". With a primitive recursive f given its natural Σ₁
definition φ_f, the equation f(n̄) = m̄ is the true Σ₁ sentence φ_f(n̄, m̄),
so Q proves it. In the language PA(PR) of §4.0 the equation is a closed term
equation, and its proof is the computation, unfolding the defining equations.

**Where it is used.** Only inside the paper proofs of §§4–5: closed
validity facts about fixed derivations, and the bounded facts BF₀, BF₁.
No formal theorem takes S2 as a hypothesis.

## 3. S3 — conservativity of E-PA^ω over PA

**Statement used.** For every arithmetical sentence σ: if E-PA^ω ⊢ σ, then
PA ⊢ σ. Only the instance σ = `Con_λ`, a Π₁ sentence, is used.

**The theory.** E-PA^ω is classical arithmetic in all finite types, with full
extensionality. It is E-HA^ω (Troelstra's E-HA^ω₀) with the law of excluded
middle. The definition followed is that of Troelstra (ed.), *Metamathematical
Investigation of Intuitionistic Arithmetic and Analysis*, Lecture Notes in
Mathematics 344, Springer 1973, and U. Kohlenbach, *Applied Proof Theory*,
Springer 2008, as used by van den Berg, Briseid and Safarik
(arXiv:1109.3103, §2.1) *[checked: that their E-HA^ω is "the system called
E-HA^ω₀ in [Troelstra 1973] and E-HA^ω in [Kohlenbach 2008]"]*.

**Source of the theorem.** B. van den Berg and L. van Slooten, "Arithmetical
conservation results", arXiv:1706.05901 (2017), §4.3 *[checked]*:
> **Theorem 4.13 (Kohlenbach).** The system E-PA^ω + QF-AC is conservative
> over PA.

This is stronger than S3 (QF-AC added, every arithmetical sentence). Their
proof:
1. **Theorem 4.12 (Kohlenbach):** I-PA^ω + QF-AC is conservative over PA. An
   arithmetical theorem's Herbrand normal form is provable; the negative
   translation with the Dialectica interpretation (Shoenfield's
   interpretation) takes it to I-HA^ω; Goodman's theorem takes it to HA; so
   PA proves the theorem. (Compare U. Kohlenbach, "Remarks on Herbrand
   normal forms and Herbrand realizations", *Archive for Mathematical Logic*
   31 (1992), Theorem 4.1 *[unverified]*.)
2. The ECF model of E-PA^ω + QF-AC is formalized in PA^ω + QF-AC (they cite
   Troelstra 1973, Theorem 2.6.20 *[unverified]*). It does not change the
   meaning of statements about objects of types 0 and 1, so the conservation
   transfers from step 1.

**Why the detour through extensionality matters.** Full E-HA^ω is *not*
Dialectica-interpretable (Howard 1973). The standard route eliminates
extensionality first, by relativizing to the hereditarily extensional
objects (Luckhardt 1973; Feferman 1977, §4.4.2). J. Avigad and S. Feferman,
"Gödel's functional ('Dialectica') interpretation", *Handbook of Proof
Theory*, 1998, §3.1 *[checked]*.

**In phase 2.** No mechanization of S3 was found (searches of 2026-10-03 and
2026-10-05). Two options:
- mechanize Theorem 4.13 on Mathlib. That is a large proof-theoretic
  project: the Herbrand normal form, the Shoenfield interpretation, Goodman's
  theorem, and the ECF model.
- replace Step 2 of P5 by an interpretation directly in PA. R4 §6 moved away
  from a hand-built realizability model in PA in order to cite S3.

## 4. Int-6.1 — Proposition 6.1, inside PA

**Statement.**

    PA ⊢ ∀p (Prf_PA(p, ⌜0 = S0⌝) → IsSyn(tr p) ∧ CHECK(tr p, c⊥) = 1).

Here `Prf_PA` is the natural proof predicate of H_PA (the one whose
`Bew_PA = ∃p Prf_PA(p, ·)` meets D1–D3, §1). `tr` is the translation of
R4 §6.2, targeting F7's certificate format. `CHECK` is the natural
primitive recursive presentation of `Check decCert` (§4.0).

### 4.0 The arithmetization used

**(A1) Primitive recursive functions in PA.** Each primitive recursive
derivation of a function f determines a Σ₁ formula φ_f (via Gödel's
β-function) such that IΣ₁, and so PA, proves that φ_f defines a total
function and proves f's recursion equations. Adding a symbol for each such f,
with those equations as axioms, gives a definitional extension PA(PR),
conservative over PA. This is the standard foundation of arithmetization:
P. Hájek and P. Pudlák, *Metamathematics of First-Order Arithmetic*,
Springer 1993, Chapter I *[unverified: exact section]*. Foundation mechanizes
it for IΣ₁, with Δ₁-definability of the syntactic functions (Saitou–Noguchi
§2.2) *[checked: description]*. All internal reasoning below is in PA(PR),
with induction for all formulas of its language.

**(A2) Codes.** Finite trees over the label set L (`Syn`, `R` and
derivations) are coded by numbers through a primitive recursive pairing, so
that a subtree's code is smaller than its tree's. H_PA terms, formulas and
proofs are coded the same way.

**(A3) Check is primitive recursive.** `Check decCert c d` (F7) decodes `c`
to a budget, a term, a type and two derivation trees. It checks each tree
node by node (`dtCheck`: rule side conditions, premise conclusions, skeleton
typing, recorded steps), tests the size inequalities, and compares the type
code with `d`. Each part is a structural recursion on finite trees, bounded
by the size of `c`. The one call back to the checker, for a δ-step inside
the trees, is on a strictly smaller first code (`restrC`), so the whole is a
course-of-values recursion on `nodes(c)`. With the codes of (A2), that is
primitive recursive; R4 §1.6 argues the same for its own Check. The *natural*
presentation of CHECK is the one this description yields. Its defining
equations make PA(PR) prove

    CHECK(c, d) = 1 ↔ VALID(c) ∧ ROOTCTX(c) is a token context ∧ CLOSED(ROOTTYPE(c)) ∧ ROOTTYPE(c) = d,

where `VALID` is the conjunction over nodes of the local conditions of R4
§1.6, and the other conditions are those of `check_spec.clj`'s body.

### 4.1 The proof

Write the translation's parts as primitive recursive functions:
- `TRT(t)` for a term;
- `TRF(φ)` for a formula;
- `CL(φ)` for the closure `⟦∀ᶜˡ φ⟧`;
- `TPL(s, params)` for the template of an axiom scheme `s`, applied to its
  parameters (R4 §6.2, the table "The templates").

**Lemma 1 (fixed derivations).** Let `D` be one of the fixed closed
derivations: those of `EQ`, `PLUS`, `TIMES`, `REFL` and `TRANSPORT`, and the
fixed conversion chains of R4 §6.2. Then PA ⊢ `VALID(⌜D⌝) = 1`.

*Proof.* `VALID(⌜D⌝) = 1` is a closed equation between primitive recursive
terms. It is true, because `D` is a valid derivation: F6.3 proves this at
the meta level. So PA proves it, by S2. ∎

**Lemma 2 (substitution).** PA proves:

    ∀φ ∀t ∀x (FreeFor(t, x, φ) → TRF(SUB(φ, t, x)) = SUBλ(TRF(φ), TRT(t), v_x)).

*Proof.* By induction on the code of φ, inside PA. Each case unfolds one
defining equation of `TRF`, `SUB` and `SUBλ`. At `∀y ψ` with `y ≠ x`,
`FreeFor` gives `y ∉ FV(t)`, so `TRT(t)` contains no `v_y`, and the binder
of `TRF(∀y ψ)` does not capture it. At `∀x ψ` both sides are unchanged. ∎

**Lemma 3 (parametric templates).** For each scheme `s`, PA proves:

    ∀params (WF_s(params) → VALID(TPL(s, params)) = 1 ∧ CONCL(TPL(s, params)) = ⌜Θ₀ ⊢ τ :¹ CL(φ_s(params))⌝).

Here `WF_s` says the parameters are formulas and terms meeting the scheme's
side conditions, and `φ_s(params)` is the axiom instance.

*Proof.* A template is a fixed skeleton of nodes with parameter-dependent
subderivations at known positions. `VALID` is a conjunction over nodes, so
it splits into three kinds of conjunct:
1. the fixed nodes, whose local conditions are equations between primitive
   recursive functions of the parameters, holding by the defining equations
   of the translation and of the encoding;
2. the fixed subderivations, valid by Lemma 1;
3. the subderivations depending on a formula or term.

Those of the third kind are:
- the typing of `TRT(t)`;
- the formation of `TRF(φ)` and of `CL(φ)`;
- `STAB(φ)`, for DN.

Each is proved valid by induction on the term or formula, inside PA. The
inductive step for `STAB` has one case per clause of R4 §6.2:
- *an atom:* one `ElimBool` node on `EQ ⟦s⟧ ⟦t⟧`, whose branches use the
  fixed chains `T(tt) ⇝ 1` and `T(ff) ⇝ 0`;
- *⊥:* a fixed closed derivation;
- *→ and ∀:* fixed nodes around the induction hypothesis's derivation.

A4, E2 and IND also use Lemma 2, to identify the instance's translation
with the substituted type, as syntax. ∎

**Lemma 4 (the main induction).** Let `L(p, i)` say: the derivation `tr_i(p)`
of line `i` is valid, and concludes
`L₁ :ω CL(φ₁), …, L_{i−1} :ω CL(φ_{i−1}) ⊢ τ_i :¹ CL(φ_i)`.
Then PA ⊢ `∀p (Prf_PA(p, x) → ∀i < len(p) L(p, i))`.

*Proof.* Fix `p` with `Prf_PA(p, x)`. Prove `∀i < len(p) L(p, i)` by
induction on `i`, inside PA. Case on line `i`'s justification, which
`Prf_PA` makes available:
- *Axiom instance of scheme `s`:* Lemma 3, weakened to the context of the
  earlier lines. Weakening at `ω` preserves validity: its local conditions are
  primitive recursive facts about the context, proved by induction on the
  derivation.
- *MP, from lines `j, k < i`:* `L(p, j)` and `L(p, k)` give valid
  derivations. The new nodes are two `App` nodes and the instantiation of
  the closures' variables: `v_y` for each variable `y` of the conclusion,
  `zero` for the rest. By Lemma 2 the two instantiations of `CL(φ)` agree as
  syntax. The new nodes' local conditions are then equations between
  primitive recursive functions of the premises' conclusions, which PA
  verifies.
- *Gen, from line `j < i`:* one `Lam` node and the applications to the
  variables. Likewise.

The binding of the lines is `Lam` and `App` at `ω` around the lines in
order. Its local conditions hold because each `τ_i`'s context holds only
earlier lines, at `ω`, and scaling such a context by `ω` changes nothing. ∎

**Proof of Int-6.1.** Let `p` be a proof of `0 = S0` (inside PA). By
Lemma 4, `tr(p)` is a valid derivation whose last line concludes
`⊢ τ :¹ CL(0 = S0) = T(EQ zero (succ zero))`. One more Conv node, with the
fixed chain `T(EQ zero (succ zero)) ⇝ ⋯ ⇝ T(ff) ⇝ 0`, valid by Lemma 1,
gives a valid derivation of `Θ₀ ⊢ t :¹ 0`. Its root context is `Θ₀` and `0`
is closed. F7's format adds a certificate wrapper: the fuel, the budget `0`,
and padding. The fuel and padding are primitive recursive functions of the
tree, chosen to meet `check_spec`'s size tests: `check_complete` shows such
choices exist, and they are computed explicitly. Then the characterization
of CHECK in (A3) gives `CHECK(tr p, c⊥) = 1`. And `IsSyn(tr p)` holds by
construction. ∎

**What F6.3 adds.** F6.3 proves the meta-level form, in the formalization:
every H_PA proof of `φ` gives a budget-0 derivation of `CL(φ)`. Lemmas 1–4
are that proof, carried out inside PA. Lemma 1's appeal to S2 is sound
because F6.3 makes its closed equations true.

## 5. Int-6.4 — Lemma 6.4 and Corollary 6.5, inside E-PA^ω

**Statement.** For every derivation Δ of `Θₙ ⊢ t :¹ Con′_ω` (with `chk′`
denoting `Check decCert`): E-PA^ω ⊢ `Con_λ`, where
`Con_λ := ∀c (IsSyn(c) → CHECK(c, c⊥) ≠ 1)`.

**Proof.** R4 §6.3 gives the proof as Lemmas 6.2–6.4 and Corollary 6.5. Read
with the following precisions, it is a complete argument.
1. **The interpretation (§6.3) is primitive recursive in Δ.** Each node's
   judgment is assigned, by recursion on Δ:
   - E-PA^ω terms, for its terms (`recN` as Gödel's recursor; `recSyn` and
     `itR` by course-of-values recursion on tree codes; `print` as the
     identity; `chk′` as CHECK; `reflect`, `H₁` and `abort` as defaults);
   - E-PA^ω formulas `V_k(A)[η]`, for its types.
2. **Lemma 6.2** (monotonicity, substitution, conversion) is proved for the
   finitely many types and chains of Δ. For each, it is one E-PA^ω proof, by
   induction on the type or chain *at the meta level*. The cases are as in
   §6.3:
   - β and ι use System T's equations and the recursion equations of
     course-of-values recursion;
   - δ uses the closed equation `CHECK(c̄, d̄) = b̄`, which PA proves by S2 and
     E-PA^ω therefore proves;
   - congruence under binders uses extensionality.
3. **Lemma 6.3** (the bounded facts BF₀, BF₁) is proved in PA, as in §6.3.
   The list of codes with at most `n` internal nodes is produced by an
   induction on `n` outside PA, each step using only the recursion equations
   of `IsSyn` and `NODES`. Each instance is a true closed equation
   `CHECK(v̄, d̄) = 0`: true by Corollary 3.7 (proved, `cor37_concrete`),
   provable by S2.
4. **Lemma 6.4** is proved by induction on Δ at the meta level, producing
   one E-PA^ω proof per node. Every case is Lemma 3.6's case read in
   E-PA^ω, with these exceptions:
   - `reflect` at `D ≠ 0`: the default lies in `V(D)`;
   - `H` and `H₁`: vacuous by BF₀ and BF₁;
   - `RecN`, `RecSyn`, `ItR`: by induction in E-PA^ω on the scrutinee.

   No case descends to a smaller budget, which is why no outer induction on
   budgets is needed (§6.3).
5. **Corollary 6.5** applies Lemma 6.4 at the root, with the tokens mapped to
   `0` and footprint `n`. `V(Con′_ω)` unfolds to `Con_λ`.

**What F6.4 adds.** F6.4 proves the meta-level form: the default model is
sound, without induction on budgets. Steps 2–4 internalize that proof, node
by node.

## 6. The deduction (F6.5)

From a derivation Δ of `Θₙ ⊢ t :¹ Con′_ω`:
1. Int-6.4 gives E-PA^ω ⊢ `Con_λ`.
2. S3 gives PA ⊢ `Con_λ`.
3. Int-6.1, by the deduction theorem, gives PA ⊢ `Con_λ → Con_PA`. So
   PA ⊢ `Con_PA`, by MP.
4. S1, with `pa_consistent`, gives PA ⊬ `Con_PA`.

The contradiction shows that no Δ exists. Proposition 4.8 follows (R4 §4.6):
a term of `H° → Con′_ω` at budget `n` would yield `Con′_ω`, applied to T2's
closed inhabitant of `H°`.

The formal statement (`p5.clj`) takes as parameters the sentences `Con_PA`
and `Con_λ`, as elements of `pa.clj`'s `PF`, and E-PA^ω's provability on
them. It takes as hypotheses S1, S3, Int-6.1 and Int-6.4 about those
parameters, and *anchoring* hypotheses that fix their meaning in ℕ:
- `paHolds ρ Con_λ ↔ ∀c, Check decCert c (encE 0) = false`;
- `paHolds ρ Con_PA ↔ ¬PPrv(0 = S0)`.

The anchors cannot fix the *presentation*: that is the naturalness
requirement of §1 and §4.0, which only phase 2's explicit arithmetization
discharges.
