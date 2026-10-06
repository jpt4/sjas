# 2026-10-06 — Artemov's explicit provability and λᶜᵉʳᵗ₀: an assessment

*The user asked for an assessment of a ChatGPT conversation ("Existence
Versus Computation", shared 2026-10-05) about the computational reading of
the Hilbert–Bernays–Löb (HBL) conditions and Artemov's Logic of Proofs, and
for a critical comparison with λᶜᵉʳᵗ₀. The conversation was retrieved from
its share page and read in full. Artemov's texts were read where cited: S.
Artemov, "Explicit provability and constructive semantics", Bulletin of
Symbolic Logic 7(1), 2001 [checked]; J. Alt and S. Artemov, "Reflective
λ-calculus", LNCS 2183, 2001 [checked: abstract and §1].*

## 1. The HBL conditions

For a theory T ⊇ PA-strength with provability predicate □ = Bew_T:
- **D1:** if T ⊢ A, then T ⊢ □A — an external rule;
- **D2:** T ⊢ □(A → B) → (□A → □B) — internal modus ponens;
- **D3:** T ⊢ □A → □□A — formalized D1, i.e. provable Σ₁-completeness for
  provability statements.

These are Löb's 1955 simplification of Hilbert and Bernays' conditions (1939).
With a diagonal lemma they give G2 and Löb's theorem.

## 2. The conversation: what holds, what does not

**Holds, and checked.**
- Artemov 2001 says that "in order to prove what are now known as
  Hilbert-Bernays-Löb derivability conditions, one constructs computable
  functions m(x, y) and c(x)". These satisfy:
  - PA ⊢ Proof(s, F→G) ∧ Proof(t, F) → Proof(m(s,t), G);
  - PA ⊢ Proof(t, F) → Proof(c(t), Proof(t, F)).

  They are then "relaxed" to D2 and D3. LP adds a third operation, a(x, y)
  (sum). The conversation's reading is Artemov's: D2 as proof application,
  D3 as proof checking (`!`), D1 as internalization (LP's lifting lemma).
- Alt and Artemov's λ∞ is a typed λ-calculus that "internalizes its own
  derivations as λ-terms" and is strongly normalizing (not confluent).

**Does not hold.**
- **It assessed the wrong object.** It says itself that it "inspected the
  current public `jpt4/proflog` repository" — `willard_sjas.clj`, a Willard
  tableau system with a `tableau-proof(g, t, p)` relation over Gödel-coded
  proof trees. It also says it could not access the λᶜᵉʳᵗ sources. Its table
  ("proofs typed by propositions: No"; "explicit proof application: No
  corresponding primitive") describes that tableau system, not λᶜᵉʳᵗ₀. For
  λᶜᵉʳᵗ₀ both entries are wrong: its terms are proofs typed by propositions
  (Curry–Howard), application composes them, and its certificates are typed
  through the checker (□A := Σ(r :₁ R). T(chk′ (print r) ⌜A⌝)).
- **Its account of Willard is loose.** It says Willard proves consistency
  "for restricted classes of formulas such as Δ₀ or Π₁". In this project's
  codified reading, the operative restrictions are on the *proof notion*
  (semantic tableaux, which his systems can certify, versus Hilbert proofs,
  which they cannot) and on *growth* (multiplication as a relation). Formula
  classes enter his reflection results, not the restriction that makes self-
  justification possible.
- **Its research question is posed for the wrong system.** It asks whether
  "HBL-2/proof-composition [is] the missing ingredient". For λᶜᵉʳᵗ₀ that
  question has a precise answer, given in §3.

## 3. Critical comparison: LP and λ∞ against λᶜᵉʳᵗ₀

**Explicit reflection is the common mechanism.** Artemov's central
observation (2001, §4): implicit reflection Provable(F) → F is not
internally provable, but explicit reflection Proof(n, F) → F *is*, for each
n. If n is a proof, F is provable. If not, ¬Proof(n, F) is a true Δ₁ fact, so
provable. LP's axiom t:F → F is sound for exactly that reason, *per
interpretation* (Proposition 6.6). At F = ⊥, LP derives x:⊥ → ⊥:
explicit consistency, one proof at a time.

λᶜᵉʳᵗ₀'s H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0 is explicit reflection at
⊥ too. It is stated about held, explicit proofs, never about
"some code exists". The difference lies in *where the uniformity sits*:

| | LP / λ∞ | λᶜᵉʳᵗ₀ |
| --- | --- | --- |
| Explicit evidence | proof terms t, propositions t:F | certificates r : R (token-paid), with checker evidence T(chk′ (print r) ⌜A⌝) |
| Reflection at ⊥ | x:⊥ → ⊥, valid schematically (each arithmetical interpretation separately) | H°, one closed term quantifying *internally* over all certificates, derivable at budget 0 (T2) |
| Uniform implicit consistency | not derivable (G2) | Con′_ω over free codes: not derivable at any budget (P5) |
| Proof application (D2, m, ·) | a total, free operation | for programs, free; for certificates, per instance only, costing μ(B) (Theorem 4.6) |
| Proof checking (D3, c, !) | a total, free operation | per instance only, at μ(□A) > 2μ(A) (Props 4.3–4.5) |
| Internalization (D1, lifting) | every derivation yields a proof term | with a budget: a certificate v gives (lit_v, ⋆) : □A at ‖v‖ tokens (Prop 4.2) |
| Running proofs | — (λ∞: reduction of proof terms) | reflect_D runs a held certificate at base data types, at a smaller budget |

**The trade.** LP can afford free proof operations because its reflection
is only *schematic*: no internal ∀ over proofs exists to feed Löb's
argument. λᶜᵉʳᵗ₀ has an *internal* universal statement over its own
certificates (H°, H₁°), so free D2 and D3 on certificates would run Löb's
derivation and make it inconsistent (draft §8.3). It pays instead: the
certificate operations exist per instance and are priced in tokens.
Arithmetically (P5, Step 2), H° at budget n is the bounded fact BF₀(n): no
code of ≤ n nodes checks as a refutation, which PA proves for each n. So
λᶜᵉʳᵗ₀ recovers, inside the calculus, an internally uniform statement whose
arithmetical content is LP's per-instance one.

**Answer to the conversation's question, for λᶜᵉʳᵗ₀.** What separates
derivable self-consistency (H°, H₁°) from underivable consistency (Con′_ω)
is not the absence of a proof-application *operation*. Programs compose
freely, and certificates compose per instance. It is the absence of a
*uniform, fixed-budget* D2 and D3 on held certificates (Theorem 4.6, Props
4.3–4.5), while D2 and D3 hold uniformly on free codes (draft §2.0;
unverified in detail). The resource on proof objects plays the role that
schematicity plays in LP.

**Where λ∞ is the closer prior art.** λ∞, like λᶜᵉʳᵗ₀, is a strongly
normalizing typed λ-calculus that internalizes its own derivations. But:
- its proof objects are syntax (types t:F built from terms), not data
  checked by an executable checker;
- it has no internal quantification over all proofs, and so no
  self-consistency statement of H°'s kind;
- it has no resources.

Whether λ∞'s reflection t:A ⊢ A, combined with an internal quantifier over
proof terms, would collapse as G2 predicts is the question λᶜᵉʳᵗ₀ answers by
pricing. That is worth checking against λ∞'s full text, not done here.

**Not verified here.** The conversation's further references (Artemov,
"Logic of Proofs", APAL 67 (1994); Goris 2008; Artemov 2007; Davies–Pfenning,
JACM 48(3) (2001); Gross–Gallagher–Fallenstein 2016, unpublished) were not
read. They are plausible, but should be checked before citing.
