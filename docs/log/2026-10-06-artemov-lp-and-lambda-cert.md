# 2026-10-06 — Artemov's explicit provability and λᶜᵉʳᵗ₀: correctness, novelty, computational meaning

*The user asked for an assessment of Artemov's computational interpretation of
provability, critically compared with λᶜᵉʳᵗ₀. The aim is to establish that
λᶜᵉʳᵗ₀ is (1) correct, (2) novel, and (3) gives self-justification a
computational meaning, as Artemov gives one to provability. This note
supersedes the first version of the same day (commits a80d34d, 1cf6029). That
version assessed a shared ChatGPT conversation, which had examined
`jpt4/proflog`'s tableau code rather than λᶜᵉʳᵗ; the user asked that this be
set aside.*

**Sources, with how far each was read.**
- *Read in full:*
  - S. Artemov, "The provability of consistency", arXiv:1902.07404v5 (2020).
    It is held at `nachlass/works-citing-dew/Artemov_S/`, and it cites
    Willard.
- *Read in the relevant sections:*
  - S. Artemov, "Explicit provability and constructive semantics", BSL 7(1),
    2001: §§3–4 and Proposition 6.6. Willard cites it (WC0009).
  - J. Alt and S. Artemov, "Reflective λ-calculus", LNCS 2183, 2001: abstract
    and §1.
  - S. Artemov, "Serial properties, selector proofs, and the provability of
    consistency", arXiv:2403.12272 (2024): §§1–2.
  - E. Gadsby, "Properties of selector proofs", arXiv:2509.19373 (2025):
    introduction and §4.
  - G. A. Kavvos, "Intensionality, intensional recursion, and the Gödel–Löb
    axiom", arXiv:1703.01288v2 (2020): introduction, and the section on
    intensional fixed points.
- *Known from search results only, to be read before citing:*
  - Davies–Pfenning (JACM 2001);
  - Nogina on GLA;
  - Yavorsky, and Artemov–Yavorskaya, on quantified logics of proofs;
  - Kuznets on self-referentiality;
  - Bonelli–Feller (APAL 2012);
  - Artemov–Kuznets on logical omniscience.

  These are marked [search] below.

## 1. The HBL conditions, and Artemov's operations

For T ⊇ PA with provability predicate □ = Bew_T:
- **D1:** T ⊢ A ⇒ T ⊢ □A;
- **D2:** T ⊢ □(A→B) → □A → □B;
- **D3:** T ⊢ □A → □□A.

They are Löb's 1955 form of Hilbert–Bernays (1939). Artemov (2001, §3) notes
that their proofs build computable functions m and c, with
- PA ⊢ Proof(s, F→G) ∧ Proof(t, F) → Proof(m(s,t), G), and
- PA ⊢ Proof(t, F) → Proof(c(t), Proof(t, F)).

D2 and D3 "relax" these by forgetting the witnesses.

## 2. Artemov's computational interpretation of provability

It has four layers. Each layer is a different answer to "what is a proof,
computationally?".

1. **Explicit proofs and operations on them (LP, 1995; 2001).**
   - *Proof terms:* t:F, read "t is a proof of F", built by application `·`,
     sum `+` and the checker `!`, from constants for axioms.
   - *Axioms:* t:F → F (explicit reflection), t:(F→G) → s:F → (t·s):G, and
     t:F → !t:(t:F).
   - *Arithmetical semantics:* terms denote Gödel numbers of PA proofs, and
     the operations denote computable functions like m and c.
   - *Realization:* LP realizes S4, so S4 is explicit provability.
   - *Self-reference is unavoidable* [search: Kuznets]: some S4 theorems can
     be realized only with self-referential constants, c : A(c).
2. **Why explicit reflection is provable when implicit reflection is not**
   (2001, §4; Proposition 6.6).
   - *Implicit:* Provable(F) → F is not provable, because the ∃ over proofs
     may be witnessed by a nonstandard element.
   - *Explicit:* Proof(n, F) → F *is* provable, for each numeral n. If n is a
     proof, PA ⊢ F; if not, ¬Proof(n, F) is a true Δ₁ fact, so provable.
   - *So:* the axiom t:F → F is sound *one interpretation at a time*.
   - *At F = ⊥:* LP proves t:⊥ → ⊥, but only as a schema. LP has no
     quantifier over proofs.
3. **Proofs as programs (λ∞, 2001).**
   - *What it is:* a strongly normalizing typed λ-calculus that internalizes
     its own derivations as terms, with reflection and the checker `!` as term
     operations.
   - *A restriction it needs:* the λ-version of all proof polynomials does not
     normalize, since polymorphic proof variables allow (λx.xx)(λx.xx). So it
     restricts to single-conclusion proof systems.
   - *What it lacks:* quantification over proofs.
4. **Proofs of universal statements as selector plus verifier (2019; 2024).**
   - *The BHK reading:* with Kreisel's second clause, a constructive proof of
     ∀x F(x) is an operation s together with a proof v that s(x) proves F(x)
     for every x.
   - *Consistency as a serial property:* consistency is the series
     {¬n:⊥}ₙ.
   - *PA proves it as a serial property:* a primitive recursive selector s
     builds, from the code of a derivation S, a PA proof that ⊥ ∉ S. It does
     so by computing a level n with S ⊆ Σₙ and using the partial truth
     predicate Trₙ as an invariant. Then PA ⊢ ∀x [s(x) : ¬x:⊥].
   - *The gap with G2:* the internal sentence ∀x ¬x:⊥ stays unprovable, and
     Artemov attributes the gap to nonstandard x.
   - *Artemov's own criterion for "contentual" proofs* (2019 §4; 2024 §5.2).
     Take the selector v with v(x) := "if x proves ⊥, derive ¬x:⊥ from that ⊥;
     otherwise, by Σ₁-completeness".
     - It is a proof of the scheme.
     - It is *not* a consistency proof: it uses the contradiction itself.
   - *The ceiling* (Gadsby 2025, Proposition 4.1, after Ignjatović). T
     selector-proves Con(W) exactly when T ⊢ Con(T) → Con(W). Selector
     consistency *is* relative consistency, and T cannot selector-prove the
     consistency of any theory containing T + Con(T).

## 3. λᶜᵉʳᵗ₀'s computational interpretation of provability

λᶜᵉʳᵗ₀ has *two* notions of provability, in one language. They obey
different laws.

| | Free codes (`Syn`) | Held certificates (`R`, affine, paid in tokens) |
| --- | --- | --- |
| Proof object | a code c : Syn, plain data, freely built (System T strength) | r :₁ R, a tree whose nodes consume tokens ◇; `print r : Syn` |
| "A is provable" | the checker accepts some code: T(chk′ c ⌜A⌝) for some c | □A := Σ(r :₁ R). T(chk′ (print r) ⌜A⌝) |
| D1 | — | per instance: (lit_v, ⋆) : □A at ‖v‖ tokens (Prop 4.2) |
| D2 | uniform on codes (draft §2.0; not yet formal) | per instance only, and never cheaper than proving B afresh: k ≥ μ(B) (Thm 4.6, Cor 4.6′) |
| D3 | uniform on codes (as D2) | exactly at budget μ(□A) > 2μ(A) (Cor 4.6″; Props 4.3–4.5) |
| Reflection | Con′_ω, all codes: underivable at every budget (P5, from S1–S3 and Int-6.1/6.4) | H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0, derivable at budget 0 by `reflect₀` (T2); `reflect_D : □D → D` for base data D (§4.8, Thm 5.2) |
| What reflection does | — | runs the program the certificate encodes, at the smaller budget m < ‖v‖ ≤ n it declares (Lemma 2.7; Lemma 3.6, Reflect case) |

**A caveat on the D2 and D3 prices** (found by the `f4-theorem46` delegate,
2026-10-06, after this note was first written).
- *What is proved:* Theorem 4.6 is kernel-checked for every checker that
  satisfies one encoding hypothesis, `Enc46`. An accepted code that embeds
  accepted codes strictly inside must pay, in its own nodes, the size of one
  embedded certificate's term.
- *Where it fails:* F7's concrete checker refutes `Enc46` (`enc46_F7`). It
  reads a certificate's padding only through its size, so a certificate can
  carry another certificate as padding, for free (`check_pad`).
- *So, at the concrete checker:* Theorem 4.6's hypotheses are contradictory
  (`thm46_F7_vacuous`), and the lower bounds of Corollaries 4.6′ and 4.6″ are
  vacuous. Their upper bounds still hold.
- *What stands:* the prices in the table hold for unpadded formats, which
  satisfy `Enc46`. At F7's format they are open.
- *Why it matters:* by the project's encoding rule this is a research
  finding. The pricing of D2 and D3 depends on the certificate format, while
  T1 and T2 do not.

**The free side behaves like GL**, implicit provability: G2 holds for it (P5).
**The held side** has uniform reflection, like S4's T axiom, but no uniform K
(D2) or 4 (D3) at a fixed budget. It is a graded logic, whose grades are
token budgets.

## 4. Correctness, in the light of the comparison

**What is checked.** All of the following are relative to the metatheory,
Lean's type theory as Ansatz implements it:
- T1, consistency: no budget derives Θₙ ⊢ t :¹ 0;
- Corollary 3.7;
- T2: H° and H₁° are derivable at budget 0;
- all three at a concrete, computing checker (`selfjust.clj`).

P5 and Proposition 4.8 are checked from named hypotheses (`p5.clj`; the
hypotheses are in `P5-hypotheses.md`). Theorems 4.6 and 5.2 are being
formalized now (`f4-theorem46`, `f5-theorem52`).

**What the comparison exposes.**

1. **By Artemov's criterion, H°'s internal derivation is not a consistency
   proof.**
   - *What the derivation is:* H°'s inhabitant is
     λr. λe. reflect₀ r e. Given a held certificate that checks as a
     refutation, it *uses that certificate* to produce ⊥.
   - *What that is, structurally:* LP's t:⊥ → ⊥, and Artemov's
     non-contentual selector v.
   - *Where the contentual argument is:* in the metatheory's T1, whose
     invariant is the budget-stratified model (Lemma 3.6). That invariant is
     the counterpart of Artemov's Trₙ.
   - *This is no defect for Willard's notion.* His Definition 3.4 asks that
     the system (i) prove a statement of its own consistency, and (ii) be
     consistent. His Group-3 axiom is postulated in exactly this way.
   - *But the papers must not say* that λᶜᵉʳᵗ₀ "proves its own consistency"
     in Hilbert's or Artemov's contentual sense. It contains a uniform
     explicit reflection principle at ⊥, and the metatheory proves that
     principle sound.
   - *H₁° is weaker still, computationally.* It is the postulated constant
     H₁ (Willard's Level(1) form). It has no reduction rule, and Theorem 5.2
     says no evaluation ever reaches it.
2. **Arithmetically, H° is a serial property.**
   - *What each instance says:* at budget n, H° says no code of at most n
     nodes checks as a refutation. That is BF₀(n), a finite consistency
     statement in Pudlák's sense.
   - *Where the budget sits:* the runner fixes it, outside the calculus.
   - *So:* the single closed term H°, weakened to each Θₙ, is a *constant*
     selector for the series {BF₀(n)}. Its verifier is T1, outside the
     calculus.
   - *The ceiling (Gadsby, Proposition 4.1).* A selector proof of
     {BF₀(n)} in PA would exist exactly if PA ⊢ Con(PA) → Con′(λᶜᵉʳᵗ₀).
   - *So nothing escapes relative consistency here.* That agrees with the
     correction of 2026-10-05. Any arithmetical claim made for H° must be
     stated as relative consistency.
3. **Kavvos's collapse, and the descent that blocks it.**
   - *The collapse:* in iPCF, the T axiom (`eval : □A → A`) plus intensional
     fixed points (Löb's rule) inhabit every type, via
     Ω_A = fix z in eval_A z. This is the computational form of
     "reflection plus Löb is inconsistent".
   - *λᶜᵉʳᵗ₀ has the T axiom at ⊥:* that is H°.
   - *What prevents Ω:* reflect runs a decoded program at a budget strictly
     below the size of its certificate (m < ‖v‖ ≤ n, from Lemma 2.7). A
     certificate cannot be held by the program it certifies, so the fixed
     point can be formed on free codes but never through the reflection
     channel. This is the computational content of T1's induction on
     budgets.
   - *Recommendation:* state it as its own theorem — no derivation of
     "fix z. reflect z", at any budget — rather than leave it implicit in
     T1.
4. **The encoding.**
   - *What H°'s soundness uses:* that a certificate declares its budget
     uncompressed (E3), with E2 and E4. R4 §4.8 records that Willard's
     counterexample (local Π₁ reflection inconsistent; Willard1993-TR,
     Proposition 5) is avoided for this reason.
   - *An open item:* "whether Willard's counterexample can be transcribed
     into λᶜᵉʳᵗ₀ at all is not checked".
   - *Under the project's encoding-sensitivity rule,* that is a research
     finding to make explicit: does self-justification depend on E3, or does
     E4 suffice, as was noted earlier for strict overhead?

## 5. Novelty: what is prior art, and what appears new

Prior art for each ingredient taken separately:

| Ingredient | Where it already exists |
| --- | --- |
| Proofs as typed explicit objects, with a checking operation | LP (1995); λ∞ (2001) |
| A closed term □A → A in a consistent, normalizing calculus | Davies–Pfenning λ□, where □A is closed code and `eval` the T axiom [search] |
| Intensional operations on code; Löb as intensional recursion | Kavvos, iPCF (2017/2020), which is non-terminating, and collapses with eval |
| Explicit and implicit provability in one logic: ¬t:⊥ provable for each t, ¬□⊥ not | GLA (Nogina; Artemov–Nogina), arithmetically complete [search] |
| Quantifying over explicit proofs | quantified logics of proofs: not axiomatizable under the arithmetical semantics (Yavorsky; Artemov–Yavorskaya) [search] |
| Consistency as a serial property, provable in PA | Artemov (2019, 2024); Gadsby (2025) |
| Proof size as a complexity measure in justification logic | Artemov–Kuznets on logical omniscience [search]; Goris's feasible LP |
| Typed code with certificates, under resource discipline | Bonelli–Feller's certifying mobile calculus (resources for locality, not budgets) [search] |
| A self-justifying axiom system | Willard |
| Finite consistency statements | Pudlák |

So none of the following is new by itself:
- the T axiom at ⊥ (λ□ has it as a closed term);
- the split between explicit and implicit provability (GLA);
- consistency as a series (Artemov).

**What appears new, pending the literature pass in §7:**

1. **One calculus that both arithmetizes and reflects uniformly.**
   - *Arithmetizes:* raw codes with an executable checker and inspection,
     enough for G2 to hold for free codes (P5).
   - *Reflects:* it derives a *quantified* explicit reflection principle over
     all held certificates, Π(r :₁ R), as one closed term.
   - *Against λ□:* λ□ escapes G2 by not arithmetizing — □A is typed code,
     not data with a checker.
   - *Against quantified logics of proofs:* where they quantify over proofs,
     uniform reflection fails, or the logic is not even axiomatizable.
2. **The mechanism is a resource discipline.**
   - *It makes held proofs standard by construction:* an R-value is built
     from tokens the run holds, so it cannot be "nonstandard". That is the
     defect Artemov blames for the unprovability of ∀x ¬x:⊥.
   - *It prices the operations LP gets free:* D2 is never cheaper than a
     fresh proof (Theorem 4.6), and D3 costs more than twice the original
     (Corollary 4.6″). Both hold for formats satisfying `Enc46`, but not as
     yet for F7's padded format (§3's caveat).
   - *It explains why LP's free `·` and `!` cannot coexist with internal
     uniform reflection.* With them, the Löb derivation of draft §8.3 —
     Kavvos's Ω, in computational form — would go through. That is the
     draft's argument, not a formal theorem. Theorems 4.6 and 4.6″ show only
     that λᶜᵉʳᵗ₀ does not have the operations.
3. **Self-consistency as evaluation.**
   - *Reflect is a certified self-evaluator that descends in budget,* with
     soundness at base types (Theorem 5.2: no evaluation reaches abort, H or
     H₁).
   - *So the reading "consistency = evaluation at the empty type" comes with
     a proof that the evaluator is total and safe,* relative to the
     metatheory.

## 6. The computational meaning of self-justification in λᶜᵉʳᵗ₀

Artemov's answer to "what is provability computationally?" is that proofs are
objects, and that operations on proofs (m, c, `·`, `!`, selectors) are
computable functions. λᶜᵉʳᵗ₀ answers four related questions at runtime.

1. **Self-consistency is the evaluator at the empty type.**
   - *The BHK reading:* a proof of "no held certificate refutes the system" is
     a function from held refutation certificates to ⊥.
   - *λᶜᵉʳᵗ₀'s function is `reflect₀`:* run the certificate's program.
     Running a typed program yields a value of its type (Lemma 3.6, Theorem
     5.2), 0 has no values, and the run descends in budget. So the function
     is total, and its domain is empty.
2. **Certified evaluation at base types.**
   - *What it is:* `reflect_D : □D → D` turns a held certificate into a
     trusted value of D (Nat, Bool, Syn …), by running it.
   - *The payoff Artemov names* (2019, §5): trust in a verified program
     without an extra consistency assumption. λᶜᵉʳᵗ₀ delivers it inside the
     calculus, for budget-bounded certificates.
3. **The provability operations have prices.**
   - *What they cost:* D1 costs the certificate's size; D3 more than twice
     that; D2 as much as a fresh proof.
   - *This is the computational form of the G2 boundary:* uniform reflection
     is kept, and the operations that would close Löb's loop are made to
     cost tokens.
4. **H has a runtime signature** (LOG, 2026-10-04). The verifier accepts
   different programs with and without H and H₁, so whether a program runs
   at all depends on them.

**What λᶜᵉʳᵗ₀ does not yet give** is an *internal, contentual* consistency
argument. Its invariant lives in the metatheory. Closing that gap is the
λᶜᵉʳᵗ₀ analogue of Artemov's selector proof:
- **Conjecture (unverified).** For each n there is a derivation, inside
  λᶜᵉʳᵗ₀ or in PA, of BF₀(n). It would be given by a selector that computes,
  from n, a bound on the type levels of derivations of size at most n. It
  would use normalization for the fragment of System T at that level, which
  PA proves one level at a time, as Artemov's Trₙ.
- **The ceiling:** by §4.2, such a selector exists exactly when
  PA ⊢ Con(PA) → Con′(λᶜᵉʳᵗ₀). If both hold, λᶜᵉʳᵗ₀'s self-justification
  would be a typed internalization of Artemov's selector proofs. That would be
  the strongest precise form of the "computational meaning" claim.

## 7. Recommendations

1. **Do the secondary-literature pass R5 now.** ADR-0002 has listed it as
   pending since 2026-09. It is now load-bearing for novelty, so the [search]
   items above must be read, together with:
   - Artemov–Bonelli's intensional λ-calculus;
   - Pouliasis–Primiero's J-Calc;
   - graded modal type theories: bounded linear logic, Granule;
   - Rendel–Ostermann–Hofer on typed self-representation;
   - Gross–Gallagher–Fallenstein on Löb and quines;
   - the rest of Artemov (2024) and Gadsby (2025).
2. **Formalize "no certified Ω":** no budget derives a term that reflects a
   certificate of itself.
3. **Settle §6's conjecture,** and with it whether PA proves
   Con(PA) → Con′(λᶜᵉʳᵗ₀).
4. **Fix the wording of the papers.**
   - Say "self-justifying in Willard's sense (Definition 3.4)".
   - Address Artemov's contentual criterion explicitly.
   - Position the work against λ□, GLA and quantified logics of proofs.
5. **Make the dependence on E3 explicit** (§4.4), or remove it.

## 8. Appendix: the F7 encoding issue, and its effect on this comparison

*Added later on 2026-10-06. It expands §3's caveat, from the Theorem 4.6
delegate's findings (branch `f4-theorem46`, `enc46f7.clj`) and from the
definitions in `certenc.clj`, `check_spec.clj` and `prop434.clj`.*

### 8.1 F7's certificate format

F7 is the one concrete, computing checker, `Check decCert`. Through it, T1
and T2 hold with no hypotheses, relative to the metatheory. A certificate is
the code

```
sn 0  ⌜(fuel, m, t, A, typing tree, formation tree)⌝  pad
```

It decodes to a budget m, a term t and a type A. Check accepts it as a proof
of A when four conditions hold:
- the data decodes;
- the trees check (`dtCheck`);
- ⌜A⌝ matches the expected type code;
- three size tests pass, each on the *whole* code c:
  - **m < ‖c‖**: strict overhead (Lemma 2.7), for the *declared* budget.
    `reflect`'s budget descent relies on this one.
  - **2f < ‖c‖** (TokSize). Here f is the number of the m declared tokens
    that occur *free in the term t*, so f ≤ m, and m − f counts tokens
    declared but never used. Formally (`prop434.clj`):
    ```
    f = cntU (maskUF (thetaU m) (freshF t))
    ```
    `thetaU m` is Θₘ's usage vector, every token at usage 1. `freshF t i` is
    true when variable i is not free in t, and `maskUF` zeroes those
    entries. `cntU` counts what is left.
  - **‖⌜A⌝‖ < ‖c‖** (TypeSize): a certificate is larger than its type's code.

**Why the paper has these facts, and why f matters.**
- *The paper derives them from the encoding's layout.* Each token the term
  uses is recorded twice, in disjoint subtrees under the root derivation node
  (E2): once as a context entry (E3), once as an occurrence in the encoded
  term (E4). So ‖c‖ ≥ 1 + m + f, and with f ≤ m this gives 2f < ‖c‖. The
  context alone gives m < ‖c‖, and the judgment contains ⌜A⌝ (E2, E3).
- *f bounds what the term can build.* By T3's refinement, a certificate value
  the term constructs has at most f nodes, since each node consumes a distinct
  token the term names.
- *Proposition 4.3 combines the two:* a certificate w of □A must build a
  certificate of A, so f ≥ μ(A), and ‖w‖ ≥ 1 + m + f ≥ 1 + 2μ(A). That is
  D3's quotation cost, "more than 2μ(A)".

**Where padding comes from.** F7 *tests* the size facts instead of deriving
them. To keep completeness — every real derivation has an accepted certificate
(`check_complete`) — the certificate carries padding, which inflates ‖c‖ until
the tests pass (`prop410`'s `padC`). The checker reads the padding only
through its size, so the padding may be any code.

### 8.2 Why Theorem 4.6 needs `Enc46`, and why F7 refutes it

**What Theorem 4.6 says.** A term turning certificates of A₁ … Aⱼ into a
certificate of a different B needs k ≥ μ(B) tokens, the size of B's smallest
certificate.

**How it is proved.** Feed the term opaque "phantom" certificates as inputs,
and ask where the output came from:
1. It was built from the term's own k tokens, so k ≥ μ(B).
2. It *is* one of the inputs. That is impossible, since B ≠ Aᵢ.
3. It *embeds* an input strictly inside itself.
   - The paper's encoding facts (E6, the rule facts, E2–E4) say an embedded
     certificate can sit only as a sub-derivation.
   - Its term is then a literal subterm of the output's term, which the
     output's own nodes must re-encode.
   - So the embedding costs at least the embedded term's size.

`Enc46` packages case 3: an accepted code that embeds accepted codes strictly
inside pays, in its own nodes, at least one embedded certificate's term size.

**Why F7 refutes it.**
- Padding is a place inside an accepted certificate, outside every derivation
  node, where any code can sit at no cost in the outer certificate's own
  nodes (`check_pad`).
- So put a huge accepted certificate into the padding of a tiny one. The
  result is still accepted, its own nodes are few, and the embedded term is
  huge. `Enc46` is false at F7 (`enc46_F7`).
- So Theorem 4.6's hypotheses are contradictory there (`thm46_F7_vacuous`).
  At the concrete checker, the theorem and the lower halves of
  Corollaries 4.6′ and 4.6″ say nothing. Their upper bounds still hold.

**What it does not show.** It does not show that D2 is cheap at F7.
- *What still costs tokens:* an output certificate of B still has to contain
  a full derivation of B, built from tokens.
- *What an input can supply for free:* only bulk, to pass the size tests.
- *An unverified estimate:* the true bound at F7 is k ≥ μ(B) minus the padding
  B's smallest certificate needs, which may be small. Nothing is proved
  either way.
- *The open question* is whether free carrying makes D2 cheaper, or only
  breaks this proof.
- *The repair under way* (branch `f7-enc46`): make padding canonical or
  remove it, so that the size facts follow from the encoding as the paper
  intends; then prove Theorem 4.6 at `Check decCert`.

### 8.3 Effect on the comparison with Artemov

**Unaffected.**
- *Self-justification.* T1, T2, Corollary 3.7 and `self_justification` hold
  at F7 as before. They use only soundness and the tested size facts, which
  padding satisfies by construction.
- *H° as evaluation, and the blocking of Kavvos's Ω (§4.3).* `reflect` runs
  at budget m < ‖v‖, which Check tests directly. Padding only makes
  certificates larger, so a program still cannot hold its own certificate.
  The reading "self-consistency is evaluation at the empty type" (§6) stands.
- *The serial-property and selector reading (§4.2), and P5.*

**Affected: the pricing of provability operations.**
- *The claim (§5, item 2):* the half of the novelty claim that separates
  λᶜᵉʳᵗ₀ from LP is that LP's `·` and `!` are free, while λᶜᵉʳᵗ₀ charges for
  D2 (at least μ(B)) and D3 (more than 2μ(A)). That pricing is what lets
  uniform reflection coexist with arithmetization.
- *Its status:* for the one concrete checker there is, it is not yet a
  theorem. It is a theorem about every checker satisfying `Enc46`. That class
  is non-empty, but shown so only by a toy checker (`enc46w`).

**What it sharpens: padding is half of LP's sum.**
- *The parallel:* padding implements one half of LP's sum operation,
  s:F → (s+t):F. A justification stays a justification when arbitrary extra
  evidence is attached.
- *In LP:* this monotonicity is harmless, and it is needed to realize S4.
- *In λᶜᵉʳᵗ₀:* it is exactly what lets one certificate carry another for
  free, and exactly where the D2 lower bound's proof breaks.
- *So the comparison gets a precise condition:* λᶜᵉʳᵗ₀'s pricing, as proved,
  needs *tight* certificates. Every node must be part of the checked
  derivation or canonical: no free `+`.

**The finding, under the project's encoding rule.** Self-justification is
robust to the certificate format. The prices of the provability operations,
which are the boundary with LP, depend on it.
