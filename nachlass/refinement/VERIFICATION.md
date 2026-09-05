# Refinement Verification Registry

The Refinement was opened without the Codification's machinery — no coverage
registry, no `../codification/audit.sh` participation, no unconditional imaging rule. The
2026-09-02 review found that **every Tier-1 defect was of a kind that machinery
exists to prevent**, committed in the one stage not covered by it. This file
closes that gap, and `../codification/audit.sh` now checks it (checks R-A–R-F,
in `../codification/audit-r.sh`).

## The rules

Four inherited from ADR-0001, one added by this stage's own failures.

1. **Every quotation is image-verified. Extracted text is not a source.**
   *Strengthened 2026-09-02 by standing instruction, after the text layer caused
   defects in three consecutive passes.* The earlier form exempted "prose
   containing no mathematics", and the exemption failed twice: a prose citation
   got the wrong page (Pakhomov p. 21 for p. 22, p. 25 for p. 24), and a prose
   sentence was quoted non-verbatim while tagged image-verified. `pdftotext`
   also strips typography silently — every `□` from a Beklemishev–Shamkanov
   block quote, and the `□` from Theorem 4's conclusion `⊠⊤ =_S □⊥`, which a
   draft reproduced as `⊠⊤ =_S ⊥`.

   **The rule is unconditional: read the page.** Extracted text may *locate* a
   passage and may never be quoted from. The `txt` status is retired; check
   **R-B** fails any register row that is not `img`.
2. **Every claim about a Willard result carries that result's `Proof` status**
   from `../codification/registry/results.md`, at the point of *use*.
3. **Every claim about what the corpus does or does not say is checked against
   the extraction records first.** Finding #1 was a claim that Willard never
   names a derivability condition, contradicted by an extraction section
   titled "Which derivability condition fails".
4. **Departures are declared** (ADR-0002), and so is the reverse — Willard's own
   statements are not presented as the Refinement's inferences.
5. **Arithmetic is computed, never read off an author's informal gloss.**
   `Willard2006a` calls the Hybrid convention "midway between the additive and
   multiplicative"; a draft took that as exact and built a criterion on it. The
   actual growth is `Θ(n (log n)^H)`, which refutes the criterion. Every growth
   or size claim in this stage is now recorded in **Computed claims** below with
   the computation that produced it.

## Consolidation, 2026-09-04

**Drafting history lives here, not in the documents.** Through twenty-five
review passes, each correction was recorded *in place* in the prose — "an earlier
version said X, which was wrong". Eighty-one such passages accumulated, and the
cost was real: `refined-sjas.md` grew from about 670 lines to 955, a large part
of it audit trail rather than content, and §2.3b came to contain a correction of
a correction. A reader wanting the refined statement of SJAS had to wade through
the record of its drafting.

The apparatus has been removed from all four prose documents. What was a
narrated withdrawal is now either a plain statement of what holds, or — where
the withdrawal is itself a finding — a statement of the finding without the
drafting story. Nothing verified was lost: every substantive caveat (proof
statuses, the `sketch` on Lemma 4.6, the undetermined Hybrid term, the retracted
Level(n) instance, the unguarded encoding hazard) survives as a present-tense
claim.

| Document | Before | After |
| --- | --- | --- |
| `refined-sjas.md` | 955 | 901 |
| `R3-the-margin.md` | 711 | 686 |
| `R2-…-assessment.md` | 273 | 256 |
| `R1-review.md` | 173 | 171 |

The corrections table below is the record. **Check R-D now guards ten of the
retracted strings** and none of them appears in the prose at all, so it is
protecting against reintroduction rather than tolerating a mention.

## Corrections applied 2026-09-04, twenty-sixth pass

**The consolidation itself, reviewed.** It rewrote about eighty passages, and the
first check after it was structural only — balanced markup, intact headings and
tables. That verifies nothing about *meaning*. Reviewing the diff hunk by hunk,
concentrating on those that removed more than they added, found two places where
substance left with the narration.

| # | Defect | Where it was |
| --- | --- | --- |
| 111 | **A provenance anchor deleted with the drafting story.** §2.3's retraction paragraph carried the pointer that `../codification/extraction/willard1993-tr.md` §3.7c is titled *"Which derivability condition fails"* — the Codification's own record of the very reading §2.3 argues for. Removing the narration removed the anchor. Restored as a plain citation | `refined-sjas.md` §2.3 |
| 112 | **`Willard2006a` Theorem 4\*'s stronger half lost.** The consolidated text said only that it "drops the third hypothesis". Theorem 4\* also "isolate[s] a `Π⁻₁` theorem `W` of Peano Arithmetic, where no consistent r.e. axiom system `α ⊃ W`, using the multiplicative naming convention, can formally verify its own Hilbert consistency" — a strictly stronger statement, not merely the same theorem with one fewer hypothesis. Restored verbatim | `refined-sjas.md` §2.3b |

**Verified and found intact** in this pass, hunk by hunk: every other
consolidation edit preserved its substance. `Willard2020` Thm 4.5's `sketch`
status and its hypothesis on `β`; Theorem 4's three hypotheses; the §3.1 anchor,
`Tab-U*₁-List` and `Willard2005` Thm 5 in §2.3c; the two-methods correction in
§6; Lemma 4.6's `sketch` in R3 §6; the "not complete for it" verdict in R3 §5.3;
Theorem 4's `⊠⊤ =_S □⊥` and the Introduction-not-abstract correction in R2 §1;
and R1-review's p. 22 citation and superseded-B5 note. Markup balanced, headings
and table counts unchanged, and no line over 100 columns except the intentional
single-line display of the three derivability conditions.

## Pass twenty-seven, 2026-09-04 — no corrections

The target named at the close of the twenty-sixth pass: `lawvere-sjas.md` §§2.2
and 4.4, whose van Dijk–Oldenziel citations were the last load-bearing claims in
the stage with no image verification behind them. Checked against printed
pp. 26, 28 and 30, now registered above.

**Every citation in §2.2's table is correct.** Prop. 6.5 is `Nec` and its proof
is "Immediate from lemma 5.15", as the table says; Prop. 6.6 is `Four` and its
proof is "Apply the functor `□ = Γ ∘ R`", verbatim; Prop. 6.8 is *Internal Modus
Ponens* and does carry the evaluation map
`ev : Hom(φ',ψ') × Hom(1',φ') → Hom(1',ψ')`; Props. 6.7, 6.9 and 6.10 are the
modus-ponens and composition steps, used at steps 4, 6, 7, 9 and 10 of Thm 6.13's
proof; Lemma 6.12 supplies the Löb sentence through `T_φ : P1' → P1'`. §6 does
open "This is a sketch of a proof of Löb's theorem", and Rem. 6.14 is the
non-cartesian-closure warning.

**"Theorem 6.13 derives Löb" is right, and worth stating precisely.** The paper
*states* Löb as Theorem 6.4 on p. 28 and *derives* it as Theorem 6.13 on p. 30
("Recall our formulation of Löb's theorem"). R6's verb is the correct one.

**Thm 5.20 is the direct G2 result**, verbatim: "Assume that `U_0` is
consistent. Then the subobject `⟦True' = False'⟧ ↪ 1` does not equal the minimal
subobject `0 ↪ 1` in `U_0`" — which, with the sentence before it, is exactly
R6's "rules out an internal proof of the consistency of `U'_0` when `U_0` is
consistent".

**§4.4 is sound.** Its `H := SelfRef(B,D)`, `α* := B + H`, the observation that a
consistency point alone is cheap because an inconsistent theory has one too, the
appeal to `Willard1993-TR` Proposition 1 with its `full` status carried, and the
caution against moving between `SelfCons_0` and `SelfCons_1` without rechecking
the proof relation — all check out.

*This is the first pass whose named target yielded no correction.*

## Quotation register

Every row is `img` — verified against a rendered page image; check **R-B** fails
any row that is not.

**A standing interaction, noted after it happened four times.** Every check that
scans prose will eventually flag *this file*, because the corrections table
names the defective artifact it is recording — a stripped `□`, an elided path, a
retracted sentence, a bare filename. R-C, R-D and R-F have each been caught by
it. The two remedies in use are a line-local guard word (R-D, R-C) and simply
naming the artifact in plain text rather than in backticks (R-F). A future check
over these documents should expect the same and choose one of them deliberately,
rather than discovering it as a failure.

**What R-B cannot do, stated so it is not mistaken for coverage.** It validates
the rows that exist. It cannot detect a quotation *in the documents* that has no
row here, and it does not diff any quotation against its source. **Register
completeness and verbatim transcription are human obligations**, like the one
ADR-0002 **B6** already records. The 2026-09-02 review of `refined-sjas.md`
found nine quotations with no row (listed in that document's §9); each is backed
by an image-verified row in `../codification/registry/results.md`, and none has
been checked against the page in this stage.

| Quote / claim | Source | Anchor | Verified |
| --- | --- | --- | --- |
| "every self verifying system must breach … one of the three fundamental Hilbert-Bernays conditions" | `Willard1993-TR` | printed p. 1 | img (C5) |
| "may arise because of either the axiom system employed or the choice of deduction method, but not necessarily due to both" | `Willard1993-TR` | printed p. 1 | img (C5) |
| **"only in the *degenerate case* where `x` and `y` are *fixed constants*"** — the condition-(2) failure | `Willard1993-TR` | printed p. 12 | img (C5); recorded at `../codification/extraction/willard1993-tr.md` §3.7c |
| Theorem A.1 (`stated-only`) and footnote 16: for `Theta(z) := forall x forall y (Subst(z,x) -> not Derive(x,y))` and `N = code(Theta)`, `alpha` proves `Theta(N) <-> not Der(code(Theta(N)))`; the fixed-point identity is the only step said to need PA's `Pi^-_1` theorems, while the rest uses conditions (1)-(3) | `Willard2001` | pp. 44–45 | **img (2026-09-02; formula re-verified 2026-09-04)** |
| Hájek–Pudlák `ω_i` (`ω₀(x) = 2x`, `ω_{i+1}(x) = 2^{ω_i(⌈Log₂(x+1)⌉−1)}`) and Wilkie–Paris `ϖ_i` (`ϖ₀(x) = x`, `ϖ_{i+1}(x) = x^{ϖ_i(⌈Log₂(x+1)⌉−1)}`) | `Willard2001` | p. 45 | **img (2026-09-02)** — a **third** `ω` family; see drift **D82** |
| Printed p. 1's Feferman contrast: "[Fe60] illustrates a self-verifying system which employs all the axioms of Peano Arithmetic, but which draws upon a deduction method which captures the numerical but not intensional definition of classic deduction"; `IS(A)` uses "a deduction method as natural as semantic tableaux" and weakens "several of the axioms of Peano Arithmetic, including the axiom that the multiplication function is total". Also: the governing HBL fact is stated on **four** pages — printed pp. 1, 2, 6 and 12 | `Willard1993-TR` | printed pp. 1, 2, 6 | **img (2026-09-02)** |
| Theorem 2.2 and its proof sketch: composing `Λ`, `Θ`, `Λ∧Θ ⇒ Ξ`, where under a cut-free method the composite's "length can certainly be super-exponentially longer than the combined lengths of `p`, `q` and `r`" | `Willard2002a` | printed p. 7 | **img (2026-09-02)** |
| **Continuously Expanding**: "there exists a sequence of constants `K₁, K₂, K₃, …` with `K_i < K_{i+1}` such that the set of axioms with Gödel numbers less than `K_i` is sufficient to generate a proof of the existence of an integer larger than `K_{i+1}`"; the incremental convention fails it "because it grows too slowly". **Theorem 4**'s three hypotheses verbatim, and **Remark 1**'s Theorem 4\*, which "will drop Theorem 4's 'Concise Encoding' assumption" | `Willard2006a` | printed p. 7 | **img (2026-09-02)** |
| Abstract: "we show that **static type checking in F_ω can exclude the proof's diagonalization gadget**, leaving open the possibility for a self-interpreter"; and the normalization barrier "stems from a theorem in computability theory that says that a total universal function for the total computable functions is impossible" | Brown–Palsberg 2016 | p. 1 | **img (2026-09-02)** |
| "Alternative Naming Conventions": hybridized conventions "lie midway between the additive and multiplicative conventions"; Hybrid(`H`) defines `C_i = ⌈2^{[Log(i)]^H}⌉·C_{i−1}` **for `i ≥ 3`**; "This convention can unify the formalisms of Theorems 3 and 4"; `H = 1` keeps Thm 3's exception, `H > 1` generalises Thms 4 and 4\*. Also §6's `ISINF(A)`, "Infinitely Far-Reaching without sustaining an ability to prove successor is a total function" | `Willard2006a` | printed pp. 8, 30 | **img (2026-09-02)** |
| Remark 6.16b: the SelfCons axiom "causes axiom systems to produce essentially a 1-line proof of their own consistency … an *instinctive faith* … rather than it supporting a full-length proof-justification"; and that `ξ*`, `ξ**`, `ξ^R` are Type-A systems recognising addition as total | `Willard2011` | printed p. 28 | **img (2026-09-02)** |
| §8's statement ###: "Is it not *almost cheating* when an axiom system verifies its own consistency by using ⊕'s formalized 'I am consistent' axiom as an intermediate step…"; and §9's "arithmetics that were weaker than traditional arithmetics **in, at least, some respects**. (The Second Incompleteness Theorem's significance in refuting the original objectives of Hilbert's Consistency Program is thus, simply, undeniable.)" | `Willard2016` | printed pp. 27, 29 | **img (2026-09-02)** |
| Theorem 4.5 verbatim: `IS_Xtab(β)` "is automatically inconsistent whenever `β` proves some conventional `Π*₁` theorems stating that addition and multiplication satisfy their usual associative, commutative, distributive and identity properties"; and the `sketch` status on the page — the proofs "would be quite lengthy, if they were derived from first principles" | `Willard2020` | printed p. 12 | **img (2026-09-02)** |
| Theorem A.1's three conditions, verbatim, and "Suppose **for any two sentences** `Φ` and `Ψ`" — the uniformity the TR's condition-(2) failure denies; status `stated-only` | `Willard2001` | p. 44 | **img (2026-09-02)** |
| `IS(A)` cannot prove `∀x∃y SUBST_i(x,y)`, but proves `∃y SUBST_i(k̄,y)` for each fixed integer `k̄`; the fixed instance suffices for Group-3, whose sentence and self-dependent proof predicate are Equations (A.2)-(A.3) | `Willard1993-TR` | printed pp. 37–38, Point of Clarification and Eqs. (A.2)–(A.3) | **img (2026-09-04)**; recorded at `../codification/extraction/willard1993-tr.md` §3.8 |
| Lemma 7.1 "replaces the Hilbert-Bernays derivability conditions with a semantic argument" | — | `../codification/registry/results.md` row `Willard2001#Lem7.1` | **img — via the registry digest, image-verified at C7.** Not quoted from the paper directly, and cited as the registry's wording |
| "Conventional generic configurations `ξ` will satisfy the Hilbert-Bernays derivability conditions [16, 15]. Their `G^ξ_k(θ)` will thus be automatically inconsistent because of a Gödel-like diagonalization argument." | `Willard2011` | printed p. 21 fn. 11 | **img (2026-09-02)** |
| Tight, Def. 4.5 — refutation length at least envelope plus two | `Willard2011` | printed p. 13–14 | img (C6, 2026-08-21) |
| Defs. 5.1 / 5.3 / 5.5 — A-Stable, E-Stable, EA-stable; Thm 5.9's dependencies are 5.5 and 5.7, not 5.1 | `Willard2011` | printed pp. 16–18 | img (C6); statuses from `../codification/registry/results.md` |
| Def. 4.5 (Tight) verbatim; Lemma 4.6 with Eq. (19) `Log(q) = ♯(Ψ)+1`, Eq. (20), and "because all of `B^ξ`'s axioms are true under the Standard-M model"; status `sketch`, proof "kept brief and informal" | `Willard2011` | printed pp. 14–15 | **img (2026-09-02)** |
| Def. 4.1 (`E(n)` denotes the value `2^n`), Defs. 4.2–4.4, footnote 6 (`Good(N)` runs opposite ways on `Π` and `Σ`) | `Willard2011` | printed pp. 12–13 | **img (2026-09-02)** |
| R-View = "any recursively enumerable (r.e.) set of `Π^ξ₁` sentences … It only needs to be r.e."; RE-Class; Defs. 5.1 and 5.3 verbatim; Remark 5.2's "Theorem 5.11 will show the presence of A-stability, alone, is sufficient" | `Willard2011` | printed pp. 15–16 | **img (2026-09-02)** |
| Conventional Tableaux Encoding Requirement, `32^J` / `5J` bits — a **lower** bound | `Willard2011` | printed p. 39 fn. 23 | img (C16 Q1) |
| the two sequences and their encoding lengths | `Willard2020-LFCS` | p. 11 | img (C10) |
| `U-Height(p)`, `θ` values | `Willard2005` | pp. 19, 26 | img (C10) |
| Remark 4 — the strong half attributed: "a Level(0-) tableaux generalization **in [68] implies that** … no useful analog of Definition 5 can be found …" | `Willard2005` | printed p. 21 | img (C10); `remark`, no proof status |
| `ℑ(H) = Σ χ(p_i)` | `Willard2004` | printed p. 363 | img (C16 Q18) |
| Hybrid(H) recurrence | `Willard2006a` | p. 30 | img (C16 Q20) |
| `ISINF(A)` Infinitely Far-Reaching, Theorem 6 | `Willard2006a` | printed p. 8 | img (C11) |
| §4's `(∗)`: "If `p` is a proof of the theorem `T` from `α`, then a proof from `α` that '`p` proves `T` from `α`' will assuredly have a Gödel number less that `ω₁^K(p)`, for some fixed constant `K`"; `ω₁(x) = x^{Log(x)}`; and `IΣ₀` cannot prove `∀x ∃y ω₁(x) = y` (Eq. 32) | `Willard2007-APAL` | printed pp. 18–19 | **img (2026-09-02)** — previously unrecorded anywhere in the Codification; now in `../codification/registry/notation.md` with drift **D82** |
| Lemma 5 — `O(n)` nodes, "whose binary encoding has a `2^n` length that is much larger than `F`'s length"; status `sketch`, "We obviously have omitted many details here" | `Willard2009` | printed p. 24 | img (C11) |
| "the true reason … Lemma 8 … collapses entirely" | `Willard2000-TAB` | §5 | img (C10) |
| Def. 3.7 plain contraction and weakening; Prop. 3.8's C3 step **with `□`**; Thm 3; Thm 4 (contraction **and** weakening; conclusion `⊠⊤ =_S □⊥`); Rem. 3.9's `□`-contraction and `□`-weakening | Beklemishev–Shamkanov | p. 8 | **img (2026-09-02)** |
| Defs. 2.1 and 2.3 (preorder, external consistency, APS); Defs. 3.1–3.4 (multiset consequence, implication, conjunction, L1–L3); and Rem. 3.6's no-built-in-contraction warning | Beklemishev–Shamkanov | pp. 3–7 | **img (2026-09-04)** |
| Löb conditions L1–L3 and the no-built-in-contraction warning; concrete `S` has modalized fixed points and its displayed modal rule; Prop. 4.1 states failure of formalized G2 after calling it easy from cut-freeness but gives no proof; Prop. 4.3 gives many fixed points; Lem. 5.1 proves weakening admissible; Thm 5 proves cut admissibility | Beklemishev–Shamkanov | pp. 7, 9–11 | **img (2026-09-04)** |
| "the system `S` does not provide a counterexample to the non-formalized version of G2, since `⇒ ¬□⊥` is not provable" | Beklemishev–Shamkanov | p. 14 | **img (2026-09-02)** |
| "we are still missing convincing examples of mathematical theories based on weak logics for which G2 would fail" | Beklemishev–Shamkanov | p. 14 | **img (2026-09-02)** |
| superexponentiation, the cut `S`, the finite model of size at most the tower, and `Con^pred` | Pakhomov | p. 4 | **img (2026-09-02)** |
| "Important restriction here is that both `H` and `H_{<ω}` could not prove totality of successor function"; and Pudlák's structure — a failure of G2 for `T` "leads to a failure of G2 in a different theory `T′` (interpretable in `T`), where HBL are satisfied" | Pakhomov | **pp. 3–4** — the sentence straddles the page break | **img (2026-09-02)** |
| "the properties of `SUM` in theory `H` that could not even prove totality of successor function" — about `H` alone, in the proof of Cor. 5.3 | Pakhomov | **p. 22** (not p. 21) | **img (2026-09-02)** |
| footnote 3 (Beklemishev's simplification "still unpublished"); Acknowledgments | Pakhomov | **p. 24** (not p. 25; p. 25 is References) | **img (2026-09-02)** |
| "The theories in his examples are not completely natural in the sense that some of axioms are constructed using Diagonal Lemma. The main result of the present paper is the construction of a more natural example of this kind." | Pakhomov | p. 3 | **img (2026-09-02)** |
| §6's opening — "**This is a sketch of a proof of Löb's theorem in Arithmetic Universes**"; Def. 6.1 (`U ⊨ φ ⊢ ψ` as an inclusion of subobjects); Def. 6.2 (implication of judgements via `i*(σ) ↪ i*(τ)` in `U[φ ≤ ψ]`); "An arithmetic universe is typically **not cartesian closed**" | van Dijk–Oldenziel | printed p. 28 | **img (2026-09-04)** |
| **Prop. 6.5** "If `U ⊨ φ` then `U ⊨ □φ`", proof "Immediate from lemma 5.15"; **Prop. 6.6** `U ⊨ □φ ⊢ □(□φ)`, proof "Apply the functor `□ = Γ ∘ R`"; **Prop. 6.7 (Modus Ponens)**; **Prop. 6.8 (Internal Modus Ponens)** with its evaluation map `ev : Hom(φ',ψ') × Hom(1',φ') → Hom(1',ψ')` | van Dijk–Oldenziel | printed p. 28 | **img (2026-09-04)** |
| **Thm 6.13**, the derivation of Löb ("Recall our formulation of Löb's theorem") with its ten-step proof citing Props. 6.5–6.10 and Lemma 6.12; **Lemma 6.12** supplying the Löb sentence via `T_φ : P1' → P1'`; **Rem. 6.14**, "As arithmetic universes are not in general cartesian closed there is no one notion of implication" | van Dijk–Oldenziel | printed p. 30 | **img (2026-09-04)** |
| **Thm 5.20 (Gödel's Second Incompleteness Theorem)**: "Assume that `U_0` is consistent. Then the subobject `⟦True' = False'⟧ ↪ 1` does not equal the minimal subobject `0 ↪ 1` in `U_0`" — with the preceding sentence defining what it is for `U_0` to prove the consistency of `U'_0` | van Dijk–Oldenziel | printed p. 26 | **img (2026-09-04)** |
| `□ = Γ∘R`; propositions as subobjects of `1`; classifying-AU implication; direct G2 Thm 5.20; Props. 6.5–6.10 including internal-Hom evaluation in Prop. 6.8; Lem. 6.12 fixed points; Thm 6.13 Löb; Rem. 6.14 non-cartesian-closure warning; §6 is explicitly introduced as a sketch | van Dijk–Oldenziel | printed pp. 26, 28–30 | **img (2026-09-04)** |

## Computed claims

Rule 5. Every growth or size claim, with the computation.

| Claim | Where used | Computation |
| --- | --- | --- |
| additive naming: `Log₂ C_n = n` | `refined-sjas.md` §§4–5 | `C_i = 2C_{i−1}` |
| multiplicative naming: `Log₂ C_n = 2^n` | `refined-sjas.md` §§4–5 | `C_i = C_{i−1}²` |
| **Hybrid(H): `Log₂ C_n = Σ_{i≤n} (Log i)^H = Θ(n (log n)^H)`** | `refined-sjas.md` §5, `R3-the-margin.md` §3.1 | summed numerically for `H ∈ {1,2}`, `n ∈ {10,100,1000}`: `H=1` gives 21.8 / 524.8 / 8529.4 against `n` = 10 / 100 / 1000. Hybrid(1) is **not** `O(n)`, which refutes the withdrawn `O(L)` criterion |
| encoding requirement gives `Log(q_β) ≥ 5J` | `R3-the-margin.md` §1.1 | `Gödel number ≥ 32^J` and `log₂ 32 = 5`. A **lower** bound: it excludes over-compressed encodings, not wasteful ones |

## Corrections applied 2026-09-02

| # | Defect | Where it was |
| --- | --- | --- |
| 1 | Claimed Willard never names the breached condition; he names (2) for `IS(A)`, as a uniformity failure | `refined-sjas.md` §2.3 |
| 2 | Claimed Pakhomov's `H_{<ω}` proves successor totality and so falsifies R1; it cannot, and does not | `R1-review.md` §2, `refined-sjas.md` §1, `ADR-0002` |
| 3 | Text-layer `□`-stripping. Two distinct instances, not one quote in three places: Prop. 3.8's C3 step in `refined-sjas.md` §2.5, and Thm 4's conclusion `⊠⊤ =_S □⊥` in `R2` §1. `R3` §5 was touched by the same pass but its p. 14 material had a different defect — see #23 | `refined-sjas.md` §2.5, `R2` §1 |
| 4 | Listed Beklemishev–Shamkanov's `S` as a route to self-verification; their §6 says it is not | `R3` §5, `R2` §2.3 |
| 5 | `Willard2020` Thm 4.5 called "Established"; it is `sketch`, with a dropped hypothesis on `β` | `refined-sjas.md` §2.3, `R3` §§3.2, 4 |
| 6 | `Willard2001` Thm A.1 said to be "proved"; it is `stated-only` | `R2` §2.1 |
| 7 | The `O(L)` naming criterion misclassifies Hybrid(1), the positive case | `refined-sjas.md` §5 |
| 8 | Hybrid(H) assigned to the envelope side without argument; withdrawn | `R3` §3.1, `refined-sjas.md` §§5.1, 9 |
| 9 | Level(n) said not to change proof cost, then said to lower it | `R3` §3.3 |
| 10 | EA-stability given as Def. 5.1 and as the `Π` half only; it is Def. 5.5, both halves, quantified over possibly-false R-Views | `R3` §1 |
| 11 | Encoding requirement said to guard against *wasteful* encodings; it is a lower bound and guards the other direction | `R3` §1.1 |
| 12 | `ℜ` and `Z` treated as interchangeable — the merge codified §8.1 forbids (D34, O46, G22) | `R3` §3.2 |
| 13 | BS Thm 4 said to need weakening "not contraction"; it needs **both**, plus Löb's conditions | `R2` §1 |
| 14 | Dial-status accounting said "two of five are sketch", counting one dial's halves as two dials and omitting `Z` | `R3` §4, `refined-sjas.md` §9 |
| 15 | "Self-verification is the failure of G2's argument" — a biconditional `S` refutes | `R3` §5, `R2` §2.3 |
| 16 | "Four things are given up", followed by five | `refined-sjas.md` §7 |
| 17 | `Willard2005` Remark 4's attributed clause ("in [68] implies that") elided; status not carried | `refined-sjas.md` §7 |
| 18 | `Willard2009` Lemma 5 called "the cleanest available form"; it is `sketch` | `refined-sjas.md` §4 |
| 19 | §9 still instructed a continuation to acquire G7, closed by `R1-review.md` §1 | `refined-sjas.md` §9 |
| 20 | §1's roadmap off by one from §6 onward | `refined-sjas.md` §1 |
| 21 | Pakhomov's model bound glossed as small; it is superexponential, on the cut, and the sentence is `Con^pred` | `refined-sjas.md` §1, `R3` §5.1 |
| 22 | Conditions (1) and (2) called "permissions to use a proof again"; reuse is (3) | `R2` §2.1 |

## Corrections applied 2026-09-02, second pass

From the independent review at [`REVIEW-2026-09-02.md`](REVIEW-2026-09-02.md).
The first four are the serious ones: three are failures of the very discipline
this file exists to impose, and two are defects in the checks meant to enforce it.

| # | Defect | Where it was |
| --- | --- | --- |
| 23 | **Two Beklemishev–Shamkanov quotations tagged "(image-verified)" were not verbatim** — the paper's own abbreviation "G2" was expanded, a clause "in it" was added, and a mid-section sentence was called the section's close. The substance held; the fidelity claim did not | `R2` §1, `R3` §5.2 |
| 24 | "The abstract's" quote about uniqueness and weakening is from the **Introduction, p. 2**; the abstract mentions neither | `R2` §1 |
| 25 | "The encoding requirement is precisely what keeps them within constant factors" — contradicts this document's own correction #11 two paragraphs earlier: a lower bound relates the measures in one direction only | `R3` §1.1 |
| 26 | **R-B's hazard regex tested only the *stripping* family** (`□`, turnstiles, corners) and missed the *substitution* family — `℧`→`0`, `ℑ`→`=`, flattened floors and towers (drift **D71**) — which is the more dangerous one, because the output reads as ordinary text. Constructed `txt` rows carrying `℧` and `ℑ` passed as well-formed | `../codification/audit-r.sh` |
| 27 | **R-D's context-window guard was defeated by a withdrawal word in neighbouring, unrelated prose.** The window had already been narrowed once from six lines to one for this exact failure; narrowing raised the bar without closing the hole. Now line-local: a line is guarded iff it quotes the retracted text or itself carries a retraction verb | `../codification/audit-r.sh` |
| 28 | Pakhomov page numbers: the "both `H` and `H_{<ω}`" sentence spans **pp. 3–4** (correct), but the second citation is **p. 22**, not 21, and is about `H` alone; footnote 3 and the Acknowledgments are on **p. 24**, not 25 | `VERIFICATION.md`, `R1-review.md` §2.1 |
| 29 | `refined-sjas.md` §2.3b's cross-reference "§4 below cites the same Thm 4" cites nothing — §4 does not cite `Willard2006a`, and the document's other "Theorem 4" is `Willard2009`'s | `refined-sjas.md` §2.3b |
| 30 | `R1-review.md` finding #7 still called G38 "acquisition target for R5" while two files in the same commit recorded it closed | `R1-review.md` §3 |
| 31 | Correction #3's location list read as one quote in three documents; it was two distinct instances in two | `VERIFICATION.md` |
| 32 | **B6 overclaimed.** "Every mathematical quotation image-verified … audit.sh green" was asserted while #23 stood, and `../codification/audit.sh` cannot check quotation *fidelity* at all — R-B tests only whether a `txt` row carries hazard symbols | `ADR-0002` |

**What R-D is, stated plainly.** A regression guard on *fixed strings*. It
cannot detect a retracted claim that has been reworded, and it does not verify
that a marker is about the same claim as the text it guards. A green R-D means
"these exact sentences have not returned unmarked" and nothing stronger. The
review demonstrated the rewording gap by construction; it is inherent to the
mechanism, and is recorded rather than papered over.

## Corrections applied 2026-09-04, third pass

From a review of `refined-sjas.md` for completion and correctness.

| # | Defect | Where it was |
| --- | --- | --- |
| 33 | **§5.1 and §9 contradicted R3's verdict, committed the same day** — both still counted four dials and kept Level(n) on the cost side, which `R3-the-margin.md` §3.3 retracts; §9 still said the margin "generalises" Def. 4.5, withdrawn in R3 §2 | `refined-sjas.md` §§5.1, 9 |
| 34 | §9 omitted R3's findings entirely — the retraction, the `♯`-is-`d`-independent lemma, the forced threshold, and the withdrawal of the stability bridge's novelty | `refined-sjas.md` §9 |
| 35 | §2.5b asserted Pakhomov "does not remove" a derivability condition. He is silent on the point; p. 3 says HBL "do not necessary hold" for weaker c.e. theories. What he blocks is Pudlák's *interpretation* step, which needs successor totality — Willard's door | `refined-sjas.md` §2.5b |
| 36 | "the tableaux line as tabulated above" — no table exists above that line | `refined-sjas.md` §2.3c |
| 37 | "the distinction §8 is simultaneously trying to draw" — §8 is the computational analogue; the distinction is §5.1's. Second mis-numbered cross-reference in the same subsection, the first fixed in the previous pass without sweeping for others | `refined-sjas.md` §2.3c |
| 38 | Nine quotations in `refined-sjas.md` have no register row, so under the images-only rule they are unverified in this stage. Recorded in §9 and above; R-B structurally cannot catch this | `refined-sjas.md`, `VERIFICATION.md` |
| 39 | **Codification gap, not a Refinement error.** `ω₁(x) = x^{Log(x)}` and the `ω₁^K(p)` bound (`Willard2007-APAL` §4), on which §3 rests, were recorded nowhere in the registries — while `R1-review.md` listed §3 as "verified against `Willard2007-APAL` §4". Verified from the page images; §3 is **correct as written**. Now registered, with drift **D82** for the collision with `Willard2020-LFCS`'s different `ω_j` | `../codification/registry/notation.md`, `../codification/concordance/drift-ledger.md` |

## Corrections applied 2026-09-04, fourth pass

The nine quotations flagged unverified in the third pass were read against the
page. Eight were accurate; the register debt is closed.

| # | Defect | Where it was |
| --- | --- | --- |
| 40 | **A dropped hedge.** `Willard2016` §9 says the evasions rested on arithmetics "weaker than traditional arithmetics **in, at least, some respects**". §7 quoted only "weaker than traditional arithmetics", making Willard's concession absolute. Third elision of this class — after `Willard2005` Rem. 4's "in [68] implies that" and `Willard2020` Thm 4.5's hypothesis on `β`. The pattern is dropped hedges and attributions, not mistranscribed symbols | `refined-sjas.md` §7 |
| 41 | §5.1 glossed `Willard2002a` Thm 2.2 as "cut elimination preserves theoremhood while the proof can be longer". The blowup is in **building the composite** of three proofs under a cut-free method, not in eliminating cuts from one already built; §2.3 had it right | `refined-sjas.md` §5.1 |
| 42 | Hybrid(H)'s domain restriction **`i ≥ 3`** was omitted from the recurrence wherever it appears, and Willard's own framing — the convention "can unify the formalisms of Theorems 3 and 4" — was never cited, though it supports R3's project | `refined-sjas.md` §5, `R3` §3.1 |
| 43 | §9 still listed nine quotations as unverified after they had been verified | `refined-sjas.md` §9 |
| 44 | **Drift D82 understated the problem.** It recorded two `ω` families; there are **three**. `Willard2001` p. 45 defines both the Hájek–Pudlák `ω_i` and the Wilkie–Paris `ϖ_i`, and **`ϖ₁` is exactly `Willard2007-APAL`'s `ω₁`** under a different letter — while `Willard2001`'s Hájek–Pudlák `ω₁ ≈ x²` is essentially `Willard2020-LFCS`'s `ω₀`. The hierarchies are offset by an index as well as differing in definition, so a bare "`ω₁`" is ambiguous even once the paper is named | `../codification/concordance/drift-ledger.md`, `../codification/registry/notation.md` |

## Corrections applied 2026-09-04, fifth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 45 | **§1 said "eleven formula classes".** Codified §3.3 and drift **D50** both say **ten**; D50 is titled "ten formula-class notations for one idea". Also "five apparatus families" for what codified §4.1 catalogues as five *apparatuses* (four textbook plus one of Willard's own), four of which are Hilbert-style | `refined-sjas.md` §1 |
| 46 | **§6 used two argument orders four paragraphs apart.** The Rosser sentence was written `Prf(y, ⌜R⌝)` (proof first, textbook convention) while `SemPrf^K_α(x, y, z)` below it and `Willard2020` Eq. (5)'s `Prf_{IS_D(β)}(x,p)` are theorem-first — in the one section whose point is that the two predicates are the same move | `refined-sjas.md` §6 |
| 47 | **§4 claimed ten occurrences of the doubling device; the map listed nine** — and omitted the **origin text**. `Willard1993-TR` printed p. 6 carries the device (`a₀ ≥ 2`, `a_{i+1} = (a_i)²` needing `2^n` bits; then `b_{i+1} = b_i + b_i`), framed as a philosopher's hedge; the map cited only the 12-page published abbreviation. Row added, image-verified; the count is now genuinely ten | `../codification/concordance/replication-map.md` |
| 48 | §2.3b's hypothesis-count criticism was incomplete: `Willard2006a` **Remark 1** supplies **Theorem 4\***, which drops the Concise Encoding hypothesis. The logical point (affirming the consequent) is unaffected | `refined-sjas.md` §2.3b |
| 49 | **An elided witness path.** §2.1 cited Yanofsky as `lit/…Working Category Theorist…pdf` — unfollowable, and R-C's path pattern did not match it, so it passed in silence. Real path given; **R-C now rejects elided paths** | `refined-sjas.md` §2.1, `../codification/audit-r.sh` |
| 50 | **A bug in that new R-C clause.** It read its hits from a pipeline, so the `while` ran in a subshell and `err`'s assignment to `FAIL` was lost — the run printed `AUDIT-FAIL` lines and still exited green. Now reads from a process substitution; red-green re-tested | `../codification/audit-r.sh` |

## Corrections applied 2026-09-04, sixth pass

Completion, not correctness: what the document omits about its own stage.

| # | Defect | Where it was |
| --- | --- | --- |
| 51 | **§9 never mentioned R5.** ADR-0002's secondary-literature pass — Artemov, Pudlák, Visser, Adamowicz, Kołodziejczyk, Salehi — is a pending component of this stage, and the document's own account of what is unsettled omitted it entirely | `refined-sjas.md` §9 |
| 52 | **Pudlák's paper is not held, and the Refinement leans on it throughout.** *(That entry originally gave a count, "14 times across six files"; correction #95 withdraws hand-maintained counts of this kind.)* That his cut-shortening technique *requires successor totality* is what §§1 and 2.5b use to explain how both Willard's Hilbert line and Pakhomov's `H_{<ω}` evade him — and every invocation rests on Willard's or Pakhomov's description of a paper nobody here has read. Recorded as gap **G39**; §9 now says so, and so does the header | `refined-sjas.md` §§1, 2.5b, 9; `../codification/registry/gaps.md` |
| 53 | **The header overclaimed.** "Every claim it makes about the literature is anchored in the codification's registries" is false in two directions: the secondary literature was never in the Codification's scope, and the Pudlák claims are anchored in nothing held. Rewritten to say which claims rest on what | `refined-sjas.md` header |
| 54 | §2.3 identified the TR's condition-(2) phrasing with Theorem A.1's without saying how they correspond, though §2.3a's whole argument turns on where the quantifiers sit — A.1 is schematic in `Φ`, `Ψ` with the proof quantifier inside `Der`; the TR unpacks the same content over proof codes | `refined-sjas.md` §2.3 |

## Corrections applied 2026-09-04, seventh pass

| # | Defect | Where it was |
| --- | --- | --- |
| 55 | **The corpus has two preservation engines, and two documents conflated them.** §6 said "*every* consistency-preservation proof in the corpus" is a minimal-counterexample argument on `Θ(P)`, and cited `Willard2005` Theorem 1 as an instance. Codified §6.9 opens by denying both: "The Hilbert line does not use `θ`-Compactification. Its preservation results are proved directly, by minimal counterexample." `Willard2005` Thm 1 **is** the `θ`-compactification engine (codified §6.4) — apparatus-neutral, the other method. The least-number mechanism is `Willard2001` Thms 3.4 and 4.3, `Willard2006a` Thm 3, `Willard2016` Thm 6.7. **This entry's own closing clause said that mechanism "belongs to the Hilbert line alone", which correction #94 withdraws: Thm 4.3 is about `IS(A)`, a semantic-tableaux system.** | `refined-sjas.md` §6, `R3-the-margin.md` §1.1 |
| 56 | R3 §3's heading asserted "every dial moves one of the two terms" — the claim §3.3 retracts for Level(n). A reader skimming headings got the withdrawn version; the claim block now carries the verdict beside it | `R3-the-margin.md` §3 |
| 57 | `R1-review.md` §4 still stated B5 as "prove **the five dials** are instances of the general form", superseded by R3's three-of-five and by §5's refutation of the premise that a third instance exists | `R1-review.md` §4 |

## Corrections applied 2026-09-04, eighth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 58 | **§7 used a conditional result without carrying the condition.** Willard's reply to his own "almost cheating" objection is Def. 8.1 + Corollary 8.2, and Cor. 8.2 depends on Theorem 6.7, which is conditional on **Conjecture 6.6** (`stated-only`). Obligation **O66** requires the conditionality be flagged *wherever the result is used*; §7 used it bare, so Willard's answer to his strongest self-objection was presented as unconditional | `refined-sjas.md` §7 |
| 59 | **§8 contradicted itself two paragraphs apart** — "the mechanism is the same as Willard's", then "that **analogy** … is not yet a correspondence". Willard blocks by cost, Brown–Palsberg by typing: one shape, two mechanisms | `refined-sjas.md` §8 |
| 60 | §8 said Brown–Palsberg "do not block the diagonal". Their abstract says static type checking "can **exclude the proof's diagonalization gadget**" — the gadget is precisely what is blocked; what survives is the self-representation | `refined-sjas.md` §8 |
| 61 | **Codification gap.** `Willard2016` Definition 8.1's registry digest was empty — the bare words "Platonic Stability" — which the Refinement discovered by trying to lean on it. Content supplied from Corollary 8.2, with the definition's own statement flagged for the next visual pass | `../codification/registry/results.md` |

## Corrections applied 2026-09-04, ninth pass

Rule 2 — "every claim about a Willard result carries that result's `Proof`
status, at the point of *use*" — was being violated systematically, and only a
deliberate sweep found it.

| # | Defect | Where it was |
| --- | --- | --- |
| 62 | **`Willard2001` Theorem A.1 is `stated-only`, and §2.2 did not say so.** That is the theorem the whole Hilbert–Bernays frame rests on: "Willard's systems prove all of PA's `Π⁻₁` theorems, they therefore *must* breach one of (1)–(3)". Willard writes "we will not give a formal proof of Theorem A.1 in this very short appendix" and offers footnote 16's reduction instead | `refined-sjas.md` §2.2 |
| 63 | `Willard2002a` Theorem 2.2 is **`sketch`** (headed "Proof Sketch" on the page) and was cited bare in three places | `refined-sjas.md` §§2.3, 5.1; `R3-the-margin.md` §3.2 |
| 64 | `Willard2006a` Theorem 6 is **`sketch`**, and `R1-review.md` §2.1 leant on it as the corpus analogue of `H_{<ω}` without saying so | `R1-review.md` §2.1 |
| 65 | **A third check that passed by never matching.** The R-E prototype stripped backticks from the citations it extracted, then grepped the files for the stripped form — which never occurs — so it reported every status as carried. That is the same failure as R-D's first window and R-C's subshell: a check green because its own matching is broken. Caught by red-green testing, which is the only reason any of the three was caught | `../codification/audit-r.sh` (prototype) |

**R-E is informational and never fails on status-carrying**, because no
positional heuristic captures rule 2: a status may sit in a following paragraph
or in a table's surrounding prose, and both a ±2-line and a paragraph window
produced three false positives out of four. R-E instead *lists* every non-`full`
result the Refinement cites, with its status and citation count, so the
obligation is visible. It does fail on one thing it can decide: a citation that
resolves to no row in `../codification/registry/results.md`.

## Corrections applied 2026-09-04, tenth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 66 | **The status that governs the whole of R3 was never carried, in six places.** `Willard2011` Lemma 4.6 — Tightness implies `B^ξ + SelfRef` is a consistent self-justifying system — is **`sketch`**, "kept brief and informal", deferred to §5. But §5's rigorous engine (Thm 5.9, `full`) consumes **EA-stability, not Tightness**, so **the margin's sufficiency has no `full` proof in the corpus**. R3 §6 had said "the corpus already proves the thing R3 wanted proved about it" | `R3-the-margin.md` §§2.2, 6; `refined-sjas.md` §9 |
| 67 | **R3's "shorter route" is also the less rigorous one, and §2.2 implied otherwise.** Willard's chain — A-Stable → (Lemma C.1, `full`) → 0-Stable → (Thm 5.11, `full`) → self-justifying — is `full` end to end. R3's runs A-Stable → (its own argument) → Tight → (**Lemma 4.6, `sketch`**) → self-justifying. Calling it a simplification needs the qualification that it trades rigour for brevity | `R3-the-margin.md` §2.2 |
| 68 | R3 §4 pointed at "§4's second paragraph" for the dial statuses; that paragraph is "Argued, not proved" and holds none. The statuses are in the section's "Inherited weakness" table | `R3-the-margin.md` §4 |
| 69 | **An anchor introduced while repairing another.** Round three replaced §2.3c's dangling "as tabulated above" with "codified §3.2" — which is *The grounding functions* and tabulates no systems. The Type-A row is codified **§3.1** | `refined-sjas.md` §2.3c |

**Verified and found correct** in this pass: every `codified §N` anchor cited
anywhere in the Refinement resolves to a section with the expected content (ten
checked); every cross-document `§` reference into R1-review, R2 and R3 resolves;
§4's claim that Willard names the dying lemma "three times, in three papers" is
verbatim-accurate against codified §7.2.

## Corrections applied 2026-09-04, eleventh pass

The charter's own bookkeeping.

| # | Defect | Where it was |
| --- | --- | --- |
| 70 | **B6 was asserted met while rounds seven and eight were falsifying it** — the second time this criterion has been declared satisfied and then broken. B6 now records both falsifications and says plainly that neither half is machine-checkable, so any future "B6 met" is a claim requiring a fresh sweep rather than a standing fact | `ADR-0002` |
| 71 | The charter's "what remains open" list omitted the two largest open items found this session: that **the margin's sufficiency has no `full` proof** (Lemma 4.6 is `sketch`), and that **R5 is unstarted with Pudlák unheld** (G39) | `ADR-0002` |
| 72 | "reviewed twice" in the status line, after ten passes | `ADR-0002` |

## Corrections applied 2026-09-04, twelfth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 73 | **A residual `□`-stripping, in the document that records the `□`-stripping correction.** R2 §2.3 described Proposition 3.8's step as `Γ ⊢ φ`, `Γ ⊢ ¬φ` ⟹ `Γ ⊢ ¬⊤` — unboxed. The page image has `Γ ⊢ □φ`, `Γ ⊢ □¬φ` ⟹ `Γ ⊢ □¬⊤`, and the boxes are the point: `□¬⊤` *is* `⊠⊤`, which is why dropping contraction costs only *formalized* G2. `refined-sjas.md` §2.5 had it right | `R2` §2.3 |
| 74 | **A citation to a file that does not exist.** R2 §3 pointed at `docs/design/affine-tree-sjas.md` as "the C4 pilot's fidelity target"; it was an untracked working-tree file and is not present. `code/alsjas/` exists but is untracked, so neither is in any committed state a reader can reach | `R2` §3 |
| 75 | **Nine repository paths a reader could not follow.** The codification registries were cited bare — as "registry/results.md" and its siblings — from `refinement/`, where such a path resolves to nothing; from here they are `../codification/registry/…` | `VERIFICATION.md`, `R1-review.md` |
| 76 | New check **R-F**: every backticked repository path must resolve, from the citing file or the repo root. Red-green tested. Nineteen paths currently cited, all resolving | `../codification/audit-r.sh` |

## Corrections applied 2026-09-04, thirteenth pass

**The review had not covered the whole stage.** `lawvere-sjas.md` (642 lines)
and `ADR-0004` (138 lines) are component **R6**, complete as of 2026-09-04, and
twelve passes of adversarial review had read neither. Checks R-A–R-F do cover
them; the reading did not.

| # | Defect | Where it was |
| --- | --- | --- |
| 77 | **§2 conflated the two selected diagonals**, and R6 §1.3 names the distinction: the **G2 diagonal** `G ↔ ¬□G` is what the limitative argument consumes; the **self-consistency diagonal** `H ↔ Con(B+H,D)` is how a self-referential consistency assertion is formed. "Neither instance by itself implies the other." Willard's Group-3 is `H`, **not** `G` — so "they contain the fixed point as an axiom" read as though his systems contain the Gödel sentence | `refined-sjas.md` §§2, 2.5b |
| 78 | **§9's Lawvere paragraph was stale.** It said the framing "does not give a categorical proof of any Willard result … a research direction, not a construction" — which ADR-0004 quotes as the gap R6 was written to fill, and R6 has since built the construction. §9 now records what R6 delivers *and* the non-claims its own §6 lists | `refined-sjas.md` §9 |
| 79 | ADR-0002's status line said "R1 done…; R4–R5 pending" with no mention of R6, though the charter's own component table has an R6 row marked complete; and the "what remains open" list did not carry R6's non-claims | `ADR-0002` |

| 80 | R3 §5.3's classification predates R6 and is coarser than it can now be: "a structural licence to use a context twice" is general contraction, where B–S lack only the **boxed** diagonal; and "the fixed point" conflates three strengths (universal / modalized / selected) and two distinct selected diagonals. Pointers added | `R3-the-margin.md` §5.3 |

| 81 | **R6's comparison omits Pakhomov, undeclared.** R1–R3 establish `H_{<ω}` as one of only two demonstrated routes to a self-verifying theory; R6's §5 square compares AU, B–S and Willard, and §0 closes by contrasting B–S's missing internal point with Willard's — which reads as a claim about the field. Recorded as a declared scope limit in `lawvere-sjas.md` §6 and `ADR-0004`, with what a fourth row would require | `lawvere-sjas.md`, `ADR-0004` |
| 82 | `refined-sjas.md` §2.5b's three-ingredient table is coarser than R6 §5's **two independent axes** (structural diagonal on boxed hypotheses; uniform internal HBL evaluation) plus a separate fixed-point-strength axis. R6's warning that these "should not be collapsed into a single *weakness* ordering" is the sharper statement | *(pointer only; §2.5b's content is not wrong)* |

| 83 | **A direct self-contradiction in R3 §5.3, surviving eight rounds.** "The margin … is complete for that cell, covering **all five** of Willard's dials" — while §§3.3, 3.4, 4 and 6 of the same document retract Level(n) and leave Hybrid(H) undetermined. Three of five | `R3-the-margin.md` §5.3 |
| 84 | **A fresh rule-2 violation, introduced in round eleven while writing about care.** The new §5.3 passage cited `Willard2001` Theorem A.1 without its `stated-only` status — the very rule round seven had swept. It recurs under active guard | `R3-the-margin.md` §5.3 |

## Corrections applied 2026-09-04, fourteenth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 85 | **Seventeen unfollowable bare-filename references**, the same class round ten fixed for slashed paths: the audit scripts, the results registry, the codified statement, the nachlass log, and the repo README and org file were all cited by bare filename from `refinement/`, where each resolves to nothing | `ADR-0002`, `ADR-0004`, `R3-the-margin.md`, `VERIFICATION.md` |
| 86 | **R-F could not see them.** Its pattern required a `/`, so a bare filename was never checked — the same shape of blind spot as the three checks that passed by never matching. Extended and red-green re-tested; it now sees 59 paths where it saw 20 | the refinement audit script |

## Corrections applied 2026-09-04, fifteenth and sixteenth passes

An end-to-end read of `refined-sjas.md`, hunting the one class the kind-organised
sweeps miss: a claim inconsistent with its own document.

| # | Defect | Where it was |
| --- | --- | --- |
| 87 | **§2.1 said Willard "uses [Kleene's recursion theorem] directly".** He cannot: `Willard1993-TR` printed p. 37 says `IS(A)` "will clearly be too weak to prove that `∀x∃y SUBST_i(x,y)`", and proves only `∃y SUBST_i(k̄,y)` "for any fixed integer `k̄`" — the general recursion theorem is exactly what is *not* internally available. This is R6 §4.1's two-levels distinction, missed | `refined-sjas.md` §2.1 |
| 88 | **§2.5b listed three ingredients where `R3-the-margin.md` §5.3 lists four**, folding Pakhomov's semantic step into the derivability row. Now four, matching R3 | `refined-sjas.md` §2.5b |
| 89 | "The **third row** is a result about G2's proof" — broken by correction 88's own change to a four-row table, in the same round. Now row (iv) | `refined-sjas.md` §2.5b |
| 90 | "The row **below** is not wrong" pointed past the table it was about | `refined-sjas.md` §2.5b |
| 91 | **A systematic dating error.** After a mid-session date change, nine entries in the nachlass log, ten pass headings here, and five registry/concordance entries were dated **2026-09-02** for work whose commits are timestamped **2026-09-04**. Corrected against `git log`, which is the only authority available for this | the nachlass log, this file, and the gaps, notation, results, drift-ledger and replication-map registries |
| 92 | **This file's passes were out of order** — the third preceded the second, because insertions anchored on different strings | `VERIFICATION.md` |
| 93 | **Round nine had no entry in the nachlass log.** Commit 8199a22 went in without one; the log's own per-round structure is what made the omission visible. Written and inserted in sequence, marked as back-filled | the nachlass log |

## Corrections applied 2026-09-04, seventeenth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 94 | **The two preservation methods do not partition by line, and round five's repair said they did.** It filed the least-number method under "the Hilbert line", citing `Willard2001` Thm 4.3 — but Thm 4.3 proves consistency preservation for **`IS(A)`**, which is *semantic tableaux* (registry `SJAS-Origin`; codified §3.1's Type-A row). The division is between **proof methods**, not families of system, and codified §6.9's title "The Hilbert line's engine" is looser than its own table. A repair that introduced a fresh error of its own kind — the third such this session | `refined-sjas.md` §6, `R3-the-margin.md` §1.1, and correction #55 above |

## Corrections applied 2026-09-04, eighteenth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 95 | **A hand-maintained count went stale, in three places.** §9 said "this document invokes his theorem three times"; there are seven mentions, and the count was already wrong when written. The same figure appeared as "14 times across six files" in the gaps registry, the charter and correction #52. All four replaced by a characterisation — a claim about how often a document says something should not be maintained by hand | `refined-sjas.md` §9, `ADR-0002`, the gaps registry, and #52 above |

| 96 | **The charter's own review count and date were stale**: "Reviewed and repaired 2026-09-02 across ten passes", twice, after eighteen rounds spanning two days. Replaced with the dates and a pointer to the numbered corrections, which is the record rather than a restated figure | `ADR-0002` |
| 97 | **R2 said `Willard1993-TR` "says three times" that every self-verifying system must breach an HBL condition.** Round three checked the TR page by page and found **four** — printed pp. 1, 2, 6, 12 — and `refined-sjas.md` §2.2 says four. Two documents of the same stage disagreed on a fact one of them had verified | `R2` §2.1 |

## Corrections applied 2026-09-04, twentieth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 98 | **R3's header block pointed at "§1.2"** for the A-Stability bridge, which the eighth pass renumbered to §2.2 — a stale reference inside the document's own summary, the first thing a reader meets | `R3-the-margin.md` header |
| 99 | **And that summary omitted the document's most consequential finding**: that the margin's *sufficiency* has no `full` proof, Lemma 4.6 being `sketch`. A "what this delivers, stated first" block that leaves out the caveat governing everything it delivers is misleading by omission. Added, together with the fact that its A-Stability route is *less* rigorous than Willard's | `R3-the-margin.md` header |

## Corrections applied 2026-09-04, twenty-first pass

The two charters disagreed with each other. Found by reading `ADR-0004`'s
Acceptance and Consequences sections, which no earlier pass had opened.

| # | Defect | Where it was |
| --- | --- | --- |
| 100 | **`ADR-0004` says B3 is "reopened and upgraded to a comparison theorem"; `ADR-0002`'s B3 still recorded it as *met*, with Lawvere "downgraded to a framing".** The two charters of one stage gave contradictory dispositions for the same acceptance criterion, and had done since R6 was written | `ADR-0002` B3 |
| 101 | **`ADR-0004` declares a consequence for R4 that `ADR-0002`'s R4 row did not carry**: R6 supplies a *precise transfer target* — the boxed diagonal `copy_A : □A → □A ⊗ □A` and the uniform proof-composition map exhibited **separately**, so a candidate type theory can be tested against each independently. Recorded in the R4 row and in `refined-sjas.md` §8, which had only its own weaker conjecture | `ADR-0002` R4 row, `refined-sjas.md` §8 |

## Corrections applied 2026-09-04, twenty-second pass

| # | Defect | Where it was |
| --- | --- | --- |
| 102 | **§1's "arithmetic form" was the withdrawn naming slogan.** It said the condition takes "one specific form … *naming an integer must not be cheaper than proving something about it*", and that the rest of the document develops it. It does not: of the five boundary dials, the three now **proved** to move the margin — `ℜ`, `Herb−k`, `Z` — are **apparatus** dials with nothing to do with naming, and the one dial that *is* about naming, Hybrid(H), is precisely the one whose term the Refinement could not identify. The arithmetic form is the margin, a comparison of two sizes; naming is one instance of it, and currently the least supported | `refined-sjas.md` §1 |
| 103 | §2.3b still spoke of "the naming criterion" as though it were *the* criterion | `refined-sjas.md` §2.3b |

## Corrections applied 2026-09-04, twenty-third pass

Round twenty-two's lesson applied: read the framing blockquotes as claims.

| # | Defect | Where it was |
| --- | --- | --- |
| 104 | **The Pakhomov page correction of the fourth pass was applied in one file and missed in two.** `refined-sjas.md` §1 and `R3-the-margin.md` §5.1 both still cited "pp. 3–4, 21"; the second site is **p. 22** and is about `H` alone. A correction applied to the document where it was noticed and not swept across the stage | `refined-sjas.md` §1, `R3-the-margin.md` §5.1 |
| 105 | §1 leant on `Willard2006a`'s `ISINF(A)` without carrying that **Theorem 6 is `sketch`** — the same rule-2 lapse the seventh pass swept, at a site that sweep did not reach | `refined-sjas.md` §1 |
| 106 | **The document's headline claim carried no status.** "A system can consistently assert its own consistency when …" is *this Refinement's identification*, generalised from two instances rather than derived, and its sharp arithmetic form is proved sufficient by the corpus only through a `sketch`. Every claim about Willard in this stage carries its status; the stage's own central sentence did not | `refined-sjas.md` §1 |

## Corrections applied 2026-09-04, twenty-fourth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 107 | **The class round twenty-three exposed — a correction stopping where it was noticed — is mechanically checkable, and R-D was only guarding five strings.** Extended to ten: the Pakhomov page citation, "covering all five of Willard's dials", "every consistency-preservation proof in the corpus", the naming slogan as *the* arithmetic form, and "the cleanest available form". Red-green tested. Any future recurrence of a correction this session made now fails the audit rather than waiting to be re-noticed | the refinement audit script |

## Corrections applied 2026-09-04, twenty-fifth pass

The class round nineteen fixed, recurring in the very edit that fixed it.

| # | Defect | Where it was |
| --- | --- | --- |
| 108 | **Two stale correction-range figures in the charter** — "#1–#69" from the eleventh pass and "#1–#95" from the nineteenth. The second was written *in the round that removed three other hand-maintained counts for going stale*, and was itself stale within five passes. Both replaced by a pointer to the table, with the instruction not to copy a count out of it | `ADR-0002` |
| 109 | "checks R-A–R-D" in three places after R-E and R-F were added — the charter, this file's opening, and the audit script's own header comment and section banner | `ADR-0002`, `VERIFICATION.md`, the main audit script |
| 110 | A duplicated link to this file, introduced by correction #108's own edit | `ADR-0002` |

**Verified and found correct** in this pass: the commit hashes cited in these
records (`4eb63c7`, `8199a22`) resolve and match the work they are cited for;
the numbered corrections run without duplicate or gap; and R-D's ten patterns
each fire on a deliberate reassertion.

**Verified and found correct** in this pass: `lawvere-sjas.md` §0's summary
table agrees with §5's three tables row by row (AU, B–S and Willard on the boxed
diagonal, uniform HBL evaluation, fixed-point strength, internal consistency
point and external consistency); R6's only count, "these three maps", matches
its own table of Nec/BoxMP/Four; the Computed-claims table's "where used"
pointers all still resolve to sections containing those claims; and R6 renders
Theorem 4's conclusion `⊠⊤ =_S □⊥` **with** the box, where R2 had it stripped
until the twelfth pass.

**Verified and found correct** in this pass: `ADR-0004`'s acceptance criteria
L0–L6 are each discharged by `lawvere-sjas.md` as claimed — L3's "the B–S
instance corrects the premise that an HBL condition fails" matches §3.2's L1–L3
list, L4's "names HBL (2) at its corpus locus" matches §4.2's TR p. 12 citation,
and L5's separation of the two selected fixed points from the AU's universal one
matches §§1.3 and 4.3. `ADR-0002`'s open-list item 5 already credits R6 for
sharpening the empty cell.

**Verified and found correct** in this pass — the cross-document sweep: all five
documents agree that Willard's Type-A breach is condition **(2)** (seventeen
sites checked) and that the Hilbert line's condition is *not* identified in the
corpus; every `§N.M` reference in `refined-sjas.md`, `R3-the-margin.md` and
`lawvere-sjas.md` resolves, whether internal or cross-file; and `Willard2001`
Theorem A.1's `stated-only` status is carried at each site that rests on it.
(§2.3's mention is a comparison of two *statements* of condition (2), not a
claim resting on A.1's truth, and needs no status.)

**Verified and found correct** in this pass: every other hand-maintained count
in the stage. `refined-sjas.md` §2.2's "four times" (the TR's HBL passages),
§4's "ten occurrences" (the replication map, now genuinely ten), §6's "three
times, in three papers" (verbatim against codified §7.2), and R3 §3.0's "none of
the four mentions `d`" (Defs. 4.1–4.4) all hold. The counts inside this file's
correction table are historical records of past states and are left as written.

**Verified and found correct** in this pass: the correction record's own
integrity — 94 numbered entries, 1 through 94, no duplicates and no gaps; and
R3 §2.2's A-Stability ⟹ Tightness proof re-checked step by step (`q_{β₀} = q_β`
by mutual inequality on the least Gödel number; `♯(β₀) ≥ ♯(β)` because `♯` of a
set is a minimum over its members; `β₀` r.e. as a finite extension of an r.e.
base; `0=1` `Good(N)` for no `N` by Def. 4.2's Special Note).

**Verified and found correct** in this pass — the rest of R6, read in full:
Theorem 1.1's derivation (the contraction use is exactly where `Four` and
`BoxMP` must both consume `□G`, matching B–S Proposition 3.8); §2.1's AU
construction `□ := Γ∘R` with propositions as subobjects of `1`; §4.2's
quantifier equivalence between the witness form of HBL (2) and the
terminal-fibre `BoxMP`; §4.3's rendering of `Willard2001` footnote 16's
`Θ(z) := ∀x∀y (Subst(z,x) → ¬Derive(x,y))`, verbatim; and its citation of
`Willard1993-TR` printed **p. 37** for the `SUBST_i` fixed/uniform split, which
the extraction record confirms word for word ("Point of Clarification"). R6
carries `Willard2001` Theorem A.1's `stated-only` status itself. Every
"Theorem A.1" reference in the stage is attributed to its paper — necessary,
since `Willard1993-TR#ThmA1` (majorization, `full`) and `Willard2001#ThmA.1`
(generalized Hilbert–Bernays, `stated-only`) are different theorems.

**Verified and found correct** in this pass — R6's own claims, spot-checked
against sources: `Willard2001` Theorem A.1's **footnote 16** does give the
selected `Θ(N) ⇔ ¬Der(⌜Θ(N)⌝)` from `Π⁻₁` strength, as R6 §0 says
(image-verified); `Willard1993-TR` **Proposition 1** is `full` ("for each nice
`A`, `IS(A)` is consistent"), and R6 carries that status in its own table;
Beklemishev–Shamkanov **Lemma 5.1** is indeed the weakening rule, as R6 §3.2
claims; and R6's readings of Remark 3.9, the affine-PA caution, and Willard's
Type-A breach as HBL (2) all agree with the image-verified record here. **R6
observes rule 2 more consistently than `refined-sjas.md` did** — it carries
`full`, `sketch` and proof-omitted statuses at each point of use, including the
AU source's sketch-level §6 Löb derivation.

**Verified and found correct** in this pass: R6's account of Beklemishev–
Shamkanov (it satisfies L1–L3 and lacks the *boxed diagonal*) agrees with
`refined-sjas.md` §2.5 and `R2` §2.3; its identification of Willard's Type-A
breach as HBL **(2)** agrees with §2.3; its limits section carries the AU
source's sketch-level §6 Löb derivation; and its declared witness
`../../lit/2004.10482.pdf` is held.

**Verified and found correct** in this pass: §2.1's statement of **Lawvere's
Fixed Point Theorem** (the uncurried weakly-point-surjective form, `f : A × A → B`
forcing a fixed point for every `t : B → B`, with Cantor/Russell/Tarski/Turing/
Gödel-I as its contrapositive); §8's account of **Brown–Palsberg**, whose
abstract states exactly the mechanism §8 attributes to them; §2.3a's paraphrase
of **Continuous Expansion**; §2.3b's statement of **Theorem 4's three
hypotheses**; and every internal section cross-reference in the document.

**Verified and found correct** in the fourth pass, beyond the eight accurate
quotations: §2.2's claim that `Willard1993-TR` states the governing
Hilbert–Bernays fact **four times** (printed pp. 1, 2, 6, 12 — checked page by
page); §2.3c's claim that the tableaux line keeps successor and addition total
(`Willard2011` Rem. 6.16 has `ξ*`, `ξ**`, `ξ^R` as "Type-A systems that
recognize addition as a total function", and TR p. 2 records Solovay's condition
(b) as the one the tableaux line does *not* relax); `Willard2020` Thm 4.5
verbatim with its `sketch` status visible on the page; and `Willard2006a` §6's
`ISINF(A)`, "Infinitely Far-Reaching without sustaining an ability to prove
successor is a total function", which is the uncited corpus analogue of
Pakhomov's `H_{<ω}` that `R1-review.md` §2.1 identifies.
