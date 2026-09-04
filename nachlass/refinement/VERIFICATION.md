# Refinement Verification Registry

The Refinement was opened without the Codification's machinery — no coverage
registry, no `audit.sh` participation, no unconditional imaging rule. The
2026-09-02 review found that **every Tier-1 defect was of a kind that machinery
exists to prevent**, committed in the one stage not covered by it. This file
closes that gap, and `../codification/audit.sh` now checks it (checks R-A–R-D,
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

## Quotation register

Every row is `img` — verified against a rendered page image; check **R-B** fails
any row that is not.

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
| Defs. 5.1 / 5.3 / 5.5 — A-Stable, E-Stable, EA-stable; Thm 5.9's dependencies are 5.5 and 5.7, not 5.1 | `Willard2011` | printed pp. 16–18 | img (C6); statuses from `results.md` |
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
| §4's `(∗)`: "If `p` is a proof of the theorem `T` from `α`, then a proof from `α` that '`p` proves `T` from `α`' will assuredly have a Gödel number less that `ω₁^K(p)`, for some fixed constant `K`"; `ω₁(x) = x^{Log(x)}`; and `IΣ₀` cannot prove `∀x ∃y ω₁(x) = y` (Eq. 32) | `Willard2007-APAL` | printed pp. 18–19 | **img (2026-09-02)** — previously unrecorded anywhere in the Codification; now in `registry/notation.md` with drift **D82** |
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

## Corrections applied 2026-09-02, third pass

From a review of `refined-sjas.md` for completion and correctness.

| # | Defect | Where it was |
| --- | --- | --- |
| 33 | **§5.1 and §9 contradicted R3's verdict, committed the same day** — both still counted four dials and kept Level(n) on the cost side, which `R3-the-margin.md` §3.3 retracts; §9 still said the margin "generalises" Def. 4.5, withdrawn in R3 §2 | `refined-sjas.md` §§5.1, 9 |
| 34 | §9 omitted R3's findings entirely — the retraction, the `♯`-is-`d`-independent lemma, the forced threshold, and the withdrawal of the stability bridge's novelty | `refined-sjas.md` §9 |
| 35 | §2.5b asserted Pakhomov "does not remove" a derivability condition. He is silent on the point; p. 3 says HBL "do not necessary hold" for weaker c.e. theories. What he blocks is Pudlák's *interpretation* step, which needs successor totality — Willard's door | `refined-sjas.md` §2.5b |
| 36 | "the tableaux line as tabulated above" — no table exists above that line | `refined-sjas.md` §2.3c |
| 37 | "the distinction §8 is simultaneously trying to draw" — §8 is the computational analogue; the distinction is §5.1's. Second mis-numbered cross-reference in the same subsection, the first fixed in the previous pass without sweeping for others | `refined-sjas.md` §2.3c |
| 38 | Nine quotations in `refined-sjas.md` have no register row, so under the images-only rule they are unverified in this stage. Recorded in §9 and above; R-B structurally cannot catch this | `refined-sjas.md`, `VERIFICATION.md` |
| 39 | **Codification gap, not a Refinement error.** `ω₁(x) = x^{Log(x)}` and the `ω₁^K(p)` bound (`Willard2007-APAL` §4), on which §3 rests, were recorded nowhere in the registries — while `R1-review.md` listed §3 as "verified against `Willard2007-APAL` §4". Verified from the page images; §3 is **correct as written**. Now registered, with drift **D82** for the collision with `Willard2020-LFCS`'s different `ω_j` | `registry/notation.md`, `concordance/drift-ledger.md` |

## Corrections applied 2026-09-02, second pass

From the independent review at [`REVIEW-2026-09-02.md`](REVIEW-2026-09-02.md).
The first four are the serious ones: three are failures of the very discipline
this file exists to impose, and two are defects in the checks meant to enforce it.

| # | Defect | Where it was |
| --- | --- | --- |
| 23 | **Two Beklemishev–Shamkanov quotations tagged "(image-verified)" were not verbatim** — the paper's own abbreviation "G2" was expanded, a clause "in it" was added, and a mid-section sentence was called the section's close. The substance held; the fidelity claim did not | `R2` §1, `R3` §5.2 |
| 24 | "The abstract's" quote about uniqueness and weakening is from the **Introduction, p. 2**; the abstract mentions neither | `R2` §1 |
| 25 | "The encoding requirement is precisely what keeps them within constant factors" — contradicts this document's own correction #11 two paragraphs earlier: a lower bound relates the measures in one direction only | `R3` §1.1 |
| 26 | **R-B's hazard regex tested only the *stripping* family** (`□`, turnstiles, corners) and missed the *substitution* family — `℧`→`0`, `ℑ`→`=`, flattened floors and towers (drift **D71**) — which is the more dangerous one, because the output reads as ordinary text. Constructed `txt` rows carrying `℧` and `ℑ` passed as well-formed | `audit-r.sh` |
| 27 | **R-D's context-window guard was defeated by a withdrawal word in neighbouring, unrelated prose.** The window had already been narrowed once from six lines to one for this exact failure; narrowing raised the bar without closing the hole. Now line-local: a line is guarded iff it quotes the retracted text or itself carries a retraction verb | `audit-r.sh` |
| 28 | Pakhomov page numbers: the "both `H` and `H_{<ω}`" sentence spans **pp. 3–4** (correct), but the second citation is **p. 22**, not 21, and is about `H` alone; footnote 3 and the Acknowledgments are on **p. 24**, not 25 | `VERIFICATION.md`, `R1-review.md` §2.1 |
| 29 | `refined-sjas.md` §2.3b's cross-reference "§4 below cites the same Thm 4" cites nothing — §4 does not cite `Willard2006a`, and the document's other "Theorem 4" is `Willard2009`'s | `refined-sjas.md` §2.3b |
| 30 | `R1-review.md` finding #7 still called G38 "acquisition target for R5" while two files in the same commit recorded it closed | `R1-review.md` §3 |
| 31 | Correction #3's location list read as one quote in three documents; it was two distinct instances in two | `VERIFICATION.md` |
| 32 | **B6 overclaimed.** "Every mathematical quotation image-verified … audit.sh green" was asserted while #23 stood, and `audit.sh` cannot check quotation *fidelity* at all — R-B tests only whether a `txt` row carries hazard symbols | `ADR-0002` |

**What R-D is, stated plainly.** A regression guard on *fixed strings*. It
cannot detect a retracted claim that has been reworded, and it does not verify
that a marker is about the same claim as the text it guards. A green R-D means
"these exact sentences have not returned unmarked" and nothing stronger. The
review demonstrated the rewording gap by construction; it is inherent to the
mechanism, and is recorded rather than papered over.

## Corrections applied 2026-09-02, fourth pass

The nine quotations flagged unverified in the third pass were read against the
page. Eight were accurate; the register debt is closed.

| # | Defect | Where it was |
| --- | --- | --- |
| 40 | **A dropped hedge.** `Willard2016` §9 says the evasions rested on arithmetics "weaker than traditional arithmetics **in, at least, some respects**". §7 quoted only "weaker than traditional arithmetics", making Willard's concession absolute. Third elision of this class — after `Willard2005` Rem. 4's "in [68] implies that" and `Willard2020` Thm 4.5's hypothesis on `β`. The pattern is dropped hedges and attributions, not mistranscribed symbols | `refined-sjas.md` §7 |
| 41 | §5.1 glossed `Willard2002a` Thm 2.2 as "cut elimination preserves theoremhood while the proof can be longer". The blowup is in **building the composite** of three proofs under a cut-free method, not in eliminating cuts from one already built; §2.3 had it right | `refined-sjas.md` §5.1 |
| 42 | Hybrid(H)'s domain restriction **`i ≥ 3`** was omitted from the recurrence wherever it appears, and Willard's own framing — the convention "can unify the formalisms of Theorems 3 and 4" — was never cited, though it supports R3's project | `refined-sjas.md` §5, `R3` §3.1 |
| 43 | §9 still listed nine quotations as unverified after they had been verified | `refined-sjas.md` §9 |
| 44 | **Drift D82 understated the problem.** It recorded two `ω` families; there are **three**. `Willard2001` p. 45 defines both the Hájek–Pudlák `ω_i` and the Wilkie–Paris `ϖ_i`, and **`ϖ₁` is exactly `Willard2007-APAL`'s `ω₁`** under a different letter — while `Willard2001`'s Hájek–Pudlák `ω₁ ≈ x²` is essentially `Willard2020-LFCS`'s `ω₀`. The hierarchies are offset by an index as well as differing in definition, so a bare "`ω₁`" is ambiguous even once the paper is named | `concordance/drift-ledger.md`, `registry/notation.md` |

## Corrections applied 2026-09-02, fifth pass

| # | Defect | Where it was |
| --- | --- | --- |
| 45 | **§1 said "eleven formula classes".** Codified §3.3 and drift **D50** both say **ten**; D50 is titled "ten formula-class notations for one idea". Also "five apparatus families" for what codified §4.1 catalogues as five *apparatuses* (four textbook plus one of Willard's own), four of which are Hilbert-style | `refined-sjas.md` §1 |
| 46 | **§6 used two argument orders four paragraphs apart.** The Rosser sentence was written `Prf(y, ⌜R⌝)` (proof first, textbook convention) while `SemPrf^K_α(x, y, z)` below it and `Willard2020` Eq. (5)'s `Prf_{IS_D(β)}(x,p)` are theorem-first — in the one section whose point is that the two predicates are the same move | `refined-sjas.md` §6 |
| 47 | **§4 claimed ten occurrences of the doubling device; the map listed nine** — and omitted the **origin text**. `Willard1993-TR` printed p. 6 carries the device (`a₀ ≥ 2`, `a_{i+1} = (a_i)²` needing `2^n` bits; then `b_{i+1} = b_i + b_i`), framed as a philosopher's hedge; the map cited only the 12-page published abbreviation. Row added, image-verified; the count is now genuinely ten | `concordance/replication-map.md` |
| 48 | §2.3b's hypothesis-count criticism was incomplete: `Willard2006a` **Remark 1** supplies **Theorem 4\***, which drops the Concise Encoding hypothesis. The logical point (affirming the consequent) is unaffected | `refined-sjas.md` §2.3b |
| 49 | **An elided witness path.** §2.1 cited Yanofsky as `lit/…Working Category Theorist…pdf` — unfollowable, and R-C's path pattern did not match it, so it passed in silence. Real path given; **R-C now rejects elided paths** | `refined-sjas.md` §2.1, `audit-r.sh` |
| 50 | **A bug in that new R-C clause.** It read its hits from a pipeline, so the `while` ran in a subshell and `err`'s assignment to `FAIL` was lost — the run printed `AUDIT-FAIL` lines and still exited green. Now reads from a process substitution; red-green re-tested | `audit-r.sh` |

## Corrections applied 2026-09-02, sixth pass

Completion, not correctness: what the document omits about its own stage.

| # | Defect | Where it was |
| --- | --- | --- |
| 51 | **§9 never mentioned R5.** ADR-0002's secondary-literature pass — Artemov, Pudlák, Visser, Adamowicz, Kołodziejczyk, Salehi — is a pending component of this stage, and the document's own account of what is unsettled omitted it entirely | `refined-sjas.md` §9 |
| 52 | **Pudlák's paper is not held, and the Refinement leans on it 14 times across six files.** That his cut-shortening technique *requires successor totality* is what §§1 and 2.5b use to explain how both Willard's Hilbert line and Pakhomov's `H_{<ω}` evade him — and every invocation rests on Willard's or Pakhomov's description of a paper nobody here has read. Recorded as gap **G39**; §9 now says so, and so does the header | `refined-sjas.md` §§1, 2.5b, 9; `registry/gaps.md` |
| 53 | **The header overclaimed.** "Every claim it makes about the literature is anchored in the codification's registries" is false in two directions: the secondary literature was never in the Codification's scope, and the Pudlák claims are anchored in nothing held. Rewritten to say which claims rest on what | `refined-sjas.md` header |
| 54 | §2.3 identified the TR's condition-(2) phrasing with Theorem A.1's without saying how they correspond, though §2.3a's whole argument turns on where the quantifiers sit — A.1 is schematic in `Φ`, `Ψ` with the proof quantifier inside `Der`; the TR unpacks the same content over proof codes | `refined-sjas.md` §2.3 |

## Corrections applied 2026-09-02, seventh pass

| # | Defect | Where it was |
| --- | --- | --- |
| 55 | **The corpus has two preservation engines, and two documents conflated them.** §6 said "*every* consistency-preservation proof in the corpus" is a minimal-counterexample argument on `Θ(P)`, and cited `Willard2005` Theorem 1 as an instance. Codified §6.9 opens by denying both: "The Hilbert line does not use `θ`-Compactification. Its preservation results are proved directly, by minimal counterexample." `Willard2005` Thm 1 **is** the `θ`-compactification engine (codified §6.4) — apparatus-neutral, the other method. The least-number mechanism belongs to the Hilbert line alone: `Willard2001` Thms 3.4 and 4.3, `Willard2006a` Thm 3, `Willard2016` Thm 6.7 | `refined-sjas.md` §6, `R3-the-margin.md` §1.1 |
| 56 | R3 §3's heading asserted "every dial moves one of the two terms" — the claim §3.3 retracts for Level(n). A reader skimming headings got the withdrawn version; the claim block now carries the verdict beside it | `R3-the-margin.md` §3 |
| 57 | `R1-review.md` §4 still stated B5 as "prove **the five dials** are instances of the general form", superseded by R3's three-of-five and by §5's refutation of the premise that a third instance exists | `R1-review.md` §4 |

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
