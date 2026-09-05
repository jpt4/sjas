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

## Pass twenty-eight, 2026-09-04 — no corrections

The remaining targets named at the close of pass twenty-seven:
`lawvere-sjas.md` §§1.4 and 3.4, and `ADR-0004`'s L0–L6 discharge claims
re-derived rather than read off.

**Theorem 1.1's proof is correct, step by step.** `Nec` + `BoxMP` on the forward
fixed-point arrow gives `□G → □(□G → ⊥)`; `Four` gives `□G → □□G`; and the
second `BoxMP`, at `A = □G`, `B = ⊥`, needs the *same* `□G` to feed both — which
is exactly and only where `BoxContr_G` is consumed. `Con → ¬□G` follows
monoidally by currying (no contraction: `Con` and `□G` are distinct resources),
and the closing step needs `Con` twice but has it as a **global point**, which
duplicates through the unit isomorphism without contraction. The theorem is
correctly labelled a *sufficient package*, not a biconditional.

**§3.4 checks out.** `⊠A := □¬A` matches the source's `⊠φ := □(φ → ⊥)` under this
document's `¬A := A ⊸ ⊥`; and `⊠⊤ =_S □⊥` is precisely Theorem 4's conclusion as
image-verified above. The external-consistency derivation — `⊤ ≤_S ⊥` would give
`⊢_S ⊥` by I4, then `F, ⊥ ⊢_S □⊥` and Theorem 5's admissible cut would give
`F ⊢_S □⊥`, contradicting Proposition 4.1 — is valid, and §6 already declares it
as this comparison's inference rather than a quoted source theorem.

**L0–L6 all discharge.** L0's methodological order is confirmed structurally:
Theorem 1.1 is derived at line 202, before the AU (§2), B–S (§3) and Willard (§4)
instances. L1's package is separated in §§1.2–1.3; L2's arrows and the cartesian
source of contraction in §§2.1–2.2; L3's L1–L3-hold-but-no-`δ^□` in §§3.2–3.3;
L4's HBL (2) at TR p. 12 with the pointwise/uniform split in §4.2; L5's two
selected fixed points against the AU's universal one in §§1.3 and 4.3; L6's final
table in §5.

*Second consecutive pass whose named target yielded no correction.*

## Corrections applied 2026-09-04, twenty-ninth pass

**Target chosen mechanically, not by intuition.** My own sense of where to look
was exhausted, so instead of picking a section I enumerated a *layer* I had never
touched: the `Proof` statuses in `../codification/registry/results.md` that the
Refinement leans on and had taken on trust. Four of them carry R3 §2.2's route
comparison, and one — Thm 5.11, `full` but with its proof "deferred to Appendix
C" — was worth checking at the source.

The statuses are right. What the check found instead is that **the two routes do
not end in the same place.**

| # | Defect | Where it was |
| --- | --- | --- |
| 113 | **R3 claimed its A-Stability bridge "re-derives a conclusion Willard already has (Thm 5.11)". It does not.** Chained with Lemma 4.6 it reaches `B^ξ + SelfRef(B^ξ,d)`; Thm 5.11 reaches `B^ξ + SelfCons⁰(B^ξ,d)`. **Remark 5.8** (printed p. 17, now image-verified) says `SelfCons^k` is "significantly stronger" than `SelfRef` — the first denies simultaneous proofs of a `Π^ξ_k` sentence and its negation, the second "merely the non-existence of a proof of `0 = 1`". A shorter route to a **weaker** destination | `R3-the-margin.md` header, §§2.2, 6; `refined-sjas.md` §9 |
| 114 | The route-comparison table therefore compared unlike things: "Willard's longer route is `full` end to end; this one terminates in the sketch" reads as two proofs of one result. The table now carries a **Destination** column | `R3-the-margin.md` §2.2 |
| 115 | **A consequence for the margin itself, not previously stated anywhere.** The self-justification the margin certifies is the `SelfRef` form — no proof of `0=1` — not the `SelfCons^k` form Willard's stability engines deliver. The margin certifies *less*, and by a sketch | `R3-the-margin.md` §6, `refined-sjas.md` §9 |

**Verified and found correct** in this pass: `Willard2011` Thm 5.11's `full`
status — Appendix C is headed "The Proof of Theorem 5.11" and delivers it as
Lemmas C.1 and C.2, both with complete proofs — and Lemma C.1's statement, which
is exactly the A-Stable → 0-Stable step R3's table attributes to it.

## Corrections applied 2026-09-04, thirtieth pass

Second mechanically-enumerated layer: the **`Depends` column** of
`../codification/registry/results.md`, which R3 §1 leans on for "Thm 5.9's
dependencies are Defs. 5.5 and 5.7, not 5.1".

| # | Defect | Where it was |
| --- | --- | --- |
| 116 | **The Appendix C dependency cluster was inverted.** The registry had `LemC.1` depending on `Thm5.11` and `LemC.2` on `LemC.1`. The page shows the opposite and simpler structure: Appendix C **is** 5.11's proof, C.1 and C.2 are its two halves, and they are independent of each other — "The remainder of this appendix will focus on Definition 5.10's 0-stability condition. (This is sufficient to justify Theorem 5.11 because Lemma C.1 showed all E-stable and A-stable configurations are 0-stable.)" Rows corrected: 5.11 now depends on Def. 5.10 and both lemmas; C.1 on Defs. 5.1, 5.3, 5.10; C.2 on Def. 5.10 and Thm 5.9, whose proof its own "is similar to" | `../codification/registry/results.md` |
| 117 | **The main audit script read the `Depends` column and never validated it** — no resolution check, no cycle check — which is how an inverted cluster sat there unnoticed. Both checks added and red-green tested. Over 522 rows: every id resolves, and the graph is acyclic | the main audit script |
| 118 | **The pipeline-subshell bug, a fourth time.** The new check's reporting loop read from a pipe, so `err`'s assignment to `FAIL` was lost and the run printed `AUDIT-FAIL` lines while exiting green. Caught by the red-green test, fixed with a process substitution, and the reason is now a comment in the script | the main audit script |

**Neither new check would have caught defect #116** — the inverted cluster was
acyclic and every id resolved. It needed the page. That is worth recording as a
limit: the `Depends` column is now machine-checked for *coherence* and remains
unchecked for *direction*.

**Verified and found correct** in this pass: `Willard2011` Thm 5.9's dependencies
really are Defs. 5.5 and 5.7 — its statement is "Let `ξ` … be EA-stable. Then
`B^ξ + SelfCons¹(B^ξ,d)` must satisfy Section 1's definition of
self-justification." And its proof's opening corroborates the twenty-ninth
pass's finding in Willard's own words: it "will be a more elaborate version of
Lemma 4.6's mini-proof … will replace Definition 4.5's Tightness constraint with
an EA-stability requirement. It will also replace `SelfRef(β,d)`'s 'I am
consistent' axiom with a ***stronger*** `SelfCons¹(β,d)` statement."

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
| **Def. 5.6** (Level(`k^ξ`) Consistent: no proofs from `α` of both a `Π^ξ_k` sentence and the `Σ^ξ_k` sentence that is its negation); **Def. 5.7** (`SelfCons^k(β,d)`); **Remark 5.8**: "even when `k = 1`, Definition 5.7's `SelfCons^k(β,d)` statement will be **significantly stronger** than the axiomatic declaration ⊕ used by Section 1's `SelfRef(β,d)` axiom … while `SelfRef(β,d)` establishes merely the non-existence of a proof of `0 = 1`" | `Willard2011` | printed p. 17 | **img (2026-09-04)** |
| **Appendix C: The Proof of Theorem 5.11** — "a straightforward modification of Theorem 5.9's proof … divided into two lemmas"; **Lemma C.1** (E-stable or A-stable ⟹ Def. 5.10's 0-stability), proof complete; **Lemma C.2** (0-Stable ⟹ `B^ξ + SelfCons⁰(B^ξ,d)` consistent, "and hence self-justifying") | `Willard2011` | printed p. 35 | **img (2026-09-04)** |
| §6's opening — "**This is a sketch of a proof of Löb's theorem in Arithmetic Universes**"; Def. 6.1 (`U ⊨ φ ⊢ ψ` as an inclusion of subobjects); Def. 6.2 (implication of judgements via `i*(σ) ↪ i*(τ)` in `U[φ ≤ ψ]`); "An arithmetic universe is typically **not cartesian closed**" | van Dijk–Oldenziel | printed p. 28 | **img (2026-09-04)** |
| **Prop. 6.5** "If `U ⊨ φ` then `U ⊨ □φ`", proof "Immediate from lemma 5.15"; **Prop. 6.6** `U ⊨ □φ ⊢ □(□φ)`, proof "Apply the functor `□ = Γ ∘ R`"; **Prop. 6.7 (Modus Ponens)**; **Prop. 6.8 (Internal Modus Ponens)** with its evaluation map `ev : Hom(φ',ψ') × Hom(1',φ') → Hom(1',ψ')` | van Dijk–Oldenziel | printed p. 28 | **img (2026-09-04)** |
| **Thm 6.13**, the derivation of Löb ("Recall our formulation of Löb's theorem") with its ten-step proof citing Props. 6.5–6.10 and Lemma 6.12; **Lemma 6.12** supplying the Löb sentence via `T_φ : P1' → P1'`; **Rem. 6.14**, "As arithmetic universes are not in general cartesian closed there is no one notion of implication" | van Dijk–Oldenziel | printed p. 30 | **img (2026-09-04)** |
| **Thm 5.20 (Gödel's Second Incompleteness Theorem)**: "Assume that `U_0` is consistent. Then the subobject `⟦True' = False'⟧ ↪ 1` does not equal the minimal subobject `0 ↪ 1` in `U_0`" — with the preceding sentence defining what it is for `U_0` to prove the consistency of `U'_0` | van Dijk–Oldenziel | printed p. 26 | **img (2026-09-04)** |
| `□ = Γ∘R`; propositions as subobjects of `1`; classifying-AU implication; direct G2 Thm 5.20; Props. 6.5–6.10 including internal-Hom evaluation in Prop. 6.8; Lem. 6.12 fixed points; Thm 6.13 Löb; Rem. 6.14 non-cartesian-closure warning; §6 is explicitly introduced as a sketch | van Dijk–Oldenziel | printed pp. 26, 28–30 | **img (2026-09-04)** |
| "The answer to the preceding question is quite surprisingly both affirmative and negative !" — on whether an analogue of `IS^λ(A)` can recognize multiplication as total | `Willard2001` | printed p. 36 (pdf p. 36) | **img (2026-09-04)** — round 31 |
| "if `A` is regularly consistent then so will `ISM(A)` be consistent, by a routine generalization of Section 5's proof of Theorem 5.1" — `ISM(A)` recognizes multiplication as total **and** is consistency-preserving | `Willard2001` | printed p. 36 | **img (2026-09-04)** — falsified `refined-sjas.md` §9(1)'s universal; obligation **O31** |
| "have quite different philosophical and epistemological implications, although the strictly formalistic mathematical proofs of their consistency are virtually identical" — `ISM(A)` vs `IS^λ(A)` | `Willard2001` | printed p. 36 | **img (2026-09-04)** — round 31 |
| **Definition 7.1**: a **Z-Enriched** tableau is "the particular refinement of the Appendix's proof-tree formalism that allows Line (9) as an added permissible logical axiom, for any `Υ ∈ Z`", contrasted with "`α`'s set of proper axioms" | `Willard2020` | printed p. 17 (pdf p. 18) | **img (2026-09-04)** — confirms the `Z` dial varies the apparatus `d`, which R3 §3.0 needs |
| **"Hence, there are inherent tradeoffs that prevent a self-justifying axiom system from possessing all of features (I) through (VII), *simultaneously!* Thus, it is futile to seek an idealized form of self-verifying system, that combines the advantages of the many different approaches *simultaneously*. Each should be viewed as providing differently desirable insights."** | `Willard2001` | printed p. 7 (pdf p. 7) | **img (2026-09-04)** — the corpus carried it cut at "self-verifying system"; the tail inverts its force |
| "It is infeasible to devise a *hybridized* self-justifying axiom formalism, that simply combines our methods with the prior literature [20, 21, 24, 28, 36, 38] because such a formalism would violate one of the four variants, (A) through (D) of the Incompleteness Theorem" — fixes the subject of the futility verdict | `Willard2001` | printed p. 7 | **img (2026-09-04)** — round 32 |
| **`Scalar_k`(x): "comprising a family of functions that for *any fixed constant* k represents the scalar product of k·x. {Unlike normal multiplication, scalar multiplication is a function under IS(A) because it is defined by Scalar₀(x) = 0 and Scalar_{k+1}(x) = Scalar_k(x) + x."** | `Willard1993-TR` | printed p. 39 (pdf p. 41 of the collated searchable witness) | **img (2026-09-04)** — OCR-grade witness, read from the page image; establishes that the boundary is *variable* multiplication only (obligation **O17**) |
| "Point of Clarification. Since it does not recognize multiplications as a function, IS(A) will clearly be too weak to prove that ∀x∃y SUBST_i(x,y). However, IS(A) will be able to prove for *any fixed integer* k̄ that ∃y SUBST_i(k̄,y). It will turn out that the latter will be sufficient for IS(A) to formally define the *particular instance of the reflection principle* needed to define H." | `Willard1993-TR` | printed p. 37 (pdf p. 39) | **img (2026-09-05)** — all three fragments quoted in §2 are verbatim, elisions marked; previously confirmed only against the extraction record |
| The `a_i`/`b_i` pair: "`a₀ ≥ 2` and `a_{i+1} = (a_i)²`, a hedge would quickly follow because `a_n` requires at least `2ⁿ` bits" against "the sequence `b₀, b₁, b₂, b₃ …` with `b_{i+1} = b_i + b_i`" | `Willard1993-TR` | printed p. 6 (pdf p. 8) | **img (2026-09-05)** — confirms §4's origin-occurrence claim for the replication map |
| **"(Also, it should be noted that IS *can represent* floating point multiplication as a *total function*, provided some form of constraints are present on the bit-length of the mantissa.)"** | `Willard1993-TR` | printed p. 6, footnote | **img (2026-09-05)** — unrecorded anywhere in the corpus until now; the 1993 origin of the `Willard2005-TAB`/`Willard2006b` line, qualifier included |

## Computed claims

Rule 5. Every growth or size claim, with the computation.

| Claim | Where used | Computation |
| --- | --- | --- |
| additive naming: `Log₂ C_n = Θ(n)`, exactly `n − 1` | `refined-sjas.md` §§4–5 | `C_i = 2C_{i−1}` from Group-0's `C₂ = 2`, `i ≥ 3` (codified §5.1), so `C_n = 2^{n−1}`. **Corrected 2026-09-05**: this row read `= n`, which needs `C₀ = 1` |
| multiplicative naming: `Log₂ C_n = 2^{Θ(n)}`, exactly `2^{n−2}` | `refined-sjas.md` §§4–5 | `C_i = C_{i−1}²` from the same base, so `C_n = 2^{2^{n−2}}`. **Corrected 2026-09-05**: this row read `= 2^n`, which needs `C₀ = 2` — so the two rows as written held under no single base case |
| §4's `x_i`/`y_i` **sequences** are a different object from the `C_i` **naming convention** and do start at 2: `x_n = 2^{n+1}`, `y_n = 2^{2^n}` | `refined-sjas.md` §4 | `x_0 = y_0 = 2` (`Willard1993-TR` p. 6's `a_0 ≥ 2`, image-verified). §4's table is correct as written; the collision is that both are written `Log₂` of a doubling recurrence |
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

## Corrections applied 2026-09-04, twenty-sixth pass

Layer checked: **`../codification/codified-sjas.md`'s own claims at the ten sections the
Refinement cites.** The pass found a class rather than a scatter — *findings
recorded in the C16 R5 and R6 disposition documents were never written back to
the documents they were about*. Three of the eight below were already on file,
one of them for three days, each in a document whose purpose is to say what
still needs doing.

| # | Defect | Where it was |
| --- | --- | --- |
| 111 | **D38's own counts contradict its own table in four places** — heading "the boundary has **three** dials, and **only one** has its threshold located"; first bullet "**Four** different parameterisations"; last bullet "**three** known coordinatisations" — against a five-row table. The two amendments that added dials never renumbered the prose. Recorded as **F-L2** by the C16 R6 walk and never applied | `../codification/concordance/drift-ledger.md` |
| 112 | **Obligation O49 was discharged carrying three claims already retracted elsewhere**: "three coordinatisations" (D38 tabulates five); "`Hybrid(H)` alone has its threshold stated exactly" (retracted by D38's 2026-08-27 amendment — `Herb−k` and `R(i,j)` locate theirs too); and "`ℜ`'s positive side is **proved**" (G35 settled Theorems 2 **and** 3 as permanently `sketch` on 2026-08-27). C16 R5 item 7 recorded the third and it was never applied | `../codification/concordance/composition-obligations.md` |
| 113 | **The retracted `Hybrid(H)`-uniqueness claim was live in two further places**, both asserting it flatly as "the corpus's only exactly-located boundary". What survives is that it is the only one on the **Hilbert** side | `../codification/extraction/willard2006a.md`, `../codification/registry/results.md` |
| 114 | **Codified §6.9 filed a semantic-tableaux system under "The Hilbert line's engine".** `Willard2001` Thm 4.3 is about `IS(A)`, registry `SJAS-Tableaux-Level0`, apparatus *Semantic Tableaux*. C16 R5 item 6 recorded the mismatch and it was never applied. Retitled **"The minimal-counterexample method"**, which is what its table actually collects | `../codification/codified-sjas.md` §6.9 |
| 115 | **This stage quoted that defective sentence instead of getting it fixed.** §6 reproduced §6.9's "The Hilbert line does not use `θ`-Compactification…" and then observed that the title "is looser than its own table" — correct, and the wrong remedy: the Codification is this project's to repair. The quotation and the criticism are both gone; §6 now states the method/line split directly | `refined-sjas.md` §6 |
| 116 | **The wrong registry key for `IS(A)`.** §6 cited `SJAS-Origin`, which is `Willard1993`'s system; `Willard2001`'s is `SJAS-Tableaux-Level0`. Two distinct systems in the corpus carry the name `IS(A)`, so the key is the thing that disambiguates them | `refined-sjas.md` §6 |
| 117 | **§7(1) stated a false universal.** "Every affirmative system is Type-A (no multiplication as a total function) or Type-NS (nothing total)." `Willard2001` §6's **`ISM(A)`** recognizes multiplication as a total function **and** is consistency-preserving — "if `A` is regularly consistent then so will `ISM(A)` be consistent, by a routine generalization of Section 5's proof of Theorem 5.1" (printed p. 36, image-verified). What (61)'s wider gap costs it is the *flat* statement `∀y ¬SemPrf_{ISM(A)}(⌈0=1⌉, y)`. Multiplication-totality is fatal to the **reflection**, not to consistency-preservation. Codification obligation **O31** says exactly this and was marked `carried … (C16 R5; **Refinement**)` — neither document had carried it | `refined-sjas.md` §7 |
| 118 | **Codified §7.2 presented multiplication-totality as simply fatal**, the over-statement O31 exists to forbid, and C16 R5 item 1 had recorded it as "a live over-statement". Same page, same paragraph as #117 | `../codification/codified-sjas.md` §7.2 |
| 119 | **Codified §6.9 mis-described what D24 contains.** "Drift **D24** holds the three names side by side" — D24's three (`Willard2001`, `Willard2005`, `Willard2011`) are not §6.1's three (`Willard2005`, `Willard2016`, `Willard2020`). The union is **five** distinct admission conditions, no one place lists all five, and D24 is open on precisely that | `../codification/codified-sjas.md` §6.9 |

**Verified and found correct** in this pass: the `Z` dial, which is what sent
the pass into this layer, because R3's "three proved dials" needs `Z` to vary the
*apparatus*. `Willard2020` **Definition 7.1** (printed p. 17, image-verified)
defines a Z-Enriched tableau as "the particular refinement of the Appendix's
**proof-tree formalism** that allows Line (9) as an **added permissible logical
axiom**, for any `Υ ∈ Z`", explicitly against "`α`'s set of proper axioms" — so
`Z` moves `d`, and codified §8.1's gloss is right. Its sibling **Definition 7.2**
(Z-Base-Variable-Enriched) is separately registered, in `../codification/registry/results.md`,
`../codification/registry/notation.md` and D39. Every one of the Refinement's ten `Z` sites attributes the
dial to `Willard2020`; the corpus's *other* `Z` — `Willard2005`'s Z-Based
Deduction Tree, where `Z` is a `Normed(a,b)` **axiom system** — is never confused
with it.

**Verified and found correct** in this pass: codified §8.1's dial table row by
row against `../codification/registry/results.md` and `../codification/registry/gaps.md`. `ℜ`'s `sketch`/`sketch` and Level(n)'s
`full`/`full` both cite **G35** and are both right — they cite *different*
deferrals within it (Theorems 2 and 3 permanently sketch; Theorem 1's `T* ≤ N²`
footnote). D34 is correctly cited by both `../codification/registry/results.md` and §8.1, for the two
different things each says about it. `../codification/concordance/result-matrix.md` §5 already said five.

## Corrections applied 2026-09-04, twenty-seventh pass

Same layer, continued: the remaining C16 R5 items that land on sections this
stage cites. Item 4 pointed at codified §1.4. Following it to the page found a
truncation, and the truncation had been doing real work.

| # | Defect | Where it was |
| --- | --- | --- |
| 120 | **A quotation cut where the cut inverts it.** `Willard2001` §1's verdict was carried as "…Thus, **it is futile to seek an idealized form of self-verifying system**" and stopped there. The sentence continues: "**, that combines the advantages of the many different approaches *simultaneously*. Each should be viewed as providing differently desirable insights.**" And the sentence before it fixes the subject — "It is infeasible to devise a *hybridized* self-justifying axiom formalism, **that simply combines our methods with the prior literature**". Read whole (printed p. 7, image-verified), Willard is denying that any one **system** dominates on all seven criteria and keeping the plurality of approaches; he is not declaring an idealized form unattainable. Same class as `Willard2016` §9's elided "in, at least, some respects" (#40) | `../codification/extraction/willard2001.md` §3.9, obligation **O35** |
| 121 | **Codified §1.4 paraphrased inside a section headed "The limits, in Willard's words."** "Willard lists criteria a fully satisfying self-justifying arithmetic would meet and states that no system meets them all" — the one limit of the four given no quotation, and the only appearance of the verdict anywhere in either stage. C16 R5 item 4 recorded the paraphrase and it was never applied | `../codification/codified-sjas.md` §1.4 |
| 122 | **§7 mis-attributed its own list.** "**Five things are given up.** Willard states the first four himself (codified §1.4)" — three of the four are §1.4's (items 2, 3, 4). Item 1 is the corpus's boundary result, not a statement of Willard's about the programme, and §1.4's remaining limit was not carried at all | `refined-sjas.md` §7 |
| 123 | **The stage never engaged the one limit aimed at it.** O35's text is explicit — "Refinement must engage this claim rather than inherit it silently" — and O35 stood `discharged:2026-09-01`. "futile", "idealized" and the seven criteria had **zero** occurrences across all five Refinement documents. §7 now engages it on the completed quotation, and answers it: what the verdict forbids is nominating a best system, which this stage does not do | `refined-sjas.md` §7 |

**Verified and found correct** in this pass: the other three limits of codified
§1.4 are quoted, not paraphrased, and each is carried in `refined-sjas.md` §7
with its source — `Willard2011` Remark 6.16b's "1-line proof", `Willard2016` §9's
scope statement with the "in, at least, some respects" qualifier restored at
#40, and `Willard2016` §8's "almost cheating" objection with its conditional
answer flagged under O66.

## Corrections applied 2026-09-04, twenty-eighth pass

The rest of the C16 R5 items, and the end of that list. Two of the four were
still live; one had already been applied; one was a count that had gone stale
for the second time.

| # | Defect | Where it was |
| --- | --- | --- |
| 124 | **The boundary was stated as stronger than the corpus proves.** Codified §3.1's Type-A row and this stage's §2.3c and §9 said "multiplication" without saying *which* multiplication. `Willard1993-TR` printed p. 39, image-verified from the page: `Scalar_k(x)` is "a family of functions that for *any fixed constant* `k` represents the scalar product of `k·x`", and "**Unlike normal multiplication, scalar multiplication is a function under `IS(A)`**". `Willard2005`'s and `Willard2020`'s **`Double`**, which sits in the Type-A language, is exactly that `Scalar₂`. So what the affirmative systems give up is *variable* multiplication, and the boundary is one more instance of the fixed-parameter-versus-uniform split §2.3 already identifies as the programme's signature. Obligation **O17** said this and was marked `carried … (C16 R5; **Refinement**)` on 2026-09-01; neither stage had carried it | `../codification/codified-sjas.md` §3.1, `refined-sjas.md` §2.3c and §7 |
| 125 | **O90's qualifier was missing where the claim is made.** Codified §8.2: "`IS_D(A′)` recognises multiplication as total **over simulated reals**" — which is equally true of `Willard2006b` Definition 6's `LongMult`, the thing Theorem 6 **forbids**. The affirmative result is Theorem 2's, for Definition 4's **truncated** floating-point `×`, and Remark 3 gives the reason: `2^{2^n}` costs `k + n` bits truncated and `O(k·2^n)` untruncated. This is the corpus's only affirmative multiplication-totality result, so the qualifier is the whole content | `../codification/codified-sjas.md` §8.2 |
| 126 | **A hand-maintained count, stale for the second time.** "The full draft cites **53** of the 87 obligations at their point of discharge" — and "**57**" before that, which was wrong when written (four of its grep hits were range boundaries in §10.4's own numbering sentence). Replaced with the machine-countable source, the register's `discharged:` column, and a note saying not to restate it a third time — the same remedy `ADR-0002` adopted for the correction range at #108 | `../codification/concordance/composition-obligations.md` |

**Verified and found correct** in this pass: C16 R5's item 5 **had** been applied
— codified §7.3 presents the TR p. 36 ZF parenthetical as "a hedge about one
system, not a general identification of LEM-as-axioms with cuts", which is
exactly what O55 requires. And O90 had nothing to carry into this stage: the
Refinement makes no claim about the real-valued line. With #124-#126 the C16 R5
item list is fully applied, three days after it was written.

## Corrections applied 2026-09-04, twenty-ninth pass

Three rounds running, the same shape of finding surfaced by hand: an obligation
the Codification had marked as this stage's to carry, which this stage had not
carried, each one a live mis-statement (#117, #124, and O90's #125). That is a
class, and it had a hiding place.

| # | Defect | Where it was |
| --- | --- | --- |
| 127 | **The bucket those three came out of was unreadable, and nothing watched it.** 26 obligations are dispositioned `carried:2026-09-01 (C16 R5; Refinement)`. The C16 R5 legend defines `carried` as "deliberately deferred to Refinement" and then concedes in the next paragraph that it "is used for genuine absence as well as for deliberate deferral, because the register offers no fourth status". So a genuine Refinement obligation and a plain hole in `../codification/codified-sjas.md` are written identically, and O17, O31 and O90 sat among 23 holes for three days. Now watched: **R-G** in `../codification/audit-r.sh` prints, on every run, how many of these obligations this stage cites and lists the ones it does not. Red-green tested | `../codification/audit-r.sh`, `../codification/registry/gaps.md` (**G40**) |

**Verified and found correct** in this pass — and this is the reason #127 is a
correction to the *audit* and not to this document: all 24 uncited rows read
"Absent", "Half-done" or "Blocked and absent" in the C16 R5 dispositions. They
are content the **codified statement** was obliged to carry and does not, so
there was nothing for this stage to inherit. **G40** rules them a Codification
completeness gap against acceptance criterion A2, to be closed by writing the
content or by re-dispositioning with an exclusion rationale — explicitly not by
relabelling the rows. The three that *were* this stage's — O17, O31, O90 — are
discharged as of this session.

## Corrections applied 2026-09-05, thirtieth pass

The last enumerated layer: **the page anchors this stage asserts.** All of R3's
are already marked image-verified. Two of `refined-sjas.md`'s were not, both on
`Willard1993-TR` — an OCR-grade witness, where the standing rule is that the page
image is authoritative and the extraction record is not a substitute. Both were
read from the page. Both came back clean. The finding was on the same page.

| # | Defect | Where it was |
| --- | --- | --- |
| 128 | **The corpus's only affirmative multiplication-totality result has a 1993 origin that nothing recorded.** `Willard1993-TR` printed p. 6, footnote (image-verified): "**IS *can represent* floating point multiplication as a *total function*, provided some form of constraints are present on the bit-length of the mantissa.**" That is `Willard2005-TAB` Def. 4 and `Willard2006b` Def. 4 — truncated `×` under max-mantissa-length rounding — stated twelve years earlier, with the qualifier restored at #125 already in it. `grep -i mantissa` across the whole corpus returned `Willard2005-TAB`, `Willard2006b`, `Willard2005` Remark 7 and nothing else. Now recorded at `../codification/extraction/willard1993-tr.md` §3.7a | `../codification/extraction/willard1993-tr.md`, `../codification/registry/gaps.md` (**G36**) |
| 129 | **G36 said the unlocated ASL-2005 technical report is "the only place in the corpus where a self-justifying system recognizes multiplication as total."** Wrong twice: `Willard2006b` Theorem 2 is held and proves it — G36's own 2026-08-29 amendment records the extraction without revisiting the sentence — and the 1993 footnote above states it. The unlocated TR is a fuller write-up, not the only or the primary witness | `../codification/registry/gaps.md` |

**Verified and found correct** in this pass — the two anchors themselves, which
is what the pass set out to check. `Willard1993-TR` printed p. 37's "Point of
Clarification" carries all three fragments §2 quotes, verbatim, with both
elisions marked and nothing dropped; it had been confirmed only against the
extraction record, which under this stage's first rule is not a source. And
printed p. 6 carries the `a_i`/`b_i` pair exactly as §4's origin-occurrence claim
for the replication map requires. Every page anchor asserted anywhere in this
stage is now image-verified against its witness.

## Corrections applied 2026-09-05, thirty-first pass

Layer: the **Computed claims** — this stage's own arithmetic, re-derived from
scratch rather than re-read. Four claims. Two hold, two did not.

| # | Defect | Where it was |
| --- | --- | --- |
| 130 | **The two naming-growth rows held under no single base case.** "additive naming: `Log₂ C_n = n`" is exact only if `C₀ = 1`; "multiplicative naming: `Log₂ C_n = 2^n`" is exact only if `C₀ = 2` — and under `C₀ = 1` the multiplicative recurrence is constant at 1. The corpus's base case is neither: Group-0 fixes `C₀ = 0`, `C₁ = 1`, `C₂ = 2` and the recurrences run from **`i ≥ 3`** (codified §5.1) — the condition §5's table carries on its Hybrid row and drops on the other four. Exactly: additive `n − 1`, multiplicative `2^{n−2}`. The column is asymptotic and now says so, with the base case stated once. **No verdict in either table changes** | `VERIFICATION.md` Computed claims, `refined-sjas.md` §5 |
| 131 | **The same value asserted as exact in the comparison that carries the `O(L)` refutation.** R3 §3.1: "Additive naming gives `Log₂ C_n = n`; Hybrid(1) gives `Θ(n log n)` — a full logarithmic factor above it". The gap is real and the refutation is unaffected, but one side was exact-looking and wrong while the other was an order | `R3-the-margin.md` §3.1 |

**Verified and found correct** in this pass — the two claims that carry weight.
The Hybrid row was re-derived independently rather than re-read: `Σ_{i≤n} (Log₂ i)^1
= Log₂(n!)`, which is **21.8 / 524.8 / 8529.4** at `n = 10 / 100 / 1000`,
reproducing the recorded figures to the digit. Against `n` itself those are
2.2× / 5.2× / 8.5×, growing without bound, so **Hybrid(1) is not `O(n)` and the
withdrawal of the `O(L)` criterion stands on arithmetic, not on judgement**. The
`Log(q_β) ≥ 5J` bound also checks: a Gödel number `≥ 32^J` has `Log₂ ≥ 5J`. And
§4's `x_i`/`y_i` table is correct as written — those are the TR p. 6 *sequences*,
not the `C_i` naming convention, and they genuinely start at 2, giving `2^{n+1}`
and `2^{2^n}`. The collision is that both objects are a doubling recurrence
measured by `Log₂`, and only one of them starts where the table assumed.

## Corrections applied 2026-09-05, thirty-second pass

Layer: **`ADR-0002`'s acceptance criteria** — the charter's own claims about what
this stage has achieved, never checked at source. B1, B2, B3, B4′ and B5 all
hold. B6 did not, and checking B6 turned up a defect this review had introduced
itself.

| # | Defect | Where it was |
| --- | --- | --- |
| 132 | **B6's own falsification record was stale — at "twice", when it is four.** The criterion whose text reads "**Treat any future assertion that B6 is met as a claim requiring a fresh sweep, not as a standing fact**" carried a standing count of its own falsifications, and that count went stale. Beyond #23/#32 and #62–#66 it has since been falsified by **#120** (`Willard2001` §1's futility verdict cut mid-sentence in a way that inverted it) and by the **thirtieth pass** (two `Willard1993-TR` anchors never read from the page — one confirmed only against an extraction record, which rule 1 says is not a source). That last is a new kind for this criterion: **a quotation can be perfectly faithful to a record that is itself derived**, and neither half of B6 as previously worded catches it | `ADR-0002` |
| 133 | **Every section label this review wrote in rounds 31–33 pointed at §9 when the content is in §7.** "Five things are given up", the `ISM(A)` universal, the O35 engagement and the multiplication qualifier are all in **§7, "What the idea costs"** — not §9, "What is not settled". Eight sites: corrections #117, #122, #123, #124 and their "Where" cells, the O31 and O35 discharge notes in `../codification/concordance/composition-obligations.md`, and two `../LOG.md` entries. Verified against `git show 0837d52^` that the content was in §7 before this session too, so the error is entirely this review's. **#113 and #115 do correctly cite §9** and are unchanged | this file, `../codification/concordance/composition-obligations.md`, `../LOG.md` |

**Verified and found correct** in this pass: **B1** — every claim added in rounds
31–33 is checkable against the codified statement, because each round wrote the
Codification side first (§1.4, §3.1, §7.2, §8.2) and cited it. **B2** — §7 "What
the idea costs" exists and is the departures-and-costs section the criterion
names. **B3** — Lawvere reopened and delivered as R6, Rosser discharged at R1 §6.
**B4′** — R2's two external witnesses are as described. **B5** — "partly met, at
three of five" matches R3 §3.4's table row for row: `ℜ`, `Herb−k` and `Z` proved,
Hybrid(H) undetermined, Level(n) retracted.

**What #133 says about this review.** It is a mis-citation of exactly the class
this session has been hunting — the wrong-registry-key error at #116, the two
`IS(A)`s, the three `Z`s, the two doubling recurrences at #130. The review
introduced one while cataloguing them, and did not catch it until it audited its
own charter. No mechanical check would have: `../codification/audit.sh` verifies that a cited
path resolves and that a `§N.M` reference exists, never that the section cited is
the section meant.



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
