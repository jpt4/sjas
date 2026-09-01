# C16 — R5 Obligation Discharge Audit

Walk of all 87 composition obligations
([`composition-obligations.md`](composition-obligations.md)) against the
discharge target, [`../codified-sjas.md`](../codified-sjas.md). Satisfies
`C16-verification-goal.md` **R5** and is the evidence for acceptance criterion
**A-O**.

## Method

For each obligation: (1) locate every citation of **Onn** in
`codified-sjas.md`; (2) read the cited passage and ask whether the document
*does what the obligation says*, not whether it mentions it; (3) where the
obligation is uncited, sweep the document for the obligation's own content —
its named quotations, systems, constants, theorem numbers and vocabulary —
since an obligation may be discharged without being cited.

Dispositions:

- **discharged** — the document does what the obligation says, at a named
  chapter/section. Shortfalls within a discharged obligation are recorded in
  the reason column; they are refinements of a delivered requirement, not the
  requirement itself.
- **cited-only** — the obligation is cited but its substance is not delivered
  at that point. This is deficiency **10**, the O60 pattern.
- **carried** — deliberately deferred to Refinement, with the reason stated.
  Used where the content is absent from the document, not merely thin.

`carried` is used for genuine absence as well as for deliberate deferral,
because the register offers no fourth status; every `carried` row states which
it is and what is missing.

## Citation count correction

`composition-obligations.md`'s C15 discharge note says the draft "cites **57**
of the 87 obligations at their point of discharge". The true figure is **53**.
`grep` for `O\d+` in `codified-sjas.md` returns 57 distinct ids, but four of
them — **O81**, **O89**, **O90**, **O96** — occur only inside §10.4's numbering
sentence ("Ids run O1–O80 and O90–O96; O81–O89 were never issued"), which is a
range statement, not a citation at a point of use. This is deficiency **9**
(counts drift from the artifacts they describe) and should be corrected in the
register when the status column is updated.

## Dispositions

| Id | Disposition | Where / reason |
| --- | --- | --- |
| O1 | discharged | §6.4 restores the glossed side condition explicitly: "the closing inequality needs **`m ≥ 3`**, since `g > h` unfolds to `m − 1 > m^{2θ}`". C6's 2005-only scoping (that `Willard2011` Thm 5.9 needs no analogue) is not stated, but the obligation's action clause is met |
| O2 | cited-only | §6.6 cites O2 and does separate `1/5`, `1/4`, `1/3` — but that is not the obligation's triple, which is **6** bits per language symbol, **5** in Eq. (20), **1/4** the proved θ. The 6-vs-5 relation ("one convention viewed two ways") is never presented: no base-32 payload / base-64 byte distinction, no 1993 `i^{6/5}`, and `Willard2011`'s Conventional Tableaux Encoding Requirement (`32^J`) with its two derivations has zero occurrences. Pairs with O33 |
| O3 | discharged | §6.6 states the window `1/d ≤ θ < 1/3` with `d = 5` and `θ = 1/4` chosen inside it, says in terms that density bounds θ from **below**, and records ADR-0001's inverted `θ ≤ 1/5` as drift D6 |
| O4 | discharged | §4.6 and §6.8 both list `Willard2005` Remark 1's four apparatus claims (cut-free sequent, Herbrand, `Tab-Q*₁-List`, `Q*₁`-restricted Hilbert) as `stated-only`; §7.3 labels Remark 8's cut-free-generality clause `stated-only`; §8.2 explains the matrix's empty apparatus columns. Remark 6 is not separately labelled, and §8's grid marks these by omission plus prose rather than as `asserted-not-proved` cells |
| O5 | discharged | §2.4 gives the April-1994 Solovay telephone provenance, records that Solovay never published it, and names `Willard2001` App. A as the corpus's only extended exposition |
| O6 | discharged | `ℑ` is used correctly for the consistency-preserving map (§6.1, §6.2); `℧` for the Gödel sentence (§4.5, §7.3); §10.3 records all three corruption families including dropped numeral overbars. Shortfall: the *semantics* of the overbar — `Γ(n̄)` numeral-substitution vs `Γ(n)` variable-substitution, i.e. the fixed point itself — is never explained, and no overbarred numeral or Eq. (9) appears in the body |
| O7 | carried | Absent. Both `IS(A)` (§6.9) and `IS_D(A)` (§5.1, §5.4, §6) appear and are typographically distinct, but the delta is never stated: nowhere does the document say `Willard2001`'s system is Level(0−)/tableaux while `Willard2005`'s is Level(1)/apparatus-parameterized, nor that 2005 delegates Group-1 and the `Δ*₀`-encodability appendices to 2001. §6.9 additionally files `IS(A)` (`Willard2001` Thm 4.3) under "The Hilbert line's engine", where O7 describes it as a semantic-tableaux system — **flag to R2/R4** |
| O8 | discharged | §9.3 presents the origin conjecture (`Willard1993` §6), its refutation, the four sources, and labels the refutation a codifier's synthesis rather than a Willard result. Shortfall: the surviving fragment is not presented as such — §6.8 gives `Willard2005` Thm 5 only as the `Tab-U*₁-List` preservation result, not as a Level-1 restricted modus ponens proven near-maximal |
| O9 | carried | Absent. "duplicat" has zero occurrences; the `Willard1993` p. 328 phrases and the multiplication↔Gödel-diagonalization link are not present. §7.2 cites `Willard1993` pp. 328–331 for the multiplication-trigger point only, so the attribution the obligation asks for cannot yet be made |
| O10 | carried | Absent. Only the eight `U-Grounding` symbols appear (§3.2); `Willard1993-TR`'s `G₀` bit-string eight (StringCount, Shift, Extract, Andreverse, Address) has zero occurrences. The trap is not sprung, but neither is it defused for a reader who meets `G₀` in the registries |
| O11 | carried | §3.2 motivates the U-Grounding set correctly in shape ("addition is total there, so it may be a function symbol") but does not draw the load-bearing consequence: that the function symbol is what makes the axiom `Π*₁`, hence what lets Definition 4 require `Π*₁`/`Σ*₁` axioms and lets Theorem 1 treat the system as Normed. `Willard2011` D.1(ii)'s "very precise **Π*₁ styled** declaration" sentence the obligation asks to be quoted is absent |
| O12 | discharged | §3.3's ten-relativisation table, plus the explicit statement that `Willard1993-TR` writes `Δ₀/Π₁/Σ₁` for 2005's starred classes ("the classes are the same, the notation is not"), and the record that only two relations between the families are ever stated in print. Shortfall: no single convention is formally *fixed*, and the TR-vs-chapter divergence within 1993 (prefix form vs majorization) is not carried |
| O13 | discharged | §3.3 states directly that "it is the **growth class**, not the signature, that is load-bearing", citing `Willard1993-TR` Remark 6; §3.1's Type lattice and §3.2's Non-Growth definition organise the systems by growth class with signatures as variants. Shortfall: the slowly-growing class `f̂[i,j](x̄) ≤ i·2^j·Max(x̄)` and the O10 compatibility payoff are absent |
| O14 | carried | Half-done, so deferred. Only the 2005/2020 route is given (§6.7's envelope-overwhelm, §7.2's dying lemma). The 1993 route — multiplication-totality makes local `Π₁` reflection derivable, which TR Proposition 5 shows inconsistent — is absent ("Proposition 5" zero occurrences), so the obligation's "present both as routes to one boundary" is not met |
| O15 | carried | Blocked and absent. "tangib" has **zero** occurrences in the document, so the Tangibility Reflection Principle is never introduced and its 1993 lexical origin has nothing to attach to. Unblocking requires O28/O29 first |
| O16 | carried | Absent. "concatenation" has zero occurrences; TR Observation A.10 and the demote-to-relation move are not presented, so the origin cannot be cited alongside the Quine/Grzegorczyk/Visser/Murwanashyaka literature |
| O17 | carried | Absent, and this one is a live under-qualification rather than a clean omission. `Scalar_k` has zero occurrences, §3.2 introduces `Double(x) = x+x` without noting it is `Scalar₂`, and the document nowhere says that "multiplication is not total" means *variable* multiplication. §3.1's Type table and §7.2 therefore read as a stronger boundary than the corpus proves |
| O18 | discharged | §5.4 introduces axiom-versus-theorem once as a named device (`IS_D` vs `NS^{k,m}_D`, with `Willard2016` Rem. 7.4's benign use) and it is referred back to from §2.2 clause (i), §3.1's Type-Almost-M, and §7.3. Shortfall: 1993's INVALID.1/INVALID.2 instance is absent |
| O19 | carried | Absent. The Meta-Logic convention is not carried: `Willard1993-TR` Remark 2 has zero occurrences, no 1993 restatement in the document says whether the original constructs a proof or only establishes its existence, and the cut-free-*object*-vs-cut-free-*metatheory* warning is missing |
| O20 | carried | Absent. `Prf2_B`, `IS2`, trace-carrying proof predicates and the `Step`/`TraceOK` link all have zero occurrences, so the 1993 origin of explicit-trace checking is not cited |
| O21 | carried | Absent. "modest change" and "premature" both have zero occurrences. The document's How-to-read section states the tracked-not-merged policy in its own editorial voice (lines 21–23) but does not ground it in Willard's own 1993 statement — which is precisely the attribution the obligation exists to secure |
| O22 | carried | Absent. Fixed-parameter versus uniform totality is never presented as one mechanism: `SUBST_i`, Lemma 5.3's `T(p,k)`, the "degenerate case" modus ponens and the second Hilbert–Bernays condition all have zero occurrences. The obligation's payoff — naming exactly which derivability condition fails — is unavailable to the reader |
| O23 | cited-only | §5.6 cites O23 and delivers **stage two** (Thms G.2/G.3, three added sentences, `c` fixed in advance). Stage one is absent: the single **Global Simulation Sentence** replacing the schema and yielding only the Test-relativized `∀x Test^ξ_j(⌜Ψ⌝,x)` (Def. 6.8, Rem. 6.9) has zero occurrences — §5.6's `Test_i` is `Willard2014`'s kernel image, a different construct. The obligation requires "its two-stage structure and its cost"; only the cost of stage two is given |
| O24 | discharged | §1.4 carries Remark 6.16b's 1-line-proof caveat — the barrier the obligation says no presentation may omit. Shortfall: barrier (a) (Theorem 2.1 and its tableaux analogues bounding what can be evaded) is carried only diffusely, by §7 as a whole, not as Remark 6.16(a) |
| O25 | carried | Absent, and worse than absent in one place. "Introspectively" and "self-verifying" have zero occurrences. §2.2 fixes one vocabulary (`Willard2016` Def. 3.4's self-justifying configuration), which is half the obligation, but the other names are never tabulated — and §9.3 then uses "part (ii) of **self-verification**", an unglossed 1993 term, for §2.2's clause (ii). Fix the tabulation and that slip together |
| O26 | discharged | §3.1 states that Willard weakens the *theory*, never the vocabulary, and that "total function" keeps its usual meaning throughout. Shortfall: the contrast the obligation names as the point — Rosser-style alteration of the *provability predicate* versus Willard's alteration of the *theory* — is absent, as is the Feferman "numerically correct but intensionally incorrect" reference ("Rosser", "Feferman" zero occurrences) |
| O27 | discharged | §6.5 re-derives the closing-pair impossibility from `Willard2005` footnote 3 plus condition II-c rather than inheriting it as a gloss, and §4.4 fixes the prenex\* and parameter conventions that travel with it. Shortfall: this discharges the **2005** delicacy. The 2011 counterpart the obligation opens with — that `Scope_E(¬Υ,N)` is not the syntactic negation of `Scope_E(Υ,N)`, and that the two rebounded forms coincide with complementary bounded statements over `< E(N)` — is not established, though §6.2 lists `Scope_E`/`Good(N)` |
| O28 | carried | Absent. The three tangibility predicates and `Willard2001` §1's apparatus↔totality results (i)/(ii) — which the obligation calls the corpus's most compressed statement of its own design space — have no counterpart. `TangPred`, `TangDiv₂`, `TangRoot`, `ISTR(A)`, `IS^λ(A)` all have zero occurrences; `ISREF(A)` appears only as a naming-convention row in §5.5 |
| O29 | carried | Absent, and it is the missing link the obligation says it is. §4.3 defines the Definable Cut by all three clauses including closure under successor, but never names the **Tangibility Criteria** ((a)+(b) alone) nor states that the single dropped clause is the whole difference between Willard's programme and the Nelson/Pudlák/Wilkie–Paris tradition. Without it §4.3's relation to the prior literature stays atmospheric |
| O30 | carried | Absent. "Case 11", "Tertiary" and `β/6` have zero occurrences. §6.6 gives the density constant but not 2001's derivation of *why* the budget covers addition and nothing faster, which the obligation calls the clearest statement of the point in the corpus |
| O31 | carried | Absent, and a live over-statement. `ISM(A)` has zero occurrences, and the document nowhere separates consistency-preservation from self-knowledge. §6.7 and §7.2 present multiplication-totality as fatal without `Willard2001` §6's qualification that it is fatal to *reflection* (Thm 7.3), not to consistency-preservation. Correction needed, not merely deferral |
| O32 | carried | Absent. Theorem 3.5's `y−1` against Theorem 7.2's impossible `y` — one apart — has zero occurrences, and §1's motivation chapter, where the obligation says it belongs, contains no counterpart |
| O33 | carried | Absent. No base-32/base-64 payload distinction, no `32^J`, no `i^{6/5}`, no `β/6`. §6.6 mentions "Appendix A's six-bit-byte encoding" and §9.2 "six bits per symbol" without relating them, so the four measurements are never presented as one scheme. Pairs with O2; both need a single encoding passage |
| O34 | carried | Absent. "Bennett", "NUON" and Theorem C.2 have zero occurrences. §3.2 presents the grounding sets as varying lists without the stated design payoff, which is what the obligation says makes them read as a design rather than an arbitrary list |
| O35 | discharged | §1.4 records the no-ideal-system verdict, and §8.1/§8.2 present the trade-off space — five dials, with an explicit honest reading of their statuses — rather than nominating a best system. Three shortfalls: criteria I–VII are not enumerated, credit for I–IV to the definable-cut literature is not given, and "futile to seek an idealized form" is **paraphrased, not quoted**, although the How-to-read section declares §1.4 verbatim — **flag to R1**. The obligation's Refinement clause remains for Refinement |
| O36 | discharged | §9.4 states that the finite-set reading originates in `Willard2001` Remark 3.8, not in the 2008 ZCF drafts, citing O36 with O94. Shortfall: the `ISTR(A)`/Bitwise-Or content itself is absent, and the note that its proofs live in the unwitnessed `Willard1997` is not made (G17 is uncited anywhere in the document) |
| O37 | discharged | §7's opening states the asymmetry as a general property of the two halves — "the negative results are non-constructive while the positive ones are not" — with the obligation cited. Shortfall: the document does not say which side of *each* result is constructive, and `Willard2001` Thm 7.2's mechanism (reaching Eq. 69 by stepping outside α, with no bound on either derivation) is absent |
| O38 | discharged | §4.2 quotes the AN IMPORTANT POINT passage with footnote 2's Gödel-Completeness reason and states both halves in the obligation's own terms: "extensionally degenerate and intensionally decisive" |
| O39 | discharged | §5.3 leads with the object-level doubling/squaring formulation and its `Log(x_n) = n+1` vs `Log(y_n) = 2^n` consequence, exactly as required. Shortfall: the bit-counting forms are not *derived* from it, because O30 and O33 are absent. Sourcing note: the document cites `Willard2020-LFCS` p. 11 Eqs. 7–8 where the obligation cites `Willard2020` §5 — the same construction in two witnesses |
| O40 | discharged | §4.2 and §4.5 identify the Linear-Sum Effect as the active ingredient, name Xtab's recovery of it via LEM-as-logical-axiom, and give its three aliases (Tableaux `Δ₀` Compression, Hyper-Constructivity, the unnamed `∗` invariant). Shortfall: statement **⊙** is never stated — its only occurrence is in §10.3's corruption list — so the obligation's hinge, "⊙ generalises Theorem ++ because both satisfy the Linear-Sum Effect", is absent. Same gap as O48 |
| O41 | discharged | §5.2 delivers the substance at the point the levels are introduced: why the level distinction is substantive, because there is no decision procedure for enumerating true `Π⁻₁` sentences. Shortfall: sourced to `Willard2002c` rather than to the `Willard2020` p. 22 Level-0/Level-1 comparison the obligation asks to be quoted; "Rank-1" and "simpler but substantially weaker" have zero occurrences |
| O42 | discharged | §4.4 fixes `Willard2007-APAL` Def. 1's eight rules with a stated reason (`Willard2009` App. B invokes rules 6–8 by number) and states both travelling conventions — prenex\* root, previously-introduced parameters. §6.5 then re-establishes the `Δ*₀` closing-pair fact under them rather than importing Lemma 4.6 verbatim, which is the obligation's operative demand |
| O43 | discharged | §4.5's table presents one construction and instantiates it twice (arbitrary-`℧` LEM; `Δ₀`-only LEM), with the shared mechanism named once. Shortfalls: the instantiation the obligation names — `Willard2002a` Lemma 6.3's implication form `Θ_Υ ⇒ Υ` — is absent, as are the `V₄` CLARIFICATION and `Willard2001` Lemma 7.1's comment that the shared construction is supposed to explain |
| O44 | discharged | The device is stated once in §5.3 and reused. All four appearances the obligation names are present across §5.3 (`Willard2020-LFCS`; `Willard2006a`'s naming conventions) and §7.2 (`Willard2002a` Lem. 4.7; `Willard2001` Rem. 4.5 footnote 7), and §5.3 states the obligation's conclusion outright: the affirmative and negative halves are two readings of one observation |
| O45 | discharged | §5.2's Level table plus the `Willard2004` §§1–2 quote on equivalence-under-strong-models-but-not-provably-so is the obligation's content, delivered where the index is introduced |
| O46 | discharged | §8.1 states in terms that the two dials must **not** be merged, with the `ℜ`=intermediate-theorems / `Z`=LEM-instances distinction and drift D34 cited; §9.1 then uses the distinction to make the open problem statable, which is the obligation's stated purpose. The `ℜ` positive's `sketch` status is carried at §8.1, §7.5 and §9.1 |
| O47 | discharged | §5.5 states that `ISCE` **is** `ISREF` with the convention upgraded and quotes the "degenerate incremental-naming version" attribution; Continuous Expansion is given as the stated payoff at §5.3 and in §5.5's table |
| O48 | carried | Absent, so there is no chain to restore. Statement **⊙**, the Hájek–Pudlák pp. 172–174 citation, Locally-J-Closed / Locally-1-Closure and conditions (11)/(12) all have zero occurrences. `Willard2020-LFCS` is used in the document only for §5.3's Eqs. 7–8. Pairs with O40; both wait on a passage that states ⊙ |
| O49 | discharged | §8.1 gives five coordinatisations of one boundary with the status of both halves per dial, exceeding the obligation's three, and names `Hybrid(H)`'s exact threshold. **Note the obligation's own text is stale**: O49 says "`ℜ`'s positive side is proved and its negative side sketched", but G35 (2026-08-28) established `sketch` for the positive too. §8.1, §7.5 and §9.1 all carry `sketch`/`sketch`. The document is right and O49's text needs amending — deficiency **4** |
| O50 | discharged | §5.3's own formulation delivers the obligation's substance — a system that can name `y_n` names "an integer whose binary encoding is `2^n` bits long — long enough to swallow the system's own proof of its own consistency" — which is Remark 3's proof-Gödel-number-versus-integer comparison in the document's words. Two shortfalls: Remark 3's sentence is not quoted, and "lead with it" is not followed, because O39 and O50 give conflicting precedence instructions and the document led with O39's object-level form. Reconcile the two obligations rather than the document |
| O51 | carried | Absent. "uncompressed" has zero occurrences and the document nowhere disclaims the coding-density requirement for the Hilbert line. §6.6 does confine density to the tableaux engine, so the requirement is not asserted universally either — but §5.3's Hilbert-line naming discussion and §9.2's "six bits per symbol" for `IQFS` sit next to it with the distinction undrawn |
| O52 | cited-only | Cited at §4.3, but §4.3 delivers the *disambiguation* of "cut", which is O78's content. O52's own content is absent: the two metatheoretic toolkits — definable cuts **and thinnings** for the Hilbert side versus generalized Gödel sentences `⧫^λ_D(α)` for the cut-free side, with `Willard2006a` p. 12's reason quoted and `Willard2004` named as the explicit hybrid. "thinning", "⧫" and "generalized Gödel" all have zero occurrences. O60-pattern failure: the citation sits at a point that does something else |
| O53 | cited-only | The clearest O60-pattern failure in this walk. §9.2 says "Obligation **O53** requires both frontier problems to be carried, not just the better-known one" — and the document then carries only the better-known one. Pudlák's **Extender-Function** question is nowhere: "Extender", "G27", "Infinite Far Reach", "injective" and `Willard2006a` Theorem 7's monotonicity result all have zero occurrences, and §9.3's "smaller open question" is `Willard2002a`'s shortest-`V` problem instead. The Hilbert-side frontier item is missing while the obligation is cited as met |
| O54 | discharged | §6.2 presents the envelope as one construct rebuilt five times with a dated lineage from `Willard2001` Def. 1.1, and its table gives `Willard2004` §6's **`G-good`** measured by `ℑ(H) = Σ χ(p_i)`, labelled "the direct ancestor of `Normed`" — which is the obligation's "one construct with a dated lineage rather than 2005's innovation". Shortfall: `G-good`'s rising-`Σ`/falling-`Π` asymmetry and 2004's own impossibility step ("impossible for both `X` and `Y` to have G-good proofs") are not stated, so the O27 link is not visible |
| O55 | discharged | §4.5 and §7.3 both record the 1993/94 provenance. Two shortfalls, the first a live over-claim: O55 says the TR supplies a **definition** ("a tableaux proof with cuts" = tableaux + `Φ ∨ ¬Φ`) and warns that the p. 36 ZF parenthetical "must not be cited as a stated identification", but §7.3 asserts "The identification of LEM-as-axioms with cuts is already in `Willard1993-TR`" — **flag the wording to R1/R3**. Second, §4.5's table still credits `Xtab` to `Willard2020` App. alone, so the obligation's conclusion (2020's contribution is the negative theorem, not the apparatus) is not drawn |
| O56 | carried | Absent. "polynomial" has zero occurrences; the TR p. 35 footnote's construction and its reason — Group-2 supplies `Prf_A(⌜Φ ∨ ¬Φ⌝) ⊃ Φ ∨ ¬Φ`, so `IS(A)` already simulates a `Δ₀` cut rule with only polynomial proof-length increase — is not carried. §8.1 and §9.1 give the `Z` dial's `Δ*₀` positive as `stated-only` without the reason, so the obligation's payoff (making the open `Π*₁` question precise as whether Group-2 delivers those instances cheaply one level up) is undelivered |
| O57 | discharged | §2.4's two-branch table derives the tableaux and Hilbert lines from Solovay's theorem as Propositions 1 and 7, with "**The architecture follows immediately**" and the 2016 restatement in a parallel column — the two lines presented as the two ways out of one theorem, not as parallel programmes |
| O58 | carried | Absent, and blocked. "tangib" has **zero** occurrences, so tangibility is neither introduced as a founding notion nor its 2011 abandonment recorded. Depends on O15/O28/O29 landing first |
| O59 | cited-only | Cited at §5.2, but §5.2 delivers a *different* `Willard2002c` point — no decision procedure for true `Π⁻₁` sentences, which is O41/O45 content. The obligation's rule is absent: weakest `D_i` when generalizing G2, strongest feasible `D_i` when building boundary-case exceptions, which is what forces each paper's level choice. "weakest"/"strongest" occur only in §6.1's preservation-hypothesis comparison and §6.7. O60-pattern failure; without the rule the Level index still looks like an arbitrary per-paper parameter |
| O60 | discharged | **The recorded deficiency-10 instance is repaired.** §8.1 now presents the two-index `R(i,j)` with `R(1,1)` working and `R(2,1)` failing, and states that `Willard2004`'s `ℜ` dial is a *projection* of it carrying strictly less information; §4.5's table lists `R(i,j)` among the restricted-cut hierarchies. Shortfall: the two-index **format** (`Π*ᵢ` **or** `Σ*ⱼ` intermediate theorems) is never defined, so a reader cannot see why raising the `Π` index alone is what collapses; and the single-class dials other than `ℜ` are not presented as projections |
| O61 | discharged | §7.4's title and body state that `Willard2009` evades by re-axiomatising, not by weakening, with the Ax-1/Ax-2/Ax-3 table and the cannot-prove-their-own-equivalence explanation; §2.4 names it a third line belonging to neither branch. Shortfalls: **Split Representations** — the base-`(a₀+1)` digit tuples that make every quantifier bound a `Max`, which the obligation names as the mechanism — has zero occurrences, and the axis is carried as a "line" in §7.4 rather than promoted into `axes.md`'s five axes |
| O62 | discharged | §7.1 presents Paris–Wilkie as one question answered "in **two opposite directions**, and both answers are Willard's", and supplies the tableaux half with `Willard2002a` Thm 6.4. Shortfall: the Herbrand half (`Willard2009` Thm 4) is delivered in §7.4 with no cross-reference from §7.1, so the split answer is asserted in one section and completed in another |
| O63 | discharged | §2.3 gives Definition 1's Threshold/Anti-Threshold vocabulary, states the `⊇` axiom-containment convention with the "stronger than the more modest construct" quote, flags it load-bearing, and the vocabulary is then used in §7.4's table |
| O64 | discharged | §7.4 presents Theorem 5 as the counterweight and says in terms that quoting Theorem 4 without it "misrepresents the result as a general evasion", with Theorem 5's `sketch` status carried. Shortfall: the Kolodziejczyk [18,19] Herbrand/tableaux-inequivalence connection is absent |
| O65 | discharged | §1.4 carries the `Willard2016` §9 summary judgement verbatim, including the parenthetical calling G2's significance "**undeniable**". Shortfall: Remark 8.3's 25-year retrospective — "a **quite skinny form of proof**" — is absent ("skinny" zero occurrences), so one of the obligation's two named passages is missing |
| O66 | discharged | The conditionality is carried at every point of use, which is exactly what the obligation demands: §5.5's table ("works, *conditionally*"), §6.9's table (`full`, **conditional**) with the rule stated explicitly, and §9.2. Shortfall: the frontier statement the obligation asks for — that the appendix's argument bounds the *canonical* route to large integers, not every route — is not made; §9.2 gives §A.1/§A.2 and the three differences instead |
| O67 | discharged | §4.6 and §6.8 carry the apparatus-generality claims as `stated-only`, and §8.1's table gives the `Herb−k` dial with `k = 1` positive / `k = 2` negative, **both** `stated-only` — the weakness the obligation insists on. Shortfall: the obligation's *point* is not stated — that this is the corpus's only evidence the threshold is a property of the cut-restriction rather than of the deduction method carrying it ("cut-restriction" zero occurrences), so the dial appears without the inference it licenses |
| O68 | carried | Absent. §3.3's table records `Δ^R₀`'s "quantifier-bounding terms use **only `Max`**" but nowhere says the restriction is *not* load-bearing; `Willard2009` Remark 2 (Theorem 4 also holds with Addition in the bounding terms, omitted "for the sake of simplifying the presentation") has zero occurrences. Since every other restriction in the corpus's positive results is at or near a threshold, the omission leaves the reader to assume this one is too — the assumption the obligation exists to block |
| O69 | discharged | §7.4 quotes the conference form (`Diag(1)`/`Diag(2)` provably logically invalid, `Diag(3)` valid), names it the sharpest form of the thesis, and records the journal version's drop with drift D47 |
| O70 | discharged | Already `closed:2026-08-29` in the register, and the content is present though the obligation is uncited: §7.1 carries `Willard2007-APAL` Theorem 6 as `full`, §7.4's table places it in the tableaux column alone and pairs it with Theorem 5's `sketch` status, and §4.5 carries Passive Induction and Def. 5's Tableaux `Δ₀` Compression |
| O71 | discharged | §2.4's table carries one derivation with 1993/2016 naming columns and records that neither text mentions the other. Shortfall: `Willard2016`'s third datum — Willard [52,57] with Adamowicz–Zbierski showing **Type-M systems cannot recognize their tableaux consistency**, closing the remaining cell — is not recorded with that attribution; §8's Type-M/Tableaux cell carries only "**−** `sketch`" |
| O72 | discharged | §1.1 opens the document with the Q-1/Q-2 split in Willard's own words and states that everything in the corpus answers Q-2 — the framing at the outset that the obligation asks for |
| O73 | discharged | Already `closed:2026-08-29`. §5.6 carries `Willard2014` §§5–6's `IS_D^#(β)` under its printed name (not Rem. 7.3's `ISD(β)`, per drift D76), with Theorem 3's `sketch` status, the kernel-image price, and Example 2's PA-vs-weak-system caveat |
| O74 | discharged | §5.4 quotes `Willard2007-APAL` p. 29 with the "**One of these systems**" opening — the phrasing deficiency **6** recorded as previously mis-quoted — and §2.2 cites it at clause (i)'s axiom-or-theorem disjunction |
| O75 | discharged | §2.4 routes the reader to `Willard2001` Appendix A, and adds the qualification that the appendix proves a **weaker** version "with the virtue of a four-page proof" |
| O76 | discharged | The `Willard2000-TAB` §5 passage is quoted in full at §5.3 and again at §7.2, and §7.2 draws the obligation's own conclusion: the growth device "is what the negative proofs consume", so the two halves of the corpus are two readings of one observation |
| O77 | discharged | §1.2 carries both chains — Sacks direct, Nerode via Tennenbaum — presents them as recollections rather than documents, and quotes Willard's caveat about Gödel's publication record. Shortfall: Harvey Friedman's 14 March 2014 lecture, named by the obligation as a second datum, is absent |
| O78 | discharged | §4.3 disambiguates in Willard's own words, quoting both the `Willard2009` p. 16 and `Willard2004` p. 348 statements, gives each sense its own definition, and adds the ubiquity fact about definable cuts in every sub-PA system |
| O79 | discharged | §1.4 quotes statement `###` verbatim, records that Willard added §8 late so the theorem's meaning "could not be confused", and gives Definition 8.1's Platonic Stability as his answer |
| O80 | discharged | §4.6 lists all five apparatus-generality claims with the 2002 provenance and marks them `stated-only`; §7.6 repeats the point with the eighteen-year gap. Shortfall: resolution is not placed in the apparatus axis — §4.1's catalogue (`d_E`, `d_M`, `d_H`, `d_F`, `d_ER`) omits it and §8's matrix has no resolution column; §8.2 accounts for the absence in prose instead |
| O90 | carried | Absent, and a live under-qualification. "truncated", "LongMult" and "Tier(1)" have zero occurrences; neither `Willard2006b` Thms 2/6 nor `Willard2005-TAB` Thms 3/4/5/6 is stated. §8.2's single sentence on the real-valued line says `IS_D(A′)` "recognises multiplication as total **over simulated reals**" — a qualifier, but *not* the truncated-mantissa one the obligation requires, and one that covers the forbidden Def. 6 `LongMult` case too. Needs correction, not merely deferral |
| O91 | carried | Absent. The Numerical-Analysis versus Number-Theory framing has no counterpart in §1 or elsewhere; `Willard2006b` Remarks 1–2 and `Willard2005-TAB` pp. 13–14 are unused. Deferred with O90/O92 as one floating-point passage |
| O92 | carried | Absent. `Willard2005-TAB` Theorems 3/6/7, the p. 12 Important Comment and the Tier(1)^⊕ translation are not presented, so nothing in the document could yet conflate Theorem 6 with Theorem 3. Deferred with O90/O91 |
| O93 | cited-only | Cited at §7.6, which does deliver the *principle* — "These are open, not discharged by the journal forms" — but for `Willard2004`'s four deferrals and `Willard2000-TAB` Lemma 1. The obligation's named items are `Willard2005-TAB`'s: Lemma 1's `Δ*₀` compression for ×/÷, Theorem 4's derivation from Eq. (13), and Theorem 7's LinH argument. None appears, and gap **G37** is not cited anywhere in the document |
| O94 | discharged | §9.4 states that the finite-set reading originates in `Willard2001` Remark 3.8 and presents `dew-2008-zcf-group` as a later, unpublished set-theory programme applying related intuitions, citing O36 alongside as the obligation asks |
| O95 | discharged | §9.4 records that the 2025 drafts carry title-page and referee witnesses for `Willard2021`, and §9.4's gap table carries G1 as unacquired. Thin, but it does what the obligation says. Shortfall: drift D78 and the tender→fine title shift are not named |
| O96 | carried | Absent. `Willard1998` appears only as a source row in §3.2 (Root rounding, drift D80) and §3.3 (the `Δ⁻₀` family). None of the obligation's content is present: the `ISREF` debut, Tangibility Reflection, the apparatus↔totality table, the Cognitive Conjecture, `ISREF^R`, Theorems 9.5–9.6's P=?NP linkage, cascade-self-verifying and `ISTM^λ` all have zero occurrences, and D79 is uncited. Blocked in part by the total absence of tangibility (O15/O28/O29/O58) |

## Totals

**52 discharged, 6 cited-only, 29 carried** — 87 rows, matching the register.

| Disposition | Count | Ids |
| --- | --- | --- |
| discharged | **52** | O1, O3, O4, O5, O6, O8, O12, O13, O18, O24, O26, O27, O35, O36, O37, O38, O39, O40, O41, O42, O43, O44, O45, O46, O47, O49, O50, O54, O55, O57, O60, O61, O62, O63, O64, O65, O66, O67, O69, O70, O71, O72, O73, O74, O75, O76, O77, O78, O79, O80, O94, O95 |
| cited-only | **6** | O2, O23, O52, O53, O59, O93 |
| carried | **29** | O7, O9, O10, O11, O14, O15, O16, O17, O19, O20, O21, O22, O25, O28, O29, O30, O31, O32, O33, O34, O48, O51, O56, O58, O68, O90, O91, O92, O96 |

Every `cited-only` row states its reason, as **A-O** requires. All six are the
O60 pattern: a citation placed at a passage that delivers something else. The
one previously recorded instance of that pattern, **O60** itself, is repaired.

## What this walk found beyond the dispositions

Items for `C16-review-record.md`, in descending order of consequence.

**Three live mis-statements, not omissions.** These are not deferrals; the
document currently says something the corpus does not support.

1. **O31** — §6.7 and §7.2 present multiplication-totality as fatal without
   `Willard2001` §6's qualification that it is fatal to *reflection* (Thm 7.3),
   not to *consistency-preservation*: `ISM(A)` recognises multiplication as
   total and *is* consistency-preserving.
2. **O90** — §8.2 says the real-valued line "recognises multiplication as total
   over simulated reals", which is the qualifier `Willard2006b` Def. 6's
   forbidden `LongMult` also satisfies. The obligation requires the
   truncated-mantissa qualifier specifically.
3. **O17** — nothing in the document says that "multiplication is not total"
   means *variable* multiplication, so §3.1 and §7.2 read as a stronger
   boundary than the corpus proves. `Double` is introduced without noting it is
   `Scalar₂`.

**Two quotation/attribution findings for R1 and R3.**

4. **O35** — the `Willard2001` "futile to seek an idealized form of
   self-verifying system" sentence is paraphrased in §1.4, which the
   How-to-read section declares verbatim.
5. **O55** — §7.3's "The identification of LEM-as-axioms with cuts is already
   in `Willard1993-TR`" is the over-claim O55 explicitly warns against: the TR
   supplies a *definition*, and the p. 36 ZF parenthetical "must not be cited
   as a stated identification".

**One placement question for R2/R4.**

6. **O7** — §6.9 files `IS(A)` (`Willard2001` Thm 4.3) under "The Hilbert
   line's engine", while O7 and O28 describe `Willard2001`'s `IS(A)`/`IS^λ(A)`
   as cut-free (tableaux/Herbrand) and `ISREF(A)` as the Hilbert-line system.
   Either the row or the section heading needs re-reading against the paper.

**Two stale artifacts (deficiency 4 and 9).**

7. **O49's own text** is stale: it says the `ℜ` dial's positive side is proved,
   but G35 (2026-08-28) settled it as `sketch`. The document is correct;
   the obligation needs amending. Same class as the D34/D38/§9.1 sweep.
8. **The register's "57 cited"** is really **53**; four of the 57 grep hits are
   range boundaries in §10.4's numbering sentence.

**One structural cluster.** Fifteen of the 29 `carried` rows are one gap.
Tangibility has **zero** occurrences in `codified-sjas.md`, which strands O15,
O28, O29, O58 and most of O96 outright; the `Willard1993-TR` mechanism
obligations (O9, O16, O17, O19, O20, O21, O22) and the `Willard2001` design
obligations (O30, O31, O32, O34) have no host section either. The document's
1993 and 2001 material is confined to architecture (§2.4), notation (§3.3) and
naming conventions (§5.5). A Refinement chapter on the founding papers'
mechanisms and the tangibility line would discharge most of the block at once;
the floating-point cluster (O90, O91, O92, with O93's `Willard2005-TAB`
deferrals) is a second, independent passage.
