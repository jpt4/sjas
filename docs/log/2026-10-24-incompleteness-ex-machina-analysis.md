# Incompleteness Ex Machina: analysis, and the case for Self-Justification Ex Machina

Date: 2026-10-24

## Request (verbatim)

> We are going to analyze the paper "Incompleteness Ex Machina"
> (https://www.scottaaronson.com/incompleteness.pdf), and write, if it can be
> done, "Self-Justification Ex Machina", a similar exegesis of Willard's
> self-justification from a computational perspective.

Outcome: it can be done. The essay is
[`prose/self-justification-ex-machina/self-justification-ex-machina.tex`](../../prose/self-justification-ex-machina/self-justification-ex-machina.tex)
(13 pp. compiled, PDF beside it). This note is the reading behind it.

## Source and reading record

- Sebastian Oberhoff, *Incompleteness Ex Machina*, dated 31 December 2018,
  pdfTeX, 9 letter pages. Downloaded to the session scratchpad, not to the
  repository.
- All 9 pages read twice: as `pdftotext -layout` text and as rendered page
  images (110 dpi).
- One instance of hazard H2 (codified statement §10.3): the halting lemma's
  diagonal program is **H̄** (H with an overbar, p. 5); the text layer renders it
  as plain `H`, which makes "H(A) { if H(A) = false …" read as a
  self-contradictory definition. The image is authoritative.

## What the essay does

| § | Title | Content |
| --- | --- | --- |
| 1 | The Incompleteness Theorems On Fast-Forward | G1 for an *honest* system as an explicit case analysis on G; Rosser's sentence R ("for every proof of me there is a shorter disproof") as a symmetric case analysis; G2 from "the first half of G1" plus the Gödel-hand-waved internalization |
| 2 | The Rise Of The Machines | The thesis: Gödel's work is (a) teaching arithmetic to compute and (b) the theorems, which are "victory laps" |
| 3 | To Halt Or Not To Halt | Halting lemma (program H̄); G1 for honest F via a complete-and-honest F deciding halting |
| 4 | Welcome To The World Of Mirrors | Program P (search for a proof that A(A) doesn't halt; halt if found); G̈ = «P(P) doesn't halt» undecidable, with the same case structure as G, including "sub-inconsistency" (ω-inconsistency) in case 2; Rosser via program B (halt on proof of non-halting, loop on proof of halting); G2 via P |
| 5 | Taking Off The Glasses | G̈ *is* "I am not provable": P(P) is searching for a proof of G̈ |
| 6 | Refactoring | Theorem 7, "Turing completeness of elementary arithmetic", left "to smarter people" |

Method: the classical and computational proofs are written in the same
painstaking case format so the computational versions can be produced "by
copy-pasting wholesale and then tinkering a little". Oberhoff's reference [3]
credits Aaronson's *Quantum Computing since Democritus*, where program B gives a
pseudo-proof of G2; the essay began as an effort to plug its holes.

## Hypotheses, and where each is used

| Result | Uses |
| --- | --- |
| Thm 4 (G1 via halting) | honesty, the completeness assumption, and the ability to *state* halting facts. No representation of computation inside F is needed: the search does the work. |
| §4 G̈ undecidable | case 1: consistency + "write out the run in F" (external Σ₁-completeness); case 2: honesty. Only Σ₁-soundness is actually used in case 2, not full honesty. |
| Thm 5 (Rosser via B) | consistency; external Σ₁-completeness in both cases; in case 2 also a Π₁ fact, that an entered infinite loop is never left (footnote 12). Proving that needs some induction (PA has it; Q in general does not), which Rosser's arithmetic formulation avoids by its ordering trick. |
| Thm 6 (G2 via P) | consistency; external Σ₁-completeness; and the step taken "with some faith": the internalized form of "we can write out [the run] in F". |

## Points of imprecision (none fatal to the essay's purpose)

1. **"Honest"** is soundness. Gödel used ω-consistency; Oberhoff glosses it as
   "super-consistency". Case 2 needs only Σ₁-soundness.
2. **The faith step** (Theorem 6, third step) is the internalization of
   formalized Σ₁-completeness: F must prove «for every proof s of G̈, there is a
   proof of "P(P) halts"». That is the third Hilbert–Bernays–Löb derivability
   condition, in the special case where the program is a proof search. If
   "consistent" means "no proof of 0=1", the internal argument also needs proof
   composition (D2).
3. **Theorem 7 conflates two theorems**: external representability (F proves
   each true halting fact) and internal representability (F proves that it
   proves them). The first suffices for everything in the essay except
   Theorem 6. This conflation is the hinge of the companion essay.
4. **Formalizing halting.** If «P(P) halts» means "the search succeeds" (∃s
   Prf(s, ⌜G̈⌝)), step (1) of the internal argument is free. If it means "an
   execution trace exists", even identifying the two costs growth: given s, the
   trace up to s must exist. Oberhoff's suggestion of putting Turing machines
   "directly into the bedrock" makes this a live issue.
5. **Footnote 12** ("any formal system capable of reasoning about algorithms can
   prove that `while true {}` runs forever") understates what Rosser's case 2
   needs: a Π₁ non-halting proof from a finite prefix.

## Why the companion essay can be written

Willard's self-justifying systems meet every hypothesis Oberhoff's first
theorem uses: they watch every halting computation, prove every Π₁ theorem of
PA through Group-2, are sound, and obey G1, including Rosser's version through
program B, which Willard states himself (TR 93-10 p. 6). They fail only the
faith step, and Theorem 6 run backwards shows that they must fail it. That
gives the essay its spine.

| Oberhoff | Self-Justification Ex Machina |
| --- | --- |
| program P, sentence G̈ = «P(P) doesn't halt» ("I am not provable") | program C (search for a refutation of F₀ + «A(A) doesn't halt»), sentence Ẅ = «C(C) doesn't halt» ("I am consistent") = Willard's Group-3 |
| "write out the run in F" | split: *watching* (external Σ₁-completeness, holds) vs *knowing that one watches* (formalized, fails) |
| Theorem 6 with faith | Prop. C: with faith (e.g. F₀ = PA), F₀ + Ẅ is inconsistent: the Kleene/Rogers/Jeroslow warning |
| — | Willard's theorem: without faith (Type-A tableaux or Type-NS Hilbert), F₀ + Ẅ is consistent |
| case analysis for G̈ | Willard 2001 Remark 4.4 / Assertion ++ as an Oberhoff-style case analysis: shortest refutation y; walk (Probe); Ẅ blocks only at a named refutation; names stay below y (doubling); smaller refutation, contradiction |
| — | Prop. E: Theorem 6 run backwards: a consistent F proving its own Level(1) consistency cannot prove the faith step at its own G̈ |
| Rosser B, sub-inconsistency | Rosser goes through verbatim; the boundary results (squaring, cut amplification, LEM, Level 2⁺) become "the price of faith" |
| Taking off the glasses | self-reference is cheap; self-*simulation* is the cost |
| Theorem 7 | Theorem 7a (watching) + 7b (knowing), with 7b's price: uniform multiplication without lemmas; successor with them |

Design decisions:
- LaTeX, in Oberhoff's case-analysis format.
- The narrative core is Willard's own intuition, Assertion ++, rather than the
  2005 engine, which appears as the "two horizons" remark.
- Every Willard result carries paper / item / page / status from
  `nachlass/codification/registry/results.md`, collected in a provenance
  appendix.
- Our own interpretive apparatus is labelled as such in the essay's last
  section.

## Claims in the essay that are ours, not Willard's

- The watching / knowing-that-one-watches split, and Theorems 7a/7b.
- Program C and Ẅ as the computational rendering of Group-3.
- **Proposition E**: a paper proof, the contrapositive of Theorem 6, under
  explicit hypotheses. The fixed-point equivalence for G̈ is *assumed* for
  `IS_D(A)`, as for Willard's own Group-3; it is not verified against Willard's
  Δ*₀ substitution machinery. The essay claims no novelty for it.
- The "horizon" picture and the reading of cut as subroutine call / cut
  elimination as inlining. The latter is standard Curry–Howard folklore; the
  growth-amplification point is Willard's own (TR 93-10 p. 7).
- The cut-shortening outline in §7 is a hedged textbook gloss. Pudlák's and
  Solovay's theorems are cited through Willard 2006a Theorems 1–2, status
  `cited`.

## Verification record

Quotations checked in this session against rendered page images:
- TR 93-10 printed pp. 6–7 (PDF pp. 8–9 of the searchable witness): epigraph;
  "satisfy Godel's First but not Second"; "either fails … or fails to meet the
  full Hilbert-Bernays conditions"; "adequate memory"; a₃₀₀; cuts and Solovay.
- Willard 2001 p. 20: Remark 4.4, Assertion ++, the (p, u) sentence; p. 21
  (text) for "fairly routine and easy".
- Willard 2006a p. 24: Remark 3.

Quotations taken from the codified statement's verbatim blocks: 2000 §5 ("the
true reason"), 2005 §1 (Kleene/Rogers/Jeroslow), 2016 Example 3.5 ("ironically
typically useless"), 2016 §8 ("almost cheating"), 2011 Rem. 6.16b ("1-line
proof"). All proof statuses come from the registry. Bibliographic data for the
non-Willard references (Hilbert–Bernays 1939, Löb 1955, Pudlák 1985, Boolos
1984, Aaronson 2013) is from memory and was not checked against the
publications.

## Open items (draft 1; superseded by the revision-2 list below)

1. Verify Prop. E's fixed-point hypothesis against Willard's Group-3
   construction (2005 §3 and the 2001 Appendices B–C encoding).
2. The "transcripts" heuristic (§3 of the essay) could become a lemma via a
   Herbrand-style bound on tableau terms over U-Grounding symbols.
3. A Hilbert-line remark was considered and left out as unverified: gluing two
   Hilbert proofs costs roughly a product of their codes, so D2 should also fail
   internally for Type-NS systems.
4. Optional external read-only review of the essay (the Cursor/Grok recipe in
   memory).

## Revision 2 (2026-10-25): the precision pass

Draft 1 was committed on branch `sjexm-essay` (84eba77). The user then set
the standard for revision (verbatim):

> Oberhoff's work satisfies the requirements for a paper proof, at the level
> of granularity necessary for G2; self-justification is a finer-grained
> concept, and so SJexM must be more technically precise to meet equal
> proof-status validity.

An adversarial read-only review by Grok 4.7 (Cursor CLI, `--mode ask`) was
launched on draft 1 against that standard. In parallel the essay was audited
against the source texts and the extraction records. Revision 2 adds a
Conventions section that fixes configuration, language, consistency level,
the formalization of halting, the fixed point, the encoding and the metatheory.
Every claim is restated against those conventions.

Corrections made in the self-audit, each checked against the source text:

| # | Draft 1 said | Source | Revision 2 |
| --- | --- | --- | --- |
| 1 | F₀ has "constants for 0, 1, 2, with addition total" | 2001 pp. 9–10: a constant `n̄` for every n, addition total by the Π⁻₂ axiom (6), seven grounding functions. 2005 pp. 4, 8: `c̄₀`, `c̄₁`, with `+` and `Double` as function symbols | the two languages are stated separately |
| 2 | one Willard theorem: "if A's Π₁ theorems are true, F₀+Ẅ is consistent" | 2001 Thm 4.3 assumes *regular consistency* (Δ⁻₀ `Prf_A` and true Group-2), uses tableaux, Level(0−); 2005 Thms 3, 5 assume Π\*₁-truth, use tableaux and Tab-U\*₁-List, Level(1) | Theorem D(a), D(b) |
| 3 | step (1): the two sentences are "the same sentence" | 2001 p. 39: the equivalence needs α to prove uniqueness of `SUBST(n̄, ·)`, a Π⁻₁ theorem of PAX | Fact FP; step (1) is one half of (FP) |
| 4 | Prop. E assumes the fixed-point equivalence | 2001 Thm 7.2 proof, Eqs. (63), (66)–(67) | FP proved from (U), the order axioms, and an encoding hypothesis (E) that Willard does not state (a proof's code exceeds its conclusion's code). (E) checked against 2001 App. B p. 51 and 2005 App. A pp. 39–40 |
| 5 | Prop. E: F does not prove step (2) | — | sharpened: given Level(1) self-consistency, step (2) and the formalized-Σ₁-completeness instance at ¬G̈ are each F-provably equivalent to G̈ |
| 6 | §3: "§6 turns [the size heuristic] into a theorem" | Prop. E is proved through consistency, not size | false; replaced by Lemma (transcripts): under 2005 Eq. (20), a tableau proof containing a closed term of value v has code > v⁵. The exact theorem is 2005 Thm 6 |
| 7 | case analysis = an informal "walk" (2001 Assertion ++) | 2005 Thm 1 + Thm 2 (θ = 1/4), full, with Probe (Lem 1) and non-closure (Lem 2) | Oberhoff-format proof of Thm 1 and of Thm 2 Part A; Assertion ++ kept as Willard's intuition, with its formal counterpart (cut-off at y−1, Lemmas 4.6, 4.8) |
| 8 | (absent) | 2005 Thm 6, (27)–(33), pp. 33–36: NS^{k,m}_D(PA+) is inconsistent; (27) is step (2), bounded by n, a Π\*₁ theorem of PA; the axiom Υ(k,m) supplies n | new section, "The Faith, Bought" |
| 9 | moral: "if you reuse your lemmas, you must be unable to count" | 2005 Thm 5 allows Π\*₁/Σ\*₁ lemmas; 2004 Thm 2 (sketch): Π\*₂/Σ\*₂ lemmas defeat Type-A at Level(0−) | the lemma dial is stated in three rungs; moral corrected |
| 10 | "on the Hilbert line results survive uncompressed encodings; density load-bearing for tableaux" | 2005 p. 40: "All our theorems would also be valid for uncompressed encoding methods" | density is a *floor* (Eq. 20; 2001 Case 11); a more compact notation is what breaks the 2005 proof. **Discrepancy with codification obligation O51**, which says the tableaux engine "genuinely fails" without compression; not edited here (codification workstream) |
| 11 | 2006a Thm 4: "multiplicative naming fails" | needs Willard's Concise Encoding property and PA's Π⁻₁ theorems | stated |
| 12 | 2002a Thm 6.4 "under a technical condition" | Conventional Deciphering Property, Def. 5.1 | stated |
| 13 | 2002a Thm 2.2 status "---" | registry: sketch | sketch |
| 14 | 2004 Thm 1 status full | full, but one step deferred in a footnote saying it "should not be considered a formal proof" (codified §7.5) | qualified |
| 15 | IS^λ(A) tangibility reflection | consistency only at λ = 3/4 (Thm 5.1) | stated, with k > 4 |
| 16 | closing inequality "needs m ≥ 3" | needs m − 1 > m^{2θ}: m ≥ 4 for every θ < 1/3, m ≥ 3 at θ = 1/4 | stated |
| 17 | abstract and §1: "they are consistent", "are right" | relative-consistency rule | qualified throughout; new "Relative consistency" paragraph |
| 18 | Willard 2002c title from memory | 2002c text p. 1 | "Some New Exceptions for the Semantic Tableaux Version of the Second Incompleteness Theorem", LNCS 2381, pp. 281–297; the weakest/strongest rule now quoted from p. 2 |

New claims that are ours in revision 2: Fact FP's proof and hypothesis (E);
Lemmas (instances, transcripts, watching, honesty); Proposition E (a)–(c);
Corollaries (IS_D(A); IS(A) disjunction); the observation that by
θ-compactification any IS_D(PA+) proof of Υ(k,m) has code ≥ (2^m_k)⁴. One
step is a transposition: Willard asserts (U) for Δ⁻₀ `SUBST` and PAX; the
essay uses it for 2005's Δ\*₀ `Subst` and PA+.

Revision 2 builds clean with pdflatex (20 pp., no warnings, no overfull boxes).

## Revision 3 (2026-10-25): the adversarial review

Reviewer: Grok 4.7 (`grok-4.7-xhigh`) through the Cursor `agent` CLI in
`--mode ask` (read-only). Its workspace held the draft-1 source, Oberhoff's
text layer, twelve Willard text layers, the codified statement, the results
registry and two notes. The prompt (numbered questions, under the user's
standard quoted above) and the report are kept in the session scratchpad
(`review-sjexm-r1/prompt.md`, `REPORT.md`, 48 kB), not in the repository.
Verdict on draft 1: does not meet the standard. 15 findings: 2 critical,
10 major, 3 minor.

Each finding was checked against the source text before it was acted on.
Several duplicated corrections already made in revision 2. Disposition:

| ID | Sev. | Finding (short) | Disposition |
| --- | --- | --- | --- |
| R1-01 | major | Prop. C's hypotheses don't give G2; "Theorem 6's argument written out" conflates internal/external | fixed in rev 2 (IΣ₁, Hilbert, canonical Prov, derivability conditions [HP93]; phrase deleted); rev 3 names the diagonal lemma explicitly |
| R1-02 | critical | Prop. E: same predicate unstated; step (2) is equivalent to G̈ under Level(1); fixed-point paragraph wrong for 2005 language; 2001 cut claim unproved | (1), (2) fixed in rev 2 (H4 and Cor. on Willard's own predicate; Prop. (a) proves the equivalence). (3) rev 2 uses PA+ (2005 p. 32), not PA. **Partly not adopted:** Grok proposes getting (U) from Group-1 as "a single true Δ\*₀ computation". (U) quantifies over all h and is bounded only if `Subst` bounds h by a U-Grounding term, which we have not established. We keep Willard's route (2001 p. 39: (U) inherited as a Π₁ theorem) and flag the transposition. The 2001 claim became Corollary (IS(A)): at least one of steps (2), (3) is unprovable, and we do not know which |
| R1-03 | major | step (1) identity, "free" wording, §6 ≠ size theorem, 7a/7b imprecise | rev 2 fixed the first three; rev 3 states 7a/7b as properties of (α, D), 7b as a schema, and says where each holds |
| R1-04 | major | merged consistency theorem | split in rev 2; rev 3 adds that Thms 3, 5 inherit Thm 1's inequality gap and its repair |
| R1-05 | major | 2001/2005 systems conflated; watching = Δ\*₀-completeness + ∃-generalization | fixed in rev 2 (separate descriptions; Lemma (watching)); 2001's Δ⁻₀-completeness comes via PAX (2001 p. 39), not Group-1 |
| R1-06 | major | case analysis ≠ Willard's proof; Group-3 instance fails at a proof, not a bound; y^{5/6} not the formal bound | rev 2 rebuilt it from 2005 Thms 1, 2; rev 3 marks y^{5/6} as our unpacking of Lemma 4.2's sketch and adds the formal majorant Eq. (26) with Case 11's six bits |
| R1-07 | minor | inequality; "below" vs "≤" | rev 2 inequality; rev 3: Normed(m−1, 1) presupposes m ≥ 3, and witnesses are "≤" |
| R1-08 | major | encoding contrast reversed | fixed in rev 2 (self-audit item 10; 2006a's Hilbert-line remark is cited too) |
| R1-09 | critical | price slogan contradicts 2005 Thm 5 and TR Δ₀ cut; Solovay row drops algebra; incremental naming is 2006a's reading | rev 2 gave the lemma dial; rev 3 adds the Δ₀-cut rung (TR p. 7; O56), the Π₁ ring laws to Solovay's rung and table row, and 2006a p. 22's own words that 2001 never used the phrase "incremental naming", with ISREF's Group-3 = Eq. (10). Moral rewritten as a list of conditions with statuses |
| R1-10 | major | Paris–Wilkie attribution; 2004 Thm 1; 2020 row; "back door" | rev 3: 2002a presented Thm 6.4 as the answer; 2007 (abstract) says it covered only the textbook scheme, and 2007 Thm 6 covers both. Xtab is now introduced as TR 93-10's own "tableaux proof with cuts" (printed p. 33). 2004 Thm 1 and the 2020 row were already fixed in rev 2 |
| R1-11 | major | Thm 7.2 hypotheses; "Löb's theorem in Willard's setting"; Thm 6.1 is Hilbert | fixed in rev 2 |
| R1-12 | major | honesty omits soundness step and hypothesis; Rosser not "verbatim"; TR cited as proof | rev 2 Lemma (honesty) with sound D and the hypothesis; rev 3 lists what Rosser's cases need (bounded search fact; Group-2 import of the Π₁\* loop sentence). The TR is quoted only as Willard's statement |
| R1-13 | major | unqualified consistency claims | rev 2 qualified the abstract, §1 and honesty; rev 3 adds the ISM footnote hypothesis, defines "self-justifying" in the table, and rewrites the moral |
| R1-14 | minor | Platonic Stability is conditional on Conj. 6.6; Q-2 wording; "2021" | rev 3 quotes Cor. 8.2's opening ("Let us assume Conjecture 6.6 is correct …", p. 29) and Q-2 verbatim (text layer); the date span was fixed in rev 2 |
| R1-15 | minor | footnote 12 misquoted | fixed in rev 2 (quoted) |

Checked by the reviewer and found correct: Oberhoff quotations; the TR pp. 6–7
quotations (text-layer artifacts "cut mle", "Ag", "a399_" noted); 2006a
Remark 3; 2001 p. 20; Lemmas 4.1, 4.6, 4.8; 2000 §5 and Lemma 8; 2002a
Thms 2.2, 3.5; 2005 Remark 4; 2006a Thms 1–4; 2001 Def. 1.2, Thm 3.5, p. 8;
2016 Ex. 3.5; 2005 §1.

Revision 3 builds clean (21 pp., no warnings, no undefined references).
Theorem-like items share one lettered counter, so Proposition E of draft 1 is
now Proposition K, and Willard's consistency theorem is Theorem H.

## Open items (revision 3)

1. A second review round of revision 3 would test whether the fixes meet
   the standard; not yet run.
2. (U) for PA+ and Δ\*₀ `Subst` is assumed by transposition from 2001 p. 39.
3. Willard states Lemma 4 of 2005 only for NS^{k,m}_D(PA+)'s proof
   predicate; the analogous bounded faith step for IS_D(PA+)'s own predicate is
   expected, not stated.
4. Codification obligation O51's wording conflicts with 2005 p. 40 (item 10
   above); raise with the codification workstream.
5. Bibliography: Pudlák 1985 (JSL 50(2), 423–441), Boolos 1984 (JPL 13(4),
   373–378), Löb 1955 (JSL 20(2), 115–118), Hájek–Pudlák 1993 (Springer,
   Perspectives in Mathematical Logic) and Aaronson 2013 (CUP) were checked by
   web search on 2026-10-25. Hilbert–Bernays 1939 is from memory.
6. How much metatheory Willard's consistency proofs use is undetermined.

## Revision 4 (2026-10-25): the second adversarial review

- **Request (user).** "Track LOG.md; push to remote on this branch, then review again."
- **Review.** Grok 4.7 (Cursor CLI, read-only), run against revision 3 with the same standard. Part A rechecked every round-1 finding in the essay itself, without trusting the disposition table; Part B asked 19 questions about the new text.
  - Workspace: `scratchpad/review-sjexm-r2/` (prompt, REPORT.md, 34 kB, about 34 minutes).
  - Verdict: revision 3 does not meet the standard. 16 findings: 1 critical, 8 major, 7 minor.
  - Part A: R1-04, 05, 06, 07, 10, 11, 14 and 15 resolved; R1-01, 02, 03, 08, 09, 12 and 13 partly resolved. Each partial one is reopened as an R2 finding.
  - The declined round-1 suggestion (deriving (U) from Δ\*₀-completeness) was judged correctly declined.
- Every finding was checked against the source texts before it was acted on. Two of the reviewer's source claims were wrong:
  - Willard writes the U-Grounded Binary Representation with an overline, not an underline (page image, 2005 p. 4). The essay now uses Willard's overline.
  - 2001 Appendix B does encode tableau trees, on p. 52.

| ID | Sev. | Finding | Disposition |
|---|---|---|---|
| R2-01 | critical | Prop K(a) fed `Pair` the code of the prenex existential. Willard's `Pair` pairs ⌜f⌝ with ⌜¬f⌝, the syntactic negation (2005 Eq. 28, p. 34), and the two codes differ (Remark 5). | Fixed. ¬G̈ (syntactic) and **H** (the halting sentence) are now distinct throughout (§2). Steps (2) and (3) are aimed at c′ = ⌜¬G̈⌝, and Definition `def:know` covers both written forms. A remark after Prop K says it does not cover the transcript aimed at **H**: that needs F ⊢ Prov(⌜H⌝) ⊃ Prov(c′), which is open for Willard's systems. |
| R2-02 | major | Lemma `lem:instances`(a) used a bounded sentence that watching does not supply; Prop K(a) cited (H2) for `Pair`. | Fixed. (a) now goes through Prov(⌜σ⌝), a true Σ₁ sentence, and Fact closure, and covers syntactic negations of Π₁ sentences. Prop K(a) cites (H3) for `Pair`. |
| R2-03 | major | "G̈ makes step (2) true": the closure citation was wrong. Cor M contradicted Thm 4.3 without the completeness step. | Fixed. Both implications are stated as conclusions χ of Fact closure, from the theorems (3) and (FP). (H1) is now Level(0−), and Prop K(b) derives 0=1 by closure. Cor M then uses (H1) = Thm 4.3 directly. |
| R2-04 | major | Cor L: Group-2 detachment unwritten; (U) used as a fact; correctness of SubstPrf unstated; (H4)'s reason wrong. | Fixed. Detachment is written out. (U) is the explicit assumption (U\*), and Cor L(a), G̈'s unprovability, does not use it. Correctness of SubstPrf, including Eq. 7's bound h ≤ p, is taken from Willard and needed only in the metatheory. (H4) is Eq. 8 under the definition of Prf, with Willard's `Pair`. |
| R2-05 | major | Abstract and §1 merged the two systems' hypotheses. §12 attached the 7b failure to IS(A). §13 was unqualified. The receipt joined unlike results. | Fixed. The abstract and §1 state the 2005 and 2001 theorems separately. §12 splits IS_D(A) (Cor L, under (U\*)), IS(A) (Cor M, a disjunction) and the open **H** case. §13 is qualified. The receipt is now the four boundaries. |
| R2-06 | major | "7a is enough for every form of G1" ignores Rosser's Π₁ loop sentence. | Fixed in §12 and §11. |
| R2-07 | major | "Mere counting" / "counting is too much" dropped Solovay's ring laws. | Fixed in §9, §12 and the moral: successor plus the Π₁ ring laws for + and × as relations. |
| R2-08 | major | (E) argument: wrong comparison string; lists unargued; Eq. 7's bound conflated with (E). | Fixed. First-byte bound 32·64^{N−1}; root is prenex(¬Φ) with an explicit assumption that it is at most one byte shorter than Φ; the concatenation suffix argument for lists; 2001 App B pp. 51–52; Eq. 7's h ≤ p stated separately. |
| R2-09 | major | §11 "Theorem 6, written out". | Fixed: the metatheory shows a contradiction exists (Prop C), so the search halts. |
| R2-10 | minor | Numerals in the 2005 language; `∀y < c̄` is not Willard's bounded-quantifier form. | Fixed. The bar is Willard's term notation in 2005 (said once in §2). The bounded sentence is `∀y ≤ c̄ ¬Prf(c̄,y)`, true by (E), with the split y ≤ c̄ ∨ c̄ ≤ y. Fact FP is split into (FP⇒), which needs no (U), and (FP). |
| R2-11 | minor | Lemma `lem:transcript`: U-Height ≥ U-Depth unstated; 2^L misattributed. | Fixed, with the U-Length/U-Depth/U-Height definitions (p. 22) and Willard's "clearly satisfy" wording for Eq. 20. |
| R2-12 | minor | "Fewer than four bits" should be "four or fewer"; 2006a's remark is analogous, not the same. | Fixed (footnote 5, p. 27; 2006a App A p. 37 quoted). |
| R2-13 | minor | θ-compactification observation: a must exceed 1; "below" should be "at most"; Group-3's truth unstated. | Fixed: a = p > 1, witness ≤ p^{1/4}, Theorems 3/5, plus the prefix argument for lists. |
| R2-14 | minor | Rosser credited PA, not PA+; "and D is sound" was not why Group-3 is true. | Fixed. |
| R2-15 | minor | 2001 p. 39 quotation spliced; 2016 page convention mixed. | Fixed. The two clauses are quoted separately. Provenance now states one convention, printed page numbers: 2016 Conj 6.6 p. 23, Cor 8.2 p. 28; 2020 Xtab p. 10, Thm 4.5 p. 12; 2004 Thm 1 p. 342, Thm 2 p. 344. |
| R2-16 | minor | Prop C: the IΣ₁ equivalence of the two levels was unargued. | Fixed (explosion and modus ponens for the canonical predicate). |

Our own changes in this revision:
- **Willard's Theorem 6, fourth step.** Definition 6, as printed (page image, p. 33), has q prove (25), "∃p Prf(⌜t⌝,p)". Step (31) needs q to prove ¬℧(k−1,m). These differ in code and in content: (25) lacks the substitution and the Log bound. The essay now says this in §8 and gives the repair: read Definition 6 with q a proof of ¬℧. Lemma 3's sketch is expected to cover that reading; this is our observation.
- **Group-3 notation.** Group-3's formula is written Γ₃ with code n₃, apart from the Gödel sentence's Γ and n.

Revision 4 builds clean: 25 pp., no warnings, no undefined references.

## Open items (revision 4)

1. (U\*), the uniqueness of `Subst` for PA+ and Δ\*₀, remains a transposition from 2001 p. 39. It is now an explicit hypothesis of Cor L(b) only.
2. Whether IS_D(A) proves Prov(⌜H⌝) ⊃ Prov(⌜¬G̈⌝), and so whether it fails 7b at the Σ₁ sentence **H** itself, is open.
3. The (E) argument assumes that Willard's prenex conversion never shortens ¬Φ by more than one byte relative to Φ, and, for 2005, that a sentence's code is its byte string's value.
4. Correctness of SubstPrf in the standard model, including Eq. 7's h ≤ p, is taken from Willard.
5. Willard's Theorem 6, step (31), needs the Definition 6 reading above. Whether Lemma 3's sketch covers it is our expectation, not Willard's statement.
6. Carried over: Lemma 4 is stated only for NS's predicate (item 3 of the revision-3 list); O51 versus 2005 p. 40 (item 4); Hilbert–Bernays 1939 is still cited from memory; the metatheory Willard's proofs use is undetermined.
7. A third review round has not been run on revision 4.

## Revision 5 (2026-10-25): the third adversarial review

- **Request (user).** "What is the 'faith step'? Run a third review." The answer was given in the conversation; the review is recorded here.
- **Review.** Grok 4.7 (Cursor CLI, read-only), run against revision 4, with both earlier reports and the revision-4 disposition table, about 33 minutes.
  - Workspace: `scratchpad/review-sjexm-r3/` (REPORT.md, 28 kB).
  - Verdict: does not yet meet the standard. 10 findings: 0 critical, 8 major, 2 minor.
  - Part A: R2-02, 03, 06, 07, 09, 10, 12, 13, 14, 15 and 16 resolved; R2-01, 04, 05, 08 and 11 partly resolved, each reopened as an R3 item.
  - Question 6, whether IS_D(A) proves Prov(⌜H⌝) ⊃ Prov(⌜¬G̈⌝): open, but narrowed. The roots of the two tableau proofs differ by one bounded duality and possibly a double negation, and Willard's p. 6 does not fix the Prenex\* conversion.
- One reviewer claim was rejected. R3-07 said §1's single hypothesis, "a bounded proof predicate and true Π₁-class theorems", is not 2001's regular consistency. It is: 2001 p. 12 defines regular consistency as a Δ⁻₀ proof predicate plus the Group-2 axioms valid for every Π⁻₁ sentence. §1 now names it.

| ID | Sev. | Finding | Disposition |
|---|---|---|---|
| R3-01 | major | Fact FP's hypotheses omitted soundness and completeness, a bounded Prf, and the truth of Subst(n̄,c̄); the closing sentence was not Lemma 7.1B. | Fixed. Hypotheses added. The proof now names the theorems (trichotomy, ∀y ≤ c̄ ¬Prf(c̄,y), Subst(n̄,c̄), and (U)), shows their conjunction implies the target in every structure, and applies Lemma 7.1B. |
| R3-02 | major | The general (E) argument fails for non-prenex sentences (stacked double negations). | Fixed. (E) is now stated for proofs of G̈ only, which is all FP uses, under two explicit assumptions: Prenex\* dualizes the prefix and negates the matrix, shortening by at most one byte; and 2005 numbers sentences by byte value, as 2001 states. The corollaries list (E) as an assumption. |
| R3-03 | major | Cor L: "(U\*) is the one open hypothesis" is false. | Fixed. Cor L lists its assumptions: (E) under the encoding assumptions, correctness of Subst, and Willard's correctness of SubstPrf including h ≤ p. (U\*) is additional for (b) only. |
| R3-04 | major | Cor M's (H2) stopped at Δ⁻₀-completeness. | Fixed in Cor M and in §6's IS(A) remark: the witness instance, then Fact closure. |
| R3-05 | major | The §6 Theorem 1 retelling fed App∃ a syntactic negation; "Group-3 says exactly" identified two codes. | Fixed, and generalized. §2 now sets out Willard's two readings of `Pair`: (i) the Σ₁\* sentence representing the negation (p. 34 "represents"; Theorem 1, p. 20; Remark 5), and (ii) the string ¬Φ (Eq. 28; Level(n), p. 5). **N** is the sentence `Pair` pairs with G̈, and c′ = ⌜N⌝. Prop K, Lemma `lem:instances`, the corollaries and §12 are stated for **N**. The retelling uses Ψ, the Σ₁\* negation. A new remark says Theorems 3/5 give the truth of Group-3 under (i), and under (ii) only with a Prenex\* convention that Willard does not fix. Theorem H(a) and Lemma `lem:honest` are qualified accordingly. Under (i), the 7b failure is at a Σ₁-class sentence. |
| R3-06 | major | Theorem 6: "(25) is the analogue of H" is wrong; the claim that Lemma 3's sketch covers the repair is unsupported. | Fixed. (27) is now the bounded literal transcript, and (25) is not equivalent to ¬℧, since ℧ speaks only of short proofs. Step (31) is a gap, with two possible repairs. The first repair is labelled a conjecture with no length estimate proved. Theorem 6 is listed "full; gap at step (31)" in its header, the table and provenance. §12's sentence is withdrawn. |
| R3-07 | major | The abstract and §13 overstate honesty and G1 for IS(A) and drop hypotheses; "Peano Arithmetic" should be PA+; the §12 boundaries lack hypotheses. | Fixed. The abstract confines honesty and G1 to IS_D(A) with its hypotheses, says PA+, says "under stated conditions", lists the corollary's assumptions, and mentions the Theorem 6 gap. §13 cites Cor L(a) for ⊬G̈, honesty for ⊬¬G̈, and §6 for Rosser; for IS(A) only TR 93-10's statement. §12: the one-block boundary has the truth hypothesis; the two-block boundary has V_A, V_B and finiteness. The §1 objection was rejected (above). |
| R3-08 | major | Prop K(b)'s list of what it uses was incomplete. | Fixed: (H1), (H2), (FP⇒)'s hypotheses, correctness of Prf, soundness and completeness of D; not (U) or (H4). |
| R3-09 | minor | Lemma `lem:transcript`: the U-Depth definition dropped the node's own U-Length; the leaf case was missing. | Fixed. |
| R3-10 | minor | Remark 5 covers only Σ₁\*; the remark needed ¬G̈ ↔ H besides (FP); 7b's syntactic clause is ours, not Oberhoff's. | Fixed. p. 5 and footnote 3 (p. 26) are cited, the remark cites both, and §12 calls 7b "our formulation". |

Revision 5 builds clean: 27 pp., no warnings, no undefined references.

## Open items (revision 5)

1. (U\*) remains a transposition from 2001 p. 39; it is an assumption of Cor L(b) only.
2. Encoding assumptions for (E): the Prenex\* rewriting of a prenex sentence's negation shortens it by at most one byte, and 2005 numbers sentences by byte value.
3. Correctness of SubstPrf (h ≤ p in Eq. 7) is Willard's assertion.
4. The reading of `Pair`. Under (ii), Willard's Theorems 3/5 give the truth of Group-3 only with an unfixed Prenex\* convention. Lemma `lem:honest`, and so ⊬¬G̈ and the honesty claims, are stated under (i) or under that convention.
5. Prov(⌜H⌝) ⊃ Prov(⌜N⌝) for H ≠ N is open (narrowed as above).
6. Willard's Theorem 6, step (31), is a gap. Repair (a), with a new length estimate, is conjectured.
7. The Herbrand-style argument from the conversation is not yet in the essay: Willard's systems cannot prove the uniform formalized Σ₁-completeness, or even totality of the numeral-code function, because provably total functions of a true Π₁\*-axiomatized U-Grounding theory are linearly bounded. It awaits the user's decision.
8. Carried over: Lemma 4 is stated only for NS's predicate; O51 versus 2005 p. 40; Hilbert–Bernays 1939 is cited from memory; the metatheory Willard's proofs use is undetermined.
9. A fourth review round has not been run on revision 5.

## Revision 6 (2026-10-25): closing the open items

- **Request (user).** "Yes, add the Herbrand argument. That list of open points reads like the entire point of the essay is not yet accomplished — get it done."
- **Approach.** Most open items came from conventions Willard leaves unspecified. The essay now fixes those conventions itself, as part of fixing the configuration, and proves what follows.

| Revision-5 open item | How it is closed |
|---|---|
| (U\*), the transposed uniqueness of `Subst` | Eliminated. `Subst` is written in least-witness form, Subst₀(g,h) ∧ ∀h′ ≤ h [h′ = h ∨ ¬Subst₀(g,h′)]. (U) then follows by pure logic from trichotomy at c̄ and the true bounded sentence Subst(n̄,c̄) (Fact FP). |
| (E) and its two encoding assumptions | Eliminated. G̈ drops Willard's bound h ≤ x; his own ℧ (Eq. 24) has none. So (FP⇒) is one instantiation and needs no encoding hypothesis. |
| Correctness of SubstPrf (Eq. 7's h ≤ p) | Proved in Lemma `lem:honest`(a). It rests on the fixed encoding (sentences numbered by byte value, as 2001 p. 51 states; Appendix A for proofs), on axioms entering tableaux as nodes (p. 6), and on Willard's arithmetization (2001 Thms C.1–C.3, sketch / full given them), cited as Oberhoff's Theorem 7 is. |
| The two readings of `Pair` | Closed by fixing the Prenex\* rewriting: pnx(¬Θ) := dual(pnx(Θ)). Then pnx(¬¬Φ) = pnx(¬dual(Φ)), so ¬Φ and dual(Φ) have the same tableau proofs (p. 6: whether a tree proves Φ depends on Φ only through the root). `Pair` accepts every θ whose tableau proofs are those of ¬Φ. Willard's Theorem 1 covers Group-3 as written, and his p. 7 says results hold even without the Prenex\* requirement. |
| Prov(⌜H⌝) ⊃ Prov(⌜N⌝) | Dissolved. The halting sentence is now **H** := dual(G̈), a Σ₁ sentence that `Pair` pairs with G̈. Step (2) is aimed at **H** directly, which is Oberhoff's literal step. A formalization with different tableau proofs is a different sentence and outside the claims. |
| Theorem 6, step (31) | Repaired. q proves dual(℧) and is assembled from (i) Lemma 3's object, a proof of Prf(⌜℧⌝, p̄); (ii) a proof of Log^{k−1}(p̄) < m̄; (iii) Subst(n̄, ⌜℧⌝); (iv) a closing tableau whose code has logarithm O(Log p). The repaired theorem rests on Lemma 3's sketch, applied to one more polynomial-time bounded fact. |
| Lemma 4 for IS_D(PA+)'s own predicate | Stated, with status sketch: Lemma 3's sketch is generic in the axiom system. |
| Herbrand argument | Added as Proposition `prop:herbrand` (Linear witnesses), with a model-theoretic proof via the linear closure of an element in a model of true arithmetic. Corollary `cor:uniform`: under Willard's consistency hypotheses, neither IS_D(A) nor IS(A) proves ∀x∃y y = x·x, nor ∀x∃y Num(x,y) ("every number has a written name"), so neither knows uniformly that it watches even x = x. The d-versus-s point is in the text. |
| Hilbert–Bernays 1939 | Verified: vol. II, Grundlehren 50, Springer, Berlin, 1939, xii+498 pp. |
| Metatheory strength | Reclassified as outside the essay's scope, and stated as such. |

Also changed:
- Lemma `lem:honest` now covers IS(A) as well.
- The first incompleteness theorem is now proved for IS(A) in the essay (Cor M plus honesty), not only cited from TR 93-10.
- Cor M also gives IS(A) ⊬ G̈.
- §12 states 7b in closed and uniform forms. The uniform form fails for both systems; the closed form fails at **H** for IS_D(A); for IS(A) the closed form is a disjunction.
- The abstract was rewritten to match.

Revision 6 builds clean: 28 pp., no warnings, no undefined references.

## Status after revision 6

- **Cited, with Willard's statuses (not open):**
  - Theorems 1–5 of 2005 (full);
  - Lemma 3 (sketch) and so Theorem 6 as repaired;
  - the arithmetization (2001 C.1–C.2 sketch, C.3 full given them);
  - Eq. 20 ("clearly satisfy");
  - 2004 Theorem 2 (sketch); Solovay (cited); 2016 Conjecture 6.6 (stated-only, quoted as such);
  - 2002c, quoted from the codification and not in the review workspace.
- **Conventions fixed by the essay (not open):** least-witness `Subst`; G̈ without h ≤ x; the Prenex\* rewriting; `Pair`; sentence numbering by byte value.
- **Genuinely open, and stated as such in the text:**
  - Cor M: which of step (2) and the Level(0−) step (3) fails for IS(A). Its uniform form fails outright.
  - Formalizations of the halting statement with tableau proofs different from **H**'s: outside the claims.
- **Outside scope:**
  - metatheory strength;
  - codification obligation O51's wording versus 2005 p. 40, which belongs to the codification workstream.
- A fourth review round is to be run on revision 6.

## Revision 7 (2026-10-25): the fourth adversarial review

- **Review.** Grok 4.7, read-only, against revision 6 with all three earlier reports, about 30 minutes. 8 findings: 5 major, 3 minor.
- **What it confirmed as proofs at the standard:** the least-witness `Subst` and the unbounded fixed point; Fact FP; the Prenex\* convention and `Pair` (including that Probe and Lemma 2 are insensitive to a ¬¬ in the matrix, and that dual(Φ) has the class App∃ needs); the halting sentence **H**; correctness of SubstPrf from the node argument; Prop K; Cor L; the second clause of Cor M.
- Part A: R3-01, 03, 04, 05, 08 and 09 resolved; R3-02 moot; R3-06, 07 and 10 partly resolved (taken up as R4-02, R4-04 and R4-07).

| ID | Sev. | Finding | Disposition |
|---|---|---|---|
| R4-01 | major | Prop G's 2001 half used + as a function symbol; Group-0 atoms are not Π₁⁻; the order facts of the 2005 half were unstated. | Fixed with full proofs. The proposition now covers sentences that are "bounded or Π₁-class". The 2005 half spells out the compactness type and the order facts (transitivity; n̄ ≤ n̄·u; closure under +, Double and the non-growth functions), each a true sentence of Th(ℕ). The 2001 half expands M by the bounded-definable addition graph z ∸ x = y ∧ x ≤ z and runs the same argument; the addition axiom holds in I. The k ≈ 2^d remark is labelled as intuition. |
| R4-02 | major | The Theorem 6 repair does not discharge the length bounds for parts (ii)–(iv); "fixed size", "at once" and "as printed" overstate; the root-code identity is needed. | Fixed in two ways. (28) is replaced by Pair(⌜℧⌝, ⌜dual ℧⌝), so (31) follows from the new (30) by pure logic and no root identity is needed. Part (iv) now instantiates vacuous quantifiers. ⌜℧⌝ < p is argued from byte length and the encoding. The remaining length claims for (ii) and (iii) are stated as claims of Lemma 3's kind that its sketch does not address. **Theorem 6 is now counted a sketch at this granularity** (full as printed in the registry), in its header, the table, §12 and the abstract. |
| R4-03 | major | Correctness of IS(A)'s proof predicate cited Eq. 7's argument rather than 2001 Eq. 87; Cor M's (H3) omitted ⊢ Subst(n̄,c̄). | Fixed. Lemma `lem:honest`(b) argues correctness for Θ(N̄) (Eqs. 86–87). It cites Thm C.1 item 25 and C.2 (sketch), Willard's design bound h ≤ s for the node test (App. C, item 24 and p. 59), and the node argument via 2001's curly-bracket node encoding (p. 52). Cor M's (H3) names Subst(n̄,c̄). |
| R4-04 | major | The abstract said "Peano Arithmetic" and an unrestricted Level(1) system. | Fixed: PA+ and PAX are named, and the closed faith-step claim carries Prop K's conditions. |
| R4-05 | minor | Byte order unstated, so 32·64^{N−1} could fail. | Fixed: the encoding fixes the first byte as the highest place. |
| R4-06 | minor | IS(A)'s loop sentence was a U-Grounding sentence; the figure labelled the witness ¬Φ. | Fixed: a Π₁⁻ loop sentence proved by PAX; the figure now says dual(Φ). |
| R4-07 | minor | 7b was presented as Oberhoff's. | Fixed: 7a is what the essay takes from Oberhoff's Theorem 7; 7b in both forms is the essay's formulation. |
| R4-08 | major | The note listed three unproved closures as closed. | R4-01 and R4-03 are now written as proofs. R4-02 is reclassified: Theorem 6 is a sketch at this granularity. |

Revision 7 builds clean: 29 pp.

## Status after revision 7

- **Proved in the essay**, from cited results with their statuses and the fixed conventions:
  - Fact FP (no (U\*), no (E));
  - Lemmas `lem:instances`, `lem:transcript`, `lem:watch`, `lem:honest`(a, b);
  - Prop G (Linear witnesses) and Cor `cor:uniform`, for both systems;
  - Prop K;
  - Cor L;
  - Cor M, with G̈ unprovable plus the disjunction;
  - the first incompleteness theorem for both systems.
- **Cited:**
  - Willard 2005 Theorems 1–5 (full);
  - 2001 Theorem 4.3 (full);
  - the arithmetization (2001 C.1–C.2 sketch, C.3 full given them);
  - Eq. 20;
  - 2004 Theorem 2 (sketch); Solovay (cited); 2016 Conjecture 6.6 (stated-only); 2002c via the codification.
- **Willard's Theorem 6:** a sketch at this granularity (step (31) repair sketched; parts (ii), (iii) length claims).
- **Genuinely open:** Cor M's disjunction, for the closed instance in IS(A) only.
- **Outside scope:** metatheory strength; O51 (codification workstream).

## Revision 8 (2026-10-25): the fifth adversarial review

- **Review.** Grok 4.7 against revision 7, about 30 minutes. 5 findings: 2 major, 3 minor.
  - Confirmed: Theorem 6's sketch status and its consistent reporting; the repair's tableau and code bound; the uniform code bounds and axiom classes; Cor L; the 2001 expansion argument.
  - Part A: R4-02, 05, 06 and 07 resolved; R4-01, 03, 04 and 08 partly resolved.

| ID | Sev. | Finding | Disposition |
|---|---|---|---|
| R5-01 | major | `lem:honest`(b) cited item 25 (ExSemPrf, with its PseudoTransform₂ template step) and the h ≤ s design bound, which are not IS(A)'s proof predicate. | Fixed. Correctness is argued for SubstSemPrf itself (2001 item (iii), Eqs. 86–87, p. 52), with its Δ⁻₀ definition from Thm C.3 (via routine analogs of C.1, and C.2, sketch), written in Eq. (7) form. The proof argues h_N ≤ y from the node fact (2001 p. 16) and the curly-bracket node encoding (p. 52), and gives the converse from Subst's uniqueness in ℕ. Item 25 and the h ≤ s sentence are no longer cited. |
| R5-02 | minor | Prop G: a⃗ ∈ I and the coefficient comparison were unstated; Eq. 6 is not the functional graph. | Fixed: x_i ≤ x₁+⋯+x_m+1 and ī·u ≤ j̄·u (i ≤ j) are stated and used for tuples and for binary non-growth functions. The 2001 case says Eq. 6 asserts solvability of z ∸ x = y and the expansion uses its functional sharpening. |
| R5-03 | minor | The abstract's faith-step sentence omitted watching and consistency. | Fixed: it now names consistency, watching, the fixed point and Level(1) self-consistency, and IS_D(A) "under the hypotheses of his consistency theorem". |
| R5-04 | major | The status list called the IS(A) claims proved while R5-01 stood. | With R5-01 written, the IS(A) claims (honesty, the IS(A) half of Cor `cor:uniform`, G1 for IS(A)) rest on a proof again. Statuses are unchanged otherwise. |
| R5-05 | minor | The step list still called (27) a PA theorem for the redefined CheckProof. | Fixed: (27) is from PA+ for Willard's CheckProof, and is the sketched length claim for the redefined one. |

Revision 8 builds clean: 29 pp. A focused sixth check of these fixes follows.

## Revision 9 (2026-10-25): the focused sixth check

- **Check.** Grok 4.7, read-only, verified only the round-5 fixes in revision 8.
  - R5-01 resolved: SubstSemPrf with a real node bound and converse; quotations accurate.
  - R5-02 resolved.
  - R5-05 resolved.
  - No new issue.
  - R5-03 not resolved: the abstract put IS_D(A) under Prop K's conditions on Willard's consistency hypothesis alone. Trichotomy comes from PA+ (Cor L), and the general list omitted the true bounded sentences (which give Pair(c̄,c̄′)) and soundness and completeness of D.
- **Revision 9.** The abstract's sentence now lists all of Prop K's conditions and says IS_D(A) meets them when its base theory has true Π₁-class theorems and proves those of PA+. 29 pp., clean.

## Final status (revision 9)

- **Proved in the essay**, from cited results with their statuses and the conventions fixed in §2:
  - the fixed point (Fact B);
  - watching, honesty and proof-predicate correctness for both systems;
  - Lemma `lem:transcript`;
  - Prop G (Linear witnesses) and Cor `cor:uniform` for both systems;
  - Prop K;
  - Cor L;
  - Cor M (G̈ unprovable, plus the disjunction);
  - the first incompleteness theorem for both systems.
- **Willard's Theorem 6:** a sketch at this granularity; the step (31) repair rests on Lemma 3's sketch plus length claims for two bounded facts.
- **Genuinely open:** Cor M's disjunction (the closed faith step for IS(A)).
- **Outside scope:** metatheory strength; O51 (codification workstream).
- **Reviews:** six rounds. The last full round (5) and the focused check (6) leave no unresolved finding after this revision.
