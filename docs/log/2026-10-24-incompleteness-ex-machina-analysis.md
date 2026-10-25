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

## Open items (revision 2)

1. Grok review findings: to be verified and integrated (separate commit).
2. (U) for PA+ and Δ\*₀ `Subst` is assumed by transposition from 2001 p. 39.
3. Willard states Lemma 4 of 2005 only for NS^{k,m}_D(PA+)'s proof
   predicate; the analogous bounded faith step for IS_D(PA+)'s own predicate is
   expected, not stated.
4. Codification obligation O51's wording conflicts with 2005 p. 40 (item 10
   above); raise with the codification workstream.
5. Non-Willard bibliographic data (Hilbert–Bernays, Löb, Pudlák, Boolos,
   Aaronson, Hájek–Pudlák) is from memory and unverified.
6. How much metatheory Willard's consistency proofs use is undetermined.
