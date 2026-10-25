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

## Open items

1. Verify Prop. E's fixed-point hypothesis against Willard's Group-3
   construction (2005 §3 and the 2001 Appendices B–C encoding).
2. The "transcripts" heuristic (§3 of the essay) could become a lemma via a
   Herbrand-style bound on tableau terms over U-Grounding symbols.
3. A Hilbert-line remark was considered and left out as unverified: gluing two
   Hilbert proofs costs roughly a product of their codes, so D2 should also fail
   internally for Type-NS systems.
4. Optional external read-only review of the essay (the Cursor/Grok recipe in
   memory).
