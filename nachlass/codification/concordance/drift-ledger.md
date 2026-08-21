# Drift Ledger

Cross-paper discrepancies, definitional drift, and bibliographic identity
questions — **retained, never silently resolved**. Each entry states the
conflicting readings with anchors; where the codified statement must choose a
presentation, it cites the entry and states both readings. Populated chiefly
during extraction (C4+) and the C6 subsumption audit; seeded now with what
the C1/C2 passes established.

Format per entry: id, topic, the readings with sources/anchors, status
(`open` | `explained:YYYY-MM-DD` — an explanation is recorded but the variant
readings remain on record).

---

## D1 — TR 93-10 citation identity ("March 1994, 50 pages")

- Reading 1: the scanned artifact titles itself "Technical Report 93-10"
  (title page) and its preface dates the published KGC abbreviation to
  25 Aug 1993 (`papers/1993technicalreport/`, collated witness pp. 1-2).
- Reading 2: JSL 2001 ref. [41] cites "Self-Verifying Axiom Systems and the
  Incompleteness Theorem, SUNY-Albany Technical Report, March 1994. (This
  50-page technical report expands the abbreviated proofs in the 12-page
  Extended Abstract [40].)"
- Working identification: same artifact under a loose date/page description
  (55 printed pages + 4 figure pages vs "50 pages"; series number 93-10 vs
  issue date March 1994). To be confirmed by content-level comparison when
  Willard1993-TR (C5) and Willard2001 (C7) are extracted: every [41] usage
  in JSL 2001 must resolve to TR 93-10 content.
- Status: open (gaps ledger G9).

---

## D2 — Willard2006a cited under a pre-publication title

- Reading 1: `Willard2005` reference [72] (p. 43) cites "A new variant of
  Hilbert styled generalization of the second incompleteness theorem and some
  exceptions to it", Annals of Pure and Applied Logic (2006), noting that a
  200-word abstract appeared at the 2nd St. Petersburg Conference on Logic and
  Computability (2003).
- Reading 2: the published article (corpus key `Willard2006a`) is "A
  generalization of the Second Incompleteness Theorem and some exceptions to
  it", APAL 141 (2006), pp. 472–496.
- Working identification: same article; the 2005 citation uses a
  pre-publication title that names the Hilbert-styled framing explicitly.
  Confirm during the C9 extraction of `Willard2006a`; note that the dropped
  words ("new variant of Hilbert styled") are exactly the ones that locate the
  paper on the apparatus axis, so the pre-publication title is the more
  informative one for the concordance.
- Status: open.

---

## D3 — Three distinct constants around the compactification exponent

Within `Willard2005` the following are separate quantities and are easy to
conflate (see the C4 fidelity check, F1/F2):

- **6** — bits per language symbol in the Gödel encoding (Appendix A, p. 39: a
  "byte" is six bits; base-64; 24 symbols coded 32–55);
- **5** — the constant in Eq. (20), `U-Height(p) < (1/5)·Log₂(p)`, bounding
  accumulated U-Grounding *function* symbols along a branch, not all symbols;
- **1/4** — the compactification exponent θ actually proven (Theorems 2, 4).

The admissible window is `1/d ≤ θ < 1/3` for coding density `d`, non-empty
exactly when `d > 3`. Willard's Eq. (20) gives `d = 5`, and he selects
θ = 1/4 inside `[1/5, 1/3)`.

- Cross-paper link to confirm at C6: Willard 2011 Definition D.1 part (iv)
  promotes the Eq. (20) condition to a definitional clause admitting "any
  natural method" satisfying ≥ 5J bits for J function symbols (Gödel number
  ≥ 32^J). If confirmed, 2005 Eq. (20) and 2011 D.1(iv) are the same condition
  under two presentations, and the codified statement should present them as
  such.
- Status: open (C6 confirmation pending).

---

## D4 — The Solovay private-communication thread

Not a discrepancy but a provenance chain that must survive codification.

- `Willard2005` reference [52] (p. 42): "Several private telephone
  communications during April of 1994 describing Solovay's generalization of
  one of Pudlák's theorems [41], using the additional formalisms of Nelson and
  Wilkie-Paris… Solovay never published this result (which we call Theorem ∗
  in Section 4) or any of his other observations about 'Definable Cuts'."
  Echoed in the Acknowledgment (p. 38).
- `Willard2020-LFCS` reference 41 cites the same April-1994 telephone
  conversations.
- The only expositions of Theorem ∗ are Willard's own: a 4-page interpretation
  in `Willard2001` Appendix A, and §4 here.
- Consequence: Theorem ∗ enters the codified statement as a **cited,
  unpublished** result whose sole exposition is Willard's, and the result
  matrix must mark it accordingly.
- Status: explained:2026-08-21 (chain recorded; expositions to be compared at
  C7).

---

## D5 — `IS(A)` versus `IS_D(A)`

- `Willard2001` defines `IS(A)`: Level(0-) self-consistency, semantic tableaux.
- `Willard2005` defines `IS_D(A)`: Level(1) self-consistency, apparatus
  parameter `D`, Tab−U\*₁−List as the headline instance; §7 (p. 37) states the
  two advances explicitly.
- The names differ by one subscript while the group architecture is shared and
  Group-1/Appendices are delegated back to `Willard2001`. The codified
  statement must keep them typographically distinct and state the delta once.
- Confirm the exact `IS(A)` definition at C7.
- Status: open.

---

## D6 — Density/θ relation misstated in the affine-tree re-derivation

Recorded against prior in-house work, not against Willard. Full analysis: the
C4 fidelity check (`../extraction/willard2005-fidelity-check.md`).

- Reading 1 (`docs/adr/ADR-0001-affine-tree-sjas.md`, Decisive Finding): "The
  ratio yields the compactification exponent θ ≤ 1/5 < 1/3 that Theorem 1
  needs." Reading 1' (`docs/design/affine-tree-sjas.md` §3): "θ = 1/c < 1/3
  follows" with `c ≥ 4`; and "their ratio is θ ≤ 1/c < 1/3".
- Reading 2 (primary source): coding density bounds θ from **below**
  (`θ ≥ 1/d` via Lemma 1's hypothesis, footnote 5 p. 27), while Definition 5
  bounds it from above (`θ < 1/3`). Willard proves θ = 1/4 with `d = 5`.
- Assessment: the design conclusions survive — density 5 yields the window
  `[1/5, 1/3)`, the claimed (1/5)-compactification is the tightest admissible
  choice in that window, and the design's own remark that "density 3 is the
  θ = 1/3 boundary itself" is *correct* under the corrected reading (it is the
  `d > 3` condition). What is wrong is the stated derivation: a window is
  collapsed to a point and one inequality is inverted. The design document is
  internally inconsistent, stating the relation correctly in §8 ("lands at 1/4
  from a coding of ≥ 4–5 bits/symbol") and incorrectly in §3–§4.
- Also recorded there: the affine-tree §10 treatment of cut-free sequents as an
  established affirmative case drops the proof-status caveat that the 2005
  witness (Remark 1) is stated-only. The apparatus survey states it correctly.
- Additional obligation for the codified statement: Theorem 1's final step
  `m − 1 > m^{2θ}` is glossed as immediate by Willard *and* by the
  re-derivation, but needs `m ≥ 3` when θ = 1/4. Harmless (m is a proof's
  Gödel number) but it should be discharged explicitly rather than inherited.
- The affine-tree documents belong to another branch and are **not edited** by
  this workstream.
- Status: open (carry into C14–C15 composition).
