# Extraction: Willard2006-WoLLIC

> Tier B extraction (component C10), recorded as a **variant witness** against
> [`willard2009.md`](willard2009.md).

| Field | Value |
| --- | --- |
| Corpus key | `Willard2006-WoLLIC` |
| Venue | WoLLIC 2006 |
| Witness | `nachlass/papers/willard2006_i_sigma0_herbrand_author_wollic.pdf` (15 pp.) |
| Relation | The conference form of `Willard2009` |

## Item correspondence

Definitions 1–6, Theorems 1–4 and Lemmas 1–4 appear in both at the same
numbering, with `Ax-1`/`Ax-2`/`Ax-3`, Split Representations, Trivial-R,
Threshold/Anti-Threshold, `Δ^R₀`/`Π^R₁`, `MinG`, `B`-boundedness, the
Conventional Encoding Method and the diagonal sentence all already in place.
Theorem 1's proof sketch — Eq. (15)'s `ψ*` split-representation counterpart,
`Size_L`, and the `∃`/`∀` transformations — is the same argument in both,
renumbered (15) → (20) and gaining (21)/(22) labels in the journal.

**The numbering diverges at Definition 7.** WoLLIC's **Definition 7** is
`B`-Bounded Good Skolemization, which the journal renumbers to **Definition 8**
because the journal inserts a new Definition 7 (`Herb−k`) in its §6. A
correspondence table built on "same numbering throughout" would mis-key here;
this record's earlier claim that it did has been corrected.

## What the journal version adds

- **Remarks 1–4**, including Remark 3's note that Appendix B was added at the
  **anonymous referee's** suggestion.
- **Definition 8** and **Appendix A**, the full proof of Theorem 2.
- **Appendix B** in its entirety: `Ax-4`, **Lemmas 5–7** and **Theorem 5** —
  the result that `Ax-3` *does* satisfy the **semantic tableaux** version of the
  Second Incompleteness Theorem, which is what stops Theorem 4 from being
  over-read. The conference version states the caveat in prose but does not
  prove it.

## What the conference version has and the journal version drops

Read in full on 2026-08-27; the pair is **not** the clean expansion this record
previously claimed. Three things live only in the conference form:

- **`Diag(1)` and `Diag(2)` are provably logically invalid.** WoLLIC indexes the
  diagonal sentence by axiomatization — `Diag(i)` for `Ax-i` — and its
  Clarifying Comment (p. 10) states: "In fact, `Diag(1)` and `Diag(2)` can be
  proven to be logically invalid statements (see footnote 1). In contrast,
  Theorem 4 (below) will prove `Diag(3)` is logically valid." The journal writes
  only `Diag(Ax-3)`, and its Clarifying Comment keeps the caution but **drops
  the `Diag(1)`/`Diag(2)` invalidity claim entirely**. The conference form is
  the only place in the corpus where the *contrast* is stated: the very same
  diagonal construction is invalid over the two conventional axiomatizations and
  valid over the unconventional one.
- **Footnote 1**, the programme in three sentences: "For an arbitrary axiom
  system α, let `α^D` denote the union of α with the added sentence
  `Diagonal(α, D)`. Most such systems `α^D` are known to be inconsistent because
  they would otherwise violate Gödel's Second Incompleteness Theorem. The main
  point of our prior research [32, 34, 37, 39] is that the usual paradigm where
  an essentially classic Gödel-like diagonalization argument will render `α^D`
  inconsistent applies to most, *but not all* systems `α^D`."
- **The "3-part project" framing** of §5 Concluding Remarks: facet one is [35] =
  `Willard2002a` (`Ax-1` obeys), facet two is [40] (`Ax-2` obeys), facet three is
  this paper (`Ax-3` evades). The journal's §5 replaces this with the
  Kreisel–Takeuti/Pudlák comparison. §1's "Devil's Advocate who seeks to find
  other axiomatizations of `IΣ₀`" is dropped too.

**Bibliographic finding.** WoLLIC's [40] is "Passive Induction and a Solution to
a Paris-Wilkie Open Question", cited as "submitted for publication and also
available as University of Albany Computer Science Technical Report
**SUNYA-CS-06-01 (February 1, 2006)**". That is the preprint of
**`Willard2007-APAL`** (APAL 146 (2007) pp. 124–149, = [55] in the journal
version). A **second SUNY Albany technical report** in the corpus's provenance,
after TR 93-10 — the sibling-TR class the C2 coverage pass flagged. The
published form is held, so this is provenance, not a gap.

So the relation is the same shape as `Willard2020-LFCS`/`Willard2020`: the
journal form is far larger but is **not** a superset. Only `Willard2000-TAB`'s
pair is a strict expansion.

## Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Uncapped, case-insensitive item sweep across all 15 pp.; deltas read against `Willard2009` | 0 items absent from `Willard2009` |

| 2 | 2026-08-27 | **Visual control pass**, pp. 8, 9 | 0 new items; Lemma 1, Lemma 2, Definitions 3–5 confirmed verbatim against `Willard2009`, including `B`-Bounded Arithmetic Sets (22), `B`-Bounded Valid `Π^R₁` (23)–(24), and the **Canonical Arithmetic Condition** (all axioms `Π^R₁` **and** including `Q₀`'s nine) |
| 3 | 2026-08-27 | **Full read of pp. 1–15**, plus a visual pass on p. 10 | 0 new numbered items, but **three pieces of content the journal version drops** (the `Diag(1)`/`Diag(2)` invalidity claim, footnote 1, the 3-part-project framing) and one bibliographic identification ([40] = the SUNYA-CS-06-01 preprint of `Willard2007-APAL`); the Definition 7/8 numbering divergence corrected |

Coverage **complete** (pp. 1–15; pp. 13–15 are the reference list).
