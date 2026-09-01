# C16 — Verification Goal

> **Goal.** Verify every concordance and codification document against its
> sources and against the registries, using checks designed around the
> deficiencies this workstream has actually exhibited — not around a generic
> notion of review. Produce a review record with a disposition for every
> finding, harden `audit.sh` so the mechanizable failures cannot recur, and
> file the AAR.

This supersedes the charter's one-line "C16 — Audits, acceptance, AAR".

---

## 1. Why this specification looks the way it does

Thirteen process deficiencies have been observed in C4–C15, each with a dated
instance. They are the design input. A review that did not target them would
re-find the same class of error by luck rather than by construction.

| # | Deficiency | Evidence |
| --- | --- | --- |
| **1** | **Coverage does not imply inventory.** A witness can be fully read and fully imaged and still have most of its numbered items unrecorded. | `Willard2000-TAB`: coverage `complete`, pages 1–16 imaged, **8 of 17** numbered items in `results.md` (found 2026-08-31) |
| **2** | **Statement-level reads produce `full` statuses the page contradicts.** | C9: ten statuses overstated. C10: `Willard2009` six corrected; `Willard2016` **28 items swept but never recorded** |
| **3** | **Text-layer corruption reaches registries.** | `℧` recorded as digit `0` in `notation.md`; Conjecture 6.6 reconstructed from a mangled fraction; `Willard2000-TAB` constants entered with no image |
| **4** | **Derived claims go stale when their basis changes.** Nothing links a gap or drift entry to the entries that depend on it. | G35 established `sketch` on 2026-08-28; **D34, D38, O46 and §9.1 still said "proved"** on 2026-08-31 |
| **5** | **Cross-paper misattribution.** Nothing checks that a claim attributed to paper X occurs in paper X. | The `Ax-3` equivalence quote credited to `Willard2009`; it is `Willard2007-APAL` p. 3. `Willard2007-APAL` Thm 6 placed in the Herbrand column |
| **6** | **Non-verbatim quotation.** | "nearly 100% certain" for "essentially 100 % confident"; "[it]" substituted for "ZF"; "it is even able" for "One of these systems is even able"; "behaviour" for "behavior" |
| **7** | **Elision that moves a claim's ground.** | §6.7 deleted "a … generalization … in [68] implies that" and then drew the opposite methodological conclusion |
| **8** | **Near-duplicate rows.** `audit.sh` catches identical ids, not the same item recorded twice under different ids. | `Willard2005#Rem1` / `#Rem1b`; several `#RemN` pairs merged during C10–C13 |
| **9** | **Counts drift from the artifacts they describe.** | "514" vs 513 rows; "eleven" vs D50's ten; "four ways" vs D54's three; "ten places" vs nine rows; O81–O89 never issued |
| **10** | **Obligations cited but not discharged.** Citation at a point of use is not the same as doing what the obligation says. | §8.1 cited **O60** while never presenting `R(i,j)`'s two-index form |
| **11** | **Components leave their own obligations open.** | **D29** required the codification to place Type-Almost-M; C13 did not, and C15 inherited the omission |
| **12** | **Batch edits can abort mid-way and write nothing.** | A `rep()` script applied nine fixes, raised on the tenth, and silently discarded all nine (2026-08-31) |
| **13** | **Registry content predates its own governing rules.** Coverage rows were retro-fitted; *content* was not. | Every row entered before 2026-08-27 was written under the old conditional imaging rule |

---

## 2. Scope

**In scope — every document, in this order.**

1. `codified-sjas.md` (10 chapters)
2. `concordance/` — `axes.md`, `genealogy.md`, `result-matrix.md`, `replication-map.md`, `drift-ledger.md` (D1–D80), `composition-obligations.md` (87 rows)
3. `registry/` — `results.md` (524), `systems.md` (62), `notation.md` (224), `coverage.md` (33), `gaps.md` (37), `corpus.md` (45)
4. `extraction/` — 33 per-paper records
5. `ADR-0001-codification-charter.md`, `README.md`, `../LOG.md`

Ordered by leverage: the documents in 1–2 are what a reader consumes and what
C14–C15 built; 3 is what they rest on; 4 is the deepest and least read.

**Out of scope.** The Refinement stage; mechanization; re-extracting witnesses
whose coverage is `complete` unless a check below finds cause.

---

## 3. Mechanical checks — extend `audit.sh`

Each must be **red-green tested** before it counts as delivered.

| Id | Check | Targets |
| --- | --- | --- |
| **M1** | **Inventory completeness.** For every corpus item marked `extracted:`, run the uncapped, case-insensitive label sweep over its witness and compare against `results.md` rows for that paper. Fail on any label with no row. Accept a declared exclusion list per paper, in the extraction record, with a reason. | **1**, 2 |
| **M2** | **Near-duplicate rows.** Fail when two `results.md` rows share `Paper` + `Label`. | 8 |
| **M3** | **Cross-reference resolution.** Every `**Onn**`, `**Dnn**`, `**Gnn**` in every document resolves to an existing row/heading; every relative markdown link resolves to a file. | 4, 5 |
| **M4** | **Internal section references.** Every `§N.M` in `codified-sjas.md` names an existing heading. | 9 |
| **M5** | **Derived counts.** Every prose count that names a registry (`N results rows`, `N systems`, `N obligations`) matches the file. | 9 |
| **M6** | **Id continuity report.** Report — not fail — gaps in the D/O/G sequences, so deliberate gaps stay visible and accidental ones surface. | 9 |
| **M7** | **Status-word proximity.** Flag for review any sentence in `codified-sjas.md` or `concordance/` that names a result whose `results.md` status is `sketch`/`cited`/`stated-only` within N words of "proved", "proves", "establishes", "shows", "demonstrates". Advisory, not fatal — it produces a review queue. | 2, 4 |
| **M8** | **Stale-dependency queue.** For every gap or drift entry amended after a given date, emit the list of documents and rows citing it, for re-reading. | **4** |

M1 and M8 are the two that would have caught the largest defects found so far,
and neither exists today.

---

## 4. Reading checks — cannot be mechanized

These require reading the source. They are the substance of C16.

**R1 — Quotation verification.** Every quoted string in every document, checked
against its witness. Prose quotations may be checked against the text layer;
**any quotation containing mathematics must be checked against a rendered page
image.** Record verbatim / not-verbatim / not-found. *(Targets 3, 6.)*

**R2 — Attribution verification.** For every claim of the form "`PaperX` §N /
p. N says …", confirm the claim occurs in that paper at that anchor. This is
the check that catches a true statement filed under the wrong source. *(5.)*

**R3 — Elision check.** For each verified quotation, read the **full sentence
and the one before it** in the source. Confirm that no clause bearing on the
claim's ground has been dropped, and that the document's surrounding inference
survives the restored context. *(7.)*

**R4 — Status re-derivation.** For every result the documents present as
load-bearing, re-read the proof body and confirm the registry's status from the
page. Priority order: (a) results cited in `codified-sjas.md` §§6–8; (b) every
row still marked `full` that was entered before 2026-08-27; (c) the rest.
*(2, 13.)*

**R5 — Obligation discharge audit.** Walk all 87 obligations. For each, mark
`discharged` (the document does what it says), `cited-only` (mentioned but not
acted on — the O60 failure), or `carried` (deliberately deferred, with reason).
*(10.)*

**R6 — Drift disposition audit.** Walk all 80 drift entries. Every `open` entry
must either be resolved, or carry an explicit statement of why it remains open
and where the codified statement presents both readings. D29's survival through
C13 is the precedent. *(11.)*

**R7 — Pre-rule content re-verification.** Identify every registry row entered
before 2026-08-27 that carries a formula, glyph or constant, and re-verify it
against a page image. Coverage was retro-fitted; content was not. *(13, 3.)*

---

## 5. Process hardening

**P1 — Atomic edits.** Any batch edit script must write on partial success or
not at all, and must be followed by a verification read of what actually landed.
The nine-fixes-discarded incident is the standing example. *(12.)*

**P2 — No edit reported before its tool call returns.** Existing working
agreement 9; restated because it is cheap to violate.

**P3 — Exclusion must be declared, never silent.** If a numbered item is
deliberately not recorded, the extraction record says so and why; M1 reads that
list. This is the general form of the C9/C10 failures. *(1, 2.)*

**P4 — A finding that changes a status must trigger M8.** Closing or amending a
gap is not complete until every dependent claim has been re-read. *(4.)*

---

## 6. Acceptance

C16 is done when all of the following hold.

- **A-M** `audit.sh` green with M1–M8 present, each red-green tested. M1 green
  means every extracted witness's inventory is complete or its exclusions are
  declared.
- **A-R** A review record at `concordance/C16-review-record.md` listing **every**
  finding from R1–R7 with file, line, source anchor, and disposition
  (`corrected` / `accepted, with reason` / `carried to Refinement`). Sections
  checked and found clean are recorded as such, named, so the absence of
  findings is evidence rather than silence.
- **A-O** All 87 obligations dispositioned; no obligation left `cited-only`
  without a stated reason.
- **A-D** All 80 drift entries dispositioned.
- **A-T** The charter's five sufficiency tests run **against
  `codified-sjas.md` alone**, by someone reading only that document:
  **T1** state any affirmative system exactly; **T2** locate any result's proof
  status and source; **T3** read off any (profile, apparatus, notion) cell;
  **T4** re-derive ADR-0001's Willard column and the 2026-07-28 apparatus-audit
  claims; **T5** enumerate all known drift for a concept.
- **A-A** The AAR filed in `../LOG.md`, including the deficiency table of §1
  with what each hardening actually caught.

---

## 7. Sequencing and honest cost

The reading checks are the expensive half and cannot be compressed without
reintroducing deficiency 2 — the failure mode is precisely *deciding silently
to read less*.

| Stage | Work | Note |
| --- | --- | --- |
| **1** | M1–M8 built and red-green tested | Cheap. Do first: M1's output determines how much of R4/R7 is needed |
| **2** | R1–R3 on `codified-sjas.md` and `concordance/` | ~20 block quotes plus inline; the highest-leverage reading |
| **3** | R5, R6 — the two register walks | Bounded: 87 + 80 rows |
| **4** | R4, R7 on the pre-2026-08-27 rows M1 and M8 surface | Size unknown until stage 1 reports |
| **5** | R1–R3 across the 33 extraction records | The long tail |
| **6** | T1–T5, then the AAR | |

**If stage 4 or 5 proves larger than one component**, say so at the start and
split it — do not compress reading depth and report the component done. That is
working agreement 7, and it is on this list because it has been violated.

**Expected outcome, stated in advance.** On the evidence of C10–C15, this will
find defects rather than confirm cleanliness. The C15 review found 22 in a
document composed with care two days earlier. A C16 that reports nothing should
be treated as evidence that the checks were too weak, not that the work was
clean.
