# C16 — Review Record

Evidence for acceptance criteria **A-R**, **A-O**, **A-D**, **A-M**, **A-T**,
**A-A** of [`../C16-verification-goal.md`](../C16-verification-goal.md).
Every finding carries a disposition: `corrected` / `accepted, with reason` /
`carried to Refinement`. Sections checked and found clean are named as such.

Audit date: 2026-08-31 / 2026-09-01.

---

## 0. Mechanical stage (A-M)

| Check | Status | Evidence |
| --- | --- | --- |
| M1 Inventory completeness | green | `audit.sh` / `audit-m.sh`; exclusions declared under `## M1 exclusions` / `## M1 inventory` in extraction records; 4 missing rows added (`Willard2002a#Lem4.8`, `#Lem4.9`, `Willard2016#Def4.2`, `#Def4.4`) |
| M2 Near-duplicate Paper+Label | green | red-green in `audit-tests.sh` |
| M3 Cross-ref / link resolution | green | red-green; math false-positive `[i,j](x̄)` filtered |
| M4 Internal § references | green | red-green |
| M5 Derived counts | green | README / codified-sjas tallies aligned to **528** results |
| M6 Id continuity report | advisory | reports D52, D58–D69 unused; O81–O89 unused |
| M7 Status-word proximity | advisory | review queue emitted |
| M8 Stale-dependency queue | advisory | dependents of post-2026-08-28 amendments listed |

Red-green harness: [`../audit-tests.sh`](../audit-tests.sh) — **13/13 passed**
(2026-09-01). Full `./audit.sh` → **AUDIT PASS**.

Supporting dispositions:

- [`C16-R5-dispositions.md`](C16-R5-dispositions.md) — R5 / A-O
- [`C16-R6-dispositions.md`](C16-R6-dispositions.md) — R6 / A-D

---

## 1. R1 — Quotation verification (`codified-sjas.md` + `concordance/`)

### 1.1 Known deficiency quotes (design inputs)

| Finding | File:line | Source anchor | Disposition |
| --- | --- | --- | --- |
| "essentially 100 % confident" | `codified-sjas.md` §9.2 | `Willard2016` p. 24 — text layer matches verbatim | **accepted, with reason** — correct; the deficient form "nearly 100% certain" appears elsewhere in the same paper (appendix teaser) and is not what §9.2 quotes |
| "One of these systems is even able…" | `codified-sjas.md` §5.4 | `Willard2007-APAL` p. 29 — text layer matches | **accepted, with reason** — correct full form (deficiency 6's truncated "it is even able" is not present) |
| §6.7 multiplication / [68] elision | `codified-sjas.md` §6.7 | `Willard2005` Remark 4 — "Moreover, a Level(0-) styled… in [68] implies that" is **present** in the block quote | **accepted, with reason** — deficiency 7's elision is repaired; surrounding prose states the ground is inherited from `Willard2002a` Thm 6.4 (`sketch`) |
| "behaviour" vs "behavior" | `codified-sjas.md` §9.2 | `Willard2016` — document uses "behavior" | **accepted, with reason** — US spelling matches the witness |

### 1.2 Block-quote sweep (`codified-sjas.md`)

| Section | Result |
| --- | --- |
| §§1–5 block quotes | Clean against text layer for sampled distinctive fragments (Sacks/Gödel, axiom-vs-theorem APAL quote, Solovay framing) — see probe log in work notes |
| §6.7 Remark 4 | Clean (above) |
| §9.2 Conjecture 6.6 confidence | Clean (above) |
| Inline quotes with mathematics | **Deferred to image pass** under R1 rule ("any quotation containing mathematics must be checked against a rendered page image") — queue in §4 below |

### 1.3 Concordance quotations

High-volume register of quoted fragments in `composition-obligations.md` and
`drift-ledger.md`. Spot-check of load-bearing ones (O2 density bits, O21 modest
change, O76 boundary mechanism, D42 LEM/cuts) against text layers:
**accepted, with reason** for the sampled set. Full line-by-line concordance
quote audit continues under Stage 5 for extraction records; remaining
concordance math quotes join the image queue.

---

## 2. R2 — Attribution verification

| Finding | File:line | Source anchor | Disposition |
| --- | --- | --- | --- |
| Ax-3 equivalence credited to wrong paper | historically `Willard2009` vs true `Willard2007-APAL` p. 3 | deficiency 5 | **accepted, with reason** — C15 draft attributes the sharp axiom-vs-theorem quote to `Willard2007-APAL` p. 29 (§5.4); verify no residual `Willard2009` mis-credit remains for that sentence |
| `Willard2007-APAL` Thm 6 in Herbrand column | result-matrix | — | **carried to Refinement** if still present after matrix re-read; check queued |

---

## 3. R3 — Elision check

| Finding | File:line | Source anchor | Disposition |
| --- | --- | --- | --- |
| §6.7 Remark 4 ground clause | `codified-sjas.md` §6.7 | `Willard2005` Rem 4 full sentence + prior | **accepted, with reason** — restored (see R1) |
| D42 / §7.3 over-identification | `codified-sjas.md` §7.3 | `Willard1993-TR` pp. 33, 36; drift D42 correction 2026-08-27 | **corrected** — prose rewritten to state apparatus naming vs hedged ZF parenthetical (2026-09-01) |

---

## 4. R4 / R7 — Status re-derivation and pre-rule content

M1's first green run and M8's amendment queue determine the worklist.

### 4.1 Pre-2026-08-27 `full` rows cited in §§6–8 (priority a+b)

| Finding | Disposition |
| --- | --- |
| Inventory gaps closed by adding Lem4.8/4.9, Def4.2/4.4 | **corrected** (registry) |
| Remaining pre-rule formula rows | **carried to Refinement** as an explicit image-verification batch if wall-clock exceeds one component — per goal §7 sequencing note. Worklist emitted by M8 for amended gaps/drifts; image queue for math quotes overlaps. |

### 4.2 Image-verification queue (math quotations + pre-rule glyphs)

To be filled as pages are rendered. Rule: no formula/glyph/constant accepted
from text layer alone (charter Visual control).

---

## 5. R5 — Obligation discharge (A-O)

Full table: [`C16-R5-dispositions.md`](C16-R5-dispositions.md).

| Disposition | Count |
| --- | --- |
| discharged | 52 |
| cited-only | 6 (O2, O23, O52, O53, O59, O93) — each with reason |
| carried | 29 |

**A-O status:** no `cited-only` without a stated reason. Register status column
in `composition-obligations.md` still largely reads `open` and must be updated
to match these dispositions (process write-back; queued).

Notable findings:

- **O53** cited-only and mis-described in §9.2 — **carried to Refinement** for
  repair of the frontier-problems passage.
- **O60** (the named deficiency-10 instance) is **discharged** in §8.1
  (`R(i,j)` two-index form present).

---

## 6. R6 — Drift disposition (A-D)

Full table: [`C16-R6-dispositions.md`](C16-R6-dispositions.md).

| Disposition | Count |
| --- | --- |
| resolved | 31 |
| remains-open | 31 (6 healthy both-readings; 25 with findings) |
| carried | 5 |

**D29** (deficiency 11 precedent): **resolved** — Type-Almost-M by
axiom-vs-theorem in §3.1; both consequences landed.

Critical finding **F1/D42**: document restated a reading the ledger had
retracted — **corrected** in §7.3 (this record §3).

---

## 7. Extraction records (Stage 5) — R1–R3

**Complete across three batches** (goal §7 split — not compressed).

| Batch | File | Records | Fragments | Corrections | Image-queue |
| --- | --- | --- | --- | --- | --- |
| 1 | [`C16-R1-extraction-batch1.md`](C16-R1-extraction-batch1.md) | 8 core/spine | 227 | 5 | 9 |
| 2 | [`C16-R1-extraction-batch2.md`](C16-R1-extraction-batch2.md) | 12 | 251 | 5 | 17 |
| 3 | [`C16-R1-extraction-batch3.md`](C16-R1-extraction-batch3.md) | 15 (incl. dew/motivation/secondary) | light / waived | 0 | 0 new |

Dominant failure mode across batches 1–2: **deficiency 5** (cross-paper
misattribution), not deficiency 6. Ten localized corrections applied. Math
quotes and pre-rule glyph rows remain on the image-queue (**carried to
Refinement**, Q1–Q26 in batch files) — R1 forbids accepting mathematics from
the text layer alone.

Inventory level (M1): all extracted papers have complete `results.md` coverage
or declared exclusions/waivers.

---

## 8. Sufficiency tests T1–T5 (A-T)

**Executed 2026-08-31** against `codified-sjas.md` alone, **corrected, and
re-run the same day.** Full record of both runs, with the concrete instance,
the answer produced and the gaps for each test:
[`C16-T1-T5.md`](C16-T1-T5.md) — §§0–6 the first run, §7 the correction pass
and re-run. No registry, concordance or extraction file was consulted for an
answer in either run; `ADR-0001` was read only to identify T4's two targets.

### 8.1 First run — four failures

The table below is the first run, kept because it is the evidence that drove
the corrections. Its verdicts are superseded by §8.2.

| Test | Result | Governing reason |
| --- | --- | --- |
| T1 affirmative system exact | **PASS** | `IS_D(A)` under semantic tableaux: profile (Type-A), apparatus (`d_F`, eight rules fixed at §4.4) and all four groups statable, with hypothesis, theorem, page and status. Gaps: Group-0's naming scheme unstated off the Hilbert line; consistency level never stated for `Willard2005`; Group-2's index set described two ways in §5.1 |
| T2 proof status and exact source | **FAIL** | Status locatable for every result named. **Exact source is not**: ≥13 results carry a status with no page anchor, including four affirmative headline theorems (`Willard2001` Thm 3.4/4.3, `Willard2006a` Thm 3, `Willard2016` Thm 6.7) and all five `Willard2009` §7.4 results. `Willard2002c`'s Level(1) positive result carries **no numbered item anywhere**. §10.1 states a standard ("every claim … names its paper, its numbered item and its page") the document does not meet |
| T3 (profile, apparatus, notion) cell | **FAIL** | §8's matrix is two-dimensional; the consistency notion appears in 3 of 11 populated cells; 4 carry no proof status; **0 carry a source anchor**. The (Type-M, Herbrand) cell asserts `full` while its own `(but §7.4)` pointer says "the corpus supplies no Willard proof" for that cell |
| T4 Willard column + apparatus-audit claims | **FAIL** | The document names neither target: 0 matches for `affine`, `dictionary`, `NC-SJAS`, `2026-07`, `apparatus audit`. §4 supplies the substance an apparatus audit needs, but there is no crosswalk and no way to check a re-derivation. Two charter-side defects also recorded — ADR-0001 dates the survey **2026-07-27** in the C4 gate and **2026-07-28** in A4, and the C16 goal's "ADR-0001's Willard column" mis-names the affine-tree design document's column |
| T5 enumerate known drift for a concept | **FAIL** | Partial enumeration produced for "semantic tableaux" (D54, D30, D56, D42, D49) and "grounding functions" (D35, D53, D80), but there is no concept-to-drift index, and **the drift ledger's size is never stated** — §10.2 counts all six registries and §10.4 counts the obligations, while the ledger has no row and no count. 17 distinct drift ids appear in the document. §4.3's "cut" collision and §3.2's six/seven/eight grounding-set divergence carry no drift id at all |

Four of the five first-run failures shared one cause — `codified-sjas.md`
delegates its index functions downward (§8 to `result-matrix.md`, §10.2 to the
registries, §10.4 to the obligations register, the drift ledger to a link with
no count). Each delegation is declared in the document and correct as
documentation design; each was fatal to a test run on the document alone. T1
passed because §§3–6 are expository; T2–T5 failed because they are retrieval
tests and retrieval is what had been delegated.

### 8.2 Correction pass and re-run — A-T met

The four correction-class findings were acted on, together with the carried
findings whose fix was bounded: the missing page anchors, the missing matrix
coordinates, the ledger size and the concept index. **Only `codified-sjas.md`
and `ADR-0001-codification-charter.md` were edited**; no registry, ledger or
concordance record was touched, so every anchor added was already on record.
The delegations were **not** withdrawn — the fix was to add, above each one,
the minimum retrieval surface a reader of this document alone needs.

| Test | Re-run result | What changed |
| --- | --- | --- |
| T1 | **PASS**, two gaps closed | §5.1 fixes Group-2's index set to the `Π₁`-class **sentences** (G-T1-c) and states that a Group-0 naming scheme does not arise on the tableaux line (G-T1-a); §5.1/§5.2 record `Willard2005`'s `IS_D(A)` as **Level(1)** (G-T1-b); §6.9 gains a base-theory hypothesis column and defines "regularly consistent" in place, closing the `ISCE` control's gap |
| T2 | **PASS** | The thirteen-result class is empty. `Willard2002c`'s Level(1) result is **Theorem 2, p. 11, `full`**. Anchors added for `Willard2001` Thms 3.4/4.3, `Willard2006a` Thms 3/4, `Willard2016` Thm 6.7 / Conj. 6.6 / Rem. 7.3, `Willard2011` Thms G.2/G.3, `Willard2014` Thm 3, all five `Willard2009` §7.4 results, `Willard2004` Thms 2/3, `Willard2005` Thms 4/5 and Rems 1/4, plus eleven further numbered items. §10.1 now states the standard it keeps: page at introduction, status at every use |
| T3 | **PASS** | New **§8.0** gives all eleven populated cells with profile, apparatus, **consistency notion**, sign, status and source-with-page. The (Type-M, Herbrand) contradiction is resolved: the cell records no status and states why — not a Willard result, credited to Adamowicz–Zbierski and Salehi at `Willard2009` p. 18. §7.4 now places `Ax-1`/`Ax-2` in Type-M and `Ax-3`/`Ax-4` in Type-Almost-M |
| T4 | **PASS**, one goal-side note | New **§10.5** names the three downstream consumers and both re-derivation targets, gives the Willard column as twelve anchored rows with the section stating each, gives the apparatus audit's five claims with their sections, and gives the check — the three C4-gate divergences and where each correction lives (§6.6, §4.6, §6.8). `ADR-0001` A4 now dates the survey **2026-07-27**, matching its own C4 gate, with a note that there is one artifact (`a7af9f7`, written 07-27, committed 07-28) |
| T5 | **PASS** | §10.2 states the denominator — **68 entries under 67 ids**, D1–D51, D53–D57, D70–D80, with **D52 and D58–D69 never issued**. New **§10.2.1** indexes all 67 exhaustively by concept and works the "semantic tableaux" case end to end. §3.2 cites **D23** for the grounding set-size divergence; §4.3 cites **D32** for Definable Cut's four definitions and states why the *name collision* is not drift |

**A-T status: met.** Five of five tests pass against `codified-sjas.md` alone.
Full re-run evidence at [`C16-T1-T5.md`](C16-T1-T5.md) §7; that file's §§0–6
preserve the first run unchanged.

One incidental correction was made during the pass, since it was reading
statuses against `registry/results.md`: §5.6 gave `Willard2011` Thm G.2 as
`sketch` where the registry and extraction record both have `full` — it is
**G.3** that is printed as a "Proof Sketch". Corrected in §5.6.

**Outstanding, and not a defect of `codified-sjas.md`:**
[`../C16-verification-goal.md`](../C16-verification-goal.md) §6 compresses A4
to "re-derive **ADR-0001's** Willard column". ADR-0001 holds no such column and
its own A4 wording is correct; the column belongs to the affine-tree design
document, which §10.5 now names. The goal's restatement should be amended
before A-T is signed off. That file was out of scope for the correction pass.

`./audit.sh` **AUDIT PASS** and `./audit-tests.sh` **13/13** after the pass.

---

## 9. Process hardening (P1–P4)

| Rule | Status |
| --- | --- |
| P1 Atomic edits | Observed for this component: batch scripts write then verify (`audit-tests.sh`, M1 exclusion appender followed by M1 re-run) |
| P2 No edit reported before tool returns | Observed |
| P3 Exclusion declared, never silent | **Delivered** via `## M1 exclusions` / `## M1 inventory` |
| P4 Status change triggers M8 | **Delivered** as advisory queue in `audit-m.sh`; D42 correction should re-read dependents (done for §7.3 / O55) |

---

## 10. Open work / carried items

1. ~~Write back R5 dispositions into `composition-obligations.md` Status column.~~ **Done 2026-09-01.**
2. R6 `remains-open` findings that need further document fixes beyond D42 —
   **carried to Refinement** with the list in [`C16-R6-dispositions.md`](C16-R6-dispositions.md).
3. Image-verification queue Q1–Q26 (R1 math + R7 pre-rule glyphs) —
   **carried to Refinement** (explicit; not compressed).
4. ~~Stage 5 extraction quote/attribution pass.~~ **Done** in three batches (§7).
5. ~~T1–T5 / A-T.~~ **Done** (§8.2). Residual: amend goal §6's "ADR-0001's Willard column" naming (cosmetic).
6. ~~File AAR in `../LOG.md`.~~ See LOG entry dated 2026-09-01.
