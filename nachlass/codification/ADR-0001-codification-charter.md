# ADR-0001 (Codification): Charter for the SJAS Codification Stage

Status: Accepted

Date: 2026-08-20

Scope note: this ADR series is local to `nachlass/codification/` and distinct
from any other ADR series in the repository or its parent.

## Context

`prose/sjas_synthesis.txt` (master 53d56a0) proposes synthesizing Willard's
SJAS literature into a single mature statement — the **Codified SJAS** — that
abstracts over contingencies, de-duplicates replicated context, and clarifies
the motivation, articulation, and demonstration of the essential results. Its
governing caution: SJAS properties are delicately sensitive to precise
definitions, and Willard's publications are overlapping but **not coincident**
formalizations; codification must therefore establish exactly what the
variants are before abstracting over them. The Codified SJAS feeds the
Refinement stage (which may freely depart from Willard's presentation to
articulate the objective concept) and the computational-analogue program
(NC-SJAS plan; affine-tree ADR line; Proflog SJAS profile).

User directions in force (2026-08-20): all codification work is conducted
within `nachlass/`; audit tooling is portable bash + standard CLI (not Ruby);
registries and extraction records must be human-readable, themselves teaching
the structure of the literature; corpus membership is decided by the
**throughline test** (anything without which understanding of Willard's
conceptualization of SJAS would be impaired is in; nothing is included for
completionism; secondary literature belongs to Refinement); user-feedback
checkpoints after each definite component are obligatory until autonomous
execution is authorized.

## Decision

Execute the Codification as a layered, registry-audited workstream in
`nachlass/codification/`, on branch `sjas-codification`:

1. **Corpus layer** (`registry/corpus.md`): every Willard-authored item ruled
   in or out with rationale, witnesses named, extraction state tracked.
2. **Extraction layer** (`extraction/<key>.md`): per-item formal inventories
   using the fixed template below; saturation-closed (a re-pass finding zero
   new numbered items).
3. **Concordance layer** (`concordance/`): canonical system genealogy, the
   variation axes (language profile / deduction apparatus / consistency
   notion / axiomatization finiteness / base-theory assumptions), the
   affirmative–negative–open result matrix, the drift ledger (discrepancies
   retained, not resolved), and the replication map (the de-duplication
   instrument).
4. **Synthesis layer** (`codified-sjas.md`): the single mature statement,
   every definition and theorem carrying provenance and variant deltas.
5. **Audit** (`audit.sh`): bash/awk parsing of the registry tables —
   referential integrity, witness existence, enum validity, coverage counts.
   Green audit is a merge condition for every component.

**Extraction template** (per item): 1. Identity & witness; 2. Role in corpus
(Willard's own claim recorded; verification deferred to the concordance);
3. Systems defined (paper's own notation); 4. Numbered-item inventory
(definition/theorem/lemma/remark/conjecture: page anchor, tight paraphrase —
verbatim where load-bearing — proof status full|sketch|cited|stated-only,
dependencies); 5. Notation table; 6. Replicated context; 7. Discrepancies &
errata; 8. Saturation record **including a visual control pass**. Motivation-tier
items use a lighter variant (§1, §2, distilled theses, §7, §8). Witness-tier
(nachlass) items get witness-grade records anchored to page images; OCR text is
a search aid and never an anchor for formal statements.

**Visual control (mandatory, all tiers).** `pdftotext` is a lossy rendering of
mathematical typesetting and its losses are *silent* — it substitutes plausible
ASCII rather than failing. The C4 pilot found it rendering Fraktur `ℑ` as `=`,
a script glyph as `f`, and silently dropping numeral overbars, tower
super/subscripts, iterated-logarithm exponents, and floor/ceiling delimiters
(pilot record §8.1). Therefore: every page carrying a system definition, a
definition or theorem the codified statement will quote, or a displayed
equation whose constants matter, **must be rendered and read as an image**
(`pdftoppm -r 130 -png`) before its content enters a registry or the codified
statement. Text extraction locates material and supports grep; the page image
governs every formula, glyph, and constant. This extends the standing nachlass
page-images-are-authoritative policy from OCR'd scans to publisher-typeset
PDFs, where the risk is less obvious and therefore greater.

**Composition obligations.** Any finding that the codified statement must act
on — a side condition to discharge, a variant to present as two readings, a
constant to keep distinct — is appended to
`concordance/composition-obligations.md` as it is discovered, in addition to
wherever else it is recorded. That register is the C14–C15 checklist; findings
buried only inside a drift entry or a fidelity check will not surface reliably
at composition time.

**Components and status:**

| # | Component | Status |
| --- | --- | --- |
| C1 | TR 93-10 searchable collation | done 2026-08-20 (309ab69, a589920) |
| C2 | Web coverage verification (+ acquisitions) | done 2026-08-20 (48707d6, 01daf26, 714ee5c) |
| C3 | Charter, scaffold, corpus ruling pass | this ADR |
| C4 | Pilot extraction: Willard2005 (method gate) | done 2026-08-21; gate passed, see `extraction/willard2005-fidelity-check.md` |
| C5 | Origin: Willard1993-TR + Willard1993 | done 2026-08-21; formal core visually verified, Appendix A inventory deferred to pass 2 |
| C6 | Spine: Willard2011 + subsumption audit | done 2026-08-21 |
| C7 | Spine: Willard2001 | done 2026-08-26 |
| C8 | Spine: Willard2020 | done 2026-08-26 |
| C9 | Tier A: Willard2002a, Willard2004, Willard2006a, Willard2020-LFCS (+Willard2021 when acquired) | done 2026-08-26; partial reads logged as G23 |
| C10 | Tier B: Willard2002c, Willard2000-TAB, Willard2006-WoLLIC, Willard2009, Willard2016 | pending |
| C11 | Tier C: Willard2005-TAB, Willard2006b, Willard2007-APAL, Willard1998, Willard1997 (when acquired), motivation items | pending |
| C12 | Tier D: nachlass witness records | pending |
| C13 | Concordance | pending |
| C14–C15 | `codified-sjas.md` composition | pending |
| C16 | Audits, acceptance, AAR | pending |

The **C4 method gate**: the Willard2005 extraction is fidelity-checked
against the two known-good partial re-derivations (the affine-tree design
document §2–§3; the 2026-07-27 deductive-apparatus survey in the parent
repository). Divergences fix the template before scaling. The **C6
subsumption audit**: Willard2011's claims to unify/subsume earlier results
are audited against the Willard2001/2005 extractions — first substantive
drift-ledger entries — never assumed.

## Coverage discipline (added 2026-08-27)

Extraction records state what was read as **page ranges** in
[`registry/coverage.md`](registry/coverage.md), never as an adjective. Every
item marked `extracted:` in `corpus.md` must carry a coverage row;
[`audit.sh`](audit.sh) fails when one is missing and when `complete` is claimed
over unrecorded ranges. Proof statuses for unread ranges are recorded as
`unverified` rather than guessed.

Added after C9 recorded two items as "read in full" that were not
(`Willard2002a` pp. 15-19 and 29-33; `Willard2020-LFCS` pp. 1-14). The
pre-existing rule — once an item is ruled core, every part is extracted — did
not prevent it, because coverage lived only in prose where nothing could check
it. Criterion **A1** is amended accordingly: saturation requires a coverage row
in state `complete`, not a prose assertion.

## Success criteria

- **A1** Every `core`/`motivation`/`witness` corpus row has an extraction
  record with a recorded zero-new-items saturation re-pass.
- **A2** `audit.sh` green: keys unique, witnesses exist, enums valid, every
  results/systems/notation row keyed to a corpus row; every extracted result
  mapped into the codified statement/result matrix or listed with exclusion
  rationale.
- **A3** C4 fidelity gate passed with no unexplained divergence.
- **A4** Sufficiency tests against `codified-sjas.md` alone: T1 state any
  affirmative Willard system exactly (profile + apparatus + groups); T2
  locate any claimed result's proof status and exact source; T3 read off any
  (profile, apparatus, consistency-notion) cell of the result matrix; T4
  re-derive the Willard column of the affine-tree transfer dictionary and
  the 2026-07-28 apparatus-audit claims; T5 enumerate every known drift for
  a given concept.
- **A5** `registry/gaps.md` complete: every missing witness, stated-not-
  proved result, and version/identity question carries an action or an
  explicit accepted ruling.
- **A6** The C2 coverage conclusion stands or is amended by recorded
  evidence.
- **A7** After-action report appended to this ADR at completion.

## Failure criteria / explicitly not claimed

- No claim that the corpus is coherent in advance of the concordance; where
  variants conflict, the drift ledger retains the conflict and the codified
  statement states both readings with provenance.
- Codification does not modernize, simplify, or repair Willard's
  presentation (that is Refinement); it does not mechanize (that is the
  Proflog/affine-tree line).
- OCR-derived text (TR 93-10 layer, nachlass scans, DIMACS volume when
  collated) never anchors a formal statement; page images are authoritative.
- If the C4 gate exposes template inadequacy, the template is revised and
  C4 repeated before any further extraction is accepted.

## Consequences

- The registries become the citable index of Willard's SJAS literature
  (human-readable first; machine-checked always).
- Each component ends with a commit, a LOG entry in `../LOG.md`, and a user
  checkpoint until autonomy is authorized.
- The Refinement stage inherits: the codified statement, the result matrix,
  the drift ledger, and the recorded hooks (Beklemishev simplification
  claim; Lawvere-FPT and Rosser-analogue formulations; the Solovay
  private-communication thread surfaced in Willard2020-LFCS ref. 41).
