# SJAS Codification

The Codification stage of the SJAS synthesis (`prose/sjas_synthesis.txt`):
Willard's SJAS literature rendered into a single mature statement through
audited layers. Charter, components, and acceptance criteria:
[`ADR-0001-codification-charter.md`](ADR-0001-codification-charter.md).
Chronological record: [`../LOG.md`](../LOG.md).

| Layer | Where | State |
| --- | --- | --- |
| Coverage | [`coverage-report-2026-08-20.md`](coverage-report-2026-08-20.md) | C2 done; two escapes found, one acquired |
| Corpus ruling | [`registry/corpus.md`](registry/corpus.md) | C3 done; 45 rows ruled |
| Extraction records | [`extraction/`](extraction/) | C4–C10 done; **C11** Tier C ([`willard2005-tab.md`](extraction/willard2005-tab.md), [`willard2006b.md`](extraction/willard2006b.md), [`willard2007-apal.md`](extraction/willard2007-apal.md), [`willard1998.md`](extraction/willard1998.md), motivation: [`willard2013.md`](extraction/willard2013.md), [`willard2014.md`](extraction/willard2014.md), [`willard2018.md`](extraction/willard2018.md), [`willard2007-kgs6.md`](extraction/willard2007-kgs6.md), [`willard2006-talk.md`](extraction/willard2006-talk.md)); **C12** Tier D (nine `dew-*` / `DEW-Resume` records). **33 of 33 extractable** rows (4 remain blocked: Willard1997, Willard2021, ASL-2005-TR, TABLEAUX-2003) |
| Registries (systems, results, notation) | [`registry/`](registry/) | live; 528 results, 62 systems, 227 notation rows; topic slugs and canonical system names frozen at C13 |
| Coverage | [`registry/coverage.md`](registry/coverage.md) | what was actually read, as page ranges; `audit.sh` fails without a row per extracted item; **33 complete** |
| Gaps | [`registry/gaps.md`](registry/gaps.md) | live; G1–G37 (many closed/accepted at C11–C12; G1/G2/G3/G36 still open for acquisition) |
| Concordance | [`concordance/`](concordance/) | **C13 done**: [axes](concordance/axes.md) (five variation axes), [genealogy](concordance/genealogy.md) (canonical names frozen), [result matrix](concordance/result-matrix.md), [replication map](concordance/replication-map.md); [drift ledger](concordance/drift-ledger.md) D1-D80; [composition obligations](concordance/composition-obligations.md) O1-O96 |
| Discussion records | [`discussion/`](discussion/) | longer conversational notes, linked from the LOG; [engine machinery and Rosser](discussion/2026-08-21-engine-machinery-and-rosser.md), [boundary results and the Pi\*1 conjecture](discussion/2026-08-26-boundary-results-and-the-pi1-conjecture.md) |
| Codified statement | [`codified-sjas.md`](codified-sjas.md) | **C15 done**: full draft, all ten chapters (~1250 lines). Motivation, preliminaries, language profiles, apparatuses, the construction, the preservation engine, the negative side, the result matrix, the frontier, provenance |

Tooling (portable bash + CLI only): [`audit.sh`](audit.sh) checks registry
integrity (run it before committing registry changes; green audit is a merge
condition); [`regen-sources-text.sh`](regen-sources-text.sh) regenerates
[`sources-text/`](sources-text/) — plain-text grep/anchor aids derived from
the witness PDFs.

**Page images govern.** The text layer is for locating material and grepping,
never for transcribing formulas. `pdftotext` fails *silently* on mathematical
typesetting — the C4 pilot caught it rendering Fraktur `ℑ` as `=`, a script
glyph as `f`, and dropping numeral overbars and tower super/subscripts
outright. Render and read the page before any formula, glyph, or constant
enters a registry or the codified statement:

```sh
pdftoppm -f <page> -l <page> -r 130 -png <witness>.pdf /tmp/p
```

This is a charter requirement for all tiers, not only for OCR'd scans; see the
pilot record [§8.1](extraction/willard2005.md).
