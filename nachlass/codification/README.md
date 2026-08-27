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
| Extraction records | [`extraction/`](extraction/) | C4 pilot ([`willard2005.md`](extraction/willard2005.md) + [fidelity check](extraction/willard2005-fidelity-check.md)); C5 origin ([`willard1993-tr.md`](extraction/willard1993-tr.md), [`willard1993.md`](extraction/willard1993.md)); C6 spine ([`willard2011.md`](extraction/willard2011.md) + [subsumption audit](extraction/willard2011-subsumption-audit.md)); C7 spine ([`willard2001.md`](extraction/willard2001.md)); C8 spine ([`willard2020.md`](extraction/willard2020.md)); C9 tier A ([`willard2002a.md`](extraction/willard2002a.md), [`willard2004.md`](extraction/willard2004.md), [`willard2006a.md`](extraction/willard2006a.md), [`willard2020-lfcs.md`](extraction/willard2020-lfcs.md)); 10 of 33 extractable rows |
| Registries (systems, results, notation) | [`registry/`](registry/) | live; 271 results, 39 systems, 122 notation rows |
| Coverage | [`registry/coverage.md`](registry/coverage.md) | what was actually read, as page ranges; `audit.sh` fails without a row per extracted item |
| Gaps | [`registry/gaps.md`](registry/gaps.md) | live; G1–G29 (G9, G23, G25, G28 closed) |
| Concordance | [`concordance/`](concordance/) | drift ledger live (D1–D41); [composition obligations](concordance/composition-obligations.md) live (O1–O54); axes and matrix built at C13 |
| Discussion records | [`discussion/`](discussion/) | longer conversational notes, linked from the LOG; [engine machinery and Rosser](discussion/2026-08-21-engine-machinery-and-rosser.md), [boundary results and the Pi\*1 conjecture](discussion/2026-08-26-boundary-results-and-the-pi1-conjecture.md) |
| Codified statement | `codified-sjas.md` | composed at C14–C15 |

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
