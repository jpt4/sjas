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
| Extraction records | [`extraction/`](extraction/) | C4 pilot done ([`willard2005.md`](extraction/willard2005.md) + its [fidelity check](extraction/willard2005-fidelity-check.md)); 1 of 33 extractable rows |
| Registries (systems, results, notation) | [`registry/`](registry/) | live; 26 results, 4 systems, 27 notation rows |
| Gaps | [`registry/gaps.md`](registry/gaps.md) | live; G1–G13 |
| Concordance | [`concordance/`](concordance/) | drift ledger live (D1–D6); axes and matrix built at C13 |
| Codified statement | `codified-sjas.md` | composed at C14–C15 |

Tooling (portable bash + CLI only): [`audit.sh`](audit.sh) checks registry
integrity (run it before committing registry changes; green audit is a merge
condition); [`regen-sources-text.sh`](regen-sources-text.sh) regenerates
[`sources-text/`](sources-text/) — plain-text grep/anchor aids derived from
the witness PDFs (OCR-layer texts are search aids, never authoritative for
formulas; page images govern).
