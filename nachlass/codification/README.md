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
| Extraction records | `extraction/` | from C4 |
| Registries (systems, results, notation) | [`registry/`](registry/) | schemas seeded; filled from C4 |
| Gaps | [`registry/gaps.md`](registry/gaps.md) | live |
| Concordance | [`concordance/`](concordance/) | drift ledger seeded; built at C13 |
| Codified statement | `codified-sjas.md` | composed at C14–C15 |

Tooling (portable bash + CLI only): [`audit.sh`](audit.sh) checks registry
integrity (run it before committing registry changes; green audit is a merge
condition); [`regen-sources-text.sh`](regen-sources-text.sh) regenerates
[`sources-text/`](sources-text/) — plain-text grep/anchor aids derived from
the witness PDFs (OCR-layer texts are search aids, never authoritative for
formulas; page images govern).
