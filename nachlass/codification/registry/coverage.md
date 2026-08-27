# Coverage Registry (what was actually read)

One row per **extracted** corpus item, recording which pages of the witness
were read as text, which were only swept mechanically, and which were rendered
as page images. `audit.sh` fails when an item marked `extracted:` in
`corpus.md` has no row here.

This registry exists because C9 (2026-08-26) recorded two items as "read in
full" that were not. Coverage was previously carried only in prose inside
extraction records, where nothing could check it. Page ranges are checkable;
adjectives are not — so **never write "read in full" here, write the ranges**.

Columns — **Item**: corpus key. **Pages**: witness page count. **Read**: ranges
read as text, in witness page numbers. **Swept**: ranges covered only by the
awk numbered-item/heading sweep. **Images**: pages rendered and read as images
for the charter's visual control pass. **State**: `complete` (Read covers every
page), `partial` (gaps remain, and a gaps-ledger row says so), or `unrecorded`
(extracted before this registry existed; ranges not reconstructible).

| Item | Pages | Read | Swept | Images | State |
| --- | --- | --- | --- | --- | --- |
| Willard2005 | 43 | 1-43 | all | recorded in `willard2005-fidelity-check.md` | complete |
| Willard1993-TR | 61 | text: 1-61 (OCR); **visual: printed 3-4, 9-15, 17-20, 23-24, 28-29, 32, 34, 37-55** (PDF = printed + 2) | all | as the Read column's visual list | partial |
| Willard1993 | 12 | 1-12 (printed 325-336) | all | some | complete |
| Willard2011 | 64 | 1-64 | all | printed 14, 16, 18, 32, 39 (PDF = printed + 1) | complete |
| Willard2001 | 67 | 2-67 | all | 9, 10, 19, 22, 28, 39, 40, 51, 54, 60 | complete |
| Willard2020 | 28 | 1-28 | all | 10, 12, 15, 17 | complete |
| Willard2002a | 33 | 1-33 | all | 14 | complete |
| Willard2004 | 32 | 1-32 | all | 4 | complete |
| Willard2006a | 45 | 1-45 | all | 6 | complete |
| Willard2020-LFCS | 19 | 1-19 | all | 16 | complete |

**All C9 items and `Willard2020` are `complete`** — the outstanding ranges were
read on 2026-08-27 and gap **G23** closed. The completion read found items the
C9 sweep's length cap had dropped (gap **G28**, closed) and corrected ten proof
statuses.

**The four former `unrecorded` rows were resolved on 2026-08-27** (gap G25
closed). `Willard1993` and `Willard2005` were read to completion in that
session. `Willard2011`'s range is taken from its own saturation record — pass 1
was a "full text-layer read of all 64 pp." — and independently corroborated by
an uncapped inventory sweep and a re-read of §§1, 4 and Appendices A–C.
`Willard1993-TR` stays **`partial`**: it is an OCR witness where page images
govern, its saturation record enumerates exactly which pages had a visual pass,
and roughly seventeen printed main-body pages (1–2, 5–8, 16, 21–22, 25–27,
30–31, 33, 35–36) were covered only at OCR-sweep level. Tracked as gap **G29**.

An uncapped, **case-insensitive** re-sweep of all four confirmed **no numbered
item is missing from any extraction record**. It did find nine `Willard2011`
items that existed only inside combined inventory rows and had no `results.md`
row; those are now added. The sweep must be case-insensitive because hazard H1
turns "D EFINITION" into all-caps `DEFINITION`.
The four `unrecorded` rows are C4-C6 items extracted before this registry
existed; their ranges are not reconstructible and are **not** to be guessed.
Recorded as gap **G25**.
