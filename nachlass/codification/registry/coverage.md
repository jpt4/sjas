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
| Willard1993-TR | 61 | unrecorded | all | many (OCR witness; see record) | unrecorded |
| Willard1993 | 12 | 1-12 (printed 325-336) | all | some | complete |
| Willard2011 | 64 | unrecorded | all | several | unrecorded |
| Willard2001 | 67 | 2-67 | all | 9, 10, 19, 22, 28, 39, 40, 51, 54, 60 | complete |
| Willard2020 | 28 | 1-28 | all | 10, 12, 15, 17 | complete |
| Willard2002a | 33 | 1-33 | all | 14 | complete |
| Willard2004 | 32 | 1-32 | all | 4 | complete |
| Willard2006a | 45 | 1-45 | all | 6 | complete |
| Willard2020-LFCS | 19 | 1-19 | all | 16 | complete |

**All C9 items and `Willard2020` are now `complete`** — the outstanding ranges
were read on 2026-08-27 and gap **G23** closed. The completion read found items
the C9 sweep's length cap had dropped (gap **G28**, closed) and corrected six
proof statuses.
The four `unrecorded` rows are C4-C6 items extracted before this registry
existed; their ranges are not reconstructible and are **not** to be guessed.
Recorded as gap **G25**.
