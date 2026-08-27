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
| Willard1993-TR | 61 | text: 1-61 (OCR); **visual: every page** — printed 1-55 plus front matter and the four unnumbered Figure pages (PDF 1-61) | all | PDF 1-61 | complete |
| Willard1993 | 12 | 1-12 (printed 325-336) | all | 1-12 (all; the record's pass 1 is a full visual read) | complete |
| Willard2011 | 64 | 1-64 | all | printed 14, 16, 18, 32, 39 (PDF = printed + 1) | complete |
| Willard2001 | 67 | 2-67 | all | 9, 10, 19, 22, 28, 39, 40, 51, 54, 60 | complete |
| Willard2020 | 28 | 1-28 | all | 10, 12, 15, 17 | complete |
| Willard2002a | 33 | 1-33 | all | 14 | complete |
| Willard2004 | 32 | 1-32 | all | 4 | complete |
| Willard2006a | 45 | 1-45 | all | 6 | complete |
| Willard2020-LFCS | 19 | 1-19 | all | 16 | complete |
| Willard2002c | 17 | 1-17 | all | 8, 10, 16 | complete |
| Willard2000-TAB | 16 | 1-16 | all | 7, 9, 10 | complete |
| Willard2009 | 33 | 1-33 (30-33 are references) | all | 4, 8, 10, 11, 12, 14, 19, 22, 25, 26, 27, 28 | complete |
| Willard2006-WoLLIC | 15 | 1-15 (13-15 are references) | all | 8, 9, 10 | complete |
| Willard2016 | 34 | 1-34 (PDF pages; PDF = printed + 1) | all | 6, 8, 11, 12, 15, 16, 18, 19, 24, 25 | complete |

**A sweep is not a read.** The `Swept` column records that a mechanical pass
found an item's *label*; only the `Read` column records that its *content* was
taken in. Three times in C9 and C10 a statement-level read produced registry
rows that the page contradicted, and once — `Willard2016` — a pass recorded "~30
numbered items" as swept while entering only 10 into `results.md`, silently
discarding twenty. When `Read` does not cover a range, no item anchored in that
range may carry a proof status other than `unverified`, and the extraction
record must say which items are affected.

**The `Images` column is not optional.** The charter makes a rendered page a
precondition for any formula, glyph or constant entering a registry. The C10
pass of 2026-08-27 initially recorded constants for `Willard2000-TAB`,
`Willard2009`, `Willard2006-WoLLIC` and `Willard2016` without one; the
subsequent visual pass corrected three registry rows, including the printed form
of `Willard2016`'s Conjecture 6.6. Never enter a constant ahead of the image.

**All C9 items and `Willard2020` are `complete`** — the outstanding ranges were
read on 2026-08-27 and gap **G23** closed. The completion read found items the
C9 sweep's length cap had dropped (gap **G28**, closed) and corrected ten proof
statuses.

**The four former `unrecorded` rows were resolved on 2026-08-27** (gap G25
closed). `Willard1993` and `Willard2005` were read to completion in that
session. `Willard2011`'s range is taken from its own saturation record — pass 1
was a "full text-layer read of all 64 pp." — and independently corroborated by
an uncapped inventory sweep and a re-read of §§1, 4 and Appendices A–C.
`Willard1993-TR` is now **`complete`**: gap **G29** closed on 2026-08-27 by a
visual pass over the 23 pages that had never been rendered — PDF 1–4, 7–10, 18,
23–24, 27–29, 32–33, 35, 37–38 and the four Figure pages 58–61. Since this is an
OCR witness where page images govern, every page has now been read as an image.

An uncapped, **case-insensitive** re-sweep of all four confirmed **no numbered
item is missing from any extraction record**. It did find nine `Willard2011`
items that existed only inside combined inventory rows and had no `results.md`
row; those are now added. The sweep must be case-insensitive because hazard H1
turns "D EFINITION" into all-caps `DEFINITION`.
The four `unrecorded` rows are C4-C6 items extracted before this registry
existed; their ranges are not reconstructible and are **not** to be guessed.
Recorded as gap **G25**.
