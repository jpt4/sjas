# Willard TR 93-10 (1993): Self-Verifying Axiom Systems and the Incompleteness Theorem

Dan E. Willard, *Self-Verifying Axiom Systems and the Incompleteness Theorem*,
SUNY-Albany Computer Science Department, Technical Report 93-10.

This is the **original full-length 1993 technical report on SJAS**, and a
foundational work for the Codification stage of the SJAS synthesis
(`prose/sjas_synthesis.txt`). Its second page states the publication
relationship (transcribed from the scan):

> A 12-page abbreviated version of this paper, entitled Self-Verifying Axiom
> Systems, was published on 25 August 1993 in the Proceedings of the Third
> Kurt Godel Symposium pp. 325-336 (published as Springer-Verlag LNCS # 713).
> This new longer draft has been issued as SUNY-Albany Comp. Science TR 93-10.

The published 12-page KGC chapter (`paperlist` key `Willard1993`) still has no
standalone local witness; a copy of the LNCS 713 volume exists in `lit/` as a
DJVU. TR 93-10 is the full-length origin text of that chapter.

## Source scans (immutable witnesses)

Three image-only scan parts (no embedded text), collation order 0 → 1 → 2,
continuity verified at the seams (printed pp. 25→26 and 41→42):

| File | Scan pages | Content |
| --- | ---: | --- |
| `tr1993-0.pdf` | 27 | title page; preface/abstract page; printed pp. 1–25 |
| `tr1993-1.pdf` | 16 | printed pp. 26–41 |
| `tr1993-2.pdf` | 18 | printed pp. 42–55 (incl. References at p. 55); Figures 1–4 |

Printed-page completeness was verified by an OCR sweep of the page headers:
the detected numbers form the gap-free, duplicate-free lattices
`part0 scan k ↔ printed k−2` (k ≥ 3), `part1 scan k ↔ printed 25+k`,
`part2 scan k ↔ printed 41+k` (k ≤ 14); header misses on individual pages were
checked against surrounding content.

## Collated searchable PDF

`willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf`
(61 pages, all uniformly letter-size 612×792 pt, ~23.5 MB) collates the three
parts in order and carries an aligned invisible OCR text layer, making the
report full-text searchable and highlightable.

Pipeline (2026-08-20, present CLI tools only):

1. `pdftoppm -gray -scale-to-x 2550 -scale-to-y -1 -png` per part
   (normalizes part 2's oversized 1540×1995 pt pages to the same pixel width);
2. page-size regularization to exactly 2550×3300 px at a declared 300 dpi:
   `convert <page>.png -resize 2550x3300 -background white -gravity center
   -extent 2550x3300 -quality 55 -density 300 -units PixelsPerInch <page>.jpg`
   (ImageMagick; parts 0/1 render at 2550×3300 already; part 2's pages vary
   3299–3370 px in height, so they are aspect-preservingly fitted and
   white-padded — no distortion, no cropping. The explicit 300 dpi JFIF
   density matters: tesseract honors image density metadata over `--dpi`, and
   the first pass's low part-2 density reproduced the oversized pages);
3. `tesseract <page>.jpg <page> pdf txt --dpi 300` (tesseract 5.3.4, eng,
   default PSM; 2550×3300 px at 300 dpi ⇒ 612×792 pt letter pages);
4. `pdfunite` over the 61 per-page PDFs in scan order;
5. verification: `pdfinfo -f 1 -l 61` per-page size uniformity (61 × 612×792
   pt); `pdftotext` spot checks (title
   page; printed p. 26 "8. Proof of Proposition 3" at collated p. 28; printed
   p. 42 TreeCheck appendix material at collated p. 44); `pdftotext -bbox`
   word-coordinate check for highlight alignment; per-page PDF integrity
   validated with `pdfinfo` before uniting (one page damaged by an interrupted
   first OCR run was regenerated).

Collated page number = scan order: collated 1–27 = part 0, 28–43 = part 1,
44–61 = part 2. Printed page p is at collated page p+2.

## Caveat

The OCR text layer is a **search and highlight aid, not an authoritative
transcription**. Mathematical notation, subscripts, and special symbols OCR
imperfectly (e.g. Δ0 may appear as "Ag"); the scanned page image remains
authoritative for all formulas, per the standing nachlass OCR policy.

Checksums for the parts and the collated PDF are recorded in
[`../SHA256SUMS`](../SHA256SUMS).
