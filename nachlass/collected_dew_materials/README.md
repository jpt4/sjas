# Dan Willard Collected Materials (Nachlass)

This directory holds scanned images, notes, and documents from Dan E. Willard
(DEW), including originals of difficult-to-access works and unpublished drafts.
It complements the published-paper witness tree in [`../papers/`](../papers/README.md).

`SHA256SUMS` records checksums for the original PDF scans at the collection
root. `manifest.tsv` is the machine-readable inventory; [`catalog/duplicates.md`](catalog/duplicates.md)
documents exact duplicates and resolution variants.

## Layout

```text
collected_dew_materials/
  *.pdf                     # original scans (immutable witnesses)
  SHA256SUMS
  manifest.tsv
  README.md
  scripts/
    ocr_dew_materials.sh    # batch OCR driver
    update_manifest_from_qa.sh
  ocr/
    text/                   # fast-profile merged OCR (*.txt, *.qa.tsv)
    hifi/                   # hi-fi profile (600dpi, formula-oriented)
      text/
      pages/
      tsv/
    pages/                  # per-page OCR text (fast profile)
    tsv/                    # per-page tesseract TSV (fast profile)
  catalog/
    duplicates.md
    by-topic/               # topic-grouped indexes
```

Rendered PNG caches (`ocr/images/`, `ocr/scaled/`) are gitignored.

## OCR Pipeline

Run from this directory:

```bash
./scripts/ocr_dew_materials.sh --pilot    # Phase A (SJAS/high-value)
./scripts/ocr_dew_materials.sh --phase-b   # exams, PhD notes, 2014 notarized high-res
./scripts/ocr_dew_materials.sh --phase-c   # admin, computational dynamics, Dec 2025 scans
./scripts/ocr_dew_materials.sh --all       # all non-duplicate PDFs (fast profile)
```

**Fast profile (default):** 200 DPI, 850px max width, PSM 6, ocrad fallback.
Output under `ocr/text/`.

**Hi-fi profile (formula fidelity):**

```bash
./scripts/ocr_dew_materials_hifi.sh        # --hifi --force --all, exclusive lock
# or single file:
./scripts/ocr_dew_materials.sh --hifi --force ZFnote.pdf
```

600 DPI render, grayscale resize to 2550px max width, tesseract PSM 4
(with fallbacks only when primary yields little text), TSV confidence QA.
Output under `ocr/hifi/text/` (preserves the fast pass).

Expect **~30–45 minutes per page** at full hi-fi width on this host; the full
corpus (~225 pages) may take several days. Do not run concurrent tesseract jobs.

Tools: `pdftoppm`, `tesseract` (eng), `ocrad`/`gocr` as last-resort fallback
when tesseract times out or yields almost no text.

After OCR:

```bash
./scripts/update_manifest_from_qa.sh          # fast pass ratings
./scripts/update_manifest_from_qa.sh --hifi   # hi-fi pass ratings
```

## Inventory Summary

| ID | File | Pages | Type | Topic |
| --- | --- | ---: | --- | --- |
| dew-2020-10-04-incompleteness-notes | `2020Notes.pdf` | 81 | notes | incompleteness, Löb, teaching |
| dew-2008-03-14-zcf-set-theory-difficulties | `20080314_ZCF_notes.pdf` | 6 | draft | 2008 ZCF set-theory program |
| dew-2008-03-18-zcf-set-theory | `20080318_ZCF_notes.pdf` | 8 | draft | 2008 ZCF set-theory program |
| dew-2008-03-21-zcf-set-theory-variant | `20080321_ZCF_notes_notepage7doubled.pdf` | 9 | draft | 2008 ZCF variant |
| dew-2008-03-08-zf-inconsistency-draft | `ZFnote.pdf` | 1 | notarized | 2008 ZF draft |
| dew-2014-05-10-notarized-notes-high | `2024-10-08 20.13.17_05102014_notarized_notes_scan_higher_res.pdf` | 4 | notarized | 2014 notarized (preferred) |
| dew-2014-05-10-notarized-notes-low | `05102014_notarized_notes_scan_low_res.pdf` | 2 | notarized | 2014 notarized (low-res subset) |
| dew-hajek-correspondence | `Correspondence_Hajek.pdf` | 9 | correspondence | Hájek |
| dew-2014-exam | `Exam_2014.pdf` | 6 | exam | teaching |
| dew-phd-notes-1 | `Notes_Phd1.pdf` | 15 | notes | dissertation era |
| dew-computational-dynamics-notes | `Notes_Thetheoryofcomputationaldynamics.pdf` | 30 | notes | non-logic |
| dew-1991-nsf-report | `NSF_1991_Report.pdf` | 3 | report | admin |
| dew-2025-12-22-scan-a | `2025-12-22 09.55.35.pdf` | 28 | draft | incompleteness, semantic tableaux, boundary-case |
| dew-2025-12-22-scan-b | `2025-12-22 09.58.14.pdf` | 9 | notes | SJAS deduction methods (Tab/Xtab) |
| dew-2025-12-24-scan-a | `2025-12-24 14.07.32.pdf` | 3 | article | Trivers-Willard biology |
| dew-2025-12-24-scan-b | `2025-12-24 14.44.42.pdf` | 8 | draft | Hilbert consistency program |
| dew-doc102924-fragment | `DOC102924-10292024193202.pdf` | 2 | fragment | ZCF-related |
| dew-2008-03-18-zcf-set-theory-alias | `DOC102924-10292024193836.pdf` | 8 | alias | duplicate of Mar 18 file |

**Total:** 19 PDF files, 225 pages, 18 unique OCR targets.

## Topic Indexes

- [`catalog/by-topic/incompleteness-sjas-teaching.md`](catalog/by-topic/incompleteness-sjas-teaching.md)
- [`catalog/by-topic/zcf-2008-set-theory-drafts.md`](catalog/by-topic/zcf-2008-set-theory-drafts.md)
- [`catalog/by-topic/correspondence.md`](catalog/by-topic/correspondence.md)
- [`catalog/by-topic/early-career-admin-unclassified.md`](catalog/by-topic/early-career-admin-unclassified.md)

## Relation to `paperlist`

| Material | `paperlist` link |
| --- | --- |
| `2020Notes.pdf` | Overlaps incompleteness/SJAS themes in `Willard2020`, `Willard2018`; distinct lecture-note witness |
| 2008 ZCF/ZF drafts | **Not** in `paperlist`; unpublished |
| `Correspondence_Hajek.pdf` | External correspondence; not a Willard publication |
| `Exam_2014.pdf` | Teaching material; not a publication |
| `NSF_1991_Report.pdf` | Grant reporting; not in logic bibliography |
| Dec 2025 scans | See topic indexes; several overlap published incompleteness/SJAS or biology lines |

## OCR Quality Summary (2026-05-21 pass)

| Tier | Location | Quality | Notes |
| --- | --- | --- | --- |
| Fast | `ocr/text/` | mostly `needs_review` | ocrad fallback dominated; grep aid only |
| Hi-fi | `ocr/hifi/text/` | in progress | 600dpi / 2550px / PSM 4; use for formulas |

Fast pass summary:

| Quality | Count | Meaning |
| --- | ---: | --- |
| `needs_review` | 16 | Tesseract yielded little text; ocrad fallback used on most pages. Searchable but not authoritative, especially for formulas. |
| `poor` | 1 | `Correspondence_Hajek.pdf` — mostly blank or illegible pages. |
| `skipped` | 1 | Exact duplicate alias (`DOC102924-10292024193836.pdf`). |

**Important:** Prefer `ocr/hifi/text/` over `ocr/text/` when formulas matter.
The 2020 incompleteness notebook and several typed drafts are partially readable
via ocrad in the fast pass despite the `needs_review` rating. Do not treat OCR
output as a substitute for the scan when precise formulas matter.

## Known Limitations

- All source PDFs are image scans; there is no embedded text layer.
- Mathematical notation, subscripts, and Bool-4 / Gödel-bracket syntax OCR poorly.
- Handwritten notary blocks on 2008 drafts are low quality.
- English-only `tesseract` (`eng`); no `equ` math bundle installed.
- Hi-fi tesseract at 2550px can exceed 40 minutes per page on this hardware.
- OCR text is a search aid, not an authoritative transcription.

See `ocr/text/*.qa.tsv` (fast) and `ocr/hifi/text/*.qa.tsv` (hi-fi) for per-page
mean confidence scores.
