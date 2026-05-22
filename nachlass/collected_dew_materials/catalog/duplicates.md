# Duplicate and Variant Relationships

## Exact duplicates (same SHA256)

| Alias file | Canonical file | Canonical ID |
| --- | --- | --- |
| `DOC102924-10292024193836.pdf` | `20080318_ZCF_notes.pdf` | `dew-2008-03-18-zcf-set-theory` |

OCR is produced once for the canonical file. The alias is catalogued in
`manifest.tsv` with `duplicate_of` set and `ocr_status=skipped`.

## Low/high-resolution variants (different content)

| Low-res | High-res | Notes |
| --- | --- | --- |
| `05102014_notarized_notes_scan_low_res.pdf` (2 pages) | `2024-10-08 20.13.17_05102014_notarized_notes_scan_higher_res.pdf` (4 pages) | Higher-res scan is the preferred OCR witness; low-res is a subset with poorer quality. |

## ZCF 2008 draft series (related, not duplicates)

| File | Pages | Relationship |
| --- | ---: | --- |
| `20080314_ZCF_notes.pdf` | 6 | Mar 14 draft: "On Some Difficulties Encountered In Formalizing Set Theory" |
| `20080318_ZCF_notes.pdf` | 8 | Mar 18 variant (canonical; duplicate alias above) |
| `20080321_ZCF_notes_notepage7doubled.pdf` | 9 | Mar 21 variant with doubled page 7 |
| `ZFnote.pdf` | 1 | Mar 8 notarized draft: ZF inconsistency program |
| `DOC102924-10292024193202.pdf` | 2 | Short Oct 2024 rescan fragment (2 pages); distinct from Mar 18 eight-page file |
