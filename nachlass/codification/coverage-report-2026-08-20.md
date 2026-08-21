# Willard Literature Coverage Verification — 2026-08-20

SJAS Codification component **C2** (precautionary web coverage verification).
Question: has any Willard-authored literature escaped the nachlass
encapsulation? Baseline: `../paperlist`, `../papers/README.md` +
`../papers/SHA256SUMS`, `../collected_dew_materials/manifest.tsv`, and the
Willard-authored items in `../../lit/`. Predecessor: the 2026-05-10
public-witness aggregation pass (`../LOG.md`).

## Method and sources queried

- arXiv API author query (`au:"Dan E. Willard"`), full result set with latest
  version numbers.
- DBLP author record `dblp.org/pid/w/DanEWillard` (HTML and XML export),
  complete 1978–2021 publication list.
- Targeted web searches (LFCS/JLC metadata; SUNY-Albany technical reports).
- Willard's own résumé (September 2015), held locally as
  `../../lit/dewresume.pdf` — his publication list A.1–A.31, B.1–B.8, and his
  own accounting of unrefereed items.
- Internal cross-reference sweep: every "technical report" and "Bulletin of
  Symbolic Logic" mention across all witnessed Willard logic PDFs.
- Not reachable this pass (recorded as residuals, not cleared): the Wayback
  Machine (`web.archive.org` unfetchable from this tooling), Semantic Scholar
  API (rate-limited), MathSciNet/zbMATH (subscription).

## A. Escapes found (Willard-authored, on the SJAS throughline, unencapsulated)

| # | Item | Evidence | Status / action |
| --- | --- | --- | --- |
| A1 | "On the Tender Line Separating Generalizations and Boundary-Case Exceptions for the Second Incompleteness Theorem Under Semantic Tableaux Deduction", **LFCS 2020** (Logical Foundations of Computer Science, Deerfield Beach FL, Jan 4–7 2020; Springer proceedings ISBN 978-3-030-36755-8) | DBLP | **No local witness.** Acquire via SpringerLink purchase or interlibrary loan. A dokumen.pub copy of the full proceedings exists but is excluded per the standing no-shadow-library witness policy (2026-05-10 pass precedent). |
| A2 | "About the characterization of a fine line that separates generalizations and boundary-case exceptions for the Second Incompleteness Theorem under semantic tableau deduction", **Journal of Logic and Computation 31(1) (2021), pp. 375–392** | DBLP; web search | **No local witness.** Presumed journal version of A1; Willard's final journal publication. Potentially significant: the 2026-07-28 deductive-apparatus audit found the then-accessible papers state but do not print a complete resolution/tableau-specific stability proof — this paper's exact scope must be established at acquisition. OUP paywalled; no legal OA copy located yet (Semantic Scholar recheck pending). |

No other unencapsulated full-length Willard-authored logic work was found in
any queried source.

## B. Local reconciliations performed this pass

| # | Item | Action |
| --- | --- | --- |
| B1 | `lit/danwillard1993.pdf` (12 pp) is the **published KGC 1993 chapter** "Self-Verifying Axiom Systems" itself — previously recorded as a witness gap | Symlinked as `papers/willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf`, checksummed, README rows/notes amended. Gap closed. |
| B2 | `lit/willard2014sjassignificanceanalytictableaux.pdf`, `lit/willard2017thetafunction.pdf`, `lit/willard_2020_how_lem_pertains_2nd_inc_thm_boundary_case_exceptions.pdf` | Duplicates of already-witnessed arXiv items (1307.0150v2, 1612.08071v5, 2006.01057v1). No action; noted here to complete the lit/ reconciliation. |
| B3 | `lit/LICS-main.pdf`, `lit/BOUAWTv1.pdf`, `lit/IntResolution.pdf` | Not Willard-authored (Lechner–Ouaknine–Worrell; Boucher; Fitting). Background literature, out of corpus. |

## C. arXiv version audit

All six Willard arXiv items are locally witnessed **at their latest versions**:

| arXiv id | Latest | Local witness stamp |
| --- | --- | --- |
| 1108.6330 | v8 (2011-12-31) | v8 ✓ |
| 1307.0150 | v2 (2014-01-13) | v2 ✓ |
| 1612.08071 | v5 (2017-10-12) | v5 ✓ |
| 1707.00039 | v2 (2017-08-30) | v2 ✓ |
| 1807.04717 | v2 (2018-11-15) | v2 ✓ |
| 2006.01057 | v1 (2020-06-01) | v1 ✓ |

## D. The technical-report class

TR 93-10's existence raised the question of sibling reports. The bounded
answer, from the internal cross-reference sweep plus web search:

- **TR 93-10** (witnessed; C1 collation). JSL 2001 ref. [41] cites it as
  "SUNY-Albany Technical Report, March 1994" (50 pages, expanding the 12-page
  Extended Abstract) — treated as the **same artifact** under a loose
  date/page description (our scan: title + preface + 55 printed pp. +
  4 figure pp.), pending contrary evidence. Recorded for the future drift
  ledger.
- **Unlocated:** the "University of Albany technical report" accompanying JSL
  2005 ref. [71] — D. Willard, "On two partial (and not full) respects where
  an axiom system can recognize its own consistency and multiplication as a
  total function", talk at ASL Summer Meeting 2005 (Athens), 300-word
  abstract in the Bulletin of Symbolic Logic. This is the **floating-point
  application** line named in `prose/sjas_synthesis.txt`; its published
  successors are witnessed (TABLEAUX 2005, LNCS 3702, pp. 292–306; JSL 71
  (2006), pp. 1189–1199). The TR itself may contain expanded proofs.
  Acquisition candidate (SUNY-Albany CS TR archive is not online; estate
  materials are the likeliest source).
- No other Willard-authored TR is referenced anywhere in the witnessed
  corpus (the remaining "technical report" mentions cite Beame–Fich and
  Friedman; arXiv 1307.0150 calls itself a technical report).

## E. Micro-item classes (Willard-authored; accepted enumeration gaps)

- **~25 ASL-talk abstracts, Bulletin of Symbolic Logic, 1995–2012** — per
  Willard's own résumé accounting ("300-word abstracts... not refereed
  articles"). These summarize results published elsewhere. Ruling proposal
  for C3: enumerate lazily from BSL meeting reports if and when a specific
  abstract is cited; do not chase as corpus items (throughline test:
  omission does not impair understanding; the anthology satisfies
  completionism).
- **200-word abstract**, 2nd St. Petersburg Conference on Logic and
  Computability (2003), summarizing the forthcoming APAL 2006a paper (JSL
  2005 ref. [72]; `logic.pdmi.ras.ru/2ndDays`, likely defunct; Atlas
  Mathematical Conference Abstracts). Same ruling proposal.
- The 2003 announcement chain is thereby explained by Willard's own
  accounting (résumé B-list preamble: the 2003 conference announcements were
  subsumed into the FOL75 2004 book chapter and APAL 2006a). The prior
  audit's "TABLEAUX 2003 position paper" item remains metadata-only and
  unlocated; no queried source lists it (not in DBLP, not in the résumé).

## F. Standing gaps confirmed (carried to the C3 `gaps.md`)

1. **Willard 1997**, "The Tangibility Reflection Principle for Self-Verifying
   Axiom Systems", KGC5, LNCS 1289, pp. 319–334 — no witness, no legal OA
   found. Acquisition: ILL/SpringerLink.
2. **LFCS 2020 chapter** (A1) and **JLC 31(1) 2021 article** (A2) — see above.
3. **ASL-2005 floating-point companion TR** (D) — estate-search candidate.
4. **TABLEAUX 2003 position paper** — metadata-only, existence unconfirmed by
   this pass.
5. Beklemishev 2010 survey (simplified SJAS presentation) — **not
   Willard-authored, outside this corpus**; retained solely as
   Refinement-stage preparation.
6. Non-logic completeness notes, out of codification scope: DBLP lists
   data-structures items absent from the curated `paperlist` (among them the
   Fredman–Willard fusion-trees JCSS 47 (1993) paper, two JACM 32 (1985)
   papers, and the STOC/FOCS/SODA/SIGMOD/ICALP/STACS/SCG/PODS conference
   line). `papers/WillardFredman_SUNYarchive.pdf` should be identified
   against the 1993 vs 1994 JCSS pair when convenient.

## Biographical anchor

Dan Edward Willard, 1948-09-19 – 2023-01-21 (Wikipedia). The nachlass scans
in `../collected_dew_materials/` are the estate-side channel; the Dec-2025
scans there postdate his death and are estate material.

## Conclusion

Two genuine escapes exist (A1, A2 — the terminal LFCS-2020/JLC-2021 pair);
both are metadata-pinned and actioned as acquisitions. One prior gap was
closed from local holdings (B1). All arXiv witnesses are current. The
technical-report class is bounded (one witnessed, one unlocated companion).
Remaining unencapsulated Willard-authored material consists of micro-items
(abstracts) with explicit ruling proposals. Residual channels for a future
re-pass: Wayback `~dew` author-page sweep, Semantic Scholar/OpenAlex OA
recheck for A2, MathSciNet/zbMATH review listings.
