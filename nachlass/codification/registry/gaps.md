# Gaps Ledger

Every known missing witness, stated-not-proved result, identity question,
processing action, and coverage residual, each with a status and an action.
Statuses: `open` (action identified, not done), `accepted` (deliberately not
chased; rationale recorded), `refinement-prep` (outside the Codification
corpus, retained for the Refinement stage), `closed:YYYY-MM-DD` (with the
closing evidence).

| Id | Item | Kind | Status | Action / evidence |
| --- | --- | --- | --- | --- |
| G1 | Willard2021 (JLC 31(1), pp. 375-392, DOI 10.1093/logcom/exaa083) | missing witness | open | ILL/purchase; no OA (OpenAlex closed), no known shadow copy (libgen metadata-only); see coverage report addendum |
| G2 | Willard1997 (KGC5, LNCS 1289, pp. 319-334) | missing witness | open | ILL/SpringerLink; title confirmed via DBLP and resume |
| G3 | ASL-2005 floating-point companion TR (JSL 2005 ref. [71]) | missing witness | open | Estate/collected-materials search; published successors witnessed (Willard2005-TAB, Willard2006b). **Provenance confirmed by C4**: Willard2005 Remark 7 (p. 30) describes it as the "accompanying full-length technical report" for the floating-point result in which IS_D(A) recognizes addition, multiplication, subtraction and division as total over computer reals — making it the only full-length source for that result |
| G4 | TABLEAUX 2003 position paper | unconfirmed existence | open | Not in DBLP, resume, or any queried source (2026-08-20); confirm against TABLEAUX 2003 companion volume if one exists, else close as non-existent |
| G5 | BSL abstracts class (~25 items, 1995-2012) | micro-items | accepted | Enumerate lazily when a specific abstract is cited; resume attests the class |
| G6 | St. Petersburg 2003 200-word abstract (JSL 2005 ref. [72]) | micro-item | accepted | Atlas/Wayback lookup only if cited |
| G7 | Beklemishev 2010 survey (simplified SJAS presentation claim) | outside corpus | refinement-prep | Acquire before the Refinement stage begins |
| G8 | Complete resolution-specific stability/compactification proof unprinted in accessible corpus | stated-not-proved | open | 2026-07-28 apparatus-audit finding; re-evaluate after G1 (Willard2021) is acquired. **Widened by C4**: the same pattern holds inside Willard2005 for the non-tableau apparatuses — Remark 1 (p. 20) asserts consistency preservation for cut-free sequent calculus, Herbrand deduction, Tab-Q\*1-List, and Q\*1-restricted Hilbert with the case analysis explicitly omitted for space. Remarks 6 and 8 are likewise stated-only. Track per-apparatus proof status in `results.md` |
| G9 | TR 93-10 vs "SUNY-Albany Technical Report, March 1994 (50 pages)" citation identity | bibliographic identity | open | Confirm during C5/C7 extraction (content-level comparison against JSL 2001 ref. [41] usage); drift-ledger entry D1 |
| G10 | Dimacs1996.pdf volume witness is image-only (no text layer) | witness processing | open | Searchable collation via the TR-93-10 pipeline (C1 precedent) before the Willard1998 extraction in C11 |
| G11 | WillardFredman_SUNYarchive.pdf identity (1993 vs 1994 JCSS paper) | bookkeeping (out of corpus) | open | Identify when convenient; no codification dependency |
| G12 | Wayback ~dew author-page sweep; Semantic Scholar/OpenAlex OA recheck for G1 | coverage residual | open | Future coverage re-pass (web.archive.org unreachable and Semantic Scholar rate-limited on 2026-08-20) |
| G13 | Willard2006-Talk witness is a 1-page abstract | witness adequacy | open | Locate fuller slides/notes if referenced during extraction; else accept as-is at C11 |
