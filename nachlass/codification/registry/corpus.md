# Codification Corpus Registry

The ruled inventory of Willard-authored materials, per the throughline test
(charter ADR-0001): an item is **in** iff omitting it would impair
understanding of Willard's conceptualization of SJAS; nothing is included for
completionism. Witness paths are repo-relative; multiple witnesses are
semicolon-separated. `audit.sh` checks this table's integrity.

**Ruling values** — `core`: full extraction (template §1–§8). `motivation`:
lighter motivation-layer extraction. `witness`: witness-grade record anchored
to page images (OCR text is a search aid only). `out`: excluded, with
rationale. `gap`: in-scope but unwitnessed (acquisition pending). `gap-accepted`:
in-scope micro-item class, deliberately not chased (enumerate lazily).

**Tier values** — `pilot` (C4), `origin` (C5), `spine` (C6–C8), `A` (C9),
`B` (C10), `C` (C11), `D` (C12), `-` (none).

**Extraction values** — `pending`, `n/a` (out rows), `blocked` (gap rows),
`accepted` (gap-accepted rows); later `extracted:YYYY-MM-DD` and
`saturated:YYYY-MM-DD`.

## In-corpus: formal core

| Key | Witness | Pages | Ruling | Tier | Rationale | Extraction |
| --- | --- | --- | --- | --- | --- | --- |
| Willard2005 | nachlass/papers/willard2005_addition_total_consistency_author_jsl5.pdf | 43 | core | pilot | Engine paper: Normed(a,b), theta-compactification, Theorem 1, section 5.2 lemmas; two known-good partial re-derivations exist, so it is the method gate | pending |
| Willard1993-TR | nachlass/papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf | 61 | core | origin | Full-length origin text (SUNY-Albany TR 93-10); C1 collation; OCR layer is a search aid | pending |
| Willard1993 | nachlass/papers/willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf; lit/danwillard1993.pdf | 12 | core | origin | Published 12-page KGC abbreviation of TR 93-10 (LNCS 713, pp. 325-336) | pending |
| Willard2011 | nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf | 64 | core | spine | Willard's own unification/simplification/extension of the SJAS families; subsumption claims audited at C6, never assumed | pending |
| Willard2001 | nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf | 67 | core | spine | JSL foundation: self-justification definition, tangibility reflection principle, IS(A) machinery | pending |
| Willard2020 | nachlass/papers/willard2020.pdf; lit/willard_2020_how_lem_pertains_2nd_inc_thm_boundary_case_exceptions.pdf | 28 | core | spine | Terminal arXiv statement: apparatus identity (Def. 3.2), LEM boundary, Res/Xres | pending |
| Willard2002a | nachlass/papers/willard2002_semantic_tableaux_robinson_q_author_jsl2.pdf | 33 | core | A | Negative side: tableaux/cut-free G2 extended almost to Robinson's Q | pending |
| Willard2004 | nachlass/papers/willard2004_addition_not_multiplication_fol75.pdf | 32 | core | A | Negative side: G2 version for addition-total, multiplication-not systems (announced 2003) | pending |
| Willard2006a | nachlass/papers/willard2006_generalization_second_incompleteness_author_apal6.pdf | 45 | core | A | Generalized G2 and its exceptions (announced 2003; St. Petersburg abstract) | pending |
| Willard2020-LFCS | nachlass/papers/willard2020_tender_line_lfcs2020_lncs11972_chapter.pdf; nachlass/papers/willard2020_tender_line_lfcs2020_lncs11972_volume.pdf | 19 | core | A | Terminal tender-line paper (LNCS 11972 pp. 268-286); pairs with the Willard2021 gap; ref. 41 preserves the Solovay private-communication thread | pending |
| Willard2002c | nachlass/papers/willard2002_new_exceptions_tableaux_author_tab2.pdf | 17 | core | B | New exceptions for the tableaux version of G2 (LNCS 2381) | pending |
| Willard2000-TAB | nachlass/papers/willard2000_tableaux_robinson_q_author_tab2000.pdf | 16 | core | B | Conference precursor of Willard2002a (LNCS 1847, pp. 415-430) | pending |
| Willard2006-WoLLIC | nachlass/papers/willard2006_i_sigma0_herbrand_author_wollic.pdf | 15 | core | B | ISigma0 simultaneously obeys and evades herbrandized G2 (ENTCS 165) | pending |
| Willard2009 | nachlass/papers/willard2009_i_sigma0_herbrand_author_inf9.pdf | 33 | core | B | Specially formulated ISigma0 axiomatizations evading herbrandized G2 | pending |
| Willard2016 | nachlass/papers/willard2016_theta_function_symbol_arxiv_1612.08071.pdf; nachlass/papers/willard2017.pdf; lit/willard2017thetafunction.pdf | 34 | core | B | Theta-function-symbol systems appreciating Hilbert-consistency fragments | pending |
| Willard2005-TAB | nachlass/papers/willard2005_real_valued_tableaux_author_tab5.pdf | 15 | core | C | Real-valued tableaux consistency (LNCS 3702; ASL-2005 floating-point line) | pending |
| Willard2006b | nachlass/papers/willard2006_real_valued_arithmetic_author_jsl6.pdf | 11 | core | C | Real-valued arithmetic JSL paper (floating-point line, journal form) | pending |
| Willard2007-APAL | nachlass/papers/willard2007_passive_induction_author_apal7.pdf | 48 | core | C | Passive induction; solution to a Paris-Wilkie question | pending |
| Willard1998 | nachlass/papers/willard1998_self_reflection_np_hardness_dimacs_volume.pdf; lit/Dimacs1996.pdf | 170 | core | C | Reflection-principles prehistory (Self-Reflection and NP-Hardness; volume witness is image-only — searchable collation pending, see gaps) | pending |
| Willard1997 | (none) | - | gap | C | KGC5 tangibility chapter (LNCS 1289, pp. 319-334), which Willard2001 builds on; acquisition: ILL/SpringerLink | blocked |
| Willard2021 | (none) | - | gap | A | Journal version of Willard2020-LFCS (JLC 31(1), pp. 375-392, DOI 10.1093/logcom/exaa083); no OA, no known shadow copy; acquisition: ILL/purchase | blocked |

## In-corpus: motivation layer

| Key | Witness | Pages | Ruling | Tier | Rationale | Extraction |
| --- | --- | --- | --- | --- | --- | --- |
| Willard2013 | nachlass/papers/willard2013_significance_self_justifying_axiom_systems_arxiv_1307.0150.pdf; lit/willard2014sjassignificanceanalytictableaux.pdf | 34 | motivation | C | Extended epistemological-significance statement (arXiv form of Willard2014) | pending |
| Willard2014 | nachlass/papers/willard2014.pdf; nachlass/papers/willard2014_broader_epistemological_author_archive.pdf | 16 | motivation | C | WoLLIC 2014 published form; Willard's recommended entry point (resume note) | pending |
| Willard2018 | nachlass/papers/willard2018.pdf | 35 | motivation | C | Hilbert-consistency-program chasm essay | pending |
| Willard2007-KGS6 | nachlass/papers/willard2007_fourteen_year_effort_author_kgs6.pdf | 7 | motivation | C | Fourteen-year-effort summary (Collegium Logicum IX); spine-consistency probe during C6-C8 | pending |
| Willard2006-Talk | nachlass/papers/willard2006_nature_godel_second_incompleteness_talk.pdf | 1 | motivation | C | Dartmouth talk abstract page | pending |

## In-corpus: witness grade (nachlass and bibliographic)

| Key | Witness | Pages | Ruling | Tier | Rationale | Extraction |
| --- | --- | --- | --- | --- | --- | --- |
| DEW-Resume | lit/dewresume.pdf | 8 | witness | D | Willard's own bibliography and research narrative (Sept 2015); coverage authority | pending |
| dew-2020-incompleteness-notes | nachlass/collected_dew_materials/2020Notes.pdf | 81 | witness | D | Incompleteness/Loeb notebook; late-period conceptualization | pending |
| dew-2008-zcf-group | nachlass/collected_dew_materials/20080314_ZCF_notes.pdf; nachlass/collected_dew_materials/20080318_ZCF_notes.pdf; nachlass/collected_dew_materials/20080321_ZCF_notes_notepage7doubled.pdf; nachlass/collected_dew_materials/ZFnote.pdf; nachlass/collected_dew_materials/DOC102924-10292024193202.pdf | 26 | witness | D | 2008 ZF/ZCF set-theory program (the "possibly ZF manuscript" application named in the synthesis); per-file grain in ../collected_dew_materials/manifest.tsv | pending |
| dew-2014-notarized | nachlass/collected_dew_materials/2024-10-08 20.13.17_05102014_notarized_notes_scan_higher_res.pdf; nachlass/collected_dew_materials/05102014_notarized_notes_scan_low_res.pdf | 6 | witness | D | 2014 notarized notes (SJAS-era priority claims); content review at C12 | pending |
| dew-hajek-correspondence | nachlass/collected_dew_materials/Correspondence_Hajek.pdf | 9 | witness | D | Correspondence with Hajek (poor scan quality; page images authoritative) | pending |
| dew-2025-boundary-draft | nachlass/collected_dew_materials/2025-12-22 09.55.35.pdf | 28 | witness | D | Boundary-case incompleteness draft (frontier) | pending |
| dew-2025-tab-xtab-notes | nachlass/collected_dew_materials/2025-12-22 09.58.14.pdf | 9 | witness | D | Tab/Xtab deduction-method notes (frontier; bears on apparatus identity) | pending |
| dew-2025-hilbert-draft | nachlass/collected_dew_materials/2025-12-24 14.44.42.pdf | 8 | witness | D | Hilbert consistency-program draft (frontier) | pending |
| dew-1991-nsf-report | nachlass/collected_dew_materials/NSF_1991_Report.pdf | 3 | witness | D | NSF reporting from the SJAS gestation period; historical arc | pending |

## Out of corpus

| Key | Witness | Pages | Ruling | Tier | Rationale | Extraction |
| --- | --- | --- | --- | --- | --- | --- |
| Willard2000-SICOMP | nachlass/papers/willard2000_fusion_tree_perspective_sicomp.pdf | 30 | out | - | Data structures (fusion-tree perspective) | n/a |
| Willard2002b | nachlass/papers/willard2002_relational_calculus_queries_author_jcss.pdf | 44 | out | - | Databases (relational calculus queries) | n/a |
| DS-DB-line | nachlass/papers/WillardFredman_SUNYarchive.pdf; nachlass/collected_dew_materials/Notes_Phd1.pdf; nachlass/collected_dew_materials/Notes_Thetheoryofcomputationaldynamics.pdf | - | out | - | The data-structures/databases line collectively: paperlist 1978-1996 entries, DBLP conference line (STOC/FOCS/SODA/SIGMOD/ICALP/STACS/SCG/PODS), Fredman-Willard fusion-tree JCSS papers, dissertation-era notes | n/a |
| Biology-line | nachlass/papers/willard2017_trivers_willard_avian_arxiv_1707.00039.pdf; nachlass/collected_dew_materials/2025-12-24 14.07.32.pdf | - | out | - | Trivers-Willard biology line (1973 Science; 2017 avian; 2025 scan) | n/a |
| dew-2014-exam | nachlass/collected_dew_materials/Exam_2014.pdf | 6 | out | - | Teaching exam; no SJAS conceptual content beyond course delivery (revisitable if C12 review contradicts) | n/a |
| dew-2008-zcf-alias | nachlass/collected_dew_materials/DOC102924-10292024193836.pdf | 8 | out | - | Exact duplicate alias of the Mar-18 ZCF file (collected_dew_materials/catalog/duplicates.md) | n/a |

## Micro-item classes (accepted gaps)

| Key | Witness | Pages | Ruling | Tier | Rationale | Extraction |
| --- | --- | --- | --- | --- | --- | --- |
| ASL-2005-TR | (none) | - | gap | C | Unlocated University-of-Albany TR behind JSL 2005 ref. [71] (floating-point line); estate search | blocked |
| TABLEAUX-2003-position | (none) | - | gap | - | Existence unconfirmed by any queried source (2026-08-20 coverage pass); prior audit lists it metadata-only | blocked |
| BSL-abstracts-class | (none) | - | gap-accepted | - | ~25 ASL-talk 300-word abstracts, BSL 1995-2012, per resume; summarize published results; enumerate lazily on citation | accepted |
| StPetersburg-2003-abstract | (none) | - | gap-accepted | - | 200-word abstract of the forthcoming Willard2006a (JSL 2005 ref. [72]); Atlas/Wayback lookup only if cited | accepted |
