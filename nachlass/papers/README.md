# Dan Willard Paperlist Witnesses

This directory aggregates public-Internet witnesses for the entries in
`../paperlist`. The search pass used arXiv, DBLP, OpenAlex, Crossref, DOI
landing pages, publisher pages, and archived copies of Willard's former SUNY
Albany author pages at `www.cs.albany.edu/~dew/m/`.

The archived SUNY author copies are especially useful because several publisher
PDF endpoints are blocked or paywalled, while the Wayback copies preserve
author-side PDFs that state their corresponding journal or proceedings venues.
This pass intentionally does not add Sci-Hub material as a new witness.

`SHA256SUMS` records checksums for local PDF witnesses, following symlinks.

## Local Full-Text Witnesses

| Paperlist key | Local witness | Public source |
| --- | --- | --- |
| Willard2020 | `willard2020.pdf` | arXiv `2006.01057` |
| Willard2018 | `willard2018.pdf` | arXiv `1807.04717` |
| Willard2017 | `willard2017_trivers_willard_avian_arxiv_1707.00039.pdf` | arXiv `1707.00039` |
| Willard2016 | `willard2016_theta_function_symbol_arxiv_1612.08071.pdf` | arXiv `1612.08071` |
| Willard2014 | `willard2014.pdf` | Springer LNCS chapter copy already present |
| Willard2014 | `willard2014_broader_epistemological_author_archive.pdf` | archived SUNY author copy `broader.pdf` |
| Willard2011 | `willard2011_self_justifying_logics_arxiv_1108.6330.pdf` | arXiv `1108.6330` |
| Willard2009 | `willard2009_i_sigma0_herbrand_author_inf9.pdf` | archived SUNY author copy `inf9.pdf` |
| Willard2007 | `willard2007_passive_induction_author_apal7.pdf` | archived SUNY author copy `apal7.pdf` |
| Willard2006b | `willard2006_real_valued_arithmetic_author_jsl6.pdf` | archived SUNY author copy `jsl6.pdf` |
| Willard2006a | `willard2006_generalization_second_incompleteness_author_apal6.pdf` | archived SUNY author copy `apal6.pdf` |
| Willard2006/WoLLIC | `willard2006_i_sigma0_herbrand_author_wollic.pdf` | archived SUNY author copy `wollic.pdf` |
| Willard2005 | `willard2005_addition_total_consistency_author_jsl5.pdf` | archived SUNY author copy `jsl5.pdf` |
| Willard2005/Tableaux | `willard2005_real_valued_tableaux_author_tab5.pdf` | archived SUNY author copy `tab5.pdf` |
| Willard2004 | `willard2004_addition_not_multiplication_fol75.pdf` | archived SUNY author copy `fol75.pdf` |
| Willard2002a | `willard2002_semantic_tableaux_robinson_q_author_jsl2.pdf` | archived SUNY author copy `jsl2.pdf` |
| Willard2002b | `willard2002_relational_calculus_queries_author_jcss.pdf` | archived SUNY author copy `jcss.pdf` |
| Willard2002c | `willard2002_new_exceptions_tableaux_author_tab2.pdf` | archived SUNY author copy `tab2.pdf` |
| Willard2001 | `willard2001_self_verifying_axiom_systems_author_jsl1.pdf` | archived SUNY author copy `jsl1.pdf` |
| Willard2000 logic | `willard2000_tableaux_robinson_q_author_tab2000.pdf` | archived SUNY author copy `tab2000.pdf` |
| Willard2000 data structures | `willard2000_fusion_tree_perspective_sicomp.pdf` | archived SUNY author copy `sicomp.pdf` |
| Willard1998 | `willard1998_self_reflection_np_hardness_dimacs_volume.pdf` | symlink to existing `../../lit/Dimacs1996.pdf` volume witness |
| Willard1993 | `willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf` | symlink to existing `../../lit/danwillard1993.pdf` (published 12-page KGC chapter; reconciled 2026-08-20 coverage pass) |
| Willard1994 | `WillardFredman_SUNYarchive.pdf` | existing SUNY archive scan; not OCR text |

Additional related Willard witnesses that are not direct paperlist rows:

| Work | Local witness | Public source |
| --- | --- | --- |
| Self-Verifying Axiom Systems and the Incompleteness Theorem, SUNY-Albany TR 93-10 (full-length original of the `Willard1993` KGC chapter) | `1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf` (searchable collation of scan parts `tr1993-0/1/2.pdf`; see [`1993technicalreport/README.md`](1993technicalreport/README.md)) | private scan of the original report; not publicly hosted |
| On the Results of a 14-Year Effort to Generalize Godel's Second Incompleteness Theorem and Explore Its Partial Exceptions | `willard2007_fourteen_year_effort_author_kgs6.pdf` | archived SUNY author copy `kgs6.pdf` |
| On the Nature of Godel's Second Incompleteness Theorem | `willard2006_nature_godel_second_incompleteness_talk.pdf` | Dartmouth logic seminar PDF |
| On the Significance of Self-Justifying Axiom Systems from the Perspective of Analytic Tableaux | `willard2013_significance_self_justifying_axiom_systems_arxiv_1307.0150.pdf` | arXiv `1307.0150` |

## Notes And Gaps

- `willard2017.pdf`, already present before this pass, is another copy of arXiv
  `1612.08071`, not the Trivers-Willard avian paper listed under
  `Willard2017`. The correctly matched biology file is now stored as
  `willard2017_trivers_willard_avian_arxiv_1707.00039.pdf`.
- A public full-text witness was not located for the paperlist's
  `Willard1997` Springer KGC chapter ("The Tangibility Reflection Principle
  for Self-Verifying Axiom Systems", LNCS 1289, pp. 319-334) through legal
  public sources. Its DOI/DBLP metadata remains public, but no author-side
  PDF was found in the SUNY archive crawl.
- `Willard1993` is fully witnessed as of the 2026-08-20 coverage pass: the
  published 12-page KGC chapter via the `lit/danwillard1993.pdf` symlink
  above, and the full-length origin text via SUNY-Albany TR 93-10
  (`1993technicalreport/`, searchable collation added 2026-08-20), whose
  preface states the KGC chapter is its 12-page abbreviated version. JSL 2001
  reference [41] cites the same report as "SUNY-Albany Technical Report,
  March 1994" (50 pages) — treated as the same artifact under a loose
  date/page description pending contrary evidence. The LNCS 713 volume also
  exists in `lit/` as DJVU.
- Post-2020 coverage gap found by the 2026-08-20 pass (see
  `../codification/coverage-report-2026-08-20.md`): the LFCS 2020 conference
  paper "On the Tender Line Separating Generalizations and Boundary-Case
  Exceptions for the Second Incompleteness Theorem Under Semantic Tableaux
  Deduction" and its journal version, "About the characterization of a fine
  line that separates generalizations and boundary-case exceptions for the
  Second Incompleteness Theorem under semantic tableau deduction", Journal of
  Logic and Computation 31(1) (2021), pp. 375-392, have no local witnesses
  and no located legal open-access copies yet.
- Public full-text witnesses were not located for the older data-structure and
  database entries `Willard1996`, `Willard1992`, `Willard1991`, `Willard1990`,
  `Willard1989a/b`, `Willard1987`, `Willard1986`, `Willard1985a/b`,
  `Willard1984`, `Willard1983a/b`, `Willard1982a/b`, or the 1978 Garland
  dissertation/book entry, except where an existing repository file or author
  copy is listed above. Many have DOI or DBLP landing pages, but curl/browser
  access to public PDFs was blocked, paywalled, or unavailable during this pass.
- The paperlist has duplicate numeric labels and one title/date mismatch. This
  README treats the bracket keys, rather than the numeric labels, as canonical.
