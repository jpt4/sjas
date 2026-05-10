# Works Citing Willard On SJAS

This directory is the second-order archive: works by other authors that cite
Dan E. Willard's self-verifying, self-justifying, and incompleteness-boundary
logic work. It intentionally excludes second-order archiving for Willard's
biology and data-structure papers.

The May 2026 pass used OpenAlex cited-by records for Willard's SJAS-related
works, direct web search, arXiv, publisher and repository landing pages, and
manual full-text checks for `Willard`, `self-verifying`, and `self-justifying`.
`openalex-oa-citer-records.tsv` keeps the OpenAlex OA/public-URL candidate
set used during triage, including rows that were intentionally excluded as
self-citations, false positives, or blocked endpoints. `SHA256SUMS` records
checksums for PDFs, PPTX, and captured HTML files.

## Archived Public Full Text

| Work | Local witness | Why included |
| --- | --- | --- |
| Salehi, "Herbrand consistency of some arithmetical theories" | `Salehi_S/salehi2010_herbrand_consistency_some_arithmetical_theories_arxiv_1005.2654.pdf` | Cites Willard's semantic-tableaux/Robinson-Q work. |
| Salehi, "Herbrand consistency of some finite fragments of bounded arithmetical theories" | `Salehi_S/salehi2012_herbrand_consistency_finite_fragments_arxiv_1110.1848.pdf` | Cites Willard's semantic-tableaux/Robinson-Q work. |
| Artemov, "The Provability of Consistency" | `Artemov_S/artemov2019_provability_of_consistency_arxiv_1902.07404.pdf` | Discusses provability of consistency and cites Willard. |
| Beklemishev and Shamkanov, "Some abstract versions of Godel's second incompleteness theorem based on non-classical logics" | `Beklemishev_Shamkanov/beklemishev_shamkanov2016_abstract_versions_godel_second_incompleteness_arxiv_1602.05728.pdf` | Cites Willard's generalizations and boundary cases. |
| Cheng, "Godel's incompleteness theorem and the Anti-Mechanist Argument: revisited" | `Cheng_Y/cheng2019_godel_incompleteness_antimechanist_argument_arxiv_1902.05902.pdf` | Cites Willard's generalizations and boundary cases. |
| Chow, "The Consistency of Arithmetic" | `Chow_TY/chow2018_consistency_of_arithmetic_arxiv_1807.05641.pdf` | Uses Willard as an example of theories proving consistency-like statements. |
| Pakhomov, "A weak set theory that proves its own consistency" | `Pakhomov_F/pakhomov2019_weak_set_theory_proves_own_consistency_arxiv_1907.00877.pdf` | Explicitly compares against Willard-style examples. |
| Visser, "On the Depth of Godel's Incompleteness Theorems" | `Visser_A/visser2020_depth_of_godel_incompleteness_arxiv_2008.13142.pdf` | Cites Willard while surveying depth and limits of incompleteness. |
| Visser, "Current Research on Godel's Incompleteness Theorems" | `Visser_A/visser2021_current_research_godel_incompleteness_arxiv_2009.04887.pdf` | Survey treatment citing Willard's boundary examples. |
| Dvorkin, "On provability logics of Niebergall arithmetic" | `Dvorkin_LV/dvorkin2024_provability_logics_niebergall_arithmetic_mathnet.pdf` | Discusses theories proving self-consistency and cites Willard 2001. |
| Yudkowsky and Herreshoff, "Tiling Agents for Self-Modifying AI, and the Lobian Obstacle" | `Yudkowsky_E_Herreshoff_M/yudkowsky_herreshoff2013_tiling_agents_draft.pdf` | AI-safety use of Willard's self-verifying theories. |
| Sebti, "The Status of Mathematical Induction in an Axiomatic System" | `Sebti_R/sebti2014_status_mathematical_induction_axiomatic_system.pdf` | Thesis material citing Willard 2001. |
| Kant, Perez-Escobar, and Sarikaya, "Three Roles of Empirical Information in Philosophy" | `Kant_PerezEscobar_Sarikaya/kant_perez_escobar_sarikaya2021_three_roles_empirical_information_philosophy.pdf` | Philosophical discussion citing Willard 2001 as a precision reference. |
| Cheng, "Exploring the Foundational Significance of Godel's Incompleteness Theorems" | `Cheng_Y/cheng2022_exploring_foundational_significance_godel_incompleteness.pdf` | Survey discussion citing Willard 2001 and 2006. |
| Ganea, "A Remark on a Relational Version of Robinson's Arithmetic Q" | existing `Ganea_M_2015_A_Remark_on_a_Relational_Version_of_Robinsons_Arithmetic_Q/...pdf` | Already present; directly adjacent to Willard's Robinson-Q/tableaux boundary. |
| Penchev, "Dan Willard's Self-verifying theories" | `Penchev_V/dan-willards-self-verifying-theories.wordpress.html` and existing `Penchev_V/dan-willard-1.pptx` | Existing note and slide witness plus captured public blog page. |

## Public Or Cited Records Not Archived As Full Text

- "Polynomially Bounded Recursive Realizability" has an OpenAlex/Project Euclid
  PDF URL and cites Willard's Robinson-Q/tableaux work, but the Project Euclid
  endpoint returned an HTML block instead of a retrievable PDF during this pass.
- "Towards metamathematics of weak arithmetics over fuzzy logic" cites Willard
  2001 in OpenAlex/Crossref records, but no public full-text PDF was found.
- ASL meeting notices, JSL cover/back matter, whole-issue PDFs, and unrelated
  false positives such as administrative-law articles were not archived because
  they are not substantive SJAS-specific secondary literature.
- Willard self-citations are handled in `../papers`; they are not duplicated in
  this second-order directory.
