# Nachlass Log

## 2026-08-21 - Pilot Extraction of Willard 2005 and Fidelity Gate (component C4)

- Completed the pilot extraction of `Willard2005` (JSL 70, pp. 1171-1209, the
  engine paper) per the charter template:
  `codification/extraction/willard2005.md`. Full sequential read of all 43
  pages; 26 numbered items inventoried (Definitions 1-6, Theorems 1-6 plus the
  cited Theorem *, Lemmas 1-4, Corollary 1, Remarks 1-8), each with page
  anchor, statement digest, proof status, and dependencies.
- Registries populated: 26 results rows, 4 systems rows (`IS_D(A)`,
  `NS^{k,m}_D(A)`, `N_D(A,Psi)`, `PA+`), 27 notation rows. `audit.sh` extended
  to report registry sizes and proof-status counts; verified red-green against
  a deliberately corrupted table (bad enum and unknown corpus key both caught).
- **Proof-status profile of the paper**: 10 full, 1 sketch (Lemma 3), 1 cited
  (Theorem *), **6 stated-only**, 8 definitional. The stated-only set is
  load-bearing: Remark 1 (cut-free sequent calculus, Herbrand, Tab-Q\*1-List
  and Q\*1-restricted Hilbert all preserve consistency - "we will not have the
  page space"), Remark 6 (tangibility hybridization), Remark 8 (three stronger
  forms of the negative result). Gap G8 widened accordingly.
- **Fidelity gate passed** (`extraction/willard2005-fidelity-check.md`).
  Agreement with both known-good re-derivations - the affine-tree design
  document sections 2-3 and the 2026-07-27 apparatus survey - on every
  load-bearing definition, theorem statement, and proof structure, including
  the nine-case induction in Lemma 1 and the verbatim sense of Remark 4.
- **Divergence found in the affine-tree re-derivation** (drift ledger D6):
  coding density bounds the compactification exponent theta from *below*
  (`theta >= 1/d` via Lemma 1's hypothesis, footnote 5), while Definition 5
  bounds it from above (`theta < 1/3`); Willard proves `theta = 1/4` with
  density 5, i.e. selects a value inside the window `[1/5, 1/3)`. The ADR's
  "theta <= 1/5" inverts the inequality and the design document's "theta =
  1/c" collapses the window to a point; the design document states the
  relation correctly in section 8 and incorrectly in sections 3-4. The design
  conclusions survive (density 5 gives a non-empty window; "density 3 is the
  theta = 1/3 boundary" is correct as the `d > 3` condition), but the stated
  derivation does not. Also recorded: three distinct constants must not be
  conflated - 6 bits per language symbol (Appendix A), the constant 5 in
  Eq. (20) bounding U-Height, and theta = 1/4 (drift ledger D3).
- Other drift entries seeded: D2 (`Willard2006a` cited under a pre-publication
  title naming the Hilbert-styled framing), D4 (the Solovay April-1994
  private-communication thread, unpublished Theorem *, expositions only by
  Willard - the same thread as `Willard2020-LFCS` ref. 41), D5 (`IS(A)` vs
  `IS_D(A)`).
- Gap G3 provenance confirmed: Remark 7 identifies the unlocated ASL-2005
  technical report as the only full-length source for the floating-point
  result in which `IS_D(A)` recognizes addition, multiplication, subtraction
  and division as total over computer reals.
- **Corpus-wide extraction hazard discovered**: `pdftotext` renders JSL
  small-caps headings with a space after the initial letter (`D EFINITION 4.`,
  `T HEOREM 1.`), so a naive `^Definition` grep matches only prose
  cross-references and silently misses every real heading. Reproducible sweep
  and warning recorded in the extraction record section 8; affects
  `Willard2001`, `Willard2002a`, `Willard2006b`, and other venues must be
  probed per-publisher.
- Template verdict: no change required. The eight-section template carried the
  paper without strain, and its sections 6 and 7 are what surfaced the drift
  entries.

## 2026-08-20 - Codification Charter, Scaffold, and Corpus Ruling (component C3)

- Wrote the charter ADR (`codification/ADR-0001-codification-charter.md`):
  layered method (corpus → extraction → concordance → synthesis), fixed
  extraction template, component roadmap C1–C16 with status, acceptance
  criteria A1–A7 and sufficiency tests T1–T5, and the standing user
  directions (nachlass-scoped, bash tooling, human-readable registries,
  throughline test, obligatory checkpoints).
- Completed the corpus ruling pass (`codification/registry/corpus.md`):
  45 rows — 19 core (pilot Willard2005; origin TR 93-10 + KGC 1993; spine
  2011/2001/2020; tiers A/B/C), 5 motivation, 9 witness-grade (nachlass +
  resume), 6 out (data-structures/database line, biology line, teaching
  exam, duplicate alias), 4 gaps (Willard1997, Willard2021, ASL-2005 TR,
  TABLEAUX-2003 unconfirmed), 2 accepted micro-item classes (BSL abstracts,
  St. Petersburg abstract). Every in/out ruling carries a throughline
  rationale.
- Seeded the registries (`systems.md`, `results.md`, `notation.md` schemas;
  `gaps.md` populated G1–G13) and the concordance drift ledger (D1: TR 93-10
  vs "March 1994" citation identity).
- Added `codification/audit.sh` (portable bash+awk): parses the registry
  tables; checks key uniqueness, enum validity, ruling–extraction
  consistency, witness-file existence, and cross-registry key references.
  Green on the seeded state (45 corpus rows, 13 gap rows).
- Added `codification/regen-sources-text.sh` and generated
  `codification/sources-text/`: 25 plain-text extractions (~336K words) of
  the text-bearing in-corpus witnesses, as grep/anchor aids. Excluded by
  design: the image-only DIMACS volume (gap G10), the LNCS 11972 volume
  (chapter extract suffices), and nachlass scans (OCR lives under
  `collected_dew_materials/ocr/`).
- Added `codification/README.md` as the layer map / entry point.

## 2026-08-20 - LFCS 2020 Volume Acquisition and Chapter Witness (C2 follow-up)

- The user downloaded the dokumen.pub upload of the LFCS 2020 proceedings by
  browser (automation was Cloudflare-blocked; see the coverage report
  addendum). Verified as the genuine Springer LNCS 11972 volume (297 pp.,
  Artemov–Nerode eds., Acrobat-Distiller-produced).
- Renamed to `papers/willard2020_tender_line_lfcs2020_lncs11972_volume.pdf`
  and extracted the Willard chapter, "On the Tender Line Separating
  Generalizations and Boundary-Case Exceptions for the Second Incompleteness
  Theorem Under Semantic Tableaux Deduction", LNCS 11972 pp. 268–286
  (pdf-pages 278–296), DOI `10.1007/978-3-030-36755-8_17`, as
  `papers/willard2020_tender_line_lfcs2020_lncs11972_chapter.pdf`
  (ghostscript page-range extraction, 19 pp., 8,945 words searchable; the
  pdfseparate+pdfunite route duplicated shared resources to 12.4 MB and was
  discarded).
- Cataloged: `papers/SHA256SUMS` entries for volume and chapter
  (`sha256sum -c` clean), `papers/README.md` witness rows and gap-note
  update, `paperlist` entries `[36-LFCS]`/`Willard2020-LFCS` (witnessed) and
  `[37]`/`Willard2021` (metadata-only; the JLC 31(1) 2021 journal version
  remains the sole unacquired terminal item — no OA, no known shadow copy).
- Observed for later extraction: the chapter's ref. 41 cites private
  April-1994 telephone conversations between Willard and Robert M. Solovay.

## 2026-08-20 - Willard Literature Coverage Verification (SJAS Codification, component C2)

- Completed the precautionary web coverage pass over arXiv (author API with
  version numbers), DBLP (full 1978–2021 record), targeted web searches,
  Willard's own September-2015 résumé (`lit/dewresume.pdf`), and an internal
  sweep of every technical-report and BSL-abstract mention in the witnessed
  corpus. Full report: `codification/coverage-report-2026-08-20.md`.
- **Two escapes found**, Willard's terminal publication pair, both
  unwitnessed: the LFCS 2020 chapter "On the Tender Line Separating
  Generalizations and Boundary-Case Exceptions for the Second Incompleteness
  Theorem Under Semantic Tableaux Deduction" and its journal version in the
  Journal of Logic and Computation 31(1) (2021), pp. 375–392. Both actioned
  as acquisitions (no legal OA located; shadow-library copies excluded per
  the 2026-05-10 pass precedent). The JLC paper may bear on the apparatus
  audit's unprinted resolution-stability-proof finding.
- **Closed a recorded gap from local holdings**: `lit/danwillard1993.pdf` is
  the published 12-page KGC 1993 chapter itself; symlinked into `papers/` as
  `willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf`, checksummed,
  README rows amended. `Willard1993` is now doubly witnessed (published
  chapter + TR 93-10 full-length origin).
- All six arXiv witnesses verified to be the latest versions (1108.6330v8,
  1307.0150v2, 1612.08071v5, 1707.00039v2, 1807.04717v2, 2006.01057v1).
- Technical-report class bounded: TR 93-10 identified with JSL 2001 ref.
  [41]'s "March 1994, 50-page" citation (same artifact, loose description;
  drift-ledger note); one unlocated companion TR behind JSL 2005 ref. [71]
  (ASL-2005 Athens floating-point talk; published successors witnessed).
- Micro-item classes recorded with ruling proposals (~25 BSL 300-word ASL
  abstracts 1995–2012; the 2003 St. Petersburg 200-word abstract): enumerate
  lazily, do not chase. TABLEAUX 2003 position paper remains metadata-only
  and unconfirmed by any queried source.
- Biographical anchor recorded: Willard 1948-09-19 – 2023-01-21.

## 2026-08-20 - TR 93-10 Searchable Collation (SJAS Codification, component C1)

- First component of the SJAS Codification stage (`prose/sjas_synthesis.txt`;
  plan approved 2026-08-20; branch `sjas-codification`). All codification work
  is conducted within `nachlass/`; audit tooling is portable bash/CLI; user
  feedback is collected after each definite component until autonomous
  execution is authorized.
- Collated `papers/1993technicalreport/tr1993-{0,1,2}.pdf` (three image-only
  scan parts, 61 pages) into
  `papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf`:
  SUNY-Albany TR 93-10, "Self-Verifying Axiom Systems and the Incompleteness
  Theorem" — the full-length original of the published `Willard1993` KGC
  chapter, per its own preface page. Foundational witness for Codification.
- Pipeline (present tools only): pdftoppm 2550px grayscale renders → ImageMagick
  JPEG q55 → tesseract 5.3.4 per-page `pdf txt` at `--dpi 300` → pdfunite in
  scan order. Letter-size output, ~23.6 MB, aligned invisible text layer
  (searchable/highlightable), 24,221 words extracted.
- Verified: part order 0→1→2 with seam continuity (printed pp. 25→26, 41→42);
  printed-page completeness by OCR header sweep (gap-free lattices: part 0 =
  title + preface + pp. 1–25, part 1 = pp. 26–41, part 2 = pp. 42–55 +
  Figures 1–4); per-page PDF integrity via `pdfinfo` (one page damaged by an
  interrupted first OCR run was detected and regenerated before uniting);
  `pdftotext` content spot checks at collated pp. 1/28/44; `-bbox`
  word-coordinate alignment check. OCR layer is a search aid; page images
  remain authoritative for formulas.
- Recorded: `papers/SHA256SUMS` entries for the three parts and the collation
  (`sha256sum -c` clean over the full file), `papers/README.md` witness row
  and gap-note amendment, `paperlist` entry `[16-TR]`, and
  `papers/1993technicalreport/README.md` (provenance, structure map,
  reproducible pipeline).
- Checkpoint follow-up (same day): the first collation left part-2 pages at
  1569×2033–2049 pt versus letter for parts 0/1 — tesseract honors JFIF
  density metadata over `--dpi`, and part 2's low effective density (~117 dpi)
  reproduced its oversized, per-page-varying source dimensions. Regularized by
  aspect-preserving fit + white-pad of the 18 part-2 renders to exactly
  2550×3300 px with declared 300 dpi, re-OCR, and re-collation. All 61 pages
  now verify uniformly 612×792 pt; content, word count (24,221), and
  text-layer alignment re-verified; collation checksum replaced in
  `papers/SHA256SUMS` (sources unchanged).

## 2026-07-28 - Willard Deductive-Apparatus Audit

- Completed a corpus-controlled review of Willard's published logic papers,
  preprints, talks, reachable repository history, bibliographic inventory, and
  all 17 distinct high-fidelity nachlass OCR targets for resolution, sequent
  calculus, and other candidate SJAS deductive apparatuses. The full
  occurrence ledger and implementation analysis is recorded in Proflog commit
  `a7af9f7`.
- Resolution is an affirmative SJAS candidate in two distinct author-stated
  forms: the 2011 Skolemizing `xi_R`/`Level(0R)` route and the 2020 direct
  Level-1 `ISRes` route. The latter has a sharp negative control:
  LEM-as-logical-axioms changes the apparatus to `Xres`, and `ISXres` is not
  consistency-preserving.
- First-order cut-free sequent calculus is also affirmative for the
  total-addition/no-total-multiplication profile. Gentzen sequents with
  unrestricted cuts belong to a separate Hilbert-like, no-total-addition
  profile and cannot be substituted into the cut-free result.
- Any implementation must expose and arithmetize the exact proof objects of
  the selected apparatus, regenerate system identity and `SelfCons` from that
  proof predicate, and discharge its proof-growth invariant. A tableau proof
  predicate cannot be relabeled as resolution or sequent deduction.
- The audit retains all false positives, duplicate witnesses, and exact
  publication-version gaps. In particular, the TABLEAUX 2003 position paper
  remains metadata-only, and the accessible papers state but do not print a
  complete resolution-specific stability or compactification proof.

## 2026-05-21 - Collected DEW Materials OCR Pass

- Completed first OCR, assessment, and organization pass for
  `nachlass/collected_dew_materials/`: 19 original PDF scans (225 pages),
  18 unique OCR targets (one exact duplicate alias skipped).
- Added inventory artifacts: `SHA256SUMS`, `manifest.tsv`, `README.md`,
  `catalog/duplicates.md`, topic indexes under `catalog/by-topic/`, and
  reproducible scripts `scripts/ocr_dew_materials.sh` and
  `scripts/update_manifest_from_qa.sh`.
- OCR pipeline: `pdftoppm` at 200dpi, PIL downscale to 850px grayscale,
  `tesseract` (eng, PSM 6 with PSM 3 retry), `ocrad` fallback on low-yield
  pages. Merged text under `ocr/text/` with per-document `*.qa.tsv` QA files.
- Quality outcome: tesseract produced little text on most typewriter/fax-era
  scans; ocrad fallback dominates. Sixteen documents rated `needs_review`,
  `Correspondence_Hajek.pdf` rated `poor`, duplicate alias skipped. The 2020
  incompleteness notebook and ZCF drafts are partially searchable but not
  authoritative for formulas.
- Classified Dec 2025 scans: boundary-case incompleteness draft (Dec 22 a),
  Tab/Xtab deduction notes (Dec 22 b), Trivers-Willard biology article
  (Dec 24 a), Hilbert consistency-program draft (Dec 24 b).
- See [`collected_dew_materials/README.md`](collected_dew_materials/README.md).

## 2026-05-21 - Hi-Fi Re-OCR (600 DPI / formula fidelity)

- Added `--hifi` profile to `scripts/ocr_dew_materials.sh` and wrapper
  `scripts/ocr_dew_materials_hifi.sh` (exclusive flock lock).
- Settings: 600 DPI render, grayscale resize to 2550px max width,
  tesseract PSM 4 (fallbacks 3/1/6 only when primary yields little text),
  7200s per-pass timeout, TSV confidence QA. Output under `ocr/hifi/`.
- Pilot on `ZFnote.pdf` at 2550px: ~42 min/page, excellent formula text
  (`Decipher`, `ENUM`, `Support-ZFC`, etc.) vs garbage from the fast pass.
- Root cause of empty pilot: prior 900s timeout killed tesseract mid-run;
  concurrent tesseract jobs also starve each other — batch must run serially.
- Second bug: script used `$LANG` for tesseract `-l`, clobbering the locale
  (`en_US.UTF-8`); renamed to `TESS_LANG` (defaults `eng`).
- Preprocessing: grayscale resize only at hi-fi width (autocontrast/sharpen
  disabled — it did not improve tesseract on these scans).
- `update_manifest_from_qa.sh --hifi` reads `ocr/hifi/text/*.qa.tsv`.
- Full hi-fi batch completed 2026-05-22: 17/17 documents, ~225 pages under
  `ocr/hifi/text/`; manifest refreshed from hi-fi QA.
- Limitations unchanged: `eng` only (no `equ` math pack); OCR remains a search
  aid, not an authoritative transcription.


- Logged the Proflog completion audit for the finite ordinary-tableau
  `IS#_D(beta)` substrate. The audited scope now includes arithmetized
  formula/system/proof codes, structural syntax predicates, Level-1
  substitution-proof vocabulary, fixed-point substitution, structural
  theorem-code proof targets, and passing slow/fast/extended gates. The
  remaining non-goals are Tab-1/proof-list theorem reuse, general non-identity
  substitution beyond the generated fixed-point entry, and open proof-code
  synthesis.
- Logged the Proflog ADR-0068 follow-up: `tableau-proof/3` and `subst-prf/4`
  now build proof targets from structurally decoded theorem-code bytes when the
  theorem code is not part of the generated Group axiom registry. The promoted
  example is `lt(1,2)`: Proflog proves it through the SJAS arithmetic profile,
  encodes that theorem as a compact formula code, and then checks the supplied
  proof certificate against the decoded theorem target. Both proof predicates
  reject the same certificate when the theorem code is changed to `lt(2,1)`.
  The remaining implementation boundary is proof-list/Tab-1 theorem reuse over
  code terms alone; Proflog still validates decoded targets by calling its core
  tableau kernel.
- Logged the Proflog ADR-0067 follow-up: the Willard SJAS profile now parses
  formula-code byte streams structurally for `wff/1`, formula-class predicates,
  `neg-pair/2`, and identity `subst-code/2`. The red characterization used the
  code for `lt(1,2)`, a valid formula in the active SJAS language that was not
  generated as a Group axiom. Before the decoder, `wff`, `delta-star-0-code`,
  `neg-pair`, and identity `subst-code` all failed because the code was absent
  from the finite generated formula registry. The remaining Proflog boundary is
  that `tableau-proof/3` still bridges arbitrary theorem codes to kernel AST
  formulas instead of checking every theorem formula wholly at the code level.
- Logged the ADR-0066 follow-up: Proflog now exposes the finite generated
  substitution boundary as `subst-code/2`, separating Willard's `Subst(g,h)`
  relation from `SubstPrf(g,t,p)`. The active implementation still generates
  the relation for one finite `IS#_D(beta)` system rather than parsing arbitrary
  formula codes, but `subst-prf/4` no longer couples the substitution code to
  the theorem code being proved.
- Logged the ADR-0065 follow-up: Proflog's Level-1 `SelfCons1` construction now
  follows Willard 2011 Appendix A's fixed-point shape by generating a skeleton
  `Gamma_1(g)`, encoding that skeleton, and using the skeleton code as the
  `subst-prf/4` substitution argument in the final Group-3 sentence. The
  implementation remains a finite `IS#_D(beta)` substrate: it adds the required
  skeleton-to-Group-3 substitution entry and an object-level `sjas-axiom`
  certificate checked through generated `axiom-member/2`, while still leaving a
  general arbitrary-code `Subst` relation for later work.
- Logged the follow-up Proflog implementation boundary after ADR-0063: a
  `tableau-proof/3` predicate over code terms is necessary but not sufficient for
  Willard Level-1 self-justification. The `SelfCons_k(beta,d)` formulation also
  needs substitution-aware proof vocabulary. Proflog ADR-0064 therefore adds
  `subst-prf/4` and changes generated `SelfCons1` to cite it, while explicitly
  retaining the remaining gap that a general code-level `Subst` relation is not
  yet implemented. The focused SJAS and regression gates passed for ADR-0064.
- Logged the Proflog implementation boundary exposed after ADR-0062:
  hash-derived formula symbols can serve as finite generated codebook labels,
  but they are not Willard-style arithmetic Godel codes and cannot support a
  faithful object-language `tableau-proof` predicate by themselves.
- The next Proflog ADR must review Willard's own descriptions of syntax and
  semantic-tableau proof coding, identify compatible coding options, and replace
  or supplement host-side opaque labels with inspectable arithmetized codes for
  formulas, proof objects, complement relations, formula classes, substitution,
  and tableau proof checking.
- Completed the first Proflog-side research pass over local Willard witnesses
  and public arXiv records. The key citation is Willard 2011, Definition D.1,
  part iv: the Godelized method for encoding semantic-tableau proofs may be
  essentially any natural method satisfying a lower bound of at least `5J` bits
  for a proof with `J` function symbols; footnote 23 reformulates this as a
  Godel number at least `32^J`. This rules out fixed-width hashes as faithful
  formal codes.
- Proflog ADR-0063 implemented the replacement boundary using compact base-64
  code terms `(code-N b0 ... bN-1)`, matching the byte/base-64 direction in the
  Willard witnesses while avoiding a deep binary numeral stack overflow. The
  implementation still represents a finite `IS#_D(beta)` executable substrate:
  generated decode tables make codes inspectable to `tableau-proof/3`, but full
  arbitrary-code Delta-0 parsing/substitution remains a later fidelity step.

## 2026-05-10 - Willard Public-Witness Aggregation

- Completed a public-Internet aggregation pass for the Willard bibliography in
  `paperlist`, using arXiv, DBLP, OpenAlex, Crossref/DOI metadata, publisher
  landing pages, archived SUNY Albany author PDFs, and other public repositories.
- Added full-text witnesses under `papers/` where public copies were reachable,
  including author-side archive copies for many SJAS and second-incompleteness
  papers that were otherwise behind publisher controls.
- Added `papers/README.md` to map paperlist keys to local witnesses, public
  sources, and known public-full-text gaps.
- Added `papers/SHA256SUMS` and verified every listed local paper witness with
  `sha256sum -c`.
- Reviewed `works-citing-dew/` for second-order material citing Willard's work
  specifically on self-verifying/self-justifying axiom systems and related
  incompleteness-boundary logic, excluding biology and data-structure
  second-order material.
- Added public full-text witnesses for SJAS-relevant citing works by Salehi,
  Artemov, Beklemishev/Shamkanov, Cheng, Chow, Dvorkin, Pakhomov, Kant et al.,
  Sebti, Visser, Yudkowsky/Herreshoff, and related existing items.
- Captured Penchev's public WordPress page corresponding to the existing
  `Penchev_V` note and slide witness.
- Added `works-citing-dew/README.md` documenting inclusion criteria, archived
  public witnesses, excluded false positives, and blocked/non-OA citing records.
- Added `works-citing-dew/openalex-oa-citer-records.tsv` to preserve the
  OpenAlex OA/public-URL candidate set used during triage.
- Added `works-citing-dew/SHA256SUMS` and verified every listed second-order
  witness with `sha256sum -c`.
- Verified all archived PDFs with `pdfinfo`; all were readable.
- Added `.gitattributes` in the SJAS repo so PDF/PPTX/HTML archive payloads are
  treated as binary by Git.
- Committed and pushed the SJAS archive update as
  `dffbc38 Aggregate Willard SJAS paper witnesses`.
- In the parent Proflog repository only, added an ignore rule for `sjas/` and
  pushed it as `0801cb1 Ignore nested sjas repository`, so the nested SJAS clone
  is not uploaded twice by Proflog.
