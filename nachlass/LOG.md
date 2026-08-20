# Nachlass Log

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
