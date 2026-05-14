# Nachlass Log

## 2026-05-14 - Proflog SJAS Coding Boundary

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
