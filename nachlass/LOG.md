# Nachlass Log

## 2026-08-26 - Extraction of Willard 2001, the JSL Foundation (component C7)

- **Extracted `Willard2001` in full** to
  `nachlass/codification/extraction/willard2001.md` (all 67 witness pages,
  including Appendices A-D and Tables I-II). 41 numbered items inventoried;
  registries now at 167 results / 25 systems / 73 notation rows. `audit.sh`
  green. Added `cor` to the audit's type enum - 2001 contains a genuine
  Corollary (6.2), which the enum did not admit.
- **The witness is not the JSL 2001 text.** Willard's cover note claims
  identity apart from type size, but reference `[45]` cites an APAL **2006**
  paper (cited nowhere in the body) while `[44]` still calls the JSL version
  forthcoming - a partial post-publication bibliography update. Drift **D22**,
  gap **G15**: authoritative for the mathematics, not for JSL pagination.
- **Nine systems, where 2005 and 2011 have far fewer**: `IS(A)`, `IS^lambda(A)`,
  `ISREF(A)`, plus `ISTR(A)` (Bitwise-Or), `ISM(A)`, `ISMULT(A)`,
  `XIS^lambda(PAX)`, `ISREF^U(A)` and `IS^(lambda U)(A)`. "IS" abbreviates
  **Introspective Semantics** - stated only here.
- **The multiplication picture is not what the later papers suggest.**
  `ISM(A)` recognises Multiplication as total and *is* consistency-preserving;
  what it loses is the ability to state "+++" ("I am unable to produce a
  Semantic Tableaux proof of 0=1"), because its Group-3 gap is too wide.
  Meanwhile `XIS^lambda(PAX)` - multiplication *plus* tangibility reflection -
  is inconsistent for every lambda (Theorem 7.3). So multiplication is fatal to
  reflection, not to consistency-preservation. Obligation **O31**.
- **Two apparent drifts dissolved on inspection.** (a) Seven grounding
  functions here versus six in 2005/2011, with opposite roundings on Logarithm
  and Root - but p. 10 says any finite Pi^-_1-axiomatised set of non-growth
  functions works, and Remark C.5 says such extensions still keep Group-3
  Pi^-_1. Presentational (**D23**). (b) The coding-density constants 6, 5, 6/5
  and 32 across four papers are one scheme measured differently: a byte is six
  bits, five of them base-32 payload plus a tag (**D25**, obligation **O33**).
- **The density argument is at its clearest here.** Case 11 of Lemma 4.8: the
  addition axiom's tertiary deduction at most doubles a parameter's bound, and
  encoding the sentence that does it costs at least six bits - exactly one unit
  of the `2^(beta/6)` budget. The budget covers addition and nothing faster.
  Remark 4.5 then localises the multiplication failure to the loss of Lemma 4.2
  alone (`u_(i+1) = u_i^2` gives `2^(2^n)`). Obligation **O30**.
- **Lineage confirmed.** Lemma 3.2 (Pi^-_1 transfers down into the finite model
  `M_i`) with Remark 3.3 (Sigma^-_1/Delta^-_0 transfers up) is the ancestor of
  2005's `Normed(a,b)` and 2011's A-/E-Stable pair; `Phi^i_j` is the ancestor of
  `App-forall/App-exists` and `Scope_E`; the pseudo-formula machinery (club
  placeholders, `PseudoTransform`, `ExSemPrf`) is the ancestor of 2011's
  `SubstPrf`/`Gamma^k(n-bar)`. Willard states that TR 93-10 and the 1993
  abstract "contained intermediate results that were essentially equivalent to
  the Assertion ++", confirming the C5 reading of the TR's Lemma 6.2.
- **Willard forecloses the ideal system.** Section 1 lists seven comparison
  criteria, credits the definable-cut literature with I-IV and his own systems
  with V-VII, and concludes it is "futile to seek an idealized form of
  self-verifying system". Recorded as obligation **O35** - a direct constraint
  on the Refinement stage's ambition, asserted by Willard about his own work.
  Related: tangibility is precisely a definable cut minus closure under
  successor (**O29**), which makes the relation to Nelson/Pudlak/Wilkie-Paris
  exact rather than atmospheric.
- **A claim from the 2026-08-21 discussion is corrected.** I had told the user
  that Willard's size threshold "lives entirely in the metatheory" in contrast
  to Rosser's trick. True of 2005, 2011 and 2001's `IS(A)`; **false** of 2001's
  schematic systems, whose Group-3 axioms carry the size comparison in the
  object language (Eqs. 9, 10, 16, 61). Recorded as drift **D26**; the verbatim
  transcript is left intact with a correction addendum appended. The Rosser
  contrast itself survives in corrected form.
- **New gaps.** G15 (witness/JSL divergence), G16 (five stated-only results,
  including the lambda range the system is *defined* over versus the single
  lambda = 3/4 actually proven, and section 8's computable-variant boundary
  catalogue), G17 (`Willard1997` also holds `ISTR(A)`'s proofs - raising G2's
  priority a second time; note 2001 misprints its venue as the "Third" rather
  than Fifth Kurt Godel Colloquium), G18 (Solovay's full theorem is stronger
  than the printed Appendix A and was never published - accepted).
- Eight further errata recorded in the extraction record section 7, including a
  genuine repeated `<`/`<=` mismatch between the Group-3 pseudo-formulae and
  their instantiations (Eqs. 89/90 and 95/96), image-verified on pp. 54 and 60.
- **Visual control pass** (charter requirement) on pp. 9, 10, 19, 22, 28, 51,
  54, 60. It caught three text-layer corruptions: `pdftotext` rendered the
  fraction `1/3` as `31` (p. 19), dropped every overbar on the Group-Zero
  constant symbols (p. 10, where `2n` is twice `n`, not `2^n`), and rendered
  the valuation symbol `varpi` as `$`. Hazard H2 as charted.

## 2026-08-21 - Two Corrections from Checkpoint Review (C4/C6 follow-up)

- **Erratum E6 recorded against `Willard2005`.** Section 4 Item D (p. 16) cites
  `[55]` twice - "Hajek, Svejdar and Vopenka [55, 63]" and "Svejdar [55] has
  generalized this phenomena with regards to interpretability" - but `[55]` is
  Svejdar's "Modal analysis of generalized Rosser sentences" (JSL 48, 1983),
  whereas `[54]` is "Degrees of interpretability" (Comm. Math. Univ. Carolinae
  19, 1978), which is the paper the claim describes. `[54]` is cited correctly
  in Item E's list. Reading: both Item D occurrences are slips for `[54]`,
  recorded as **probable** rather than certain since confirming it requires the
  1983 paper's contents, which are not in the corpus. The codified statement
  should cite Svejdar's interpretability result by title, not by 2005's
  reference number.
- **Obligation O26 recorded, correcting a formulation of mine.** In discussion
  I contrasted Rosser-style self-verification ("changes what provable means")
  with Willard's ("changes what total function means"). The second half is
  wrong and the user corrected it: **Willard redefines nothing.** "Total
  function" keeps its standard meaning throughout; what varies is *which
  totality axioms a system takes* and *which function symbols its language
  carries*. `Willard1993-TR` p. 3 is explicit that IS(A) "will recognize
  non-zero Division as a total function (and hence can view Multiplication as a
  well-defined **partial** function)", and Willard2011's Type-M / Almost-M /
  A / S / NS taxonomy is purely a question of which of axioms (1)-(3) are
  present. The obligation records that the codified statement must never imply
  SJAS uses a weaker *notion* of totality or of provability - that reading
  converts an honest weakening of the theory into a definitional trick, and is
  exactly the Feferman-style "numerically correct but intensionally incorrect"
  route Willard rules out in the 1993 chapter's opening page.
- Context: the corpus cites Rosser only historically (the 1936 removal of
  omega-consistency) and carries Arai's "Derivability Conditions on Rosser's
  Proof Predicates" and Svejdar's "Modal Analysis of Rosser Sentences" in the
  bibliographies of both spine papers **without engaging either in the body**.
  A Rosser-comparison for the Refinement stage is therefore open rather than
  foreclosed; O26 records the constraint it must respect.

## 2026-08-21 - Spine Extraction: Willard 2011 and the Subsumption Audit (component C6)

- Extracted `Willard2011` (arXiv 1108.6330v8, 64 pp.), the unification paper:
  `codification/extraction/willard2011.md`, 47 numbered items. Registries now
  hold 126 results, 16 systems, 53 notation rows; audit green.
- The charter's distinct C6 deliverable, the **subsumption audit**, is
  `codification/extraction/willard2011-subsumption-audit.md`. Verdicts:
  **unifying** verified, **simplifying** verified, **extending** verified -
  with three qualifications recorded below. Two of the four claimed paradigms
  (`Willard2006a`, `Willard2009`) are not yet extracted, so those halves are
  explicitly **deferred to C9/C10** with the specific checks named, rather
  than assumed.
- **The unification device** is Definition 3.4's **generic configuration**, a
  5-tuple `(L^xi, Delta^xi_0, B^xi, d, g)`. Language, formula class, base
  system, deduction method **and the Godel coding method** all become
  parameters, and each earlier system becomes one choice of xi. Verified
  verbatim matches to `Willard2005`: the U-Grounding eight, the Mult graph
  (2011 Eq. 45 = 2005 Eq. 4), the eight tableau rules, the Group-2 schema,
  and the fixed point (2011 Eq. 35 = 2005 Eq. 7; Eq. 36 = 2005 Eq. 8 with
  Neg^k for Pair and the level parameter k added).
- **But the correspondence is not element-wise** (drift D21). Willard's own
  attributions map xi-star *and* xi-star-star to `Willard2005` (sections 5 and
  5.3), xi-minus to `Willard2006a` Theorem 3, and xi-R to `Willard2009` -
  leaving **`Willard1993` without a configuration of its own**. And 2011 is
  **not self-contained**: it declines to repeat the Tab-U\*1 definition
  ("It will be unnecessary to repeat here") and imports the Normed and
  Fact D.3 machinery from 2005 for its own stability proof.
- **The simplification is identifiable and partly relocation.** 2005's
  Definition 5 (theta-compactification, parts A and B, with theta < 1/3 free)
  **splits into two named properties** - Definition 5.1 **A-Stable** (the Pi
  half) and Definition 5.3 **E-Stable** (the Sigma half) - each with the fixed
  constant 1/2. 2005's App-forall/App-exists pair collapses into the single
  measure `sharp`. Theorem 5.9 has the same skeleton as 2005's Theorem 1 but
  a sharper ending: both a sentence and its negation land on the *same*
  Good{half sharp}, so the contradiction is immediate - which is why 2005's
  Theorem 1 needs the small-m side condition (obligation O1) and 2011's does
  not. **O1 is now scoped as a 2005-only obligation.** What does not vanish is
  the per-configuration combinatorics: 2005's section 5.2 nine-case induction
  reappears as Appendix D-2's four-page Theorem D.4.
- **Four genuinely new results** with no antecedent in the extracted 1993 or
  2005 material: Theorem 6.12's **Translational Reflection Principle** (the
  abstract's headline - reflection into a Standard-M-equivalent Pi^xi_1
  translation, which Loeb does not block because Psi and Psi-translated are
  *not* equivalent from the system's own perspective); the **Global Simulation
  Sentence**; Appendix G's three-axiom result; and Theorem E.1, which shows
  the translational principle is inoperative for conventional logics.
- **Willard 2011 answers a question asked in this repository.**
  `prose/to-dw-20201206z.txt` question 2 asks whether Group-2's infinitary
  schema can be replaced by a finite set sufficient for all Pi\*1 theorems.
  The answer is two-stage: a single **Global Simulation Sentence** replaces
  the schema but yields only the Test-relativized form (Definition 6.8,
  Remark 6.9); **three** added sentences yield the theorems *purely*, but only
  for a quantifier-count bound c fixed in advance (Theorems G.2, G.3).
  Recorded as composition obligation **O23**.
- **Willard's own limitation, recorded** (Remark 6.16b, obligation O24):
  `SelfCons` "causes axiom systems to produce essentially a **1-line proof**
  of their own consistency… an **instinctive faith** in its own consistency
  (rather than it supporting a full-length proof-justification of this fact)".
  Any presentation omitting this overstates what SJAS delivers.
- New drift: **D18** (2011's "Self Justifying" is 2005's "Introspectively
  Unified Logic"; with 1993's "self-verifying" the corpus now carries three
  closely-named non-equivalent predicates - obligation O25); **D19** (a third,
  materially different formulation of Solovay's unpublished theorem);
  **D20** (see below); **D21** (the paradigm/configuration mismatch).
- **Bibliographic correction to our own inventory (D20).** `Willard2001`'s
  actual title is "Self-Verifying **Axiom** Systems, the Incompleteness
  Theorem and **Related Reflection Principles**". Willard's 2005 self-citation
  renders the subtitle as "the tangibility reflection principle", and
  `nachlass/paperlist` had inherited that; the phrase names a construct inside
  the paper and the title of the 1997 KGC-5 chapter, not this paper.
  `paperlist` and `registry/corpus.md` corrected, with the provenance recorded.
- Obligations refined: **O2** (the coding condition's mature form is the
  **Conventional Tableaux Encoding Requirement** - Godel number at least 32^J
  for J function symbols - justified in 2011 as "at least 2J logical symbols
  and thus at least 5J bits", a different derivation of the same 5 from
  1993's base-32-in-6-bit-bytes route); **O11** discharged in substance by
  Definition D.1(ii), which writes addition-totality as
  `for all x,y exists z <= x+y (z = x+y)` and calls it "a very precise Pi\*1
  styled declaration" - the function symbol is what makes the bound
  expressible, and the bounded existential is what keeps it Pi\*1.
- Also recorded: the **Type-M / Almost-M / A / S / NS** growth taxonomy (the
  origin of "Type-NS" as used in this repository's README); `SelfCons_k` as
  the origin of the Proflog line's `SelfCons1`; Appendix F's **Epistemological
  Bundle Theory**, in which A- and E-stability formalize how a thinking agent
  can treat short proofs from a technically inconsistent view as useful,
  provided its proofs are shorter than the errors.
- Verification: text-layer read of all 64 pages, plus visual control of
  printed pp. 14, 16, 18, 32 and 39 (Tight; A-/E-Stable; Theorem 5.9;
  Appendix A's equations; the coding condition). Pagination convention for
  this witness: **printed page = PDF page - 1**.

## 2026-08-21 - Origin Extraction: TR 93-10 and the KGC 1993 Chapter (component C5)

- Extracted both origin witnesses: `codification/extraction/willard1993-tr.md`
  (full template) and `codification/extraction/willard1993.md`
  (delta-focused companion, since the chapter is the TR's 12-page
  abbreviation). Registries now hold 52 results, 11 systems, 41 notation rows;
  audit green.
- **Drift D1 resolved, gap G9 closed.** The TR's reference list (printed p. 55,
  read from the page image) cites `[So94] R. Solovay, private communications
  (April 1994)`, cites the published chapter as `[Wi93]`, and forward-
  references `[Wi94]`. A report citing April-1994 communications cannot
  predate April 1994, so TR 93-10 is a **1994 text carrying a 1993-series
  report number** - exactly why Willard cites it in JSL 2001 as a "SUNY-Albany
  Technical Report, March 1994". One artifact, two accurate descriptions.
- **New gap G14**: `[Wi94]` D. Willard, "The ETR Reflection Principles for
  Self-Verifying Axiom Systems", forthcoming report - cited as containing the
  proof of Proposition 8. Absent from paperlist, DBLP, arXiv and the resume,
  and not found by the C2 coverage pass. Its title closely matches the
  unlocated KGC-5 chapter `Willard1997` (G2), whose priority is raised
  accordingly.
- Structure recovered: the TR proves Propositions 1-7 (chapter proves only
  Proposition 1 and states 2-4 as "not proven in this 12-page Extended
  Abstract"), defers Proposition 8 to [Wi94], sketches Proposition 9, and
  carries Lemmas 5.1-5.3, 6.1-6.2, 7.1, 12.1, Remarks 1-7 and Appendix A.
- **The origin boundary, in Willard's own tabulation** (TR printed p. 3):
  systems fall into three categories by growth capacity - no growth functions,
  addition total, or both addition and multiplication total - and only the
  first "will be capable of permitting Gentzen-style deductive cuts without
  becoming inconsistent". Sharpest form (p. 11): Solovay's inconsistent
  `IS_+(PA+,G_s)` becomes consistent by removing *either* cuts *or* the
  successor function. One inconsistent system, two independent repairs.
- **The engine's informal ancestor** (chapter p. 329): proofs of `b_n`'s
  existence under addition require more bits than `b_n`'s binary encoding,
  whereas under multiplication `a_n`'s existence proof is exponentially
  shorter than its encoding. This is `Willard2005` Definition 5 and Eq. (20)
  in embryo. The 1993 proof skeleton (minimal inconsistency proof `p`, witness
  `p*` at least `p`, `INT(c_j) <= 2^j * m_max` because addition is the only
  increasing function) maps step for step onto 2005's Theorem 1 opening and
  Eq. (19); `INT` is `VAL`, "s-consistent" is `Positive(a,b)`. What 2005 adds
  is the abstraction barrier of Definition 5, separating the engine from the
  per-apparatus analysis - which is why Willard says it "did not exist in our
  prior papers".
- **The Main Conjecture and its refutation** (chapter section 6): Willard
  conjectured that the cut-permitting `IS_+(A)` and `IS*_+(A)` are also
  self-verifying. His own later work refutes it - TR Proposition 7 buys cuts
  only by dropping addition-totality, Solovay's Theorem kills the
  successor-carrying case, `Willard2004` closes Tab-Sigma\*2/Pi\*2, and
  `Willard2005` Theorem 5 recovers only a Level-1 restricted modus ponens.
  Recorded as composition obligation O8: the codified statement must carry the
  arc, not just the mature result.
- **Provenance correction (drift D13, obligation O9)**: the affine-tree
  documents present the identification of the multiplication boundary with
  *variable duplication* as a finding of their tree transposition. Willard
  states it himself at chapter p. 328 - `x*x` needs "two occurrences of the
  same variable", and "the proof of Godel's Incompleteness Theorem used
  essentially the same double appearance of a variable". The insight is
  corroborated rather than novel, which strengthens it; the transposition's
  real contribution is its carrier-independence.
- Further drift recorded: D7 (the deduction method is notation in 1993 but
  definitional by 2005; "self-verifying" and "self-justifying" are different
  predicates), D8 (`IS*(A)` vs `IS^{Sigma1}(A)` between the two 1993
  witnesses), D9 (the two witnesses define Pi1/Sigma1/Delta0 differently, and
  1993's unstarred classes are 2005's starred ones), D10 (the chapter lists
  fourteen Group-1 functions, the TR eight), D11 (the 1993 and 2005 "eight
  function" signatures share only Addition, Subtraction, Division - the
  shared cardinality is a trap), D12 (addition-totality is Pi2 in 1993 but
  Pi\*1 in 2005, which is *why* the U-Grounding function language exists),
  D14 (Willard permits pointer-sharing proof compression and says it
  strengthens his results, where the NC-SJAS plan forbids all sharing).
- Also recorded from the origin: `SUBST` demoted to a Delta0 relation with
  unprovable totality but per-instance verifiability (chapter p. 332); the
  "subcomponent" relation defining the cut-free property, absent from 2005;
  resolution and cut-free sequent calculus named as covered by the same
  analysis at TR pp. 4 and 10 and chapter p. 332 - **stated-only in every
  place**; the ZF thread ("IS(ZF) is self-verifying and affirms the
  consistency of ZF") that resurfaces in the 2008 ZCF nachlass drafts; and the
  floating-point remark (chapter p. 329) anticipating the ASL-2005 line by
  twelve years.
- **Visual verification pass over the TR main body (same day, after
  checkpoint review).** Nothing had been blocking it - the deferral was a
  scoping call, not an obstruction - so printed pp. 14, 15, 20, 23, 28, 29, 32
  and 34 were rendered and read. **Three proof-status judgements taken from the
  OCR sweep were wrong**: Lemmas 5.1, 6.1 and 12.1 carry *Proof Sketch* labels,
  not full proofs, and Proposition 9's is a "Very Informal Proof Sketch". All
  three sketched lemmas are load-bearing (5.1 underwrites the Delta0 encoding,
  6.1 is half of Proposition 1's proof, 12.1 is the entire mechanism of
  Proposition 6), so the TR's real proof profile is weaker than recorded.
  Registry now: 58 results rows, 25 full / 5 sketch / 2 cited / 16 stated-only.
- **`ETR` decoded**: TR printed p. 29 defines the **E-Tree Reflection
  Principle** and states Proposition 8 about a system `IS^{ETR}(A)`, saying
  "In [Wi94], we generalize the techniques of Sections 7 & 8 to prove" it. So
  gap G14's `[Wi94]` "The **ETR** Reflection Principles for Self-Verifying
  Axiom Systems" is precisely the paper proving Proposition 8; Proposition 9
  (the *Bounded* form) is the fragment Willard could sketch without it.
- **Remark 6 relativizes the signature drift** (printed p. 34): `G_0`'s
  definition "was quite arbitrary", and Propositions 1-4, 8, 9 hold when *any*
  set of **slowly growing** functions (bounded by i*2^j*Max) is added, with
  Proposition 7 taking any non-growth set. The differing 1993/2005 function
  lists (D10, D11) therefore do not compete - the invariant is the growth
  class, not the signature. Obligation O13; this also independently
  corroborates the affine-tree design's decision to abstract to a growth
  discipline.
- **Remark 4 sharpens D12** (printed p. 28): in 1993 a total function may be
  given *either* by a function symbol *or* by a relation symbol plus a
  totality axiom, interchangeably. By 2005 they are not interchangeable - a
  Pi2 totality axiom is inadmissible in a Normed(a,b) system - so the
  function-symbol presentation becomes obligatory. A real tightening that is
  easy to misread as agreement.
- **The pointer notation is a totality device** (printed p. 15): the encoding's
  `u#`/`v#` symbols "act as pointers to constants", and Lemma 5.2's
  parenthetical gives the reason - the analogous mappings onto the expanded
  constant form "are unknown by IS(PA+) to be total functions". Expanding a
  long constant in place is exactly what the system cannot prove total. This
  sharpens D14 considerably: Willard's sharing is load-bearing, not a cost
  optimisation, so any transposition that bans sharing outright owes an answer
  to the problem the pointers solve.
- **New drift D15**: multiplication is fatal for two different stated reasons -
  1993 (Lemma 12.1) because multiplication-totality makes local Pi1 reflection
  derivable, which Proposition 5 shows inconsistent; 2005 (Remark 4) because
  growth overwhelms Definition 5's envelope arithmetic. Two routes to one
  boundary; obligation O14.
- Also recovered: the proofs "were conceived using Gentzen's cut-free sequent
  calculus notation" and only later transposed to tableaux (p. 14) - stronger
  evidence for apparatus generality than the bare "trivially generalizes"
  claims; terse proofs, `Glue(t,p)` and Lemma 7.1's internalization of a
  meta-theorem (p. 23); the INVALID.1/INVALID.2 naming behind Figures 1-4; and
  the phrase "limited but **tangible** power" (p. 34), the lexical origin of
  the Tangibility Reflection Principle (obligation O15).
- **Appendix pass (same day, after user correction).** The plan excludes only
  material irrelevant to Willard's development of SJAS, and the exclusion test
  applies to whole corpus items - `Willard1993-TR` is ruled `core`, so
  sub-selecting within it was wrong. Every page of Appendix A and Appendix B
  was read (printed pp. 37-54). Results:
  - **An entire Appendix B had been missed** by the OCR-derived structure note.
    It is the worked seven-level tableaux derivation of INVALID.2's
    inconsistency that Figure 4 draws, and its Summarizing Comment states the
    axiom-versus-theorem mechanism (drift D16, obligation O18): a cut-free
    proof cannot cite a theorem as an intermediate step, so INVALID.2 must
    reconstruct LPi1 reflection's "essential cut-free implications" - the same
    device that separates Willard2005's IS_D from NS^{k,m}_D.
  - Appendix A defines `UNION(A)`, `ExPrf` and `SUBST` - the three constructs
    Willard2005 section 3 reuses verbatim - and its equations (A.1)-(A.3) are
    Willard2005's Eq. (7) with the diagonal argument fixed to a constant rather
    than abstracted to a parameter. A precise, checkable lineage claim.
  - **Corollary A.15 is the arithmetized proof-checker in origin form** (root
    check, node-justification check, closure check), the direct ancestor of
    Proflog's tableau-proof predicate and the affine-tree Deriv.
  - **Observation A.8 explains the 5-versus-6 constants** flagged as obligation
    O2 in C4: a base-32 digit is stored in a 6-bit byte, so the godelized form
    costs 6/5 of the natural one - five bits of content per six-bit symbol.
    That is where Willard2011's "at least 5J bits, Godel number at least 32^J"
    comes from, the 32 being the base-32 alphabet.
  - **Observation A.10: IS(A) cannot prove concatenation total**, and handles
    it by the same demote-to-relation move. The affine-tree design argues
    concatenation-strength from the external literature; Willard states the
    system-side fact himself (obligation O16).
  - **Scalar multiplication always was total**: `Scalar_k` is a function for
    fixed k, and Willard2005's `Double` is exactly 1993's `Scalar_2`. "Multi-
    plication is not total" means *variable* multiplication (obligation O17).
  - **The Added Comment (p. 52) reduces the working signature to four
    functions** - Addition, StringCount, Shift, Extract - the other four being
    presentational. With Remark 6 this largely dissolves drift D10/D11: the
    1993 and 2005 signatures are related by promotion and derivation within one
    growth class, not by disagreement. (Willard2005's primitive `Maximum` is
    derived in 1993 as x + (y-x).)
  - Three appendix items are sketch-grade: A.5 (Proof Sketch), A.13 (Proof
    Outline), A.16 (Proof Sketch). Registry now: 76 results rows, 38 full /
    8 sketch / 2 cited / 16 stated-only / 12 n-a.
  - Lemma A.16 also records that `G_0` is finitely many axioms **plus two
    infinite schemata** for the integer constants - schemata the 2005
    U-Grounding language eliminates by making numerals terms, which sharpens
    obligation O11 about why that language exists.
- **Completion pass (same day).** Cleared the two items left unverified, and
  the clearing turned up three more:
  - **Lemma 5.4 had been missed entirely** (printed p. 19). It defines
    `Prf2_B(x,y1,y2)` - a proof `y1` paired with **a Turing-machine trace `y2`
    verifying `y1` is well defined** - and the system `IS2` built on it.
    Willard adds that "Appendix A is in some sense **unnecessary**" because
    every theorem about IS translates to IS2 by replacing `y` with the pair.
    The chapter offers the same escape under `Prf-star`. A trace-carrying proof
    predicate is therefore Willard's own device from 1993, not a
    native-computational innovation of the NC-SJAS line (obligation O20). Its
    proof is labeled "**Quite Informal Proof Sketch**", the weakest label in
    the document.
  - **Remark 2 is on p. 24, not p. 22**, and is a global methodological
    caveat: "our proofs shall *apply a cut rule at the meta-theoretical
    level*… we will *often omit constructing formally the cut-free proofs*…
    Instead, *their theoretical existence* will be established implicitly via
    the Cut Elimination Theorem." The object systems are cut-free; the
    metatheory uses cut freely (drift D17, obligation O19).
  - **Lemma 5.3 is a sketch**, not a full proof, and Willard states that it
    **fails without the `u#` pointer notation** - "Lemma 5.3 would not follow,
    had this section employed slightly different definitions (such as omitting
    the symbol u#)" - while scoping the loss: Propositions 1, 2a, 3, 5 and 7
    survive without it, so only 2b, 2c, 4 and 6 depend on it. This settles
    drift D14: Willard's constant-pointer compression is **necessary to four
    of the origin results**, so a no-sharing cost model must replace it or
    give those results up.
- Two further passages recovered from printed p. 12 (still section 3, not
  section 4 as the OCR structure had it):
  - **Which derivability condition fails**: "only in the *degenerate case*
    where x and y are *fixed constants*" can IS(A) prove the Hilbert-Bernays
    condition that from proofs of alpha and alpha-implies-beta a proof of beta
    exists. Fixed-parameter totality versus uniform totality is the corpus's
    recurring device - the same shape as SUBST for fixed k-bar, Lemma 5.3's
    T(p,k), and Scalar_k (obligation O22).
  - **Willard states the delicacy thesis himself**: "a modest change in a
    formalism's definition will cause Propositions 1 thru 7 and Solovay's
    Theorem to produce starkly different results… it appears premature to
    develop hasty conclusions". That is the premise of
    `prose/sjas_synthesis.txt`, asserted by Willard about his own results in
    1993 - the Codification's governing caution is the corpus's own lesson
    (obligation O21).
- E1993-1 resolved and not an erratum: section 4 does carry a display heading,
  "Intuition Behind the Proof of Proposition 1" (p. 13); the OCR sweep missed
  it because the running page number ran into the heading (`-134. Intuition`).
  Lemma 5.4 and Appendix B were missed the same way, so the pass-2 obligation
  is now specifically to re-sweep for items the heading passes could not see.
- Registry: 77 results rows (37 full, 10 sketch, 2 cited, 16 stated-only).
- Verification discipline: everything marked `V` in the extraction records was
  read from page images; OCR-only items are marked `O` and may not be quoted
  in the codified statement until pass 2. The TR's OCR layer renders
  `IS^{Sigma1}(A)` as `is-TM (A)` and `G_0` as `Gg`, so the visual control was
  not optional. Appendix A's Lemma A.\* inventory is deferred to pass 2.

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
- Template verdict: no change required for the eight sections themselves; two
  method additions were made in response to checkpoint review (below).
- **Visual control pass added and run** (extraction record section 8.1). Read
  the PDF page images for the pages carrying the load-bearing formalism
  (pp. 11, 19, 22, 26, 33) to control for text-layer rendering. `pdftotext`
  fails *silently* on mathematical typesetting: it rendered Fraktur `Im`
  (Definition 1's consistency-preserving map) as `=`, the Godel-sentence
  script glyph as `f`, and dropped numeral overbars, tower super/subscripts,
  iterated-log exponents, and floor/ceiling delimiters. The overbar loss is
  the dangerous one - `Gamma(n-bar)` versus `Gamma(n)` is numeral- versus
  variable-substitution, i.e. the fixed point itself. Corrected two notation
  rows and Eq. (9) in the extraction record; confirmed Definitions 4/5,
  Theorem 1, Eqs. (18)/(19)/(20), and Theorem 2's theta = 1/4 unchanged, so
  the D6 divergence survives visual verification. Also established that
  errata E1 and E2 are genuine features of the typeset source rather than
  extraction artifacts - a distinction the text layer cannot support. The
  charter now requires a visual control pass for every page carrying a system
  definition, a quotable definition or theorem, or a displayed equation whose
  constants matter, for all tiers and not only OCR'd scans.
- **Composition-obligations register created**
  (`codification/concordance/composition-obligations.md`, O1-O7). Findings the
  codified statement must act on were previously recorded only inside drift
  entries and the fidelity check, where they would not surface reliably at
  C14-C15; they are now also collected in one checklist: the Theorem 1 small-m
  side condition, the three-constants distinction, the corrected density/theta
  window, the stated-only apparatus cells, Theorem *'s unpublished provenance,
  the destroyed glyphs, and the `IS(A)` versus `IS_D(A)` distinction. The
  charter makes appending to it a standing obligation.

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
