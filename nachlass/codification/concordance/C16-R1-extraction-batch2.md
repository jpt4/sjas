# C16 Stage 5 — R1–R3 on the extraction records, batch 2 of 33

Evidence for acceptance criterion **A-R** of
[`../C16-verification-goal.md`](../C16-verification-goal.md) §4, continuing
[`C16-R1-extraction-batch1.md`](C16-R1-extraction-batch1.md). Every finding
carries a disposition: `corrected` / `accepted, with reason` /
`carried to Refinement` / `image-queue`. Sections checked and found clean are
named as such, so the absence of findings is evidence rather than silence.

Audit date: 2026-08-31. Nothing committed.

**Scope.** Records 9–20 of 33. Batch 1's eight records are **not** re-checked
and their findings are not restated except where this batch corroborates them.
The remaining 13 records are **not** covered here and must not be reported as
done. Finding ids continue batch 1's sequence (`F18`+, `Q10`+) so the two
records can be read as one series.

| # | Record | Witness key | Text layer |
| --- | --- | --- | --- |
| 9 | [`../extraction/willard1993.md`](../extraction/willard1993.md) | `willard1993` | **OCR** |
| 10 | [`../extraction/willard1993-tr.md`](../extraction/willard1993-tr.md) | `willard1993-tr` | **OCR** |
| 11 | [`../extraction/willard2001.md`](../extraction/willard2001.md) | `willard2001` | publisher |
| 12 | [`../extraction/willard2004.md`](../extraction/willard2004.md) | `willard2004` | publisher |
| 13 | [`../extraction/willard2006a.md`](../extraction/willard2006a.md) | `willard2006a` | publisher |
| 14 | [`../extraction/willard2006b.md`](../extraction/willard2006b.md) | `willard2006b` | publisher |
| 15 | [`../extraction/willard2005-tab.md`](../extraction/willard2005-tab.md) | `willard2005-tab` | publisher |
| 16 | [`../extraction/willard2002c.md`](../extraction/willard2002c.md) | `willard2002c` | publisher |
| 17 | [`../extraction/willard2006-wollic.md`](../extraction/willard2006-wollic.md) | `willard2006-wollic` | publisher |
| 18 | [`../extraction/willard2020-lfcs.md`](../extraction/willard2020-lfcs.md) | `willard2020-lfcs` | publisher |
| 19 | [`../extraction/willard2014.md`](../extraction/willard2014.md) | `willard2014` | publisher |
| 20 | [`../extraction/willard2018.md`](../extraction/willard2018.md) | `willard2018` | publisher |

---

## 0. Method, and one departure from batch 1

Each record was read in full. Every block quote and every inline quoted string
of ≥12 characters was extracted mechanically (`/tmp/c16x.py`, disposable) and
checked against the witness the record names. Quotes carrying an internal
ellipsis were **split at the ellipsis** and each fragment checked separately.
Matching used batch 1's normalizing comparator (`/tmp/c16v.py`), which folds
whitespace, line wrapping, hyphen-at-line-break, ligatures, curly punctuation
and diacritics before substring search, then falls back to a windowed `difflib`
ratio so a **wording difference** is distinguished from an **absent phrase**.
Every fragment that failed against the named witness was then swept across all
26 witnesses in `sources-text/`, because batch 1's dominant failure mode was a
true quotation filed under the wrong source. Page anchors were resolved against
`pdftotext`'s form-feed boundaries (`/tmp/c16page.py`, disposable), not the
printed folio.

**Two limits of the comparator, stated so they are not mistaken for coverage.**
It folds case, so a capitalisation change inside a quotation is invisible to it;
the load-bearing quotes were re-read by eye for that. And it cannot see
mathematics that `pdftotext` has destroyed, which is why §6 exists.

**The departure.** Batch 1's eight records all had publisher text layers.
Two records here — `willard1993` and `willard1993-tr` — rest on **OCR**
witnesses that the corpus's own `regen-sources-text.sh` marks as "search aids,
not authoritative". The degradation is severe: `willard1993.txt` renders
"simultaneously" as *simultanequsly*, "Peano Arithmetic" as *Peano Arithmerle*,
`Π₁` as *R 1*, and `Σ₁-Induction` as *]~-Induction*. **R1 cannot be run against
the text layer for these two**, and reporting their fragments as
"not-verbatim" would be an artifact of the OCR, not a finding. Both records
declare that every page was read as an image, so their quotations are held on
that declaration; §5 spot-checks the declaration by R2 and §3 reports the three
findings that survive OCR degradation because they are structural rather than
lexical. This is goal §7's "say so at the start" rather than compressing the
reading and reporting the records clean.

---

## 1. Counts

263 fragments were extracted. **12** were extractor artifacts — the inline-quote
regex pairing quotation marks across markdown table cells — and are excluded
throughout, leaving **251** checked.

| Record | Fragments | verbatim in the named witness | remainder |
| --- | ---: | ---: | ---: |
| `willard1993.md` | 48 | 12 | 36 (OCR) |
| `willard1993-tr.md` | 83 | 29 | 54 (OCR) |
| `willard2001.md` | 38 | 22 | 16 |
| `willard2004.md` | 12 | 8 | 4 |
| `willard2006a.md` | 11 | 11 | **0** |
| `willard2006b.md` | 9 | 6 | 3 |
| `willard2005-tab.md` | 11 | 10 | 1 |
| `willard2002c.md` | 12 | 6 | 6 |
| `willard2006-wollic.md` | 7 | 3 | 4 |
| `willard2020-lfcs.md` | 9 | 4 | 5 |
| `willard2014.md` | 5 | 2 | 3 |
| `willard2018.md` | 6 | 3 | 3 |
| **Total** | **251** | **116** | **135** |

Of the 135: **90** are in the two OCR witnesses and are not adjudicable from
the text layer (§0). The **45** in the ten publisher-text records classify as:

| Class | Count |
| --- | ---: |
| Verbatim in effect — the difference is a `pdftotext` glyph, subscript, footnote marker, hyphen or quote-style rendering | 15 |
| Extractor residue — an ellipsis split inside a formula such as `F(a₁,…,a_j)`, leaving a fragment that is not a quotation | 7 |
| The record's own scare quotes and glosses, never claimed as source wording | 7 |
| Correctly attributed to a **different** witness in the record's own prose, and located there | 5 |
| **Genuine findings** | **11** |

Eight further findings came from R2, R3 and the inventory checks, which the
quotation sweep cannot reach. **19 findings total** (§§2–5), of which **5** were
corrected (§4). Image-queue entries: **17** (§6).

---

## 2. R1 — findings, with dispositions

### 2.1 Wrong source (2)

| # | Record : line | Asserted source | Quote / claim | Actually | Disposition |
| --- | --- | --- | --- | --- | --- |
| **F18** | `willard1993.md`:44 | `Willard2001` **reference [67]** | "the intuition behind the IS(A) axiom system, without formal proofs" | `Willard2005` **[67]** — the annotation sits on 2005's entry for `Willard2001` and points back at its **[65]**, which is this chapter. `Willard2001`'s own bibliography **stops at [46]**, so a [67] cannot exist there | **corrected** — §4 C1. Load-bearing: this is the authorial ruling the record's §3 states-versus-proves table rests on |
| **F19** | `willard2018.md`:53 | `Willard2018` (by placement in quotation marks among that paper's theses) | "almost cheating" | `Willard2016` p. 34, statement `###`: "Is it not almost cheating when an axiom system verifies its own consistency by using ⊕'s formalized 'I am consistent' axiom…". The phrase does not occur in `willard2018.txt` | **corrected** — §4 C4, together with F26 |

### 2.2 Not verbatim (9)

| # | Record : line | Record prints | Witness prints | Severity | Disposition |
| --- | --- | --- | --- | --- | --- |
| **F20** | `willard2004.md`:23 | `Willard2020` §7 cites this paper for "the evasions fail at and above the Π\*₂ level" | "We can also extend our results from [49] to show that the comparable evasions of the semantic tableau version of the Second Incompleteness Effect **will fail at and above the Π∗2 level**." | **moderate** — a paraphrase set in quotation marks, and it is the record's own statement of what the paper is *for*. The attribution is sound: `Willard2020` **[49]** does resolve to `Willard2004` | **corrected** — §4 C5 |
| **F21** | `willard2004.md`:102 | block quote: "when `ℜ` represents the union of `Π*₁` and `Σ*₁` sentences" | p. 9: "when ℜ represents the union of **the set of** Π∗1 and Σ∗1 sentences" | low, but inside a **block quote** | **image-queue** — Q17. The sentence also carries `ℜ`, `Π*₂`, `Σ*₂`, Level(1) and Level(0−); p. 9 was never rendered, and R1 forbids accepting mathematics from the text layer |
| **F22** | `willard2001.md`:147 | systems table, `ISMULT(A)` Group-3: "no tableaux proof of 0=1 from ISMULT(A)" | p. 21: "Its Group-3 axiom will thus assert that **'No Semantic Tableaux proof of 0=1 exists from ISMULT(A)'**" | low-moderate — "Semantic" and "exists" dropped; the neighbouring rows in the same table print their Group-3 axioms exactly | **carried to Refinement** — restore the source wording or drop the quotation marks |
| **F23** | `willard2001.md`:503 | "and probably any `λ ≥ 10⁻⁴` or even yet smaller" | Remark 5.5, p. 34: "(and probably **for** any λ ≥ 10−4 or even yet smaller)" | low — one dropped word | **accepted, with reason** — restore "for" at Refinement. The record's assignment to **Remark 5.5** is right; the text layer floats the "Remark 5.6" heading into the middle of the sentence, an initially suspicious reading that the page-boundary check cleared |
| **F24** | `willard2002c.md`:130 | "and its generalizations **collapse** when `R(2,1)` replaces `R(1,1)`" | p. 16: "Theorem 2 and its generalizations collapse when R(2, 1) **deduction** replaces R(1, 1)." | low | **accepted, with reason** — the dropped word is the apparatus noun the record supplies in its own lead-in |
| **F25** | `willard2006-wollic.md`:69 | "available as **University** of Albany Computer Science **Technical** Report SUNYA-CS-06-01" | p. 15: "available as **univeristy** of albany computer science **tehcnical** report sunya-cs-06-01" | low — **two** source typos silently repaired inside quotation marks | **accepted, with reason** — same class as batch 1 F11/F12. The bibliographic identification is unaffected; note the typos rather than propagate them |
| **F26** | `willard2018.md`:4, 18 | `Willard2020` Ref [56]: "quite preliminary **announcement** of [57]'s results" | `willard2020.txt` line 1031: "(This quite preliminary **annnouncement** of [57]'s results appears in an essentially roughly written summary-abstract form.)" — three n's | low, but it recurs — `registry/results.md`:220 (`Willard2020#Ref56`) prints the same silent repair | **accepted, with reason** — the ref number **[56]** is correct, and [57] is indeed the LFCS chapter. Add `[sic: annnouncement]` at Refinement, in the record **and** in the registry row |
| **F27** | `willard2014.md`:25 | abstract: "into formalisms **of** purely finite size" | both witnesses print "into formalisms **or** purely finite size" — published `willard2014.txt` and `willard2014-archive.txt` alike | low — silent repair of an evident source typo | **accepted, with reason** — the record's §7 errata notes the archive's "spelling/Δ glyph differences" but not this; add it |
| **F28** | `willard2001.md`:454 | "Could this be a partial … explanation for how Human Beings seem to have an instinctive sense of their Self-Consistency**?**" | p. 42: "…of their self-consistency **despite the limitations of the Second Incompleteness Theorem**?" | low-moderate — the marked ellipsis correctly covers "(albeit not complete)", but the **terminal** truncation is unmarked and manufactures the question mark | **carried to Refinement** — see R3, §3 item 4 |

### 2.3 Inventory and internal consistency (3)

| # | Record : line | Finding | Disposition |
| --- | --- | --- | --- |
| **F29** | `willard1993.md`:260 | The 2026-08-27 verification pass writes that multiplication proves **`a₁₀`** exists within the paper's 400 lines. The **same record**, §5.1 line 114, writes **`a_400`** for the same illustration. The chapter's OCR is ambiguous (`a~o`), but the TR's parallel passage (printed p. 7) reads **`a399`** in a proof of "approximately 300 lines" — so the subscript scales with the paper and `a₁₀` is the outlier | **corrected** — §4 C2 |
| **F30** | `willard2018.md`:45 | Definition 3.11 defines **four** enrichments — **1. Infinitely Enriched**, 2. Rank-k, 3. Rank-Zero, 4. Rank-Zero-Plus. The record's thesis 5 lists three, omitting Infinitely Enriched, and `registry/results.md`:481 (`Willard2018#Enrich`) does the same. The omitted item is the one Remark 3.12's Type-S clause actually depends on (F31), so the gap is not cosmetic | **carried to Refinement** — add item 1 in both places |
| **F31** | `willard2018.md`:50 | Thesis 6: "Type-S systems fail under **enrichments**". Remark 3.12 restricts the claim: "Type-S axiom systems cannot verify their own consistency under **infinite** enrichments of the semantic tableaux". Dropping the qualifier widens a boundary claim to cover the Rank-Zero and Rank-Zero-Plus cases the same remark declares **safe** two sentences earlier | **corrected** — §4 C3 |

**Named clean.** `willard2006a.md` is the only record in either batch with
**zero** non-verbatim fragments: all 11 quotations match `willard2006a.txt`
exactly, including the ISCE expansion, the additive-convention "useful
compromise" passage, and the "it grows too slowly" verdict on the incremental
convention. `willard2005-tab.md`'s single remainder (`Π₁* sentence W`) is a
subscript rendering, and the record's own §8 records that pass 2 confirmed the
`Π₁⁻`-statement / `Π₁*`-proof-sketch divergence on the page image.

---

## 3. R3 — elision checks

Every quotation carrying an ellipsis was re-read with its full source sentence
and the sentence before it, asking goal §4's question: *does the elision drop a
clause bearing on the claim's ground, and does the surrounding inference
survive the restored context?* Twelve elision sites; **three findings, nine
clean**. The clean ones are named, so their absence from the findings table is
evidence.

### 3.1 Findings (3)

1. **F32 — `willard1993.md`:110, §5.1, the growth argument. This is the
   deficiency-7 instance batch 1 did not find.** The record renders the
   additive half of the contrast as "for `b_{i+1} = b_i + b_i`, proofs of
   `b_n`'s existence require *more* bits than `b_n`'s binary encoding", and its
   §7 item 3 quotes the same clause. The chapter's sentence (p. 329) ends
   "…requiring more bits than b_n's binary encoding **(at least when the
   Principle of Induction is unavailable to say semantic tableaux proofs)**",
   and the TR's parallel passage is emphatic: "**The caveats that the Principle
   of Induction is absent and that cut-free deduction is employed are necessary
   for the preceding metaphor to retain meaning.**" The record drops the
   condition Willard himself calls necessary, in both places. The record's
   inference — that this is the informal ancestor of `Willard2005` Definition 5
   and Eq. (20) — **survives**, because θ-compactification is itself a cut-free
   construct. But a reader of the record would take the bit-counting contrast
   as unconditional, which is exactly what the source forbids.
   **Carried to Refinement** — restore the parenthetical; it is one clause and
   it carries obligations O44/O50.

2. **F33 — `willard1993-tr.md`:393, an ellipsis spanning two passages.** The
   record writes that `Willard2011` **D.1(iv)** states "≥ 5J bits… Gödel number
   ≥ 32^J". D.1(iv) says only the first half: "at least **5 J bits** are
   required to encode a semantic tableaux proof that has J function symbols."
   The `32^J` half is a **different** passage — the Conventional Tableaux
   Encoding Criteria, two pages later. The ellipsis bridges them and presents
   both as D.1(iv)'s content. **Carried to Refinement** — split the citation.
   *Corroboration for batch 1:* that same passage reads "must be **least as
   large as** 32J", independently reproducing the missing "at" that batch 1
   flagged as F9 and held under Q1. Two retrievals now agree; the image read is
   still owed, because the exponent is the other half of Q1.

3. **F34 — `willard1993-tr.md`:352, Observation A.10.** The record quotes
   "**While `IS(A)` is too weak to prove that concatenation is a total
   function**… it certainly can represent `ConcatenateCheck_m` as a Δ₀
   formula". The source's first clause continues "…is a total function **over
   the domain arguments** …", a scope restriction on the non-totality claim
   that the ellipsis removes. The record's use — concatenation as another
   instance of the demote-to-relation pattern — survives, but the restriction
   belongs with it. **Carried to Refinement.**

### 3.2 Clean — named (9)

4. **`willard2001.md`:454, the Self-Consistency question** (F28). The dropped
   tail is "despite the limitations of the Second Incompleteness Theorem" —
   the clause that ties Willard's rhetorical question to G2 rather than leaving
   it a generic remark about human confidence. Restoring it **strengthens** the
   record's §3.10 reading and obligation O36 rather than weakening it, so the
   inference survives; the defect is the unmarked truncation, not the ground.
5. **`willard2002c.md`:122, Remark 5.** "There is no space to insert the proof
   … we will display it elsewhere" elides only "of this added theorem here,
   and". Restored sentence matches, and it confirms the record's own
   discrepancy 2: the source really does say `W` is provable from **IΣ₀** and
   the level is **Level-2**, against `Willard2004`'s **PA** and **Level(2+)**.
   **Clean.**
6. **`willard2006b.md`:110, Definition 7.** "A type of Gentzen-like deductive
   cut rule for sentences that belong to … `ℜ`" elides "the intermediate class,
   that is formalized by" — a restatement of the same referent. **Clean.**
7. **`willard1993.md`:275, `IS(A,g,d)`.** Two ellipses, both covering only
   framing verbs ("denote such generalizations"; "and that these formalisms be
   called"). The claim — that 1993's notation foreshadows `Willard2011`
   Definition 3.4's ξ — is untouched, and the anchor (LNCS p. 334 = PDF 10)
   resolves. **Clean.**
8. **`willard1993-tr.md`:66, the subscript `d`.** The ellipsis covers only the
   system name `IS^s_d(A, G)`, which the record supplies in its lead-in.
   **Clean.**
9. **`willard2018.md`:4 and 18, the Ref [56] ruling.** The ellipsis covers
   "appears in an essentially"; dropping "essentially" softens nothing. The
   separate typo finding is F26. **Clean** as to elision.
10. **`willard2020-lfcs.md`:84, the Theorem 1 / Theorem 2 divergence quote.**
    Block quote verified against p. 17 in full; the only text-layer difference
    is footnote marker 7 rendered as "only7". Footnote 7 is quoted correctly
    too: "Actually, we will only need the 'Locally 1-Closure' property to prove
    that IS_Xtab(β) cannot possibly be self-justifying." **Clean** — and this
    is the passage obligation O48's chain rests on.
11. **`willard1993-tr.md`:200, Remark 2's Meta-Logic passage.** Read against
    collated p. 26; the OCR is too degraded to adjudicate wording, but the
    structure and the surrounding two sentences match, and the record declares
    the page image-read. **Clean, on the declaration.**
12. **`willard1993-tr.md`:446, Lemma 5.1's `∃v₁<x … ∃v_{2λ}<x`.** The ellipsis
    is inside a formula, not a quotation. **Clean.**

---

## 4. Corrections applied (5, the stated maximum)

Each is unambiguous and localized. Each was verified by re-reading the file
after the edit (goal §5, P1/P2); the verification grep is recorded below.

| Id | File : line | Change | Finding |
| --- | --- | --- | --- |
| **C1** | `willard1993.md`:44 | "`Willard2001` reference [67]" → "`Willard2005` reference [67]", plus a clause recording that the annotation sits on 2005's entry for `Willard2001` and points at its [65] = this chapter, and that `Willard2001`'s bibliography stops at [46] | F18 |
| **C2** | `willard1993.md`:264 | `a₁₀` → `a_400`, aligning the verification-pass entry with the same record's §5.1, plus the TR's `a399` / "approximately 300 lines" cross-reference | F29 |
| **C3** | `willard2018.md`:50 | "Type-S systems fail under enrichments" → "fail under **infinite** enrichments", with an explicit note that Remark 3.12 restricts the failure to that case | F31 |
| **C4** | `willard2018.md`:56 | "preview of `Willard2016` statement `###`" → "the same objection `Willard2016` (Dec 2016) had already put as its statement `###` … restated here two years later", plus "The phrase 'almost cheating' is `Willard2016`'s, not this paper's" | F19 + chronology |
| **C5** | `willard2004.md`:23–24 | The paraphrase in quotation marks replaced by the source's verbatim tail "will fail at and above the Π∗2 level", with the surrounding subject restored and `Willard2020` ref. **[49]** named as the citation that resolves to this paper | F20 |

C4 also repairs a **reversed chronology**: `Willard2016` is arXiv 1612.08071
(December 2016) and `Willard2018` is arXiv 1807.04717v2 (November 2018), so a
2018 passage cannot be a "preview" of a 2016 one. The record's other lineage
claims run the right way (thesis 5's enrichments *do* preview `Willard2020`'s
Xtab; thesis 2's Sacks recollection *is* expanded later in
`Willard2020-LFCS`), which is why the one inversion is worth naming rather
than smoothing.

Post-edit verification (all five present, no residual defective string):

```
grep -n "Willard2005\` reference \[67\]" willard1993.md   → line 44
grep -n "a_400\` exists"                 willard1993.md   → line 264
grep -n "a399"                           willard1993.md   → line 269
grep -n "infinite\*\* enrichments"       willard2018.md   → line 51
grep -n "already put as its"             willard2018.md   → line 56
grep -n "ref. \[49\]"                    willard2004.md   → line 24
grep -c "Willard2001\` reference \[67\]" willard1993.md   → 0
grep -c "a₁₀"                            willard1993.md   → 0
grep -c "preview of \`Willard2016\`"     willard2018.md   → 0
grep -c 'cites for "the'                 willard2004.md   → 0
```

---

## 5. R2 — attribution verification

Three claims per record, 36 in all; anchors resolved against form-feed page
boundaries. **35 pass, 1 fails.** Two witnesses needed their anchoring
convention re-derived first, and both check out: `willard1993` PDF *n* = LNCS
324 + *n* (verified at 4/328, 7/331, 8/332, 11/335), and `willard1993-tr`
collated = printed + 2 (verified at 5/3, 26/24, 35/33, 57/55). `pdftotext`
emits one trailing empty chunk per witness, so a 32-page PDF reports 33.

### 5.1 Passing (35) — the load-bearing ones named

| Record | Claim | Verified |
| --- | --- | --- |
| `willard1993.md` | §5.2's variable-duplication passage and the Gödel diagonalization parallel at **p. 328** | PDF 4 ✓ |
| `willard1993.md` | §5.6's ZF passage, "it can prove its consistency equivalent to that of an *alternate* system IS(ZF)", at **p. 331** | PDF 7 ✓ |
| `willard1993.md` | §5.5's structural definition of the "cut-free" property at **p. 332** | PDF 8 ✓ |
| `willard1993.md` | §4's Main Conjecture, "We conjecture that the answer to both open questions is 'yes'", at **p. 335** | PDF 11 ✓ |
| `willard1993-tr.md` | Remark 2's Meta-Logic at printed **p. 24** | collated 26 ✓ |
| `willard1993-tr.md` | `[So94]` "private communications (April 1994)" in the reference list, printed **p. 55** — the evidence for the D1 re-dating | collated 57 ✓ |
| `willard1993-tr.md` | "a **tableaux proof with cuts**" = `Willard2020`'s Xtab, printed **p. 33** | collated 35 ✓, inside §13 as the record states |
| `willard1993-tr.md` | the three-category tabulation at printed **p. 3** | collated 5 ✓. This is also the string batch 1's **F5** retargeted from `Willard1993` to this witness; the retarget is confirmed from the other side |
| `willard2001.md` | the seven grounding functions at **p. 9** | PDF 9 ✓, and the non-growth definition `F(a₁,…,a_j) ≤ Maximum(a₁,…,a_j)` is verbatim on the same page |
| `willard2001.md` | Theorem 7.2's Löb comment, "we do not assume that α recognizes either Addition or Multiplication as total functions", at **p. 38** | PDF 38 ✓ |
| `willard2001.md` | Appendix B's encoding at **p. 51**: a byte is six bits, **twenty-one** atomic symbols with leading bit 1, constant for `i` in `⌈log₃₂(i+1)⌉ + 1` bytes, base 32 with lead bit 0 | all four verbatim ✓ |
| `willard2004.md` | the Level(0-)…Level(n+) definitions at **pp. 5–6** | PDF 5 ✓ |
| `willard2004.md` | the §7 complementarity passage at **p. 9** | PDF 9 ✓ (wording defect F21 is separate) |
| `willard2004.md` | Remark 1's "We will not have the page space to prove this stronger result here" at **p. 6** | PDF 6 ✓ |
| `willard2006a.md` | `ISCE` = "Introspective Semantics with Continuous Expansion" at **p. 16**, with Group-1 delegating to "Table I of [46]" | PDF 16 ✓ |
| `willard2006a.md` | Hybrid(H) naming at **p. 30** | PDF 30 ✓ |
| `willard2006a.md` | Appendix A: six-bit byte, **20** symbols, `⌈log₃₂(i+1)⌉ + 1` bytes, base 64 — "identical to `Willard2001` Appendix B but for 20 symbols rather than 21, the missing one being the tableaux parameter marker" | both halves verbatim ✓, and `willard2001.txt` p. 51 independently says of the parameter symbol "This symbol is not used in Hilbert-style proofs" — the record's explanation is the source's own |
| `willard2006b.md` | footnote 1's "This result was first announced at the Tableaux-2005 Symposium" at **p. 2** | PDF 2 ✓ |
| `willard2006b.md` | the "gendre" typo at **p. 9** | PDF 9 ✓ |
| `willard2006b.md` | `Willard2005-TAB`'s "pp. 1189–**1999**" digit slip (drift D57/D72) | verbatim in `willard2005-tab.txt` p. 1 ✓ — correctly attributed across witnesses |
| `willard2005-tab.md` | the same p. 1 typo, and Theorem 7's "It will thus appear in a longer version of this article" at **p. 13** | PDF 1 and PDF 13 ✓ |
| `willard2002c.md` | the methodological principle at **p. 2** | PDF 2 ✓ |
| `willard2002c.md` | Theorem 2's admission condition at **p. 11** | PDF 11 ✓. The record renders "section 1.2's 'Group-1' axiom schema" as "§1.2's Group-1 axiom schema"; a notation substitution, not a wording change |
| `willard2002c.md` | "`R( i , j )` Tableaux-Hierarchy Proof" and the two-index condition "each of `t₁…t_{n−1}` are required to have a `Πi∗` or `Σj∗` format" at **p. 16** | PDF 16 ✓ — and the record's erratum 1 (starred classes in the `R(i,j)` definition against `Π_n⁻`/`Σ_n⁻` in the body) is confirmed on the page |
| `willard2002c.md` | `Willard2004`'s "has a copy of it on his desk" | verbatim in `willard2004.txt` ✓ — attributed to `Willard2004` in the record, correctly |
| `willard2006-wollic.md` | the Clarifying Comment's `Diag(1)`/`Diag(2)` invalidity claim at **p. 10** | PDF 10 ✓ |
| `willard2006-wollic.md` | §5's "third facet of a 3-part project", [35] the first facet | PDF 11 ✓ |
| `willard2006-wollic.md` | [40] = SUNYA-CS-06-01 in the reference list | PDF 15 ✓, inside pp. 13–15 as the record states |
| `willard2020-lfcs.md` | Definition 4 Locally-J-Closed and Theorem 3's postponement "until a longer version of this article" at **p. 16** | PDF 16 ✓ |
| `willard2020-lfcs.md` | the Acknowledgment at **p. 15** thanking **only** Seth Chaiken | PDF 15 ✓ |
| `willard2020-lfcs.md` | the "we humans believe PA is consistent…" sentence is an **arXiv-era addition to a footnote** | `willard2020.txt` PDF 16, in footnote position below the Xtab body text ✓, and absent from `willard2020-lfcs.txt` ✓. The record truncates before "**via its 'I am consistent' axiom**" without an ellipsis — noted, below the finding threshold |
| `willard2020-lfcs.md` | LFCS says Gödel "published fewer than 85 pages" against `Willard2020`'s "only about 85 pages" | both verbatim in their respective witnesses ✓. `Willard2020` in fact says it **twice**, "only about 85 pages" (p. 3) and "only published roughly 85 pages" (p. 3) |
| `willard2014.md` | Definition 2's Kernelized Formula at **PDF 6 / printed 226** | PDF 6 ✓ |
| `willard2014.md` | Theorem 4's `Left`/`Right` half-product functions at **PDF 11 / printed 231** | PDF 11 ✓ |
| `willard2018.md` | the Sacks recollection, Hilbert's program left "very much alive and even more interesting" | PDF 3 ✓ (and again in ref. [38], PDF 33). The source continues "**than it initially was**"; the record truncates without an ellipsis and recasts "Gödel believed (according to Sacks)" as "Gödel told Gerald Sacks" — noted, below the finding threshold |
| `willard2018.md` | Definition 3.11 and Remark 3.12 exist as numbered items and are **in** `registry/results.md` (rows 481–482), so their absence from the M1 exclusion list is correct, not a silent exclusion | ✓ — an initially suspicious reading that the registry check cleared |

### 5.2 Failing (1)

| # | Record : line | Finding | Disposition |
| --- | --- | --- | --- |
| **F35** | `willard2006a.md`:26 | The three naming conventions are anchored to "**pp. 2–3**". Equations (1)–(3) and the `5 = C₄ − C₂ − C₁` illustration are all on **p. 2**; p. 3 carries the Kleene fixed-point discussion and no naming-convention material. The conventions are first *named* on p. 1 | **carried to Refinement** — narrow the anchor to p. 2, or widen it to pp. 1–2. Trivial, recorded because §5's value is that a passing anchor means something |

F18 (§2.1) is also an R2 failure — a claim of the form "PaperX reference N
says…" that does not occur in PaperX — and is counted once, there.

---

## 6. Image-verification queue (17)

R1's rule: *any quotation containing mathematics must be checked against a
rendered page image.* Each record's declared visual-control coverage was read
and its math-bearing quotes tested against it.

**A process finding first.** Of these twelve records, only `willard2006b` has
page images retained in the repository (`page-images/willard2006b/p-01.png` …
`p-11.png`). The other eleven declare visual passes performed with transient
`pdftoppm` renders that were not kept, so **their image-backing cannot be
re-checked from the repository** — it rests entirely on the record's own
declaration. That is deficiency 13's shape (content entered under a rule whose
evidence was not preserved) and it is why §6 accepts declared coverage rather
than verifying it. **F36 — carried to Refinement:** either retain the renders
for the pages a record names as visually controlled, or record the render
command and page list precisely enough to reproduce them.

**Cleared without queueing (6 records).** `willard1993` (all 12 pages),
`willard1993-tr` (all 61, closing G29), `willard2006b` (all 11, images on
disk), `willard2005-tab` (all 15), `willard2014` (all 16) and `willard2018`
(all 35) declare full-witness visual control, so their math quotes are
image-backed and none is queued.

**Queued (6 records).** The entries below are quotes or constants carrying
mathematics on pages the record never rendered. They are **held, not accepted**.

| Q | Record | Page | Object | Why queued |
| --- | --- | --- | --- | --- |
| **Q10** | `willard2001.md`:143 | 11 | `IS^λ(A)` Group-3, Eq. (9) `∀x∀y∀z {SemPrf(⌜Ψ⌝,y) ∧ y^λ < z/x ⊃ Ψ^x_z}` | Pass 2 rendered pp. 9, 10, 19, 22, 28, 51, 54, 60 only. The `y^λ < z/x` gap is the whole system |
| **Q11** | `willard2001.md`:263 | 21 | Remark 4.5 / footnote 7: `u₀ = 2`, `u_{i+1} = u_i²`, `VALUE(u_n) = 2^{2^n}` | p. 21 never rendered; the text layer flattens the tower to "22". This is the corpus's canonical statement of *why* multiplication breaks the engine |
| **Q12** | `willard2001.md`:505 | 34 | Theorem 6.1's Eq. (55) `∀x∀y∀v {x < v^{1/k} ⊃ y^λ < MAX(v,y)/x}` and the side condition `k > 1/(1−λ)` | p. 34 never rendered; the exponents are the theorem |
| **Q13** | `willard2001.md`:295, 504 | 36 | `ISM(A)`'s Eq. (61) `y·Log₂(x+2) < Log₂(z)` and footnote 14's `∀y [∃z y < Log₂ z] ⊃ ¬SemPrf_{ISM(A)}(⌜0=1⌝,y)` | p. 36 never rendered. Obligation O31's multiplication dichotomy rests on exactly this gap being "too weak" |
| **Q14** | `willard2001.md`:311 | 38 | Theorem 7.2's fixed point `Γ(g) = ∀x ∀h ≤ x SUBST(g,h) ⊃ ¬Prf^D_α(h,x)` | p. 38 never rendered. The record's own reading — that the bounded `∀h ≤ x` is what keeps `Θ` **Π⁻₁** — turns on a bound the text layer may drop |
| **Q15** | `willard2001.md`:118, 522 | 63–64 | Table I: "**30** Π⁻₁ axioms" | Never rendered, and this is a **count** asserted from an unrendered table — deficiency 1 and 9 together. `Willard2005`'s Group-1 delegates to it |
| **Q16** | `willard2001.md`:390, 517 | 56 | Theorem C.1: "**25** itemized predicates have LinH decision procedures" | Same class as Q15; p. 56 never rendered |
| **Q17** | `willard2004.md`:102 | 9 | §7's complementarity block quote: `ℜ`, `Π*₁`, `Σ*₁`, `Π*₂`, `Σ*₂`, Level(1), Level(0−) | Pass 2 rendered **p. 4 only**. `ℜ` is a Fraktur glyph the text layer renders as `<`, the class batch 1's Q4 flagged; and F21 shows the passage was transcribed from the text layer |
| **Q18** | `willard2004.md`:122 | 25–32 | §7a's G-good machinery: `ℑ(H) = Σ χ(p_i)`, witness bound `2^{ℑ(H)}`, universal bound `G·2^{−ℑ(H)}`, density `ℑ(P) < (1/3)Log₂(G)` | Appendix never rendered. `ℑ` is a second Fraktur glyph; the sign of the exponent is what distinguishes the `Σ` and `Π` halves, and obligation O54 rests on the pair |
| **Q19** | `willard2004.md`:188 | 13–14 | The Level-hierarchy mechanism: `G_i(x) = 2^{2^i}·x`, `Υ_i`, `Θ_m = [Υ_{m−1} → Υ_m]`, `0_n = Υ₀ ∧ Θ₁ ∧ … ∧ Θ_n`, Hilbert proof length `≤ c·n^c` | Never rendered; a nested tower plus a polynomial bound, and obligation O45's justification |
| **Q20** | `willard2006a.md`:160 | 30 | Hybrid(H) naming `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}`, positive at **H = 1**, negative for **H > 1** | Pass 2 rendered **p. 6 only**. The record calls this "the corpus's only exactly-located boundary threshold (D38)"; the anchor is R2-verified but the formula is not image-verified |
| **Q21** | `willard2006a.md`:81, 86 | 23 | Definition 1's Concise Encoding constraint `q < 2^p`, and Eq. (27)'s density `(1/3)Log₂` | p. 23 never rendered; the density constant is one of D25/O33's four measurements |
| **Q22** | `willard2006a.md`:155 | 13–14 | `Bit(x,i) = Count(x,i) − Count(x,i−1)`, `σ_d(x)`, and **2-reduced** Π⁻₁ sentences | Never rendered. These are what let `ISCE(A)` work without a constant symbol per natural |
| **Q23** | `willard2002c.md`:100 | 13–14 | Lemmas 2 and 3's settings: `L = 1`, `M = ½·Max(P,Q) − 1`, `T = Q` against `L = ½·Max(P,Q)`, `M = Max(P,Q) − 1`, `T = P` | Pass 2 rendered pp. 8, 10, 16. The record calls this "where `Willard2011`'s A-/E-Stability constant `½` comes from" — the two `½`s must be read from the page, and `Check`'s `½` (p. 10) is the only one that was |
| **Q24** | `willard2020-lfcs.md`:84 | 17 | The Theorem 1 / Theorem 2 divergence passage together with the derived-theorem formalisms "illustrated by lines (11) and (12)" | Pass 2 rendered **p. 16 only**. The prose is verified (§3.2 item 10) but lines (11)/(12) are the Locally-J-Closed conditions, and obligation O48's chain runs through them |
| **Q25** | `willard2020-lfcs.md`:118 | 18–19 | Appendix A's rules **5 and 6 transposed** relative to `Willard2020`'s Appendix (∀- and ∃-elimination) | Never rendered. This is erratum 2, a claim about which rule bears which quantifier — precisely the kind of thing a reading order artifact fabricates |
| **Q26** | `willard2006-wollic.md`:19 | 5 | Theorem 1's proof sketch: Eq. (15)'s `ψ*` split-representation counterpart and `Size_L` | Passes 2 and 3 rendered pp. 8, 9, 10. p. 5 was never rendered, and the record's identity claim against `Willard2009`'s renumbered (20)/(21)/(22) rests on it |

One residual note below the queue threshold: `willard2006b.md` and
`willard2005-tab.md` both state Definition 3's value formula
`𝐑ᵢ = Mᵢ · 2^{−⌊Log₂(|Mᵢ|)⌋} · 2^{Eᵢ}` and both declare it read from the image
(drift D71); the `willard2006b` images are on disk, so the shared claim is
independently supported.

---

## 7. What this batch says about the deficiency table

| Deficiency | Caught here? |
| --- | --- |
| **5** Cross-paper misattribution | **Yes, but less dominant than in batch 1** — 2 of 19 findings (F18, F19). Both are the same shape: an authorial phrase from one paper filed under another in the same lineage pair. F18 is the sharper of the two, because a reference number ([67]) was carried across from a paper whose bibliography stops at [46] — a check as cheap as "does PaperX have a reference N at all" would have caught it |
| **6** Non-verbatim quotation | **Yes — the dominant failure mode here.** 9 instances (F20–F28). Three are silent repairs of Willard's own typos (F25 "univeristy"/"tehcnical", F26 "annnouncement", F27 "or"), continuing batch 1's F11/F12 pattern; three are dropped words inside quotations (F21, F23, F24); two are paraphrase set in quotation marks (F20, F22); one is unmarked terminal truncation (F28) |
| **7** Elision moving a claim's ground | **Yes — the first instance in either batch.** F32: the 1993 growth argument is presented without the Principle-of-Induction caveat that Willard's own TR calls "necessary for the preceding metaphor to retain meaning". Batch 1 checked seven elision sites and found none; twelve more here found one, plus two elisions that bridge or truncate scope (F33, F34) |
| **3** Text-layer corruption reaching the record | **Indirectly** — Q10–Q26 show math quotes still entering from the text layer on unrendered pages, and F21 is a wording defect inside a math-bearing block quote on a page that was never imaged, exactly batch 1's Q1 construction |
| **1 / 13** Coverage does not imply inventory; content predates its rules | **Yes, in a new form (F36).** Eleven of twelve records declare visual control over pages whose renders were not retained, so the declaration cannot be re-checked. F30 is the inventory shape: Definition 3.11's fourth clause is missing from both the record and `results.md` |
| **9** Counts drifting | **Yes** — F29, a record giving two different subscripts for the same constant in two of its own sections |

Batch 1 found 14 findings across 8 records at roughly one per 16 fragments.
This batch found **19 across 12 records**, at roughly one per 13 *adjudicable*
fragments (251 minus the 90 OCR-blocked). The rate has not fallen, and the
class mix has shifted from misattribution toward non-verbatim quotation — which
is what one would expect once the records being read are the ones whose
witnesses are single papers rather than members of conference/journal pairs.

Goal §7 predicted a clean report would be evidence of weak checks. Two records
did come back with no non-verbatim fragments at all (`willard2006a`,
substantially `willard2005-tab`); in both cases the record's own saturation
table shows a completion read plus a visual pass, which is the difference.

---

## 8. Carried to Refinement

1. **F21** `willard2004.md`:102 — restore "the set of" **after** Q17's image read.
2. **F22** `willard2001.md`:147 — restore `ISMULT(A)`'s Group-3 wording or drop the quotation marks.
3. **F23** `willard2001.md`:503 — restore "for".
4. **F24** `willard2002c.md`:130 — restore "deduction".
5. **F25** `willard2006-wollic.md`:69 — note the two source typos.
6. **F26** `willard2018.md`:4, 18 **and** `registry/results.md`:220 — note "annnouncement".
7. **F27** `willard2014.md`:25 — note the "or"/"of" source typo in §7 errata.
8. **F28** `willard2001.md`:454 — restore the "despite the limitations…" tail or mark the elision.
9. **F30** `willard2018.md`:45 **and** `registry/results.md`:481 — add Definition 3.11's **Infinitely Enriched** clause.
10. **F32** `willard1993.md`:110 and :257 — restore the Principle-of-Induction caveat. **Highest priority in this list**: it is the batch's one deficiency-7 finding.
11. **F33** `willard1993-tr.md`:393 — split the D.1(iv) citation from the Conventional Tableaux Encoding Criteria.
12. **F34** `willard1993-tr.md`:352 — restore "over the domain arguments".
13. **F35** `willard2006a.md`:26 — narrow the naming-convention anchor to p. 2.
14. **F36** — retain page renders, or record the render command and page list, for every page a record declares visually controlled.
15. **Q10–Q26** — the image batch above.

**Two recommendations, both cheap.**

**M9, extended.** Batch 1 proposed M9 as "for every quoted string in an
extraction record, assert the string occurs in the witness that record names".
This batch's F18 shows the check needs a second clause: **for every citation of
the form "`PaperX` reference [N]", assert `PaperX`'s bibliography has an entry
[N]**. That is a one-line bound check against the highest reference number in
each `sources-text/*.txt`, and it would have caught F18 without reading
anything.

**M10 — the OCR gate.** `willard1993` and `willard1993-tr` are the only
extraction records in the corpus whose witness text layer is OCR, and 90 of
this batch's 135 non-verbatim fragments come from them. Any mechanical
quotation check must **skip** these two and say so, or it will emit ninety
false failures and be switched off. The corresponding manual obligation is that
their quotations are accepted only on a declared image read — which F36 says
is currently unverifiable.

---

## 9. Remaining Stage 5 scope

**13 of 33** extraction records are still uncovered after batches 1 and 2:

`dew-1991-nsf-report`, `dew-2008-zcf-group`, `dew-2014-notarized`,
`dew-2020-incompleteness-notes`, `dew-2025-boundary-draft`,
`dew-2025-hilbert-draft`, `dew-2025-tab-xtab-notes`, `dew-hajek-correspondence`,
`DEW-Resume`, `willard1998`, `willard2006-talk`, `willard2007-kgs6`,
`willard2013`.

`willard2005-fidelity-check.md` and `willard2011-subsumption-audit.md` are
derived audit records in the same directory and should be swept with them.

Note that nine of the thirteen are **nachlass** items whose text comes from
`../collected_dew_materials/ocr/` rather than `sources-text/`, so §8's M10
concern governs most of what remains: batch 3 will be closer in character to
this batch's `willard1993` pair than to its publisher-text records, and should
be planned as an image-led rather than text-led pass. On the combined rate of
batches 1 and 2 (33 findings / 20 records), expect roughly 20 further findings.
