# C16 Stage 5 — R1–R3 on the extraction records, batch 1 of 33

Evidence for acceptance criterion **A-R** of
[`../C16-verification-goal.md`](../C16-verification-goal.md) §4, restricted to
the eight highest-leverage extraction records. Every finding carries a
disposition: `corrected` / `accepted, with reason` / `carried to Refinement` /
`image-queue`. Sections checked and found clean are named as such, so the
absence of findings is evidence rather than silence.

    10|Audit date: 2026-08-31. Nothing committed.

**Scope.** Records 1–8 of 33. This is a bounded first batch, declared as such
per goal §7 ("if stage 5 proves larger than one component, say so at the start
and split it — do not compress reading depth"). The remaining 25 records are
**not** covered here and must not be reported as done.

| # | Record | Witness key |
| --- | --- | --- |
| 1 | [`../extraction/willard2005.md`](../extraction/willard2005.md) | `willard2005` |
    20|| 2 | [`../extraction/willard2011.md`](../extraction/willard2011.md) | `willard2011` |
| 3 | [`../extraction/willard2002a.md`](../extraction/willard2002a.md) | `willard2002a` |
| 4 | [`../extraction/willard2016.md`](../extraction/willard2016.md) | `willard2016` |
| 5 | [`../extraction/willard2009.md`](../extraction/willard2009.md) | `willard2009` |
| 6 | [`../extraction/willard2020.md`](../extraction/willard2020.md) | `willard2020` |
| 7 | [`../extraction/willard2000-tab.md`](../extraction/willard2000-tab.md) | `willard2000-tab` |
| 8 | [`../extraction/willard2007-apal.md`](../extraction/willard2007-apal.md) | `willard2007-apal` |

---

    30|## 0. Method

Each record was read in full. Every block quote and every inline quoted string
of ≥12 characters was extracted and checked against the witness's text layer.
Quotes carrying an internal ellipsis were **split at the ellipsis** and each
fragment checked separately, so that an elision cannot hide a wording change in
the material either side of it.

Matching used a normalizing comparator (`/tmp/c16v.py`, disposable) that folds
whitespace, line wrapping, hyphen-at-line-break, ligatures, curly punctuation
    40|and diacritics before substring search, and falls back to a windowed
`difflib` ratio so that a **wording difference** (not-verbatim) is distinguished
from an **absent phrase** (not-found). Page anchors were resolved against
`pdftotext`'s form-feed page boundaries rather than the printed folio, which is
unreliable in reading order when footnotes intervene.

Three verdict classes are used:

- **verbatim** — present in the named witness, allowing only text-layer
  artifacts (line wrap, interpolated footnote/page markers, dropped citation
    50|  brackets) that do not change the words.
- **not-verbatim** — present, but the record's wording differs from the page.
- **not-found (in the named source)** — the string does not occur in the
  witness the record names. In every instance below the string was then located
  in a *different* corpus witness, which is deficiency 5, not a fabrication.

Quotations containing mathematics were separately assessed against each
record's own declared visual-control coverage; those whose pages were never
rendered are listed in §6 as **image-queue** and are **not** accepted from the
text layer.

    60|---

## 1. Counts

| Record | Fragments checked | verbatim | not-verbatim | not-found |
| --- | ---: | ---: | ---: | ---: |
| `willard2005.md` | 39 | 38 | 0 | 1 |
| `willard2011.md` | 39 | 36 | 1 | 2 |
| `willard2002a.md` | 21 | 20 | 1 | 0 |
| `willard2016.md` | 23 | 21 | 2 | 0 |
    70|| `willard2009.md` | 14 | 12 | 1 | 1 |
| `willard2020.md` | 51 | 47 | 2 | 2 |
| `willard2000-tab.md` | 7 | 6 | 0 | 1 |
| `willard2007-apal.md` | 33 | 33 | 0 | 0 |
| **Total** | **227** | **213** | **7** | **7** |

Image-queue entries: **9** (§6).
Corrections applied in this pass: **5** (§4).

Fragment counts exceed quote counts because elided quotes were split. Every
    80|not-verbatim and every not-found finding was followed by an R3 read of the
full source sentence and the sentence before it.

---

## 2. R1 — findings, with dispositions

### 2.1 not-found in the named source (7)

These are the deficiency-5 class: a true statement filed under the wrong
    90|source. All seven were located elsewhere in the corpus.

| # | Record : line | Asserted source | Quote | Actually in | Disposition |
| --- | --- | --- | --- | --- | --- |
| **F1** | `willard2000-tab.md`:18 | `Willard2002a` §1 | "presented initially as a 16-page conference abstract" | `Willard2001` **ref. [44]** (bibliography annotation). `willard2002a.txt` contains no mention of a conference version anywhere; its only "conference" occurrence is inside reference [15]'s title | **corrected** — see §4 C3. Note the source prints an en-dash, "16–page" |
| **F2** | `willard2009.md`:28 | `Willard2020` | "verify their Herbrand but not also semantic tableaux consistency" | `Willard2011` printed p. 3. The string **"Almost-M" does not occur in `willard2020.txt` at all** | **corrected** — see §4 C2 |
| **F3** | `willard2005.md`:359 | `Willard2004` | "provably infeasible" | `Willard2005` **p. 2**, describing its own ref. [70]. [70] does resolve to `Willard2004`, so the *claim* was sound and only the *quotation* was misfiled | **corrected** — see §4 C4 |
| **F4** | `willard2011.md`:369 | `Willard2011` (errata list) | "Zermello Fraenkel" | `Willard1993-TR` only (lines 6306, 6325). The parenthetical "(1993-TR usage recurs)" asserts recurrence in the 2011 witness; it does not recur | **carried to Refinement** — delete the entry or restate it as "not repeated here" |
| **F5** | `willard2011.md`:54 | `Willard1993` | "three categories" | `Willard1993-TR`. `willard1993.txt` contains no occurrence of "categor" at all | **carried to Refinement** — retarget the citation to the `Willard1993-TR` key, which is a distinct corpus entry |
| **F6** | `willard2020.md`:39 | `Willard2018` | "quite preliminary …" | `Willard2020`'s **own reference list** — "(This quite preliminary annnouncement of [57]'s results appears in an essentially roughly written summary-abstract form.)" | **carried to Refinement** — attribute to `Willard2020` ref. entry, not to `Willard2018`'s self-description |
| **F7** | `willard2020.md`:39 | `Willard2018` | "… roughly written summary-abstract" | same sentence as F6 | **carried to Refinement** — with F6 |

    100|F6/F7 note: the record's lineage diagram (2018 → 2020-LFCS → 2020) is itself
**confirmed** by the same sentence, since Willard describes 2018 as a
preliminary announcement of **[57]'s** results and [57] is the LFCS chapter.
Only the quotation's owner is wrong.

### 2.2 not-verbatim (7)

| # | Record : line | Record prints | Witness prints | Severity | Disposition |
| --- | --- | --- | --- | --- | --- |
| **F8** | `willard2016.md`:48, 149 | `IQFS` = "Introspective Q-Function **System**" | Definition 5.7: the acronym "IQFS" stands for "Introspective Q-Function **Semantics**" | **load-bearing** — this is the paper's central system name, quoted as a definitional expansion, and it was wrong in two places including the inventory row | **corrected** — see §4 C1 |
   110|| **F9** | `willard2011.md`:187–190 | block quote: "must be **at least as large as** 32^J" | "must be **least as large as** 32J" — the witness omits "at" | moderate — a silent grammatical repair inside a **block quote**, inconsistent with the same record's practice of flagging Willard's typos ("underlied behind", "princible") three sections later | **image-queue** — see §6 Q1. Not corrected from the text layer: the sentence also carries `32^J`/`2J`/`5J`, and R1 forbids accepting mathematics without a page image. Restore as "must be least as large as" once the image confirms it |
| **F10** | `willard2016.md`:190–192 | "all the theorems and **propositions** are the same in this Version 5 as in Version 4**;** the difference is…" | "All the theorems and **propositons** are the same in this Version 5 as in Version 4**. The** difference is…" | low, but self-inconsistent — the **same record quotes the same sentence correctly** at line 4, typo preserved | **carried to Refinement** — make the §7 restatement match line 4, or drop its quotation marks |
| **F11** | `willard2009.md`:34–35 | "…Zofia Adamowicz and Konrad **Zdanowski**" | "…Zofia Adamowicz and Konrad **Zdanowksi**" | low — a silent correction of an evident source typo (the real-world spelling is Zdanowski) inside quotation marks | **accepted, with reason** — the emendation is correct as to the person; add `[sic: Zdanowksi]` if strict verbatim is wanted |
| **F12** | `willard2002a.md`:43 | "**pseudo**-constant symbol" | rule 5: "**psuedo**-constant symbol" | low — same class as F11 | **accepted, with reason** — silent repair of an obvious source typo; note it, do not propagate the misspelling into the registry |
| **F13** | `willard2020.md`:95 | "**Infinite-Ranged Exceptions**" (plural) | "which we shall call an **'Infinite-Ranged Exception'**" (singular) | low | **accepted, with reason** — the record pluralizes a quoted term of art to fit its own sentence; "Declarative Exceptions" in the adjacent clause **is** verbatim plural in the witness |
| **F14** | `willard2020.md`:209 | "**Any** finite set of Π\*₁ sentences `F` with this property may be used" | "(Any finite set of Π∗1 sentences F **, with this property,** may be used **to define Group-1, as [50] had noted**.)" | low-moderate — two commas dropped, and the truncation removes Willard's own credit to **[50] = `Willard2005`** | **accepted, with reason** — see R3, §3 item 4 |

---

## 3. R3 — elision checks
   120|
Every not-verbatim and not-found finding, plus every quote carrying an
ellipsis, was re-read with its full source sentence and the sentence before it.
The question asked was goal §4's: *does the elision drop a clause bearing on
the claim's ground, and does the record's surrounding inference survive the
restored context?*

**Clean — named, so the absence of a finding is evidence.**

1. **`willard2005.md`:161, Remark 4.** This is the exact construction that
   130|   produced deficiency 7 elsewhere in the workstream (the §6.7 "[68] implies
   that" deletion). Here the record **retains** the ground clause: "Moreover
   the Level(0-) tableaux result of **[68] implies** no useful analog of
   Definition 5 exists…, **under any deduction method D, cut-free or
   otherwise**." Restored source sentence matches. **Clean.**
2. **`willard2005.md`:160, Remark 3.** The record's lead-in carries the
   source's causal clause ("the excluded deduction methods … **fail** Parts
   (A)/(B) of Definition 5"), which is the source's "It is because their
   deductive methods do not satisfy Parts (A) and (B)". **Clean.**
3. **`willard2007-apal.md`:78, Remark 1.** The ellipsis in "…a **cut-permissive**
   140|   method of deduction … than when only a cut-free deductive calculi is
   available" elides only "such as Hilbert deduction,". Examples, not ground.
   **Clean.**
4. **`willard2020.md`:209, Group-1** (finding F14). The dropped tail is "to
   define Group-1, **as [50] had noted**" — Willard crediting `Willard2005`.
   The record instead links the clause to `Willard2001` p. 10. That link is
   **independently true**: `willard2001.txt` p. 10 reads "any other finite set
   of Π−1 axioms with these properties is equally suitable". So the record's
   inference survives; what the elision hides is that Willard himself credits a
   *different* prior paper. **Accepted, with reason**; worth one clause in the
   150|   Refinement pass.
5. **`willard2007-apal.md`:119, Appendix A.** Truncation before "or a similar
   counterpart in the Hájek-Pudlák textbook [13]" removes a *second* analogue,
   not the ground of the first. **Clean.**
6. **`willard2016.md`:70–74, the Conjecture 6.6 confidence quote.** Both
   ellipses were split and both flanks verified; the hedge "(It actually falls
   **only one tiny iota short of being a formal proof**.)" is retained, and the
   record's own §4 "Where the iota sits" paragraph *strengthens* rather than
   softens the source. **Clean** — and this is the quote named in deficiency 6
   ("essentially 100 % confident"), which is verbatim here.
   160|7. **`willard2007-apal.md`:45–52, the axiom-versus-theorem quote.** The
   restored sentence begins "(**One of these systems** is even able to prove
   the theorem…", and the record had replaced that subject with a lead-in
   naming the wrong paper. Ground clause intact; subject repaired. See §4 C5.

---

## 4. Corrections applied (5, the stated maximum)

Each is unambiguous and localized. Each was verified by re-reading the file
   170|after the edit (goal §5, P1/P2); the verification grep is recorded below.

| Id | File | Change |
| --- | --- | --- |
| **C1** | `willard2016.md`:48, 149 | "Introspective Q-Function **System**" → "**Semantics**" (both sites) — F8 |
| **C2** | `willard2009.md`:28 | "the source of `Willard2020`'s remark" → "the source of `Willard2011`'s remark (printed p. 3)" — F2 |
| **C3** | `willard2000-tab.md`:18 | Relation row: attribution moved from "`Willard2002a`, whose §1 calls this" to "`Willard2001` ref. [44] calls this", plus an explicit note that `Willard2002a` nowhere mentions the conference version — F1 |
| **C4** | `willard2005.md`:359 | "'provably infeasible' by `Willard2004`" → "'provably infeasible' — this paper's own p. 2 wording for the result of its ref. [70] = `Willard2004`" — F3 |
| **C5** | `willard2007-apal.md`:45–52 | page anchor **p. 29 → p. 30** (F15 below); lead-in "`Willard2009`'s system" replaced by the source's own subject, quoted: "**One of these systems** is even able to…"; `[45]` identified as `Willard2006-WoLLIC` (ENTCS 165, 2006, pp. 213–226), with `Willard2009` named as its journal expansion — F16 |

   180|Post-edit verification (all five present, no residual defective string):

```
grep -n "Introspective Q-Function" willard2016.md          → lines 48, 149 "Semantics"
grep -n "source of .Willard2011"   willard2009.md          → line 28
grep -n "16–page conference abstract" willard2000-tab.md   → line 18
grep -n "provably infeasible" willard2005.md               → lines 359–360
grep -n "One of these systems" willard2007-apal.md         → line 46
grep -rn 'Q-Function System"|whose §1 calls this' .        → none
```

   190|---

## 5. R2 — attribution verification

Goal §4 asks for confirmation that a claim of the form "`PaperX` §N / p. N
says …" occurs in that paper at that anchor. Three claims per record were
spot-checked; anchors were resolved against form-feed page boundaries.

### 5.1 Passing (17)

   200|| Record | Claim | Verified |
| --- | --- | --- |
| `willard2002a.md`:26–31 | boundary block quote at **p. 4** | pdftotext p. 4 ✓ |
| `willard2002a.md`:174 | `V₄`'s CLARIFICATION at **p. 10** | p. 10 ✓ |
| `willard2002a.md`:184–189 | §7's closing note | §7 **Conclusion** exists and begins on p. 29 ✓ (but see F17) |
| `willard2011.md`:35–36 | **[59]** = `Willard1993`, **[64]** = `Willard2005`, **[66]** = `Willard2006a`, **[68]** = `Willard2009` | all four bibliography entries resolve exactly ✓ |
| `willard2011.md`:202–205 | **Remark D.5, p. 45** carries the "trigger-point" sentence | pdftotext p. 46 = printed 45 ✓. The interpolated "31" in the text layer is **footnote marker 31**, not a page break — an initially suspicious reading that the page-boundary check cleared |
| `willard2011.md`:359–363 | **D20**: `Willard2001`'s title page reads "Self-Verifying **Axiom** Systems … **Related Reflection Principles**"; 2011 ref. [61] "gets the subtitle right but drops 'Axiom'" | both halves verbatim; [61] reads "Self-Verifying Systems … Related Reflection Principles" ✓ |
| `willard2016.md`:27–28 | `Willard2020` **§8** reports the conjecture as "as we are almost certain it is" | verbatim, and inside §8 *Further Generalizations* (between the §8 and §9 headings) ✓ |
| `willard2016.md`:56–68 | **Conjecture 6.6 at p. 24** | pdftotext p. 24, page opens "Conjecture 6.6 Suppose γ is an axiom system that includes:" ✓ |
   210|| `willard2009.md`:41–42 | `Willard2002a` **Theorem 6.4** answers the tableaux half of Paris–Wilkie | Theorem 6.4 states exactly that, and is printed **"Proof Sketch"**, matching `willard2002a.md`'s `sketch` status ✓ |
| `willard2009.md`:107 (Definition 16 row in `willard2007-apal.md`) | `Willard2009` **Lemma 5** calls the branch the "pivotal branch" | verbatim, inside the Eq. (41)/(43)/(45)/(48) argument that is Lemma 5's ✓ |
| `willard2005.md`:19, 165 | **Remark 7, p. 30** (submission history, floating-point TR) | pdftotext p. 30 ✓ |
| `willard2005.md`:250–256 | ref. [72]'s pre-publication title | verbatim; the NEAR score was a `pdftotext` column artifact floating "[72]" into the title ✓ |
| `willard2020.md`:29–33 | **p. 20**, "The initial 19-page draft … published by Springer [57]", [57] = the LNCS 11972 LFCS chapter | both verbatim ✓ |
| `willard2020.md`:209–210 | the Group-1 generality clause is "the same … as `Willard2001` **p. 10**" | `willard2001.txt` p. 10 ✓ |
| `willard2020.md`:299–301 | ⊙ rests on "pages 172-174 of the Hájek–Pudlák textbook [25]" | verbatim ✓ (the stray `J` in the text layer is the ⊙ glyph the record already flags) |
| `willard2020.md`:484–486 | ref. [46] "Third Kurt Gödel **Colloquium**" vs `Willard2001` ref. [40] "Third Kurt Gödel **Symposium**" | both verbatim in their respective witnesses ✓ |
| `willard2000-tab.md`:45–46 | Lemma 5's constant `Log(z, 2^{3,000})` against `Willard2002a` Lemma 4.2's `2^{1,000}` | `willard2000-tab.txt` prints `Log( z , 23,000 )`; `willard2002a.txt` prints `1,000` ✓ |
| `willard2007-apal.md`:23–26 | `Willard2006-WoLLIC` **§5** calls the work "the third facet of a 3-part project", with [35] = `Willard2002a` the first facet | §5 *Concluding Remarks*, verbatim; [35] is the year-2002 JSL article; [40] is the second facet — so this paper being facet two is correct ✓ |

   220|### 5.2 Failing (3)

| # | Record : line | Finding | Disposition |
| --- | --- | --- | --- |
| **F15** | `willard2007-apal.md`:46 | The axiom-versus-theorem quote is anchored to **p. 29**. Theorem 6 is on p. 29; the sentence *begins* there ("These papers had noted that some formal systems are able") and the **quoted portion falls on p. 30** after the page break | **corrected** — §4 C5 |
| **F16** | `willard2007-apal.md`:46 | The quote's `[45]` and its subject were credited to **`Willard2009`**. APAL's [45] is "The Axiom System IΣ0 Manages to Simultaneously Obey and Evade the Herbrandized Version…", **ENTCS 165 (Nov 2006)** = `Willard2006-WoLLIC`. A 2007 paper cannot cite a 2009 one. `willard2007-apal.md`:19's "Companion: `Willard2006-WoLLIC` / `Willard2009` ([45] there)" is the ambiguity that let this through | **corrected** — §4 C5. This is the mirror image of the known deficiency-5 instance (Ax-3 quote credited to `Willard2009` when it is `Willard2007-APAL`) |
| **F17** | `willard2002a.md`:18 | The Structure field lists §1–§6 and the Appendix but **omits §7 Conclusion (p. 29)** — while the same record cites "§7's open question" and "§7's closing note" twice at lines 182–189 | **carried to Refinement** — add "§7 Conclusion (29)" to the Structure field |

---

## 6. Image-verification queue (9)

   230|R1's rule: *any quotation containing mathematics must be checked against a
rendered page image.* Each record's declared visual-control coverage was read
and the math-bearing quotes tested against it. The entries below are quotes
carrying mathematics **on pages that record never rendered**; they are held,
not accepted.

| Q | Record | Page | Quote / object | Why queued |
| --- | --- | --- | --- | --- |
| **Q1** | `willard2011.md`:187–193 | printed 39 | Conventional Tableaux Encoding block quote: `32^J`, `2J` logical symbols, `5J` bits, and the disputed "**at least** as large as" | Pass 1v lists p. 39 as visually controlled, yet the wording defect F9 shows the text was transcribed from the text layer. Re-read the image and settle both the exponent and the "at"/no-"at" reading |
| **Q2** | `willard2005.md`:167 | 36 | Remark 8(3): "even though it fails to recognize the assured existence of any integer larger than `2^m_k`" | Tower notation; the record's own §8.1 documents that `pdftotext` renders `2^m_k` as `2m k`. Visual pass covered pp. 11, 19, 22, 26, 33 only — **p. 36 was never rendered** |
   240|| **Q3** | `willard2005.md`:85–86 | 10 | "Our results would be substantially less significant if Equation (6) had a weaker `Π*₂` encoding" | p. 10 not in the visual list; the `Π*₂`/`Π*₁` distinction is the whole point of the sentence |
| **Q4** | `willard2005.md`:366–368 | 7 | "If `ℜ` represents the universal set of all sentences, then `Tab-ℜ-List` deduction will have a deductive-efficiency essentially equivalent to that of a **Hilbert-style proof**" | `ℜ` is a Fraktur glyph of exactly the class the record shows `pdftotext` destroying (`ℑ` → `=`, `℧` → `f`). p. 7 never rendered. This quote is load-bearing for drift D33's five-name identification |
| **Q5** | `willard2005.md`:371–373 | 37 | "there is no analog of this inconsistency when `Ψ` is a logically valid `Π*₁` sentence… `N_D(A,Ψ)` is always consistent" | p. 37 never rendered; the record calls this "the ancestor of `Willard2011` Lemma 6.1", so the `Σ*₁`/`Π*₁` polarity must be read from the page |
| **Q6** | `willard2020.md`:311–314 | 18 | Definitions 7.1 / 7.2: `Υ ∨ ¬Υ` (Line 9) and `∀x Υ(x) ∨ ¬Υ(x)` (Line 10) | Pass 2 rendered pp. 10, 12, 15, 17 only. The record's §3.10 builds a two-page argument on the exact difference between Lines (9) and (10) |
| **Q7** | `willard2020.md`:318–322 | 19 | The §7 boundary table: `Δ*₀` survives / `Π*₁` conjectured / `Π*₂` fails | Same gap; this is "the sharpest boundary statement in the corpus" by the record's own assessment and gap **G22** rests on it |
| **Q8** | `willard2002a.md`:97 | 12 | Lemma 4.2's `Log(z, 2^{1,000})` and "substantially more than 1,000 bits" | Pass 2 rendered **p. 14 only**. The 1,000-versus-3,000 divergence from `Willard2000-TAB` is one of that record's four load-bearing deltas, so both sides need an image; the TAB side has one, this side does not |
| **Q9** | `willard2016.md`:194–196 | — | `C*_j = 2^{2^{j−2}}`, which the record itself reports `pdftotext` flattens to `Cj∗ = 2 2 j−2` | The record names the hazard but does not state that the corrected form was read from an image. Pass 2 covered pp. 12 and 24 only |

   250|**Cleared without queueing.** `willard2007-apal.md` (pass 3 rendered **all 48
pages**, so `℧`, Eq. (33)'s `2^{2^{√p}}` and Theorem 4's `2^{√p}` are
image-backed), `willard2000-tab.md` (pass 2 confirmed all four divergent
constants, `C ≅ 12` and Definition 3's `O(Log N)` on the page), and
`willard2009.md`'s math quotes for pp. 4, 8, 10–12, 14, 19, 22, 25–28. One
residual note, below the queue threshold: `willard2009.md`'s p.-20 `Tab−k` row
cites `Π*_k`/`Σ*_k` from an unrendered page, but nothing inside quotation marks
there carries mathematics.

---
   260|
## 7. What this batch says about the deficiency table

| Deficiency | Caught here? |
| --- | --- |
| **5** Cross-paper misattribution | **Yes — the dominant failure mode.** 7 of 14 findings (F1–F7) plus F16 are a quote or claim filed under the wrong witness. Notably three involve the `Willard1993`/`Willard1993-TR` and `Willard2006-WoLLIC`/`Willard2009` conference-versus-journal pairs, where two corpus keys hold near-identical content |
| **6** Non-verbatim quotation | **Yes** — 7 instances, one load-bearing (F8, the `IQFS` expansion) and one inside a block quote (F9) |
| **3** Text-layer corruption reaching the record | **Indirectly** — F9's silent "at" insertion and Q1–Q9 show math quotes still entering from the text layer on unrendered pages |
| **7** Elision moving a claim's ground | **No.** Seven elision sites read in full; six clean, one (F14) hiding an attribution rather than a ground. The deficiency-7 construction at `willard2005.md` Remark 4 is intact |
   270|| **9** Counts drifting | Not in scope for this batch |

Goal §7 predicted that a clean report would be evidence of weak checks. This
batch found **14 findings across 8 records** at a rate of roughly one per 16
quote fragments, concentrated in exactly the class the specification named
first.

---

## 8. Carried to Refinement

   280|1. **F4** `willard2011.md`:369 — "Zermello Fraenkel" does not occur in the 2011 witness.
2. **F5** `willard2011.md`:54 — "three categories" belongs to `Willard1993-TR`.
3. **F6/F7** `willard2020.md`:39 — the 2018 gloss is `Willard2020`'s wording.
4. **F9** `willard2011.md`:187 — restore "must be least as large as" **after** Q1's image read.
5. **F10** `willard2016.md`:190 — align the §7 restatement with the correct quotation at line 4.
6. **F17** `willard2002a.md`:18 — add §7 Conclusion to the Structure field.
7. **F14** `willard2020.md`:209 — add one clause noting Willard credits [50] for the Group-1 generality.
8. **Q1–Q9** — the image batch above.

   290|**General recommendation.** Findings F1, F2, F5, F6, F7 and F16 all arise where
two corpus keys hold the same content at different lengths (`Willard1993` /
`Willard1993-TR`, `Willard2000-TAB` / `Willard2002a`, `Willard2006-WoLLIC` /
`Willard2009`, `Willard2020-LFCS` / `Willard2020`). A mechanical check in the
spirit of **M1** — *for every quoted string in an extraction record, assert the
string occurs in the witness that record names* — would catch this whole class
and is the natural **M9**. It is cheap: the comparator used for this pass is
about eighty lines.

---
   300|
## 9. Remaining Stage 5 scope

25 of 33 extraction records are **not** covered by this pass:

`dew-1991-nsf-report`, `dew-2008-zcf-group`, `dew-2014-notarized`,
`dew-2020-incompleteness-notes`, `dew-2025-boundary-draft`,
`dew-2025-hilbert-draft`, `dew-2025-tab-xtab-notes`, `dew-hajek-correspondence`,
`DEW-Resume`, `willard1993`, `willard1993-tr`, `willard1998`, `willard2001`,
`willard2002c`, `willard2004`, `willard2005-tab`, `willard2006a`,
   310|`willard2006b`, `willard2006-talk`, `willard2006-wollic`, `willard2007-kgs6`,
`willard2013`, `willard2014`, `willard2018`, `willard2020-lfcs`.

`willard2005-fidelity-check.md` and `willard2011-subsumption-audit.md` are
derived audit records in the same directory and should be swept with them.

On this batch's rate, expect roughly 40 further findings across the remainder.
