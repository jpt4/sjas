# Results Registry (numbered-item inventory)

One row per numbered item (definition / theorem / lemma / proposition /
remark / conjecture) extracted from a corpus item. This registry is the
machine-checked index; the human-readable statements live in the per-paper
records under `../extraction/`.

Columns — **Id**: `<Paper>#<Label>` (e.g. `Willard2005#Thm1`). **Paper**:
corpus key. **Label**: the paper's own numbering. **Type**: def | thm |
lemma | prop | remark | conj. **Page**: page anchor in the witness. **Topic**:
canonical topic slug (aligned at C13). **Proof**: full | sketch | cited |
stated-only | n/a. **Depends**: comma-separated Ids or external citations.
**Notes**: variant deltas, drift pointers.

| Id | Paper | Label | Type | Page | Topic | Proof | Depends | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Willard2005#Def1 | Willard2005 | Definition 1 | def | 10 | consistency-preservation | n/a | — | The paper's central problem statement: identify the consistency-preserving axiom maps |
| Willard2005#Def2 | Willard2005 | Definition 2 | def | 11 | iterated-log-notation | n/a | — | Log, Log^k, 2^m_k, and the Sigma\*1 sentence Upsilon(k,m) (Eq. 9) |
| Willard2005#Def3 | Willard2005 | Definition 3 | def | 11 | negative-control-system | n/a | Willard2005#Def2 | NS^{k,m}_D(A): IS_D(A) plus Upsilon(k,m) in Group-0, with Group-3's "this" adjusted |
| Willard2005#Def4 | Willard2005 | Definition 4 | def | 19 | normed-class | n/a | — | Normed(a,b); clause 2 fixes the eight U-Grounding function symbols and no others |
| Willard2005#Def5 | Willard2005 | Definition 5 | def | 19 | compactification | n/a | Willard2005#Def4 | theta-Compactification, parts (A) and (B); side condition p <= ceil(a/b)+1; theta < 1/3. Willard: new in this paper, "the main engine" |
| Willard2005#Def6 | Willard2005 | Definition 6 | def | 33 | proof-checking-predicate | n/a | — | CheckProof(t,p,q) and the minimal-witness function F(t,p) |
| Willard2005#ThmStar | Willard2005 | Theorem ∗ | thm | 14 | hilbert-negative | cited | — | Solovay's generalization of Pudlak Thm 2.3 via Nelson and Wilkie-Paris; never published by Solovay; only expositions are Willard's ([67] App. A, and here) |
| Willard2005#Thm1 | Willard2005 | Theorem 1 | thm | 19 | engine-theorem | full | Willard2005#Def1, Willard2005#Def4, Willard2005#Def5 | The engine. Small-m side condition m >= 3 for theta=1/4 is glossed; see fidelity check |
| Willard2005#Thm2 | Willard2005 | Theorem 2 | thm | 26 | tableau-compactification | full | Willard2005#Lem1, Willard2005#Lem2 | theta = 1/4. Part (A) in section 5.2; Part (B) in Appendix B. Uses Eq. (20) density bound |
| Willard2005#Thm3 | Willard2005 | Theorem 3 | thm | 27 | tableau-preservation | full | Willard2005#Thm1, Willard2005#Thm2 | Immediate corollary |
| Willard2005#Thm4 | Willard2005 | Theorem 4 | thm | 28 | tablist-compactification | full | Willard2005#Thm2 | theta = 1/4; induction over the proof list reducing to Thm 2 |
| Willard2005#Thm5 | Willard2005 | Theorem 5 | thm | 29 | tablist-preservation | full | Willard2005#Thm1, Willard2005#Thm4 | Main positive result of the paper |
| Willard2005#Thm6 | Willard2005 | Theorem 6 | thm | 34 | negative-control-inconsistency | full | Willard2005#Lem4, Willard2005#Cor1, Willard2005#Def6, Willard2005#Def3 | Main negative result. Proof exhibits a 7-step Tab-U\*1-List proof W of f(k-1,m). The "generalizes to all cut-free methods" clause is stated-only |
| Willard2005#Lem1 | Willard2005 | Lemma 1 | lemma | 23 | positive-branch | full | Willard2005#Def4 | Probe(a,b,T) algorithm plus nine-case induction (a)-(i), one per tableau rule |
| Willard2005#Lem2 | Willard2005 | Lemma 2 | lemma | 25 | non-closure | full | Willard2005#Lem1 | Turns on footnote 3: co-occurring Upsilon and its strict negation must both be Delta\*0 |
| Willard2005#Lem3 | Willard2005 | Lemma 3 | lemma | 33 | proof-checking-growth | sketch | Willard2005#Def6 | Explicitly a "Proof Sketch"; asserts an analogous construction to the prior literature |
| Willard2005#Cor1 | Willard2005 | Corollary 1 | thm | 33 | proof-checking-growth | full | Willard2005#Lem3 | Argument-order slip: writes F(p,t) where Def 6 and Lem 3 write F(t,p) |
| Willard2005#Lem4 | Willard2005 | Lemma 4 | lemma | 33 | proof-checking-growth | full | Willard2005#Cor1 | Eq. (26) is Pi\*1 and provable from PA; this is what Group-2 transmits into NS |
| Willard2005#Rem1 | Willard2005 | Remark 1 | remark | 20 | apparatus-extension | stated-only | Willard2005#Thm1 | Cut-free sequent calculus, Herbrand, Tab-Q\*1-List, and Q\*1-restricted Hilbert also preserve consistency. Case analysis omitted for space. The 2005 witness behind the apparatus audit's cut-free-sequent affirmative |
| Willard2005#Rem2 | Willard2005 | Remark 2 | remark | 21 | method-note | n/a | — | Visualizability of the new proof vs [67] Thm 4.3 |
| Willard2005#Rem3 | Willard2005 | Remark 3 | remark | 21 | boundary-explanation | n/a | Willard2005#Def5 | The G2 generalizations hold exactly where Definition 5 stops applying |
| Willard2005#Rem4 | Willard2005 | Remark 4 | remark | 21 | multiplication-boundary | stated-only | Willard2005#Def5 | Multiplication as a ninth function symbol overwhelms Definition 5; no analogue exists for any D once multiplication is total (second clause cited to [68]) |
| Willard2005#Rem5 | Willard2005 | Remark 5 | remark | 27 | prenex-conventions | stated-only | Willard2005#Thm2, Willard2005#Thm3 | The Prenex\* conventions I and II are dispensable; they only simplify |
| Willard2005#Rem6 | Willard2005 | Remark 6 | remark | 30 | tangibility-hybrid | stated-only | Willard2005#Thm5 | Tangibility Reflection Principles of [67] can be hybridized; "details very lengthy, therefore not done here" |
| Willard2005#Rem7 | Willard2005 | Remark 7 | remark | 30 | floating-point-application | stated-only | Willard2005#Thm5 | The floating-point venue change makes addition, multiplication, subtraction, division total over computer reals. Refers to [71] plus an unlocated full-length TR (gap G3) |
| Willard2005#Rem8 | Willard2005 | Remark 8 | remark | 36 | negative-control-strengthenings | stated-only | Willard2005#Thm6 | Three stronger variants of Thm 6: other cut-free methods; empty A; dropping the addition and doubling totality axioms |
| Willard1993-TR#Prop1 | Willard1993-TR | Proposition 1 | prop | 8 | origin-main-theorem | full | — | For each nice A, IS(A) is consistent. Proved in section 6. Visually verified |
| Willard1993-TR#Prop2 | Willard1993-TR | Proposition 2 | prop | 9 | reflection-positive | full | Willard1993-TR#Prop1 | IS(PA+) proves local reflection for every Delta0 sentence, for decidable Pi1 sentences, and semi-uniform reflection for Delta0 Phi_i. Proved in section 7 |
| Willard1993-TR#Prop3 | Willard1993-TR | Proposition 3 | prop | 9 | reflection-positive | full | Willard1993-TR#Prop1 | IS^{Sigma1}(A) with strong-uniform Sigma1 reflection in Group-3 is consistent for nice A. Proved in section 8 |
| Willard1993-TR#Prop4 | Willard1993-TR | Proposition 4 | prop | 9 | reflection-positive | full | Willard1993-TR#Prop3 | Semi-uniform reflection for Sigma1 Phi_i in IS^{Sigma1}(PA+) and the L/U variants. Proved in section 9 |
| Willard1993-TR#Prop5 | Willard1993-TR | Proposition 5 | prop | 10 | reflection-boundary | full | Willard1993-TR#Prop3 | Negative: IS^{LPi1}(A) asserting local Pi1 reflection is inconsistent for some nice A. Proved in section 11 |
| Willard1993-TR#Prop6 | Willard1993-TR | Proposition 6 | prop | 10 | multiplication-boundary | full | Willard1993-TR#Prop1 | Negative: adding multiplication-totality (G_m) to a consistent system makes it inconsistent for some nice A. The origin of the single-axiom negative control. Proved in section 12 |
| Willard1993-TR#Prop7 | Willard1993-TR | Proposition 7 | prop | 10 | cut-permitting-positive | full | — | Dropping addition-totality (G_minus) makes IS_+(A,G_minus) consistent even with cuts. Origin of the no-total-addition Hilbert profile. Proved in section 13 |
| Willard1993-TR#Prop8 | Willard1993-TR | Proposition 8 | prop | 29 | reflection-positive | stated-only | — | Proof explicitly postponed to the unlocated forthcoming report [Wi94] (gap G14). OCR-level; pass 2 to verify |
| Willard1993-TR#Prop9 | Willard1993-TR | Proposition 9 | prop | 29 | reflection-positive | sketch | Willard1993-TR#Prop8 | Slightly weaker than Prop 8; short proof sketched, fuller justification in [Wi94]. OCR-level; pass 2 to verify |
| Willard1993-TR#SolovayThm | Willard1993-TR | Solovay's Theorem | thm | 10 | hilbert-negative | cited | — | No consistent Gentzen system with cuts can prove a finite set F of PA's Pi1 theorems, recognize Subtraction, non-zero Division and Successor as total, and prove its own consistency. Cited to [So94] private communications April 1994; discovered after learning of Willard's results |
| Willard1993-TR#Lem5.1 | Willard1993-TR | Lemma 5.1 | lemma | 14 | encoding-machinery | full | — | Majorized formulae; overcoming the absence of a multiplication function symbol. OCR-level |
| Willard1993-TR#Lem5.2 | Willard1993-TR | Lemma 5.2 | lemma | 15 | encoding-machinery | full | Willard1993-TR#Lem5.1 | Variant for fixed j and variable Phi. OCR-level |
| Willard1993-TR#Lem5.3 | Willard1993-TR | Lemma 5.3 | lemma | 28 | encoding-machinery | full | Willard1993-TR#Lem5.1 | Supports Eq. (9.1) for IS^{Sigma1}(PA+). OCR-level |
| Willard1993-TR#Lem6.1 | Willard1993-TR | Lemma 6.1 | lemma | 20 | non-subcomponent | full | — | The inconsistency sentence is not a subcomponent of any Group-1 or Group-2 axiom. Same as chapter Lemma 1 |
| Willard1993-TR#Lem6.2 | Willard1993-TR | Lemma 6.2 | lemma | 20 | witness-construction | full | Willard1993-TR#Lem6.1 | An inconsistency proof must construct a witness p-star within itself. Same as chapter Lemma 2 |
| Willard1993-TR#Lem7.1 | Willard1993-TR | Lemma 7.1 | lemma | 23 | terse-proof-trees | full | — | OCR-level |
| Willard1993-TR#Lem12.1 | Willard1993-TR | Lemma 12.1 | lemma | 32 | multiplication-boundary | full | Willard1993-TR#Prop6 | Supports Proposition 6. OCR-level |
| Willard1993-TR#Rem1 | Willard1993-TR | Remark 1 | remark | 9 | reflection-family | n/a | Willard1993-TR#Prop3 | Four alternate systems asserting local Delta0, local Sigma1, second-uniform Sigma1, second-uniform Delta0 reflection; all weaker than IS^{Sigma1}(A) |
| Willard1993-TR#ThmA1 | Willard1993-TR | Theorem A.1 | thm | 37 | encoding-machinery | full | Willard1993-TR#Lem5.1 | Appendix A majorized-formula theorem. OCR-level; Appendix A inventory pending pass 2 |
| Willard1993#Prop1 | Willard1993 | Proposition 1 | prop | 330 | origin-main-theorem | full | Willard1993#Lem1, Willard1993#Lem2 | The main theorem, proved in section 5 of the chapter |
| Willard1993#Prop2 | Willard1993 | Proposition 2 | prop | 330 | reflection-positive | stated-only | Willard1993#Prop1 | Explicitly "not proven in this 12-page Extended Abstract"; proved in the TR |
| Willard1993#Prop3 | Willard1993 | Proposition 3 | prop | 330 | reflection-positive | stated-only | Willard1993#Prop1 | IS\*(A), the chapter's name for the TR's IS^{Sigma1}(A). Includes the remark that Pi1 Reflection makes IS(A) inconsistent |
| Willard1993#Prop4 | Willard1993 | Proposition 4 | prop | 330 | multiplication-boundary | stated-only | Willard1993#Prop1 | ISVALID generalizes Props 1-3; FORBIDDEN(A) and ISFORBIDDEN are inconsistent for some nice A |
| Willard1993#Lem1 | Willard1993 | Lemma 1 | lemma | 333 | non-subcomponent | full | — | Equals TR Lemma 6.1 |
| Willard1993#Lem2 | Willard1993 | Lemma 2 | lemma | 333 | witness-construction | full | Willard1993#Lem1 | Equals TR Lemma 6.2 |
| Willard1993#MainConj | Willard1993 | Main Conjecture | conj | 335 | cut-permitting-conjecture | stated-only | Willard1993#Prop1 | Conjectures that the cut-permitting IS_+(A) and IS\*_+(A) also satisfy part (ii) of self-verification. **Refuted** by Willard's later work; see composition obligation O8 |
