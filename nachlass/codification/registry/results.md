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
