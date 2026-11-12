# Stewart's LtU remarks on SJAS, and the Dialectica interpretation (2026-10-20)

A conversation note. The user asked for an old Lambda the Ultimate (LtU) post
about pre-allocating a consumable resource to a proof system, then for Charles
Stewart's remarks there on Willard's self-justifying axiom systems (SJAS), and
then whether Stewart's claim that SJAS have no Dialectica interpretation is
true. No formal result is added. The argument in §3 is ours and unpublished:
a literature search found no work connecting Willard's systems with the
Dialectica interpretation.

## 1. Sources found

- **The thread.** "Total functional language self interpreter?", LtU node
  3076 (2008): <http://lambda-the-ultimate.org/node/3076>. The
  resource-budget idea the user remembered is Matt M's "Be careful again"
  (#comment-45185): a first language at a level such as 1e24 that loses a
  level with each self-evaluation. See also Brandon Moore's "Approximation"
  (#comment-45179) on stratified evaluators, and Matthieu Sozeau's
  #comment-45189 (Coq with n universes needs n+1).
- **Related.** The MIRI "Tiling Agents" draft, §4 (T+α with descending
  ordinals) and §4.2 (Fallenstein's κ, "safe for κ steps"):
  <http://intelligence.org/files/TilingAgentsDraft.pdf>.
- **Stewart's 2004 note** on Willard: Advogato diary of `chalst`, entry 125,
  "Self-Verifying Theories" (22 September 2004). The original link is dead;
  the archive copy:
  <https://web.archive.org/web/20100627145500/http://www.advogato.org/person/chalst/diary/125.html>.

## 2. What Stewart wrote (LtU node 3076, November 2008)

- #comment-44860, "Self-verifying theories": Willard's systems might in
  principle support a total functional language with a self-interpreter, "although
  I doubt it: while the consistency sentence does arise from an algorithmic
  intuition (from tableau search) it does not obviously support a Dialectica
  interpetation (ie. the proof theory does not yield an equivalent equational
  theory), and so it is not clear what functional language is supposed to
  correspond to such a self-verifying theory."
- #comment-44897, "Two notions of consistency": distinguishes internal
  consistency (the proof of the consistency sentence inside the system) from
  external consistency (a relative consistency proof in PRA). "There might be a
  function algebra corresponding to these systems, but it could not be based
  on the simply-typed lambda calculus. If I were to go looking for one, some
  system of light linear logic would be the place I would start."
- #comment-44898, "Diagonalisation": Willard's systems escape diagonalisation
  "by not being able to prove the totality of the necessary functions because
  multiplication is not provably total. … As I say, I doubt the existence of
  such a system."

So Stewart expressed doubt; he did not prove that no interpretation exists.

## 3. Assessment

**A Dialectica interpretation exists.**
- Willard's positive tableaux systems `IS_D(A)` are axiomatized by Π₁
  sentences only, with no induction schema. The axiom groups are Group-0
  equations, a finite Π₁ base, the Π₁ reflection schema and the Π₁
  self-consistency axiom (`nachlass/codification/codified-sjas.md` §5.1).
- Gödel's interpretation needs a recursor only to interpret induction. For
  first-order logic alone, typed λ-terms with definition by cases suffice.
  Gerhardy and Kohlenbach (2005) do this for predicate logic, and their
  realizers β-normalize to ordinary first-order (Herbrand) terms.
- A Π₁ axiom needs no witness. Its bounded quantifiers can be Skolemized by
  functions bounded by their bounding terms. Every axiom, the
  self-consistency axiom included, therefore carries over unchanged as a
  universal axiom of the target.
- Every Π₂ theorem then gets a witness term built from Willard's non-growth
  functions, addition, `Double` and cases. All such terms are linearly
  bounded, so multiplication never becomes total.

**"Not based on the simply-typed λ-calculus" is wrong for this
interpretation.**
- Over a base type of numerals with first-order constants, typed λ-terms add
  no growth: their normal forms are first-order terms.
- The claim holds for Church numerals. Schwichtenberg (1976) showed that
  typed λ-terms compute the extended polynomials on them, and these include
  multiplication.
- Light linear logic captures polynomial time, which includes
  multiplication, so it would overshoot these systems.

**What is true: the interpretation is external.**
- The interpreted self-consistency axiom still asserts that the *source*
  system's tableaux proofs derive no contradiction. The target never asserts
  its own consistency.
- A self-justifying target meets Solovay's theorem. The Dialectica soundness
  proof interprets cut as composition of realizers, so the target's own
  deduction keeps the cut. It also proves successor total (successor and
  addition are function symbols) and Solovay's Π₁ hypotheses. That is the
  Type-A/Hilbert cell, which the codification marks negative
  (`codified-sjas.md` §2.4 and the boundary table of §8). Willard's positive
  Type-A results depend on cut-free tableaux deduction.
- So the interpretation preserves SJAS's theorems but not its
  self-justification. Stewart's gloss is right if "equivalent equational
  theory" includes self-justification, and wrong if it means equivalent in
  theorems.
- Caveat: an equational calculus is not literally Willard's Hilbert
  apparatus, and his results are sensitive to such details. Whether a
  cut-free functional calculus could justify itself appears to be unstudied.

**Self-interpreters, the thread's question.** In the function algebra above a
total self-interpreter cannot exist, for a reason simpler than
diagonalization. Every function in the algebra is linearly bounded, but
`eval` on the code of `Double` iterated k times must return 2ᵏ·x, so
`eval(c, c)` grows faster than linearly. The program-level diagonal argument
(`g(x) = eval(x,x)+1`) needs only successor and composition, not
multiplication. The SJAS-style escape would be an evaluation *relation*
`Eval(c,x,y)` never asserted total, as Willard treats multiplication as the
three-place relation `M(x,y,z)`.

## 4. References

- LtU node 3076: <http://lambda-the-ultimate.org/node/3076> (#comment-44860,
  #comment-44897, #comment-44898).
- P. Gerhardy and U. Kohlenbach, "Extracting Herbrand disjunctions by
  functional interpretation", *Archive for Mathematical Logic* 44 (2005)
  633–644; BRICS RS-03-32: <https://www.brics.dk/RS/03/32/>. Checked from
  its abstract only.
- H. Schwichtenberg, "Definierbare Funktionen im λ-Kalkül mit Typen",
  *Archiv für mathematische Logik* 17 (1976). Cited from memory, not
  re-checked.
- D. Willard, "On the Significance of Self-Justifying Axiom Systems from the
  Perspective of Analytic Tableaux", arXiv:1307.0150.
