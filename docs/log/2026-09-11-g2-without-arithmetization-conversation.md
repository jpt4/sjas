# G2 without arithmetization: conversation record

Date: 2026-09-11 (America/Indiana/Indianapolis).

## User question (verbatim, with whitespace reflowed)

Goedel's 2nd Incompleteness theorem may be interpreted as proceeding by arithmeticization of syntax as its method of internalization, because arithmetic is a conveniently expressive representation of syntactic forms. That is, with only a small well-grounded extension to pure first order predicate calculus, a substrate of representation of "FOL+arithmetic" is provided. That various data structures (lists, etc.), i.e. syntactic forms, may be encoded via arithmetic via arithmetic attests to this. Alternative substrates of representation must be at least as expressive as arithmetic (e.g. hereditary finite sets, or minikanren (which can represent arithmetic with only Horn clauses and unification over lists)), and so proofs of G2 in these alternative setting _may_ reduce to the proof using arithmeticization, but _must_? Are there any proofs of G2 that, while still constructing explicit representations of syntax (i.e., not strictly proof-modal versions), do not appeal to their equivalence to arithmeticization? Are there, for example, "string calculi", which operate over FOL + String Manipulation Rules?

## Assistant response (verbatim, excluding memory-rendering metadata)

**Yes. G2 can be proved using explicit representations of syntax without invoking an equivalence with arithmetic.** Arithmetization supplies machinery for internal syntactic reasoning; another theory can develop that machinery directly. Being able to translate the resulting proof into arithmetic does not make that translation a prerequisite of the proof.

The clearest completed example is **Paulson’s formalization of Świerczkowski’s approach using hereditarily finite sets**. The underlying first-order theory has an empty-set constant, membership, an adjunction operation \(x\triangleleft y=x\cup\{y\}\), and induction. Syntax is represented using finite-set constructions, including ordered pairs
\[
\langle x,y\rangle=\{\{x\},\{x,y\}\}.
\]
The development constructs syntactic predicates, substitution, diagonalization, and the derivability conditions directly for these representations. Although HF and PA have an equivalence theorem, G2 is established through this syntactic development. Paulson notes that reaching G2 requires some elementary addition and finite-sequence facts, but no multiplication. Thus, avoiding arithmetization does not mean avoiding every elementary numerical argument. See [Paulson’s paper, especially §§1 and 4–7](https://arxiv.org/html/2104.14260).

There is also a precise qualification concerning assumptions: Paulson’s original formalization uses soundness in its treatment of extensions. Popescu and Traytel subsequently formalized G2 for **finite consistent extensions of HF**, removing that soundness assumption while retaining the concrete syntax infrastructure. See [their formalization and description](https://isa-afp.org/entries/Goedel_HFSet_Semanticless.html).

A second relevant approach is **Feferman’s \(FS_0\)**, developed in *Finitary inductively presented logics* (1989). Its basic objects are finite binary expression trees, generated from a distinguished object by pairing. Functions operate recursively on those trees, and inductively defined classes represent languages and derivability. This is close to the representation substrate you suggest through lists and logic programming; Feferman explicitly compares it with Lisp. Section 22 develops the incompleteness argument directly for extensions of \(FS_0\). However, he explicitly leaves details of the internal formalization needed for G2 to a fuller treatment, so this reference provides a **framework and proof sketch**, whereas the HF formalization provides the stronger evidence of completion. [Feferman, §§1 and 22–23](https://math.stanford.edu/~feferman/papers/presentedlogics.pdf).

Your proposed **“FOL + string manipulation”** is also an established subject: *theories of concatenation*. For example, Grzegorczyk’s theory \(TC\) has two distinguished letters and a binary concatenation operation, alongside first-order logic and equality. Its axioms describe associativity, distinct indivisible letters, and how different decompositions of the same word overlap. Grzegorczyk and Zdanowski give a direct proof of essential undecidability in this setting. That establishes the usefulness of the string substrate, but their stated result is essential undecidability, rather than the direct G2 theorem you specifically seek. [*Undecidability and Concatenation*](https://www.impan.pl/~kz/files/AGKZ_Con.pdf).

One deceptively relevant reference deserves care: Visser, de Moor, and Walsteijn’s *How to prove the First and Second Incompleteness Theorem using concatenation* (1986). Its preface explicitly explains that concatenation is embedded into PA. Consequently, it does not meet your stronger requirement, despite its title. [Report, preface](https://ics-archive.science.uu.nl/research/techreps/repo/CS-1986/1986-15.pdf).

To describe what a direct string-based G2 proof would require, let \(T\) be a classical theory with a suitable explicit representation of its syntax. Define
\[
\operatorname{Pr}_T(s)\equiv
\exists p\,\operatorname{Proof}_T(p,s),
\qquad
\operatorname{Con}(T)\equiv
\neg\operatorname{Pr}_T(\ulcorner\bot\urcorner).
\]
Here \(p\) and \(s\) range over **strings**, and \(\ulcorner A\urcorner\) is a term naming the string representing \(A\).

The substantive work is to establish, for this particular representation:

- A diagonal construction giving \(T\vdash G\leftrightarrow\neg\operatorname{Pr}_T(\ulcorner G\urcorner)\).
- Internalization of theorems and internal preservation of modus ponens.
- Positive introspection: provability of \(A\) implies provability of the sentence asserting its provability.

Those ingredients yield \(T\vdash\operatorname{Con}(T)\rightarrow G\). Consistency prevents \(T\) from proving \(G\), so it prevents \(T\) from proving that consistency sentence. This is a schematic reconstruction: establishing the ingredients inside the chosen string theory is the substantial part. The proof predicate remains an explicitly defined predicate on syntax throughout.

Finally, I would qualify **“alternative substrates must be at least as expressive as arithmetic.”** Representing finite syntax, interpreting an arithmetic theory, and proving the required facts about one’s proof predicate are different properties. Pakhomov’s *Gödel’s Second Incompleteness Theorem from Scratch* makes this distinction particularly sharply. He describes syntax through separate sorts for variables, terms, and formulas, with constructor operations, and sketches a fixed-point argument using restricted substitution. His syntax-representation hypothesis can hold even in theories that do not interpret Robinson arithmetic \(Q\), including some decidable theories. G2 is conditional on an accompanying predicate satisfying the derivability conditions; in the complete decidable examples, such predicates collapse into predicates that provably assert everything. The available report is a **proof sketch**, not a full formalization. [Pakhomov, pp. 3152–3154](https://ems.press/content/serial-article-files/46716?nt=1).

For your miniKanren example, representing arithmetic computations establishes a computational capability. Applying G2 additionally requires a specified internal proof predicate, a consistency sentence, and proofs of the relevant syntactic properties. An equivalence with arithmetic is one possible route to those results; the HF development demonstrates that it is not compulsory.

## Literature cited

The following index collects the six sources linked in the answer. Publication
dates and the dates of the linked manuscript copies are distinguished where
applicable.

1. Lawrence C. Paulson, *A Machine-Assisted Proof of Gödel’s Incompleteness Theorems for the Theory of Hereditarily Finite Sets* (2014). [Author manuscript on arXiv, posted in 2021](https://arxiv.org/html/2104.14260). Relevant passages: §§1 and 4–7.
2. Andrei Popescu and Dmitriy Traytel, *From Abstract to Concrete Gödel’s Incompleteness Theorems—Part II* (2020), Archive of Formal Proofs. [Formalization entry](https://isa-afp.org/entries/Goedel_HFSet_Semanticless.html). Covers finite consistent extensions of HF without assuming soundness.
3. Solomon Feferman, *Finitary inductively presented logics*, in *Logic Colloquium ’88* (1989), pp. 191–220. [Author reprint](https://math.stanford.edu/~feferman/papers/presentedlogics.pdf). Relevant passages: §§1 and 22–23; some G2 formalization details are sketched or omitted.
4. Andrzej Grzegorczyk and Konrad Zdanowski, *Undecidability and Concatenation*, in *Andrzej Mostowski and Foundational Studies* (2008), pp. 72–91. [Author manuscript dated April 12, 2007](https://www.impan.pl/~kz/files/AGKZ_Con.pdf). The result cited is essential undecidability.
5. A. Visser, O. de Moor, and M. J. Walsteijn, *How to prove the First and Second Incompleteness Theorem using concatenation*, Utrecht technical report RUU-CS-86-15 (September 1986). [Report](https://ics-archive.science.uu.nl/research/techreps/repo/CS-1986/1986-15.pdf). The preface identifies the embedding of concatenation into PA.
6. Fedor Pakhomov, *Gödel’s Second Incompleteness Theorem from Scratch*, Oberwolfach Report 53/2017, pp. 3152–3154. [Report containing the contribution](https://ems.press/content/serial-article-files/46716?nt=1). The contribution states the syntax-based results and sketches their proofs.
