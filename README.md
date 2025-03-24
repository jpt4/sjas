# sjas

A Preliminary Research Programme for Autarkic [0] Logic

Can Dan Willard's work on Goedel's Second Incompleteness Theorem be
extended to other limitative theorems in mathematical logic and
programming language theory? Willard's Self-Justifying Axiom Systems
[1] provide a framework for precisely characterizing the features of a
formal system required for Goedel's Second Incompleteness Theorem to
apply. In particular, SJAS are formal systems which retain 1)
consistency relative to Peano Arithmetic and 2) _self-provability of
consistency_, at the cost of weakened, carefully tuned system
expressivity [2]. Any systems stronger than SJAS thus satisfy the
prerequisites for the 2nd Incompleteness Theorem to take effect,
precluding a formal system from simultaneously i) being consistent and
ii) containing a proof of its own consistency.

From Lawvere's Fixed Point Theorem (LFPT) [4], we know that many
limitative results have a common structure. Lawvere, using
category theoretic formulations, and Yanovsky [5], using set theoretic
equivalents, have demonstrated how to use LFPT to prove
1. Cantor’s theorem that N < P(N) (the infinity of the Naturals is
   strictly less than that of the Reals)
2. The inadmissibility of Russell’s paradox
3. The non-definability of satisfiability
4. Tarski’s non-definability of truth and
5. Godel’s first incompleteness theorem.
6. Turing's undecidability of the Halting Problem

among other limitative theorems in other areas of mathematics. This
commonality can be extended, in particular due to the prevalence of
mathematical objects that can encode algorithmic behavior, and thus be
treated as instances of the Halting Problem.

Additionally, it is known that Turing's result can be used to prove
Goedel's [6]. From Undecidability, thus Incompleteness: U -> I - does
~I -> ~U also hold? The absence of Goedel's Second Incompleteness
Theorem from the above presents a gap that, if filled, could connect
Willard's SJAS with Lawvere's FPT: if the 2nd Inc Thm could be
characterized in terms of the FPT, then the features of SJAS which
allow them to evade the Incompleteness Effect could be analyzed, to
determine whether the features of such systems are _similarly
generalizable_ to those fields whose limitative results the FPT
subsumes [7].

Willard touches on the epistemological implications of his work for
"Thinking Beings" in [9]. Formal reasoning conducted within SJAS
benefits from strong epistemic security: positive answers can be given
to the question of "Is this reasoning system consistent?", without
risk of encountering future deductions which contradict this
conclusion, and without reliance on an external meta-system. An
artificial agent _defined as_ an SJAS would therefore be the maximally
expressive agent that retains autarkic confidence in its ability to
make arbitrary statements about its body of knowledge, including
self-knowledge.

[0] "Self-powered"; a logic able to provide strong systemic guarantees
using reflective methods, rather than appealing to external resources
or meta-systems.

[1] Willard 2020: https://arxiv.org/abs/2006.01057

[2] Specifically, Willard identifies a trade-off between the
arithmetic primitives available in the language of a formal system,
and its deduction method. (1) and (2) can be viewed as a "conserved
quantity"; the corresponding conservation law requires that more
expressive languages be paired with less efficient deduction
methods. The maximally expressive family of SJAS languages includes
successor and addition as total functions, but not multiplication,
which must be defined in terms of a relation atop the division
function. In exchange, this family of languages must use the method of
analytical tableaux or, it is conjectured [3], similar proof
techniques like resolution.

[3] "For the sake of simplicity, the previous sections had focused on
the semantic tableau deductive apparatus. However, it is known [15]
that resolution shares numerous characteristics with
tableau. Therefore, it turns out that Theorems 4.4 and 4.5 do
generalize when resolution replaces semantic tableau." - Willard 2020,
p18.

[4] https://ncatlab.org/nlab/show/Lawvere's+fixed+point+theorem

[5] Yanofksy 2003: https://arxiv.org/abs/math/0305282

[6] Oberhoff 2019: https://arxiv.org/abs/1909.04569

[7] Adjacent to the technical goals of the above, it is of great
interest whether there is a convergence between the features of SJAS
and those of the self-interpreter for the strongly normalizing lambda
calculus devised by Brown and Palsberg [7], which similarly speaks of
a barrier overcome by excluding diagonalization.

[8] Brown, Palsberg 2016: http://compilers.cs.ucla.edu/popl16/ 

[9] Willard 2014: https://arxiv.org/abs/1307.0150

*****

Local Objectives:

1. Implement interpreter for the Type NS languages in Willard 2017.

2. Formalize the proof of non-diagonalizability for Type NS languages
in a stronger metalanguage, Idris/Coq/Agda/etc.

3. Formalize as much of the proof in (2) as possible in a Type NS
language itself, co-eval with determining whether/to what extent this
can be done.

Claim: (3) would provide autonomous confidence in the consistency of
the system so proved to be, by closing over the metatheory.