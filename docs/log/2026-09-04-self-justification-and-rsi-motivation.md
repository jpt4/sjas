# Self-justification and reflective self-improvement: motivation and computational interpretation

Date: 2026-09-04 (America/Indiana/Indianapolis).

Conversation record requested by the user. This note records the motivation of
the broader SJAS programme and an assessment with proposed experiments. It is
not an accepted implementation ADR or an assertion that an RSI benefit has
already been demonstrated. The concurrent Codification/Refinement completeness
review is a separate task.

## User message (verbatim)

Another agent is working on that completeness/correction review. The Codification and Refinement documents are part of a broader effort whose goal is to determine whether, and if so how, Self-Justification is relevant and useful to reflective, self-improving (RSI) systems (inclusive of all substrates, but paradigmatically digital/computational). To determine this requires progress along various frontiers of inquiry: what are the limitations of RSI systems? Classically the limitative results of mathematical logic and theory of computation are here invoked; with respect to perfect assurance in the correctness of the system following self-modification, the Lobian Obstacle and Undecidability, amongst others, apply. One route around them is to weaken the standard of correctness by using probabilistic or other measures; another is, seemingly, to weaken the system to which such total confidence applies, as SJAS does with its sub-Peano arithmetic. However, this is not a complete characterization, because the same underlying lack of "absolute", "objective", or "reference frame independent" confidence in system remains; what obtains is rather insight into the lessons of G2 (and potentially other limititative theorems), which is the introduction of a distinction between outer and inner confidence in the justification of the correctness of a system. G2 specifies the circumstances under which total internal confidence in correctness is impossible. The evasion of G2 opens the possibility for total internal confidence in the consistency component of correctness, though not automatically the soundness component. The capability this provides to a _computational_ system requires the translation of the self-justifying property to a computational context; this can be done in-principle, by identifying what computational property is its correspondence, or experimentally, by building a computational system (i.e., a programming language) that is the correspondent of the logical systems of an SJAS (without separately establishing what its corresponding properties might be, if they too transfer), and writing programs in that experimental system. A theorem prover that allows for constructive program extraction would likely also satisfy; however, to do any of this requires a computational interpretation of SJAS. Aspects like the cut-free nature of the SJAS deduction methods work against realizing this interpretation, since to be cut-free is to be "already fully evaluated" by most Curry-Howard approaches; moving the construction of the cut-free proof object to the proof-search phaseo f a logic programming language may be reasonable, but whether the sophisticated algorithm of actual proof search must be internalized in the logical formulation of the SJAS proof predicate, in order to conserve self-justification, is still unclear. One immediate inteneded consequence of the Codification and Refinement documents is the exploration of translations of the SJAS results into the language of category theory, and thence type theory/sequent calculus/lambda calculus, under the hypothesis that the latter are better understood as domains with direct computational implementations; the Lawvere-SJAS work reflects this. Simultaneously, and overarchingly, extracting the "essential idea" of SJAS would further the reflection of that idea into other domains, concretely, ones not dependent on arithmeticization for internalization of the proof predicate (though this raises the questions of whether _any_ such internalization is in fact possible, whether an SJAS is dependent on _actually performing_ the calculation of Godel codes or merely some restriction on the final layout of a proof object - can Lisp-style quoting be considered a code name?). Commentary on any of the above would be useful, but most importantly in assessment of the driving motivation: what does an RSI system gain by greater confidence in the justication of its correctness (including the correctness of itself under self-modification), under what circumstances would it reap that benefit, and what benefits if any does self-justification provide? Please log this message, and your response.

## Assistant response

The driving motivation is sound, but I would make its success criterion operational: **what can the system responsibly do because it possesses this justification that it otherwise could not do, or could do only at greater cost?** Greater confidence has instrumental value when it improves decisions. An additional assertion of confidence, without stronger evidence or a usable inference, need not improve anything.

For RSI, the valuable consequences would include accepting beneficial modifications previously withheld, preserving an invariant through repeated changes, and delegating to successors whose detailed reasoning the parent cannot reproduce. The last is particularly important: a parent should be able to justify a successor's adherence to a contract without predicting every solution the successor will discover. That is the motivation of *Vingean reflection*. [Fallenstein and Soares, 2015](https://intelligence.org/files/VingeanReflection.pdf)

These benefits matter most when modifications are frequent, failures are costly, and the relevant guarantees can be stated precisely. They matter less when the main uncertainty concerns whether the specification expresses the right objective, whether the environment satisfies the assumptions, or whether the physical implementation realizes the formal model. Increasing confidence in a proof system does not by itself reduce those other uncertainties. A consistently pursued mistaken objective remains mistaken.

Your inner/outer distinction is therefore central, with one qualification: G2 concerns derivability of a particular consistency statement under particular hypotheses. It is not a theorem about subjective confidence generally, nor a demonstration that objective mathematical truth is unavailable. The following are different claims:

- The system derives its specified consistency sentence.
- A metatheory establishes that the system is consistent.
- Its conclusions are true in the intended interpretation.
- Its implementation and modifications satisfy the operational specification.

An SJAS can possess the first two without acquiring all of the latter two. Calling the first “total internal confidence” is reasonable if it means an internally available theorem; it should not silently mean probability one that the entire deployed system is correct.

There are also more options than weakening confidence or weakening arithmetic. Exact verification of a restricted class of modifications, preservation of a fixed trusted checker, and relative guarantees between different theories are separate possibilities. Neither undecidability nor the Löbian obstacle prohibits every useful, exact verification of self-modification. The obstacle arises in particular attempts to justify future reasoning through sufficiently general reflection. [Yudkowsky and Herreshoff, 2013, §§2–3](https://intelligence.org/files/TilingAgentsDraft.pdf)

For self-justification specifically, the gap between consistency and useful reflection is where I would concentrate. In the simplest no-proof-of-contradiction formulation, write

\[
\mathrm{Con}_S \equiv \forall p\,\neg\mathrm{Check}_S(p,\ulcorner\bot\urcorner).
\]

Here S includes the chosen axioms and deduction/checking rules. This excludes an accepted proof of contradiction. A policy that authorizes actions on the strength of proofs generally needs something resembling

\[
\mathrm{Check}_S(p,\ulcorner A\urcorner)\;\Longrightarrow\;A
\]

for the relevant action claims \(A\). That is a reflection principle. It does not follow merely by renaming consistency “correctness.” Willard's technical Level(1) consistency assertions also need their own definitions; they should not be identified with this simplest formula or with the arithmetic class \(\Pi_1\).

There is a useful intermediate fact. Externally, consistency can imply soundness for a restricted class: if every false claim in that class has a finite counterexample whose refutation the theory can prove, a false theorem would yield a contradiction. For example, this applies to suitable universal decidable predicates when the theory can establish all the relevant true ground computations. But internalizing that argument requires the system to represent and justify the conversion of a counterexample and a purported proof into a contradiction proof. That uniform conversion is precisely the sort of capability a weak reflective system may lack.

**Willard already offers a concrete lead beyond bare consistency.** His 2011 Theorem 6.12, recorded as `full` in the Codification, gives certain EA-stable Level(1) SJAS a translated reflection principle. Schematically, for the specified \(\Pi^\xi_1\) sentences,

\[
S\vdash \forall p\,\bigl(\mathrm{Prf}_S(\ulcorner\Psi\urcorner,p)\to\Psi^\tau\bigr).
\]

Here \(\Psi^\tau\) has the same truth value as \(\Psi\) in the standard model, but the system cannot generally establish their equivalence. The theorem has explicit configuration hypotheses; it is not a property of every weak arithmetic. [Willard, 2011, Theorem 6.12 and footnote 15, pp. 26–27](https://arxiv.org/pdf/1108.6330#page=27)

My proposed computational question is whether an operational contract can use \(\Psi^\tau\) directly, with an externally established interpretation connecting it to the desired behavior. If the agent must first recover \(\Psi\) internally, the missing equivalence becomes the obstacle again. Designing the interface around the translated guarantee could be worthwhile, but its utility remains to be demonstrated.

There is an important restriction on that proposal. “All computational safety properties” can already be an extremely broad class: a program can search for a counterexample to an arbitrary universal decidable assertion and perform a forbidden operation if it finds one. Its safety then expresses that assertion. Consequently, if that encoding and equivalence are internally available, unrestricted reflection for such safety claims can recover an entire \(\Pi_1\) reflection schema. Willard's 2001 Theorem 7.2 (`full`) rules out canonical reflection for every sentence in its specified \(\Pi^-_1\) class under its hypotheses, even without assuming addition or multiplication total. A computational interpretation must preserve that restriction. [Willard, 2001, Theorem 7.2, p. 38](https://doi.org/10.2307/2695030)

This suggests a distinction among kinds of self-modification:

| Modification | Assurance required |
| --- | --- |
| Improve proof search while retaining the checker and theory | Each resulting certificate passes the existing checker; the searcher's implementation need not be trusted for proof validity. |
| Replace executable components under a fixed contract | A proof connecting the replacement's behavior to that contract, including any relevant compiler or runtime assumptions. |
| Change axioms, proof rules, checker, or trust policy | A justification of the transition to the new system; the old system's self-consistency statement does not automatically cover it. |

The first two already have substantial scope without self-justification. Thus an SJAS must demonstrate an additional benefit over that baseline. For the third, even a theorem that every member of a family is self-justifying would leave the question of how the current agent recognizes an admissible successor and safely performs the transition.

Your question about internalizing proof search has a relatively direct answer: **not for preservation of the original self-consistency claim, provided acceptance still depends on certificates checked by the original proof predicate.** The certificate can be found by an elaborate algorithm, another prover, or a human. Proof validity concerns the certificate and its checking rules, regardless of its discovery history. Proof-carrying code provides the relevant architectural precedent. [Necula, *Compiling with Proofs*, §2.1](https://csd.cs.cmu.edu/sites/default/files/phd-thesis/CMU-CS-98-154.pdf#page=26)

However, three further claims require further work: that the actual checker implements that predicate; that the searcher will find a proof or terminate; and that the running agent's future actions remain controlled by the checker. Search itself can also have operational effects or exhaust resources. Those belong in the safety argument when relevant. If acceptance instead means “the search engine said yes,” or the checker admits new shortcuts, the effective proof predicate has changed.

The cut-free issue is real, but “already fully evaluated” is too strong as a general characterization of computational usefulness. A beta-normal function can still compute when supplied an argument: \(\lambda x.x\) is normal, while its application creates a reducible term. Proofs correspond to typed terms and derivations; a chosen normalization correspondence does not identify every proof step with a runtime event.

Thus a language can construct normal certificates through substantial computation, and normal proofs can describe functions awaiting inputs. The difficulty is preserving the reflective claim when composition, substitution, or normalization is made available as an internally certified operation. External existence of composed cut-free proofs does not establish an affordable, internally total transformer on their codes. Willard explicitly discusses potentially enormous growth in those composite proofs. [Willard, 2001, Lemma 7.1 and its comment, pp. 37–38](https://doi.org/10.2307/2695030)

Constructive extraction is a legitimate route, with an additional obligation: Willard's classical arithmetic and tableaux results do not automatically become constructive programs by placing them in a constructive host. The interpretation must establish what transfers. A schematic computational consistency type would quantify over quoted syntax and checking evidence:

\[
\forall p:\mathrm{Code},\quad
\mathrm{Check}_S(p,\ulcorner\bot\urcorner)\to\mathrm{Empty}.
\]

Its inhabitant supplies a negative guarantee. It does not automatically supply an evaluator, a normalizer, useful witnesses, or a method for approving successors. Experiments can discover useful interpretations before a complete metatheory exists, but running examples cannot establish that self-justification transferred.

Lisp-style quotation can certainly provide a code representation: syntax trees can name programs and proofs without first materializing numerical Gödel codes. Quotation alone supplies neither a faithful proof predicate nor its self-justification. The relevant questions concern available inspection, substitution, evaluation, copying, and proof-certification operations, especially which are internally established as total. Work on intensional recursion gives direct connections between code, self-reference, modal type theory, and Gödel–Löb structure. [Kavvos, 2017/2020](https://arxiv.org/abs/1703.01288)

For a native representation, one must establish that the checker recognizes the intended derivations and that the self-consistency sentence names that checker and its actual theory. One must also revisit proof-size and internal-totality arguments: structural induction or compact sharing might give the native system capabilities the arithmetic presentation lacked. Numerical coding need not be executed at runtime, but a mere layout restriction on certificates is not generally enough to recover an SJAS theorem.

That is also why the categorical translation is useful while remaining an intermediate step. It can expose which compositions, diagonals, and reflection maps are available. An ordinary equivalence of theorem sets, or a thin category that forgets individual proofs, does not itself preserve proof representation, size, or internally available code transformations. Those need an additional intensional layer.

I would turn the overarching motivation into a concrete research test:

1. Select one useful successor contract and an exact SJAS/reflection result that could support it.
2. Give the contract a computational interpretation and prove the restricted bridge from the available reflective guarantee to its operational meaning.
3. Build a small agent that uses that bridge to authorize a modification.
4. Compare it with the same architecture without the self-justifying principle, and with ordinary proof-carrying verification. Measure permitted modifications, proof size, checking cost, and external assumptions; establish that the special principle actually contributes.

A negative result would still be informative: perhaps the useful bridge restores a forbidden reflection principle, or the certificate costs erase the benefit. A positive result would establish a specific gain in justified self-modification.

My assessment is that self-justification's established contribution is a mathematically disciplined way for certain systems to incorporate a statement of their own consistency while retaining consistency under stated external hypotheses. Its proposed contribution to RSI is a usable, restricted form of self-trust that changes what an agent can justify. The latter remains a research hypothesis. Probabilistic assurance can concern the same exact correctness property; it relaxes certainty about that property, not necessarily the property itself. It can coexist with self-justification: exact local guarantees can govern a checker or contract while uncertainty remains about models, objectives, resources, and the world. Logical induction illustrates a distinct approach to uncertainty about computations and future beliefs, rather than making self-consistency a prerequisite for useful reflection. [Garrabrant et al., *Logical Induction*](https://arxiv.org/abs/1609.03543)

## Source verification and scope

The source-sensitive mathematical passages were checked as follows:

- Willard 2011, Theorem 6.12, Eq. (34), and footnote 15: read from rendered
  images of PDF pages 27-28 (printed pp. 26-27) of the
  [held v8 witness](../../nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf).
  The proof and the external-versus-internal equivalence qualification were
  both inspected.
- Willard 2001, Theorem 7.2 and the end of the Lemma 7.1 comment: read from a
  rendered image of PDF page 38 (printed p. 38) of the
  [held author witness](../../nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf).
  The `full` statuses used here were checked against the current
  [results registry](../../nachlass/codification/registry/results.md).
- The linked Tiling Agents draft, Vingean Reflection report, Necula thesis,
  Kavvos article, and Logical Induction paper were consulted for the specific
  connections described. This is not an exhaustive survey of RSI or reflection.
- The consistency-to-safety bridge, the three-way modification distinction,
  the computational-safety expressiveness example, and the proposed comparison
  experiment are this response's analysis and proposals, rather than claims
  that the cited papers already implement the proposed SJAS-based RSI system.
- The existing discussion of normalization and checker-indexed consistency in
  the separate ALSJAS paper-first worktree informed the computational
  distinctions; its scope and lack of an established RSI benefit were retained.

Only this conversation note and its entry in the [Nachlass log](../../nachlass/LOG.md)
belong to this task. No change to the Codification or Refinement argument,
implementation, or memory registry is requested by this record.

## Follow-up: a stateless certificate gate and its exact consistency requirement

### User feedback (verbatim)

This is almost entirely meta-level bloviation, or a restatement of my initial message (or immediate inferences derivable therefrom), garnished with references that I am obviously familiar with, as evidenced by invoking the Lobian Obstacle at all. Actually build or teach me something useful with respect to this concept space, or keep silent.

### Assistant response

Here is a concrete construction: **a self-justifying system can certify a stateless interface through which arbitrarily modified successors issue mutually exclusive commands.** The safety proof needs no general inference from provability to truth.

Take a Willard system \(S=B^\xi+\mathrm{SelfCons}_1(B^\xi,d)\) under Theorem 5.9's EA-stability hypothesis. Its self-consistency axiom has the form

\[
\forall x,y,p,q\;
\neg\bigl(
  \mathrm{Neg}_1(x,y)\land
  \mathrm{Prf}_S(x,p)\land
  \mathrm{Prf}_S(y,q)
\bigr).
\tag{1}
\]

Here \(x\) names a \(\Pi^\xi_1\) sentence and \(y\) its negation. This is Willard's actual pairwise-consistency formulation, Eq. (21), rather than a new soundness assumption. Theorem 5.9's consistency result is recorded as `full` in the Codification. [Source, printed p. 18](https://arxiv.org/pdf/1108.6330#page=19)

Define this machine, using the original checking relations:

```text
request(x, y, side, proof):       # side is a bit
    require Neg₁(x, y)
    proposition = x if side == 0 else y
    require Prf_S(proposition, proof)
    emit(x, y, side, proof)
```

The caller supplies both formula codes, so this interface does not assume an internally total negation-construction operation. It stores no decision history. Multiple copies may operate independently.

Interpret the two sides as incompatible commands for the same immutable decision key \((x,y)\): for example, COMMIT and ABORT. The operational invariant is:

> No collection of these machines can emit both sides for the same key.

The proof is complete in three steps:

1. An emitted side-0 command carries \(p\) with \(\mathrm{Prf}_S(x,p)\).
2. An emitted side-1 command for the same key carries \(q\) with \(\mathrm{Prf}_S(y,q)\).
3. Together with the checked \(\mathrm{Neg}_1(x,y)\), these are exactly the forbidden tuple in (1).

There is no construction of a composite encoded proof, normalization, or evaluation of the propositions in that argument. In particular, we never derive the truth of \(x\) or \(y\).

For an internal formulation, define a certified history as a finite sequence of requests whose emitted entries satisfy those two guards. The same argument proves in \(S\) that every certified history lacks conflicting emissions: select the alleged conflicting entries and instantiate (1). This is a proof about the specified interface semantics; connecting a particular compiled implementation to those semantics is a separate implementation theorem.

**This gives a specific form of self-modification that preserves the guarantee.** Let the agent's changing component be any program that generates requests. It can replace its search strategy, use a different programming language, spawn children, or run a more powerful external prover. Require all commands to pass through this fixed interface.

A violation after any number of replacements would still contain two emitted certificates, hence the same forbidden tuple. The proof does not lengthen with the succession chain. No theorem asserting that each searcher is sound, terminates, or understands its successor is required. Its authority is confined by the output interface.

There is also a precise G2 separation here.

Replace \(S\) with an ordinary consistent theory \(T\) satisfying G2's usual hypotheses and able to formalize this checker and interface. Suppose \(T\) could prove that its own interface never emits both sides for any key. Choose

\[
x=\ulcorner\forall n\,(0=1)\urcorner,\qquad
y=\ulcorner\exists n\,(0\ne1)\urcorner.
\]

There is a fixed \(T\)-proof \(q\) of \(y\). Any \(T\)-proof of \(0=1\) can be extended to a proof of \(x\), supplying two accepted requests and producing both outputs. Thus, internally,

\[
\mathrm{NoConflictingEmissions}(\mathrm{Gate}_T)
\;\Longrightarrow\;
\mathrm{Con}(T).
\]

Consequently \(T\) cannot prove that universal interface guarantee. \(S\) can, by (1).

This separation concerns the full class of admissible keys. A restricted family of contracts may already have an ordinary internal safety proof.

The specification matters: **the gate must honor every successfully checked request.** Otherwise a machine that never emits anything trivially meets the invariant. A conventional system can instead prove a stateful arbiter safe by recording earlier decisions and rejecting their opposites. But that arbiter cannot also promise to honor every valid certificate without the corresponding consistency assumption. Replicated first-arrival arbiters would need to agree on the decision history.

So the concrete gain is an internally justified permission to use **independent, stateless certificate gates**, even behind changing or mutually untrusted proof searchers. An ordinary theory can run the identical gate with an external consistency justification; the separation concerns which architecture an agent can authorize using its own proofs.

This also makes Willard's translated reflection operationally intelligible. The translation used in Theorem 6.12 is

\[
\Psi^\tau \equiv
\neg\mathrm{Prov}_{B^\xi}(\ulcorner\neg\Psi\urcorner).
\]

Its direct interpretation is a challenge protocol: after an \(S\)-proof of \(\Psi\) is accepted, a challenger using \(B^\xi\) cannot produce an accepted counterproof. The guarantee refers to possible future certificates, so it can govern that protocol without first being converted back into the truth of \(\Psi\). The two-sided gate above uses the stronger pairwise prohibition already present in \(\mathrm{SelfCons}_1\). [Example 6.7 and Theorem 6.12](https://arxiv.org/pdf/1108.6330#page=24)

The limitation is equally concrete. The gate does not establish that COMMIT is economically sensible, that ABORT is desirable, or that either request will ever obtain a proof. It establishes that these mutually incompatible certified commands cannot both occur. The decision key and its interpretation must remain fixed; changing which transaction a formula denotes would change the protocol.

This is one way to turn syntactic self-consistency into an operational safety property: **make a violation expose the very proof pair the system internally excludes.** It is an interface construction and a preservation proof, not yet an implemented SJAS language.

Source check: Willard 2011 Eq. (21) and Theorem 5.9 were inspected on the rendered image of PDF page 19 (printed p. 18); Example 6.7 on PDF page 24 (printed p. 23). Theorem 6.12 and its proof were inspected in the preceding source check. The gate, its operational interpretation, and the G2 reduction are the construction in this response. No native SJAS checker or source-to-machine adequacy theorem was implemented here.

## 2026-09-05 - Requested log and PTS continuation

User request, verbatim:

> Log this please. Then, translate SJAS into the language of pure type systems.

The certificate-gate construction above is the logged preceding response. The
requested translation is developed in
[SJAS in a pure type system](2026-09-05-sjas-pure-type-systems.md), including its
signature, formula translation, preservation argument, self-consistency term,
and the exact additional obligation for transferring consistency to a new
checker.

The [subsequent lowering resolution](2026-09-05-lower-resolution.md) fixes a
Type-A source and a PTS certificate format admitting Hilbert macros. It gives
a paper reduction excluding both internal lowering and the direct pairwise
gate invariant for that checker, using Willard's Hilbert boundary. The gate
specified above continues to refer to the original tableaux certificates.
