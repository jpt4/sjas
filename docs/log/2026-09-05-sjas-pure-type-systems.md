# SJAS in a pure type system: a conservative negative translation

Date: 2026-09-05 (America/Indiana/Indianapolis).

[CLI-readable ASCII edition of the response](2026-09-05-sjas-pure-type-systems-cli.txt).

## Request and scope

User request, verbatim:

> Log this please. Then, translate SJAS into the language of pure type systems.

The preceding [certificate-gate construction](2026-09-04-self-justification-and-rsi-motivation.md#follow-up-a-stateless-certificate-gate-and-its-exact-consistency-requirement) is logged, including the user's criticism that prompted it. This note supplies the requested translation.

The result is a **conservative presentation of a specified Willard theory in ordinary \(\lambda P\), with an axiom signature**. It gives the PTS specification, formula translation, preservation argument, self-consistency type, and explicit lambda terms. It is not a claim that the whole PTS type checker now verifies its own consistency. Section 8 identifies the additional internal theorem that would transfer the guarantee to a different checker.

This is a paper construction, not a mechanized development or implementation ADR. It does not modify the concurrent Codification/Refinement review or the separate ALSJAS designs.

## 1. Fix the source, including its proof predicate

Use the same source as the gate:

\[
S=B^\xi+\{H\},\qquad H=\mathrm{SelfCons}_1(B^\xi,d),
\]

with an EA-stable generic configuration \(\xi\), as in Willard 2011, Theorem 5.9. The language, numerical representations, deduction apparatus \(d\), and proof predicate are fixed parts of this choice.

The displayed self-consistency statement is

\[
H\equiv
\forall x,y,p,q\;
\neg\bigl(
 \mathrm{Neg}_1(x,y)\land
 \mathrm{Prf}_S(x,p)\land
 \mathrm{Prf}_S(y,q)
\bigr).
\tag{1}
\]

The apparently circular subscript is an abbreviation, not a recursive definition. Appendix A makes it finite. Put

\[
\begin{aligned}
\Gamma_1(g)&\equiv
 \forall x,y,p,q\;
 \neg\bigl(
 \mathrm{Neg}_1(x,y)\land
 \mathrm{SubstPrf}^{d}_{B^\xi}(g,x,p)\land
 \mathrm{SubstPrf}^{d}_{B^\xi}(g,y,q)
 \bigr),\\
n&=\ulcorner\Gamma_1(g)\urcorner,\\
H&=\Gamma_1(\bar n).
\end{aligned}
\tag{2}
\]

Thus \(\mathrm{Prf}_S(x,p)\) in (1) abbreviates
\(\mathrm{SubstPrf}^{d}_{B^\xi}(\bar n,x,p)\). In particular, \(n\) names the open template \(\Gamma_1(g)\), not \(H\) itself. The latter has its own substituted sentence code.

We translate the actual arithmetic formulas, including this fixed numeral. We do not replace the proof predicate by an unconstrained predicate with a suggestive name.

Source: [Willard 2011, Eq. (21), Theorem 5.9, and Appendix A, Eqs. (35)-(37)](https://arxiv.org/pdf/1108.6330).

## 2. The PTS

Take

\[
\mathcal S=\{*,\square\},\qquad
\mathcal A=\{(*,\square)\},\qquad
\mathcal R=\{(*,*,*),(*,\square,\square)\}.
\tag{3}
\]

These are the rules of \(\lambda P\). Terms have the ordinary PTS grammar

\[
M ::= x\mid *\mid\square\mid MN
      \mid\lambda x:A.M\mid\Pi x:A.B.
\]

Conversion is beta conversion. In particular,

\[
\frac{\Delta\vdash A:s_1\qquad
      \Delta,x:A\vdash B:s_2\qquad
      (s_1,s_2,s_3)\in\mathcal R}
     {\Delta\vdash\Pi x:A.B:s_3}
\]

and

\[
\frac{\Delta\vdash f:\Pi x:A.B\qquad\Delta\vdash a:A}
     {\Delta\vdash f\,a:B[a/x]}.
\]

Use the usual variable, weakening, abstraction, and conversion rules. There is ordinary contraction; this is not an affine translation.

A signature below is a fixed typing context. Its declarations introduce no new term constructor or reduction rule. An infinite source axiom set means a recursively presented family of declarations; each derivation uses a finite subcontext. "Closed" below means closed relative to that fixed signature.

The PTS specification and the relevant first-order fragment are standard; see [Sørensen and Urzyczyn, §§10.6 and 13.2](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf). Our application to the selected SJAS, and the checker-transfer argument below, are the construction here.

## 3. Translate arithmetic without strengthening it

Start the signature with

\[
\iota:*,\qquad \mathbf 0:*.
\]

Here \(\iota\) is the source arithmetic domain; \(\mathbf 0\) is the type used for logical falsity. Neither is the PTS sort \(*\). An arithmetic zero numeral is a term of \(\iota\), not \(\mathbf 0\).

For each source symbol, declare precisely its corresponding arity:

| Source symbol | PTS declaration |
| --- | --- |
| Individual constant \(c\) | \(c:\iota\) |
| \(k\)-ary function \(f\) | \(f:\iota\to\cdots\to\iota\) |
| \(k\)-ary relation \(R\) | \(R^\circ:\Pi x_1:\iota\cdots\Pi x_k:\iota.*\) |

Keep function symbols distinct from relational graphs. If multiplication is only a relation in the source, introduce

\[
\mathrm{Mult}^\circ:\iota\to\iota\to\iota\to *,
\]

not a multiplication function \(\iota\to\iota\to\iota\).

Do not add a natural-number eliminator, induction, a recursor, or even a successor function absent from the chosen source signature. Represent each source numeral by its original closed term. Translate source equality axioms/rules as first-order equality axioms; do not replace them by equality with unrestricted elimination into arbitrary host types.

This preserves a real distinction. The PTS admits

\[
\begin{aligned}
\mathrm{Ch}_\iota
  &:= (\iota\to\iota)\to\iota\to\iota,\\
\mathrm{mulCh}
  &:=\lambda m:\mathrm{Ch}_\iota.\lambda n:\mathrm{Ch}_\iota.
      \lambda f:\iota\to\iota.\lambda z:\iota.\;m\,(n\,f)\,z,\\
\mathrm{mulCh}&:\mathrm{Ch}_\iota\to
                 \mathrm{Ch}_\iota\to\mathrm{Ch}_\iota.
\end{aligned}
\tag{4}
\]

But \(\mathrm{Ch}_\iota\) is not \(\iota\). There is no supplied conversion from an arbitrary source number to its Church iterator. Equation (4) therefore does not establish multiplication totality on the source domain. Higher-order beta computation and internally available arithmetic on proof codes remain different resources.

## 4. Translate classical formulas into pure lambda types

A literal identification of every classical proposition with an intuitionistic type would be wrong. Use a negative translation. Write \(\neg_*X:=X\to\mathbf 0\); this is metanotation at each type, not a polymorphic constant.

Translate source terms homomorphically and formulas as follows:

\[
\begin{aligned}
\llbracket\bot\rrbracket
  &:=\mathbf 0,\\
\llbracket R(\vec t)\rrbracket
  &:=(R^\circ(\vec t)\to\mathbf 0)\to\mathbf 0,\\
\llbracket A\to B\rrbracket
  &:=\llbracket A\rrbracket\to\llbracket B\rrbracket,\\
\llbracket\forall x\,A\rrbracket
  &:=\Pi x:\iota.\llbracket A\rrbracket,\\
\llbracket A\land B\rrbracket
  &:=(\llbracket A\rrbracket\to
      \llbracket B\rrbracket\to\mathbf 0)\to\mathbf 0,\\
\llbracket A\lor B\rrbracket
  &:=(\llbracket A\rrbracket\to\mathbf 0)\to
     (\llbracket B\rrbracket\to\mathbf 0)\to\mathbf 0,\\
\llbracket\exists x\,A\rrbracket
  &:=(\Pi x:\iota.\llbracket A\rrbracket\to\mathbf 0)\to\mathbf 0.
\end{aligned}
\tag{5}
\]

Negation is implication to falsity. Bounded quantifiers expand into their source definitions before applying (5).

For a displayed n-ary conjunction, use
\((\llbracket A_1\rrbracket\to\cdots\to\llbracket A_k\rrbracket\to\mathbf 0)\to\mathbf 0\).
This is interderivable with iterating the binary clause, since all translated formulas are stable. This harmless associativity convention affects the translated type, never the original quoted sentence code.

Every translated formula has a definable stability term

\[
\mathrm{stable}_A:
 ((\llbracket A\rrbracket\to\mathbf 0)\to\mathbf 0)
 \to\llbracket A\rrbracket.
\tag{6}
\]

For example, for an atomic source formula, put \(X=R^\circ(\vec t)\). Then

\[
\mathrm{stable}_{R(\vec t)}
 =
 \lambda u:((((X\to\mathbf 0)\to\mathbf 0)\to\mathbf 0)\to\mathbf 0).
 \lambda k:X\to\mathbf 0.
 u\,(\lambda a:(X\to\mathbf 0)\to\mathbf 0.\;a\,k).
\tag{7}
\]

The other cases follow by structural induction. For implication, with \(U=\llbracket A\rrbracket\), \(V=\llbracket B\rrbracket\),

\[
\lambda u:(((U\to V)\to\mathbf 0)\to\mathbf 0).
\lambda a:U.
\mathrm{stable}_B
  (\lambda k:V\to\mathbf 0.
    u\,(\lambda f:U\to V.\;k\,(f\,a))).
\]

For a universal formula, the same construction is pointwise in \(x:\iota\). Conjunction, disjunction, and existential translations are negations, hence stable. Falsity has stability term
\(\lambda u:(\mathbf 0\to\mathbf 0)\to\mathbf 0.\;u\,(\lambda z:\mathbf 0.z)\).

Consequently classical double-negation elimination translates to an ordinary lambda term at each translated formula. No control primitive or global classical axiom is added. Ex falso into any translated formula follows from (6).

For each nonlogical source axiom \(A\), add a declaration

\[
a_A:\llbracket A\rrbracket.
\]

Include the translations of the first-order equality axiom schemata when equality is part of the source logic. Call the resulting signature \(\Sigma_S\).

There is a computational cost to this choice: \(\llbracket\exists x\,A(x)\rrbracket\) is a continuation type, not a dependent pair exposing a witness. This is not a claim of general constructive witness extraction from classical SJAS proofs. Negative guarantees such as the gate invariant need no such extraction.

## 5. Preservation, and exactly what is preserved

For each source sentence \(A\), with exactly the preceding signature,

\[
S\vdash_d A
\quad\Longleftrightarrow\quad
\text{there exists }t\text{ with }
\Sigma_S\vdash_{\lambda P}t:\llbracket A\rrbracket.
\tag{8}
\]

The assumptions on \(d\) here are ordinary soundness and completeness as a deductive apparatus for the source classical first-order consequence relation. This is an external theorem about inhabitation, not an internally certified proof-code compiler.

Here is the paper proof.

1. **Forward direction.** Convert a finite \(d\)-proof to a classical natural-deduction proof. Apply (5). Implication introduction/elimination become lambda abstraction/application, and universal introduction/elimination become dependent abstraction/application. The negative encodings implement the other connectives. Formula-specific stability (6) implements classical reasoning. Each arithmetic or equality axiom becomes its signature declaration. Induction on the resulting derivation gives the required PTS term.

2. **Reverse direction.** Normalize a putative PTS inhabitant, including its annotations, and put its proof spines into long form. The context and goal contain only the displayed first-order formula types. In such a normal derivation, an individual term of type \(\iota\) is built from source individual variables and source function symbols: there is no recursor, proof eliminator returning \(\iota\), or other constant that could create an exotic individual. A proof spine has a source-axiom/assumption head; its arguments are individual instantiations or proofs of first-order premises. Thus the normal derivation reads back as a minimal first-order derivation of \(\llbracket A\rrbracket\) from the translated source axioms.

3. Read the result classically. Each translated formula is classically equivalent to its original formula. Hence the original source axioms entail \(A\); completeness of \(d\) supplies a \(d\)-proof.

The normal-form argument is the usual conservativity argument for the first-order fragment of \(\lambda P\), specialized to this negative signature. Higher-order intermediate terms do not invalidate it: normalization removes detours, and no signature constant has a higher-order individual input or an elimination rule outside the source fragment.

This establishes theorem-set conservativity, **not** a bijection of raw proofs or a bound on either translation's size. It also establishes

\[
\mathrm{Con}(S)\Longrightarrow
\neg\exists t\;(\Sigma_S\vdash t:\mathbf 0).
\tag{9}
\]

For the selected \(S\), Willard's theorem supplies the left side externally. We are not inferring (9) from strong normalization alone: an arbitrary extra declaration \(b:\mathbf 0\) would preserve beta normalization and destroy consistency.

Equation (8) and its proof are supplied here at paper level; no assistant kernel has mechanically checked them.

## 6. The actual self-consistency term

Define type-family abbreviations by expanding the original formulas and applying (5):

\[
\begin{aligned}
N(x,y)&:=\llbracket\mathrm{Neg}_1(x,y)\rrbracket,\\
C(x,p)&:=\llbracket
 \mathrm{SubstPrf}^{d}_{B^\xi}(\bar n,x,p)
 \rrbracket.
\end{aligned}
\tag{10}
\]

These are well-kinded families \(\iota\to\iota\to *\). Their arithmetic definitions, and the numeral \(\bar n\), are fixed before the self-consistency declaration is made.

Let

\[
F_{xypq}:=N(x,y)\to C(x,p)\to C(y,q)\to\mathbf 0.
\]

The direct translation of the original axiom is

\[
h:\Pi x,y,p,q:\iota.\;
       (F_{xypq}\to\mathbf 0)\to\mathbf 0.
\tag{11}
\]

It is the translated \(H\) already present in \(\Sigma_S\), not an additional reflection axiom.

Now define

\[
\begin{aligned}
\mathrm{self}:={}&
 \lambda x,y,p,q:\iota.
 \lambda n_{xy}:N(x,y).
 \lambda c_p:C(x,p).
 \lambda c_q:C(y,q).\\
 &\qquad h\,x\,y\,p\,q\,
    (\lambda k:F_{xypq}.\;k\,n_{xy}\,c_p\,c_q).
\end{aligned}
\tag{12}
\]

It has type

\[
\boxed{
\mathrm{self}:
\Pi x,y,p,q:\iota.\;
N(x,y)\to C(x,p)\to C(y,q)\to\mathbf 0.
}
\tag{13}
\]

The derivation is immediate but substantive: \(k\,n_{xy}\,c_p\,c_q:\mathbf 0\); abstracting \(k\) gives \(F_{xypq}\to\mathbf 0\); applying (11) gives \(\mathbf 0\); abstract the remaining arguments. All products in (11)-(13) have sort \(*\) by \((*,*,*)\). The kind of each family in (10) uses \((*,\square,\square)\).

This is a dependent rejection function for the exact forbidden proof tuple. It does not return the proposition named by \(x\). It does not inspect the truth of that proposition. It does not compose encoded proofs.

Nor is (11) a recursive type equation: its type does not contain the variable \(h\). Its self-reference is the fixed numerical data in (10). There is no \(*:*\), fixpoint combinator, or recursive-type rule.

As an axiom parameter, \(h\) has no computation equation. Applications of \(\mathrm{self}\) beta-reduce to an application of \(h\); they do not execute a proof checker. Under (9), no closed well-typed inputs realize the forbidden tuple. This is a logical exclusion of incompatible certificates, not a newly implemented run-time consistency test.

## 7. The certificate gate as a typed program

For fixed \(x,y,p\), a positive check produces a reusable veto on every negative check:

\[
\begin{aligned}
\mathrm{veto}:{}&
\Pi x,y,p:\iota.\;
N(x,y)\to C(x,p)\to
\Pi q:\iota.\;C(y,q)\to\mathbf 0,\\
\mathrm{veto}:={}&
\lambda x,y,p:\iota.
\lambda n_{xy}:N(x,y).
\lambda c_p:C(x,p).
\lambda q:\iota.
\lambda c_q:C(y,q).
\mathrm{self}\,x\,y\,p\,q\,n_{xy}\,c_p\,c_q.
\end{aligned}
\tag{14}
\]

This is the previous stateless gate's safety argument in the target calculus. It quantifies over arbitrary future candidate certificates \(q\), not over the internals of the programs that might produce them.

There are three different objects:

| Object | Meaning |
| --- | --- |
| \(p:\iota\) | A number that may encode an original \(d\)-proof |
| \(c_p:C(x,p)\) | PTS evidence for the translated statement that this number passes the original checker |
| \(t:\llbracket A\rrbracket\) | A PTS proof of the translation of a source proposition \(A\) |

None of these is definitionally identified with either of the others. The run-time checker supplies a fact about \(p\); a proof-producing implementation must also connect that check to evidence of type \(C(x,p)\). Equation (5) alone is not such an implementation.

We can also hide the proof number without extracting a witness. Define the negative existential

\[
\Box_S(x):=
(\Pi p:\iota.\;C(x,p)\to\mathbf 0)\to\mathbf 0.
\tag{15}
\]

This is exactly the translation of source provability \(\exists p\,\mathrm{Prf}_S(x,p)\). It is not a new primitive modality and is not a type of raw proof objects.

The full pairwise prohibition has the term

\[
\begin{aligned}
\mathrm{separate}:={}&
\lambda x,y:\iota.
\lambda n_{xy}:N(x,y).
\lambda u:\Box_S(x).
\lambda v:\Box_S(y).\\
&u\,(\lambda p:\iota.\lambda c_p:C(x,p).
       v\,(\lambda q:\iota.\lambda c_q:C(y,q).
          \mathrm{self}\,x\,y\,p\,q\,n_{xy}\,c_p\,c_q)),\\
\mathrm{separate}:{}&
\Pi x,y:\iota.\;N(x,y)\to
\Box_S(x)\to\Box_S(y)\to\mathbf 0.
\end{aligned}
\tag{16}
\]

Only continuations and the original consistency axiom are used.

## 8. What would transfer this to the PTS's own checker?

The source \(d\)-proofs and the target PTS typing derivations are different certificates. Although (8) gives external translations between their theorem sets, it does not supply an internal correctness/totality theorem for those translations.

Let \(K(x,r)\) be a specified, source-representable candidate checking relation. For a PTS kernel, it would say that \(r\) is an accepted typing derivation of the translation of the source sentence coded by \(x\). Defining that predicate faithfully is part of this additional choice; it is not hidden inside the letter \(K\).

Write \(K^-(x,r):=\llbracket K(x,r)\rrbracket\). An adequate internal bridge is

\[
\mathrm{lower}:
\Pi x,r:\iota.\;K^-(x,r)\to\Box_S(x).
\tag{17}
\]

The source statement is

\[
\forall x,r\;
\bigl(K(x,r)\to\exists p\,\mathrm{Prf}_S(x,p)\bigr).
\]

Crucially, (17) asks for internally justified existence, not a function extracting a proof code as data. A total certified compiler would be sufficient but can be stronger than what the consistency transfer needs.

If (17) is available in the same translated theory, consistency transfers by the explicit term

\[
\begin{aligned}
\mathrm{separate}_K:={}&
\lambda x,y,r,s:\iota.
\lambda n_{xy}:N(x,y).
\lambda a:K^-(x,r).
\lambda b:K^-(y,s).\\
&\mathrm{separate}\,x\,y\,n_{xy}\,
   (\mathrm{lower}\,x\,r\,a)\,
   (\mathrm{lower}\,y\,s\,b).
\end{aligned}
\tag{18}
\]

There is a straightforward certificate-carrying case: an accepted request carries an original \(p\) and evidence \(c_p:C(x,p)\), in addition to any PTS proof/typing certificate. Its lowering term is

\[
\lambda f:(\Pi q:\iota.\;C(x,q)\to\mathbf 0).\;f\,p\,c_p
   :\Box_S(x).
\tag{19}
\]

The extra PTS component can be forgotten. This preserves the gate guarantee without internalizing search or a proof-normalization algorithm. It describes a certificate-carrying interface, not an ordinary PTS application rule: automatic acceptance of a newly composed PTS term would still require an original certificate or the internal bridge (17).

For unrestricted PTS typing derivations, (17) has **not** been established here. Equiconsistency, external normalization, and external proof search do not establish it internally. Adding (17) as an unchecked axiom would change the theory whose consistency has been justified.

The subsequent [lowering resolution](2026-09-05-lower-resolution.md) specializes
the source to a concrete Type-A configuration and adds a Hilbert macro format
for PTS certificates. For that specified checker, even lowering at `0 = 1`
is unprovable by the cited Hilbert-incompleteness boundary. This is a result
about that certificate representation; it does not settle the separate
checker accepting only fully expanded PTS typing trees.

The guarantee in (18) also ranges over the translated source formulas and their source negation relation. Certifying all other host types, all admissible contexts, or arbitrary future axiom extensions would require correspondingly broader statements.

## 9. Two computational consequences of the translation

### Ordinary application is not reflected proof-code composition

The PTS has the usual term

\[
\lambda f:\llbracket A\rrbracket\to\llbracket B\rrbracket.
\lambda a:\llbracket A\rrbracket.\;f\,a.
\]

This does not automatically inhabit the checker-indexed type

\[
\Box_S(\ulcorner A\to B\urcorner)\to
\Box_S(\ulcorner A\urcorner)\to
\Box_S(\ulcorner B\urcorner).
\tag{20}
\]

The inputs of (20) are translated statements about encoded \(d\)-proofs, not an applicable function and its argument. Any missing source instance/schema remains missing in the corresponding translated first-order fragment by (8). The translation has not restored a failed uniform HBL resource merely by admitting ordinary function application.

A cut-free source proof can therefore be represented and manipulated in a lambda calculus with application. The correspondence is static; it need not preserve raw proof shape or individual reduction events. To turn a newly composed target proof back into an accepted source certificate may require external cut elimination or search. Equation (17) states the separate internal obligation.

### Raw quotation cannot just be an ordinary PTS function

Even at one fixed type \(A\), an ordinary function
\(\mathrm{quote}_A:A\to\iota\) must respect beta conversion:

\[
\mathrm{quote}_A((\lambda z:A.z)t)
 =_\beta \mathrm{quote}_A(t).
\]

But the raw syntax trees \((\lambda z:A.z)t\) and \(t\) have different codes. Hence such a function cannot simultaneously respect ordinary conversion and faithfully return distinct codes for arbitrary raw syntax.

Explicit syntax data, a staged quotation form, or quotation of chosen canonical forms changes the problem. Lisp-style quoting can provide names, but that observation alone provides neither a faithful internal checker nor theorem (17). The translation above needs only the fixed numeral already used in (2); it assumes no general quote function.

## 10. Result and verification record

The construction provides:

- ordinary \(\lambda P\), with no new reduction or structural rules;
- a negative translation of the selected classical SJAS signature;
- the paper-level theorem-set equivalence (8), hence relative uninhabitability of \(\mathbf 0\);
- an actual lambda term (12) for checker-indexed Level(1) self-consistency;
- typed veto and pair-separation terms (14) and (16);
- a precise internal checker-transfer obligation (17), its consequence (18), and the certificate-carrying special case (19).

It does not provide a native self-verifying PTS checker, arithmeticization-free internalization, general witness extraction, or a running language implementation.

Source-page verification used the PDF skill: Willard's Appendix A, printed p. 32/PDF page 33, was rendered and inspected for the distinction between the template code and substituted sentence, and the exact checker arguments. Eq. (21)/Theorem 5.9 were inspected on printed p. 18/PDF page 19 in the preceding gate work. The PTS rule table and the statement of first-order conservativity were checked on rendered pages of Sørensen and Urzyczyn, printed pp. 213 and 165 (PDF pages 225 and 177 in the linked draft). Formula translation, term sorting, and the displayed derivations were checked on paper, not by a PTS implementation.

The original corpus's source-proof status labels are not changed by this note. In particular, a source theorem recorded as full does not make this new translation mechanized.
