# A PTS presentation of the constructive SJAS

Date: 2026-09-26.

User question: "What would a Pure Type System based on this constructive SJAS look like?"

This note specifies a lambda-P logical-framework presentation of the
[constructive Hilbert SJAS I](2026-09-25-native-constructive-sjas.md).
It gives an explicit signature, example proof terms, and an external
proof-translation argument. It preserves the distinction between a proof
term, its extracted realizer, and an arithmetic code checked by I.

Status: paper specification. No kernel, elaborator, extractor, or mechanized
adequacy proof is supplied. The self-consistency constant below refers to
the existing Hilbert checker. A theorem about that checker is not silently
replaced by a theorem about a new PTS typing checker.

## 1. The pure kernel and the signature

Choose the PTS lambda-P:

```text
S = {Type, Kind}
A = {(Type, Kind)}
R = {(Type, Type, Type), (Type, Kind, Kind)}.
```

The axiom says `Type : Kind`. A triple `(s1,s2,s3)` licenses
`Pi x:A. B : s3` when `A : s1` and, under `x:A`, `B : s2`.
Use the usual variable, weakening, product formation, lambda abstraction,
application, and beta-conversion rules. There is no axiom `Type : Type`.
The kernel reduction is

```text
(lambda x:A. t) u  -->  t[u/x].
```

An arithmetic theory also needs a signature of typed constants. The PTS
triple alone specifies the function/type structure, not arithmetic axioms.
Use the following declarations; each proof uses only a finite part of the
possibly infinite arithmetic signature.

```text
Num   : Type
Fm    : Type
Proof : Fm -> Type

C_0, C_1, C_2, ... : Num
Pred              : Num -> Num
Sub, Div, Max     : Num -> Num -> Num
...the other selected non-growth function symbols...

Eq, Le            : Num -> Num -> Fm
Bot               : Fm
Imp, And, Or      : Fm -> Fm -> Fm
All, Ex           : (Num -> Fm) -> Fm.
```

`Fm` is the type of represented arithmetic formulas. `Proof A` is the
type of formal proofs of the represented formula A. `Fm` is not a universe
of arbitrary PTS types. In particular, the signature does not turn
quantification over represented formulas into an object-language logical
rule. `Num` represents the original arithmetic domain and its term syntax;
the kernel treats the declared arithmetic operations as opaque symbols.
Their intended numerical evaluation belongs to the external interpretation.

There is no new successor constructor, natural-number recursor, or map from
an index i to the constant C_i. Function types and reusable proof functions
are available, while native arithmetic retains the source theory's weakness.

`Proof` is a genuine dependent type family: the type of a proof depends on
the formula it proves. For example,

```text
Proof (Eq (Sub C_2 C_2) C_0) : Type.
```

The distinction from the earlier realizer is concrete: `C_0 : Num` does
not have this proof type merely because the equation is true externally.

## 2. Logical rules as typed constants

Here `Pi A,B:Fm` abbreviates two successive Pi binders. Arrows associate
to the right. The displayed constants are the entire logical rule
signature, including equality; in particular there is no classical
double-negation-elimination rule.

```text
impI : Pi A,B:Fm.
         (Proof A -> Proof B) -> Proof (Imp A B)
impE : Pi A,B:Fm.
         Proof (Imp A B) -> Proof A -> Proof B

andI : Pi A,B:Fm.
         Proof A -> Proof B -> Proof (And A B)
andL : Pi A,B:Fm.
         Proof (And A B) -> Proof A
andR : Pi A,B:Fm.
         Proof (And A B) -> Proof B

orL  : Pi A,B:Fm.
         Proof A -> Proof (Or A B)
orR  : Pi A,B:Fm.
         Proof B -> Proof (Or A B)
orE  : Pi A,B,C:Fm.
         Proof (Or A B) ->
         (Proof A -> Proof C) ->
         (Proof B -> Proof C) -> Proof C

botE : Pi A:Fm. Proof Bot -> Proof A

allI : Pi P:(Num -> Fm).
         (Pi x:Num. Proof (P x)) -> Proof (All P)
allE : Pi P:(Num -> Fm).
         Proof (All P) -> Pi x:Num. Proof (P x)

exI  : Pi P:(Num -> Fm). Pi x:Num.
         Proof (P x) -> Proof (Ex P)
exE  : Pi P:(Num -> Fm). Pi C:Fm.
         Proof (Ex P) ->
         (Pi x:Num. Proof (P x) -> Proof C) -> Proof C

refl : Pi x:Num. Proof (Eq x x)
eqE  : Pi P:(Num -> Fm). Pi x,y:Num.
         Proof (Eq x y) -> Proof (P x) -> Proof (P y).
```

All these declarations are well-sorted in the specified lambda-P. They
quantify over `Fm`, `Num`, and functions `Num -> Fm`, each of sort Type.
They do not require the absent product rule `(Kind,Type,Type)`.

The binder in `impI` represents discharge of an assumption. `allI` binds
a fresh individual variable; the dependent context discipline enforces
that an existing assumption cannot depend on that newly bound variable.
In `exE`, C is bound outside x's scope, which enforces the eigenvariable
restriction on the conclusion. Weakening and contraction remain available.

As a small explicit term, identity implication is

```text
idProof : Pi A:Fm. Proof (Imp A A)
idProof = lambda A:Fm.
            impI A A (lambda h:Proof A. h).
```

This is a definition by abbreviation; expanding definitions adds no
kernel reduction beyond beta reduction.

This is the judgements-as-types method of the Edinburgh Logical Framework.
The arithmetic formula is represented by an Fm term; the judgement that it
has a proof is represented by a Type. The source is already intuitionistic,
so this encoding needs no negative translation of disjunction or existence.

## 3. Arithmetic axioms have proof constants

For each permitted nonlogical source axiom alpha, declare a constant

```text
ax_alpha : Proof (encode(alpha)).
```

Here `encode` is a metalevel syntax translation, not an additional
function of the object theory. Membership in the source axiom family is
authenticated by the same finite evidence as in the source construction.
Definitions below are metalevel abbreviations, fully expanded in any
source proof certificate.

For instance:

```text
P_sub = lambda x:Num. Eq (Sub x x) C_0
P_div = lambda n:Num. Le (Div n C_2) n

ax_subSelf : Proof (All P_sub)
ax_divBound : Proof (All P_div)

subSelf : Pi x:Num. Proof (Eq (Sub x x) C_0)
subSelf = lambda x:Num. allE P_sub ax_subSelf x

divBound : Pi n:Num. Proof (Le (Div n C_2) n)
divBound = lambda n:Num. allE P_div ax_divBound n.
```

`subSelf` explicitly invokes the arithmetic axiom and universal elimination.
Its type is mechanically checkable relative to the signature. Its external
realizer can still be `lambda x. 0`, since extraction forgets these proof
steps. The kernel checks uses of the declared axioms; it does not establish
the truth of those axioms by type checking their declarations.

The naming axiom constant is

```text
Add x y z = And (Le x z) (Eq (Sub z x) y)

ax_name_i : Proof (Add C_i C_i C_(i+1)), for each i >= 2.
```

The constant-definition axioms and the remaining chosen F axioms are
handled in the same way. Arbitrary additional arithmetic constants or
proof constants would change this signature and its theorem set.

## 4. A complete proof with an extracted program

Use the abbreviations

```text
Q = lambda n:Num. lambda m:Num.
      And (Eq m (Div n C_2)) (Le m n)

Goal = All (lambda n:Num. Ex (Q n)).
```

The following proof term has type `Proof Goal`:

```text
halfProof =
  allI (lambda n:Num. Ex (Q n))
    (lambda n:Num.
      exI (Q n) (Div n C_2)
        (andI
          (Eq (Div n C_2) (Div n C_2))
          (Le (Div n C_2) n)
          (refl (Div n C_2))
          (divBound n))).
```

Its typing derivation, working from the inside outward, is:

1. `refl (Div n C_2)` proves the required equality.
2. `divBound n` proves the required inequality by the arithmetic axiom.
3. `andI` pairs those proofs to prove `Q n (Div n C_2)`.
4. `exI` supplies `Div n C_2` as the existential witness.
5. `allI` abstracts the arbitrary input n, obtaining `Proof Goal`.

Translate this proof into a source I-proof and apply the previous extraction
theorem, or interpret its rule constructors directly with the same realizer
clauses. Both provide a witness program computing `floor(n/2)`. A possible
complete realizer is

```text
lambda n. (floor(n/2), (0,0)).
```

The typing derivation retains mathematical justification even though its
extracted arithmetic evidence can be trivial.

## 5. What computes in the pure presentation

LF lambda application performs substitution and ordinary function
computation. The object-logic rule constructors above are opaque constants.
For example, the expression

```text
impE A B (impI A B f) a
```

does not beta-reduce to `f a` in this bare PTS. Such a rule would be an
additional object-proof normalization rule. Likewise, `Div C_4 C_2` is a
represented arithmetic term; the pure LF kernel has no built-in division
evaluator. The example's numerical execution occurs under the extraction
interpretation.

This explains the design tradeoff. The pure lambda-P presentation gives a
small proof-checking framework with explicit proof objects. A language that
directly identifies implication with function types, conjunction with
products, disjunction with sums, and existence with dependent pairs would
have the familiar surface types

```text
A implies B          ~ A -> B
A and B              ~ A * B
A or B               ~ A + B
forall n. P(n)       ~ Pi n:Num. P(n)
exists n. P(n)       ~ Sigma n:Num. P(n).
```

Bare lambda-P supplies Pi, but not all of these additional type constructors
and their computation rules. They can be added as an extension, or a richer
PTS can support suitable encodings. Either choice needs its own precise
proof-preservation and computational analysis. In particular, the classical
continuation encoding `not forall n. not P(n)` does not preserve constructive
existential witnesses and is not used here.

### Clarification: extraction, external normalization, and conversion

The statement that numerical execution comes through extraction concerns
this particular arithmetic signature. Pure typed lambda calculi already
perform beta computation and can compute on suitable higher-order numeral
encodings. Here, however, native Num and its arithmetic functions are opaque
to the LF kernel. For example,

```text
(lambda n:Num. Div n C_2) C_4  -->beta  Div C_4 C_2
```

stops at a symbolic division term. Extraction interprets the corresponding
constants as the standard numbers 8 and 2, and executes numerical division
to obtain 4. It does not require LF to identify the source expression with
C_3 by definitional equality.

For the logical constructors, candidate proof-normalization rules include

```text
impE A B (impI A B f) a       --> f a
allE P (allI P f) n          --> f n
andL A B (andI A B p q)      --> p
andR A B (andI A B p q)      --> q.
```

These are introduction/elimination simplifications. The first rule
preserves the displayed type: both sides have type `Proof B`. They are not
beta steps of the present LF encoding, because the heads are constants.

There are two distinct ways to use such rules. An external proof transformer
can produce a new proof term and have its typing checked by the existing
kernel. No new conversion principle is required for that workflow. If,
instead, the kernel uses these rewrites R when deciding definitional equality,
its conversion rule changes from beta equality to beta-plus-R equality:

```text
Gamma |- t : T       T =_(beta+R) U       Gamma |- U : s
-----------------------------------------------------
Gamma |- t : U.
```

That is the kernel extension intended by the earlier wording. The revised
system needs a justified conversion procedure and a preservation argument;
local type preservation of the displayed contractions alone does not prove
all the properties of the enlarged dependent type system. A conservative
extension may still preserve exactly the same represented arithmetic theorems.

A more direct encoding can make implication and universal quantification
computational through type-family rewrites:

```text
Proof (Imp A B)  -->  Proof A -> Proof B
Proof (All P)   -->  Pi x:Num. Proof (P x).
```

With these equations, ordinary lambda abstraction and application can serve
directly as the associated logical rules. This is the general approach of
lambda-Pi modulo rewriting; it is a different kernel specification from
the pure beta-only presentation above. Conjunction, disjunction, existence,
and arithmetic evaluation still need their own precise treatment.

Arithmetic reductions deserve a separate check. For example, declaring
`Div C_4 C_2 --> C_3` would make a proof of
`Eq (Div C_4 C_2) C_3` available by reflexivity plus conversion. An extraction
interpreter can evaluate that numerical expression without adding this
conversion to I. To add it conservatively to the proof kernel, one must
account for the corresponding arithmetic equality in the selected source
theory. The illustrative finite F is not assumed to derive all true closed
grounding-function evaluations.

The rewriting-framework distinction is supported by Cousineau and Dowek,
[Embedding Pure Type Systems in the lambda-Pi-calculus modulo](https://arxiv.org/abs/2310.12540).
This reference supplies the general framework, not an SJAS-specific
conservativity or self-consistency theorem for the proposed rewrites.

## 6. The self-consistency constant

Expand the existing bounded formulas `FalseCode(a)` and `Prf_I(a,p)` into
the Fm representation and define

```text
H_Fm = All (lambda a:Num.
         All (lambda p:Num.
           Imp (And (FalseCode a) (Prf_I a p)) Bot))

self_I : Proof H_Fm.
```

`Prf_I` here abbreviates the full encoded arithmetic formula, not the LF
family `Proof` and not an oracle for the host type checker. The declaration
`self_I` represents the exact source axiom H, including its self-substitution
details; this readable notation does not replace those details.

Universal elimination and implication elimination turn `self_I` into a
derived operation with type

```text
reject_I : Pi a,p:Num.
             Proof (FalseCode a) ->
             Proof (Prf_I a p) -> Proof Bot.
```

Its body instantiates `self_I` at a and p, combines the two premise proofs
using `andI`, and applies `impE`. It rejects evidence of an I-proof of FALSE.
It is neither arbitrary truth reflection nor an operation that converts
an arbitrary LF proof term into an internally certified numerical code.

## 7. External proof correspondence

For closed source formulas A, this signature admits the external theorem

```text
I proves A
  iff
there exists t with Sigma_I |- t : Proof (encode(A)).
```

The argument has two directions:

1. Turn a finite Hilbert derivation into an intuitionistic natural-deduction
   derivation using the same nonlogical axioms. Each logical Hilbert scheme
   has its usual natural-deduction proof; modus ponens uses implication
   elimination, and generalization uses universal introduction with its
   source side conditions. Encode the resulting derivation constructor by
   constructor using Section 2. Nonlogical axiom leaves use `ax_alpha`.
2. Normalize an LF inhabitant and eta-expand it to canonical form. Lambda-P
   normalization applies with these constants treated as context entries;
   no recursive definitions or new reductions have been added. Canonical
   Num and Fm terms decode to source terms and formulas. A canonical
   `Proof (encode(A))` term has a head that is an assumption, a listed
   logical-rule constant, or an authenticated axiom constant. Reading these
   heads recursively produces a natural-deduction proof of A. The discharge
   and eigenvariable conditions follow from the indicated Pi scopes.
   Equality constructors decode to source equality reasoning, and an axiom
   constant decodes to the exact source axiom. Translate the result into
   Hilbert deduction by the intuitionistic deduction theorem and the usual
   quantifier schemes. Close any residual free individual parameters with
   C_0. Finally serialize full formulas, annotations, and padding exactly
   as required by the I checker.

For the syntactic decoding in step 2, contexts contain only source individual
parameters and source proof assumptions at the object-proof boundary. The
signature has no eliminator inspecting Num/Fm syntax, and no proof
reflection or arbitrary function returning Num/Fm from proof evidence.
Consequently canonical forms introduce no extra object-language constructs.
All finitely used source axiom declarations can be treated as extra closed
proof assumptions and discharged into axiom leaves when decoding.

This is the standard LF adequacy argument specialized to the selected
intuitionistic rules and source axiom signature, followed by the external
equivalence of intuitionistic natural deduction and Hilbert deduction.
It is a paper argument using LF metatheory, not a machine-checked compiler.

In particular, a closed LF inhabitant of `Proof Bot` would give an actual
accepted I-proof of FALSE. The source consistency argument therefore gives
external consistency for these object-proof judgments.

## 8. The checker boundary and an implementation path

The preceding equivalence is external. It supplies no I-proof of totality
or correctness for normalization, translation, coding, or serialization.
Accordingly the displayed `self_I` constant is not, merely by renaming,
a proof of the arithmetized consistency of a new standalone LF checker K.

For example an internal bridge sufficient to transfer consistency would
have the form

```text
forall a,r.
  K(a,r) -> exists p. Prf_I(a,p).
```

Together with H this excludes K-certificates of FALSE. External compilation
for each standard proof does not establish this internally quantified
statement. A genuinely native self-verifying PTS would instead require its
own specified arithmetization/fixed point and a corresponding external
consistency proof. Neither new result is claimed here.

An immediately faithful software architecture would elaborate LF proof
terms to the already specified full I certificates and retain those source
certificates as the acceptance basis. This gives typed proof construction
and extraction while keeping the existing consistency assertion attached
to its original checker. Software implementing even that architecture
would require the repository's usual ADR and validation work.

The older Type-A tableaux lowering obstruction concerned a different,
successor-total source and particular macro coding. It is not an impossibility
theorem for this weaker Hilbert source. Here the important boundary is that
a new internal checker bridge has not been established.

### Clarification: exactly which consistency statement is available

Abbreviate the source assertion that p is a Hilbert proof of FALSE by
HPrf(p). Suppose a new PTS checker has also been given a faithful arithmetic
predicate KPrf(r), meaning that r is a certificate of a closed inhabitant of
`Proof Bot` relative to the fixed signature. Then the two formulas are

```text
Con_H := forall p. (HPrf(p) -> FALSE)
Con_K := forall r. (KPrf(r) -> FALSE).
```

The signature contains the translation of Con_H. Re-expressing that formula
as a type preserves its reference to HPrf; it does not substitute KPrf for
HPrf. The native checker statement concerns typing certificates, including
the chosen typing/conversion rules, signature authentication, and encoding.

Section 7 supplies an external translation argument. It gives, in the
metatheory,

```text
forall r. KPrf(r) -> exists p. HPrf(p),
```

and hence an external proof of Con_K from Con_H. For an internal transfer,
one sufficient additional theorem is

```text
I |- forall r. (KPrf(r) -> exists p. HPrf(p)).
```

This is weaker than the all-formulas bridge displayed above and does not
require a total translating function symbol on every numerical input.
Only witness transfer for alleged contradictions is needed. Given the
internal theorem, the following is an entirely intuitionistic derivation:

1. Assume KPrf(r).
2. Obtain `exists p. HPrf(p)` from the bridge.
3. For an arbitrary such witness p, instantiate Con_H to obtain
   `HPrf(p) -> FALSE`; apply it to the witness evidence.
4. Eliminate the existential, discharge the KPrf(r) assumption, and
   universally generalize r. The result is Con_K.

An external effective compiler on standard finite derivations does not
itself supply the internally quantified theorem in step 2. Its formalization
can require normalization, substitution, and output-code existence/size
properties not yet proved in the weak source theory. Moreover, internal
universal quantifiers range over all numbers in each model of I, including
nonstandard models. A metatheoretic account of standard computations is not
automatically an I-proof covering those quantifiers.

This identifies an unestablished bridge for the current proposal, not an
impossibility result for the weak Hilbert source. An alternative native
construction could define its own fixed-point axiom Con_K for the complete
revised checker and establish consistency externally. That would require a
new self-justification argument.

There is also an immediate certificate interface that uses the existing
claim. Require both a PTS certificate r and a source Hilbert certificate p
for the same claimed formula a:

```text
Accept(a,r,p) := K(a,r) and Prf_I(a,p).
```

Provided K is represented in the source language, I already proves

```text
forall a,r,p.
  not (FalseCode(a) and Accept(a,r,p)).
```

The derivation projects `Prf_I(a,p)` from Accept and applies H together with
FalseCode(a). The caller supplies p, so no internally total compiler is
required. This proves the no-FALSE-certificate property of that paired
interface; it is not a theorem about acceptance by K alone. The same source
projection protects the interface even if K's computational conversions
change, as long as the original source certificate remains mandatory.

## 9. Source and validation status

- The lambda-P PTS rules were checked against Sorensen-Urzyczyn,
  *Lectures on the Curry-Howard Isomorphism*, Figure 13.2 and Section 13.5:
  [author lecture notes](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf).
- Logical-rule signatures and the canonical-form adequacy method were
  checked against Harper, Honsell, and Plotkin, *A Framework for Defining
  Logics*, Section 4.1 and Theorem 4.1:
  [author manuscript](https://homepages.inf.ed.ac.uk/gdp/publications/Framework_Def_Log.pdf).
  The source's illustrative classical reductio rule is omitted here;
  the signature above lists the chosen intuitionistic rules explicitly.
- Rendered pages inspected: LF manuscript pages 19-21 and the lecture
  notes' printed page 213 (local PDF page 225). The source arithmetic,
  consistency predicate, and extraction theorem are those in the linked
  construction note, with its existing arithmetization assumptions.
- The explicit proof terms were checked by following their displayed types;
  no executable type checker or mechanized adequacy theorem was run.
  Documentation whitespace and local-link checks are not metatheory tests.

Only this note and its LOG.md entry belong to this task. The concurrent
implementation branches and prototype directories are unchanged by it.
