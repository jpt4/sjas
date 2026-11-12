# Lowering a PTS certificate checker: a negative resolution

Date: 2026-09-05 (America/Indiana/Indianapolis).

Continuation of the [logged request to establish lower](2026-09-05-lower-obligations-conversation.md)
and the [SJAS-to-PTS construction](2026-09-05-sjas-pure-type-systems.md).
All formulas below use CLI-readable notation.

## 1. Result and its scope

Fix the Type-A tableaux theory `S` in section 2. Fix the certificate checker
`Kmac` in section 3: it accepts explicit, closed `lambda-P` typing derivations
and also Hilbert derivations interpreted as macros for such typing derivations.
Both formats certify the same translated first-order theorem set.

For this choice, **there is no lowering theorem in `S`**. Even its instance
at one fixed false sentence is unprovable:

```text
e := code(0 = 1)

S does not prove:
  forall r. Kmac(e,r) -> exists p. TabPrf_S(e,p).
```

Consequently `S` does not prove the full `lower` statement. It also does not
prove the direct pairwise-consistency statement for this checker, so omitting
the general translator does not rescue that particular gate guarantee.

The reduction below is a new paper argument using Willard's stated
Hilbert-incompleteness boundary. Its source dependency is explicit in section 8;
it is not an independently mechanized proof of that boundary.

**The certificate format is part of the result.** A Hilbert macro is a
compressed certificate, whose expansion is defined below. It is not an
additional axiom, reduction rule, or primitive of `lambda-P`. This result
does not establish that a checker accepting only fully expanded typing trees
has the same internally expressible consistency strength. Nor does it apply
to every EA-stable configuration: the source chosen here is Type-A, with
total addition and doubling.

## 2. A fixed source theory

Use Willard 2011's configuration `xi*` from Definition D.1. Its language
`L*` has the eight U-grounding functions, equality, order, and constants
for 0, 1, and 2. In particular, addition and doubling are functions;
multiplication is represented by its bounded graph, not a multiplication
function. `Delta0*` consists of the bounded formulas of this language.

For a definite recursively presented base, let `PA+` be PA with the
standard conservative definitions of these eight functions, and set

```text
B := { A : A is a closed Pi1* formula and PA+ proves A }.

S := B + H
H := SelfCons_1(B, Tab).
```

An occurrence of a `B` axiom carries a finite `PA+` proof of that axiom.
Checking this evidence is distinct from searching for it. Include bounded
sentences in the hierarchy by allowing an empty unbounded quantifier block.
Fix that same convention in `Neg1`, so `0 = 1` and its formal negation are
an admissible pair.

In the usual external metatheory, `PA+` is sound in the standard natural
numbers. Thus `B` consists of true `Pi1*` sentences, proves every true closed
`Delta0*` sentence, and contains D.1's bounded addition-totality axiom (46).
These are the hypotheses used for `xi*`. Theorems D.4 and 5.9 give external
consistency of `S`, together with its internal Level(1) consistency axiom.
This specializes the earlier generic source choice; it does not add a
lowering or compiler-totality axiom to it.

Use conventional byte-string encodings for formulas and proofs. For a byte
string `b[0] ... b[n-1]`, its numerical code is, externally,

```text
256^n + sum(i < n, b[i] * 256^(n-1-i)).
```

The leading digit preserves length. Syntax uses prefix constructor tags,
explicit variable indices and arities, and finite lists. Each function-symbol
occurrence takes at least one byte, so tableaux satisfy D.1's requirement
of at least five bits per function-symbol occurrence. This definition of
coding in the metatheory does not declare exponentiation total in `S`.

Write `TabPrf_S(x,p)` for the original tableaux predicate and
`HilbPrf_S(x,h)` for conventional Hilbert proofs from the **same axioms**.
Both check that the final formula is a sentence, and include evidence of
axiom membership in their proof certificates. For Hilbert proofs use the
conventional B-adic, `Delta0-` arithmetization covered by Willard 2001
Theorem A.2. In particular, this choice is not an arbitrary unusual proof
predicate having the same standard theorem set.

The bounded checks concern syntax and supplied certificates. Source function
symbols occurring in a formula are parsed, not evaluated. Parsing, matching
logical axiom instances, checking line references, and validating the supplied
`PA+` axiom evidence are finite string checks; their witnesses and components
are bounded by the supplied codes. They admit the usual arithmetization in
the non-growth language (the `Delta0-` sublanguage of `Delta0*`). There is no
requirement to construct a missing axiom-membership witness. This uses the
conventional arithmetization assumed by the cited boundary; it is not a new
mechanized construction of that arithmetization.

The fixed point is the one already specified in the PTS note: if `n` is
the code of the open consistency template `Gamma(g)`, then `H = Gamma(bar(n))`
and `TabPrf_S(x,p)` abbreviates `SubstPrf_B^Tab(bar(n),x,p)`. The code of
the template is not the code of the substituted sentence. Neither `Kmac`
nor its consistency sentence occurs in this construction.

Two facts about this source will be used:

```text
(C) S |- forall p. not TabPrf_S(e,p).

(H) S does not prove forall h. not HilbPrf_S(e,h).
```

For (C), choose a fixed tableaux proof `q0` of `not (0 = 1)` and let `ne`
be that sentence's code. Bounded completeness gives the closed checking facts

```text
S |- Neg1(e,ne).
S |- TabPrf_S(ne,bar(q0)).
```

Instantiate `H` with those fixed witnesses. This excludes every tableaux
proof of `e`, without a uniform proof-composition theorem.

For (H), apply Willard 2001 Theorem A.2 in its stated form. Its
Successor-Regular hypotheses, from printed p. 45, hold here:

- `S` is externally consistent, as established above.
- It recognizes successor as total: `x+1` is an existing source term.
- It proves every `Pi1-` theorem of PA: these form a subclass of the
  `Pi1*` theorems used to define `B`.
- Its Hilbert proof predicate has the conventional bounded encoding fixed
  above, and that encoding is B-adic.

The result concerns Hilbert proofs from `S`, even though the consistency
axiom in `S` concerns tableaux. Its published proof has status `sketch`;
that dependency is retained in section 8. No uniform simulation of Hilbert
proofs by tableaux is assumed.

Willard 2011 Theorem 2.1 is a related, stronger cited boundary, but is not
needed for this application. Using A.2 avoids silently identifying its
successor-totality hypothesis with 2011's more specific requirement about
containing a Type-S axiom. The bounded axiom (46) and the unbounded
totality sentence are not the same syntactic axiom.

## 3. The closed-judgment checker and its macro format

Retain exactly the PTS and negative formula translation `Tr` of the earlier
note. Its fixed signature `Sigma_S` contains the arithmetic declarations,
translations of the source axioms, and first-order equality axioms. It has
no recursor on the source domain and no additional proof constants.

First define `Ktree(x,d)` by the following certificate format.

- `x` is the code of a closed source formula `A`.
- `d` contains a source-formula parse, its `Tr` translation derivation,
  a PTS term `t`, and a finite derivation of `Sigma_S |- t : Tr(A)`.
- Terms and contexts use de Bruijn indices. Every derivation node names
  one of the ordinary PTS sort, variable, weakening, product, abstraction,
  application, or conversion rules and supplies its premises.
- The product triples are `(Type,Type,Type)` and `(Type,Kind,Kind)`;
  the sole sort axiom is `Type : Kind`.
- A conversion node carries a finite chain of beta steps or reversed beta
  steps, with the redex position and capture-avoiding substitution evidence
  for each step. Type formation is still checked by the typing premises.
- Every used signature declaration is authenticated. A `B` axiom names
  its `PA+` proof, and an `H` declaration is checked against the fixed
  sentence above. Extra proof assumptions at the root are rejected.

The translation witness and conversion witnesses are supplied data. The
checker never assumes a total syntax translator or normalizer inside `S`.
This is an inductive specification of certificates, not an implementation
of a parser or an arithmetic formula with every abbreviation expanded.

Now permit a second format, encoded by an even outer code:

```text
Kmac(x,r) :=
    (exists h <= r.
       r = Double(h) and HilbPrf_S(x,h))
  or
    (exists d <= r.
       r = Double(d)+1 and Ktree(x,d)).
```

The first arm is a **Hilbert macro certificate**. Its payload `h` is
interpreted by the following fixed elaborator, not accepted as an
uninterpreted claim that some PTS term exists.

### Macro expansion into ordinary PTS terms

A Hilbert certificate is an ordered list of formulas with axiom-instance,
modus-ponens, and generalization justifications. Earlier lines can be
referenced repeatedly. For each line `A_i`, construct a term `t_i : Tr(A_i)`.
The proof is by induction on the finite line list.

| Source justification | PTS expansion |
| --- | --- |
| An axiom of `S` | Its authenticated declaration in `Sigma_S` |
| `A -> (B -> A)` | `lambda a:Tr(A). lambda b:Tr(B). a` |
| `(A -> B -> C) -> (A -> B) -> A -> C` | `lambda f. lambda g. lambda a. f a (g a)`, with the displayed translated types |
| `((A -> bottom) -> bottom) -> A` | The formula-specific `stable_A` term from the negative translation |
| `(forall v. A) -> A[u/v]` | `lambda f. f TrTerm(u)`, with the usual free-substitution condition |
| `(forall v. A -> B) -> (A -> forall v. B)`, `v` not free in `A` | `lambda f. lambda a. lambda v:Num. f v a` |
| Modus ponens, lines `j : A -> B` and `k : A` | `t_j t_k` |
| Generalization of `A` in `v` | `lambda v:Num. t_i` |
| First-order equality axiom | Its translated equality declaration; no host equality eliminator is used |

One can take `bottom`, implication, and universal quantification as the
primitive logical basis. The other source connectives have the classical
abbreviations

```text
A and B    := (A -> B -> bottom) -> bottom
A or B     := (A -> bottom) -> (B -> bottom) -> bottom
exists v.A := (forall v. A -> bottom) -> bottom.
```

Their negative translations are exactly the encodings in the PTS note.
For source syntax retaining these connectives, use the corresponding
definitional Hilbert schemata and their expanded terms. All substitutions
and formula codes refer to the fixed source syntax; abbreviations do not
change the quoted code of the final sentence.

Line terms may initially have free **individual** variables. Source axioms
are closed; generalization abstracts the appropriate variable, with alpha
renaming to avoid capture. At a closed final formula, substitute the source
zero term for any remaining free individual parameters. There are no free
proof assumptions. This produces a term closed relative to `Sigma_S`.

Repeated references can be expanded by copying. This process is finite for
every standard certificate and can enlarge it substantially. The macros
are certificate notation only: the expanded terms use ordinary application
and abstraction, with no recursive definitions or new computation rules.

Thus both arms of `Kmac` certify closed PTS inhabitants. Conversely, every
such inhabitant has an explicit typing derivation accepted by the second
arm, with finite conversion and source-axiom evidence. In the external
metatheory the macro format changes certificate representation, not the
set of translated theorems accepted.

### The internal bridge requires only doubling

The critical point is now a direct derivation in `S`:

```text
(I) S |- forall x,h.
      HilbPrf_S(x,h) -> Kmac(x,Double(h)).
```

For the first disjunct, choose the already given `h`. The bound
`h <= Double(h)` is a universal arithmetic fact in `B`. This proof creates
no expanded PTS derivation and no new formula-translation certificate.
`Double` is an existing source function. In particular, (I) is not inferred
from the external computability or polynomial running time of an elaborator.

This design choice is exactly why the present obstruction is unconditional
with respect to a compiler-totality assumption. It also explains its scope:
removing the macro arm removes this particular internal bridge.

## 4. The obstruction, in full

Suppose `S` proved lowering only at `e`:

```text
(L_e) forall r.
        Kmac(e,r) -> exists p. TabPrf_S(e,p).
```

Reason inside `S`, for an arbitrary `h`:

```text
1. Assume HilbPrf_S(e,h).
2. Kmac(e,Double(h)).                    by (I)
3. exists p. TabPrf_S(e,p).              by (L_e)
4. Contradiction.                       by (C)
```

Discharge the assumption and universally quantify `h`. This proves

```text
S |- forall h. not HilbPrf_S(e,h),
```

contrary to (H). Therefore `S` does not prove `(L_e)`. Full `lower`
implies `(L_e)` by specialization and is unprovable as well.

There is no `up` assumption and no `applyK` assumption in this reduction.
It does not infer failure merely from the absence of a primitive tableaux
modus-ponens rule. It uses one proved injection of **Hilbert refutations**
into the chosen certificate format and the source's distinct consistency
boundaries.

Through the earlier first-order conservativity theorem, this also excludes
a closed inhabitant of the corresponding translated `lower` type in
`Sigma_S`. Negative existential encoding does not avoid the obstruction.

Unprovability of `(L_e)` is not a claim that `S` proves its negation.
Nor does it imply that adding `(L_e)` as an axiom is inconsistent. Such an
extension would be a different theory; the preceding argument concerns
what the original `S` proves about proofs from that same original `S`.

## 5. A direct gate proof is also excluded for this checker

Consider the weaker target proposed in the conversation:

```text
Safe_Kmac := forall x,y,r,s.
  not (Neg1(x,y) and Kmac(x,r) and Kmac(y,s)).
```

Choose a fixed Hilbert proof `h0` of `not (0 = 1)` from `B`. Bounded
completeness proves its closed checking fact, and (I) gives

```text
S |- Kmac(ne,Double(bar(h0))).
```

If `S` proved `Safe_Kmac`, instantiate it with this certificate and the
fixed fact `Neg1(e,ne)`. The result is `forall r. not Kmac(e,r)`.
Using (I) would again give Hilbert consistency, contradicting (H).

Thus the direct gate invariant is unprovable here even without asking
for a general proof translator. A restriction to bounded or `Pi1` **formulas**
alone does not fix this: the obstruction already uses `0 = 1`. Restrictions
on the accepted **certificates** can change the result.

The original gate using `TabPrf_S` retains its proof. The new finding rules
out transferring that same universal guarantee to this macro-enabled PTS
certificate interface within the same source theory.

## 6. An explicit external translator and the failed internal obligation

There is an elementary specification of a translator that avoids any
assumption about the efficiency of normalization:

```text
erase(x,r):
  check Kmac(x,r); reject if it fails
  for p = 0, 1, 2, ...:
    if TabPrf_S(x,p): return p
```

Each check concerns a finite supplied certificate. For accepted standard
inputs, macro expansion or the explicit typing derivation gives a closed
PTS inhabitant; the earlier external conservativity argument gives a
classical `S` theorem; completeness of tableaux gives a finite source
proof. Hence the search eventually reaches an accepted `p`.

That establishes external termination on accepted inputs. It gives no
uniformly source-provable output bound.

For precision, define `EraseRun(x,r,p,w)` to be the bounded relation checking
a complete trace of this algorithm: an accepted input, a scan of successive
candidate codes starting at zero, rejecting outcomes before the last
candidate, and the accepting `TabPrf_S(x,p)` check at return. The trace stores
the finite checking evidence as well as the candidate values. Its final
acceptance condition yields the paper derivation

```text
S |- forall x,r,p,w.
  EraseRun(x,r,p,w) -> TabPrf_S(x,p).
```

This is partial correctness: the returned certificate is checked before
return. It does not assert the existence of a return trace. It is deliberately
a search translator, not a claim that efficient structural readback has
been implemented.

The completion theorem

```text
forall x,r.
  Kmac(x,r) -> exists p,w. EraseRun(x,r,p,w)
```

would imply full `lower` by that correctness statement. Section 4 therefore
proves that `S` cannot establish this completion theorem. The missing
uniform output-existence argument has a negative answer for this checker;
it is not a pending implementation step.

## 7. What remains possible with a supplied bound

For any particular bounded acceptance condition `BudgetOK`, suppose `S`
does prove

```text
(B) forall x,r,b.
      Kmac(x,r) and BudgetOK(x,r,b)
      -> exists p <= b. TabPrf_S(x,p).
```

Then `S` cannot also prove

```text
(T_e) forall r.
        Kmac(e,r) -> exists b. BudgetOK(e,r,b),
```

since `(B)` and `(T_e)` imply `(L_e)`. This locates the boundary more
precisely than saying that a normalization algorithm might be expensive.

A gate checking `Kmac` together with a sufficient supplied bound can still
be a different, useful interface if `(B)` is established. This note supplies
no new size/rank analysis proving `(B)` and does not redefine `BudgetOK`
as the existence of a successful source proof. Likewise, requiring an
original tableaux certificate remains the previously established special
case. Neither is presented as a proof of unrestricted `lower`.

## 8. Evidence and proof status

| Dependency or new result | Evidence and status |
| --- | --- |
| Chosen Type-A configuration | Willard 2011 Definition D.1, printed pp. 38-39; p. 38 rendered and inspected in this continuation |
| External consistency of the chosen source | Willard 2011 Theorems D.4 and 5.9; theorem statements rendered at printed pp. 43 and 18; corpus status `full` |
| Hilbert boundary used in (H) | Willard 2001 definition of Successor-Regular, printed p. 45, and Theorem A.2, p. 47, both rendered and inspected; corpus status `sketch`; A.1 is `stated-only`, and A.2's footnote 20 omits formalization details |
| Related stronger statement, not needed for the reduction | Willard 2011 Theorem 2.1, printed p. 4, rendered and inspected; corpus status `cited`, attributed to Solovay's unpublished generalization |
| Macro expansion, injection (I), impossibility of `(L_e)` and `Safe_Kmac` | New paper derivations above, not mechanically checked |
| `Ktree`, macro certificate format, and `erase` | Mathematical specifications; no executable checker, compiler, or Lean development added |

Primary sources:

- [Willard 2011, arXiv 1108.6330v8](https://arxiv.org/pdf/1108.6330v8),
  [held PDF](../../nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf).
- [Willard 2001, Journal of Symbolic Logic](https://doi.org/10.2307/2695030),
  [held author PDF](../../nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf).
- [Earlier PTS proof and its sources](2026-09-05-sjas-pure-type-systems.md#5-preservation-and-exactly-what-is-preserved).

The corpus registries and the concurrent Codification/Refinement review
are unchanged. The reduction is conditional on the cited boundary in its
stated form; this continuation has not upgraded the source's `cited` or
`sketch` status to an independently replayed proof.
