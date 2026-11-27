# Executable self-justification through affine combinators

Date: 2026-11-27. Branch: `research/affine-reflection`.
Decision record: [ADR-0011](../../nachlass/refinement/ADR-0011-affine-reflection.md).
Executable artifact: [code/affine-reflection](../../code/affine-reflection/).

## Result and scope

There is a small, total combinator calculus with a primitive evaluator for
its **own complete language**, including that evaluator. Its typed consistency
term is the evaluator specialized to the empty type. Quotation,
internal quotation, internal code application, and a Gödel-style negative
recursive type are all present. Removing contraction from the underlying
computation makes every reduction strictly shrink its input tree.

Adding the single combinator `W f x -> f x x` produces a closed term of
empty type with an exact four-step reduction cycle. This is a runnable
example of the self-trust boundary.

This establishes an executable **typed-code analogue** of self-justification.
It does not yet establish the project's stronger H0/H1 specification over
an internally represented raw-code checker. That distinction remains an
explicit research obligation, rather than being hidden in the meaning of
quotation. The external Python checker is also not verified against the
formal typing relation.

## Static proof system first

Types are finite trees:

```
A ::= Unit | Bot | G | A -o A | Box A
```

`Bot` has no introduction constructor. `G` is a rigid type symbol with explicit
fold/unfold maps at `Box G -o Bot`. Type checking never identifies an infinite
type with a finite one. The arrow is affine: a function can discard its
argument but cannot duplicate it.

Terms are finite trees:

```
t ::= I | K | B | C | E | D | M | F | U | star | t t | <t>
```

There are no variables in machine syntax. Affine lambda notation is merely
construction-time abbreviation, eliminated into B, C, K and I. Each constant
occurrence can instantiate its type scheme independently:

| Constant | Type scheme |
| --- | --- |
| I | `A -o A` |
| K | `A -o B -o A` |
| B | `(B -o C) -o (A -o B) -o A -o C` |
| C | `(A -o B -o C) -o B -o A -o C` |
| E | `Box A -o A` |
| D | `Box A -o Box (Box A)` |
| M | `Box (A -o B) -o Box A -o Box B` |
| F | `(Box G -o Bot) -o G` |
| U | `G -o Box G -o Bot` |
| star | `Unit` |

Application has its usual typing rule. If `t : A`, quotation has type
`<t> : Box A`. Since machine syntax contains no free variables, this is
quotation of closed syntax. Crucially, quotations can contain E, D, M, F,
and U: no older or weaker sublanguage is being substituted for the system
under discussion.

These typing judgments are the static proof system. `Box A` denotes typed
syntax, rather than a Boolean assertion that an unspecified proof exists.
The run-time rules below give those proof terms computational behavior.

## The entire execution mechanism

Application associates to the left. There are eight root rules:

```
I x             -> x
K x y           -> x
B f g x         -> f (g x)
C f x y         -> f y x
E <t>           -> t
D <t>           -> <<t>>
M <f> <x>       -> <f x>
U (F t)         -> t
```

Execution chooses the leftmost outermost rule outside quotation. Quoted
payloads remain inert until E removes their quotation. E is a primitive
unquoter: it transfers the stored syntax into the running machine, whose
same reduction rules then execute it. It is not a separately programmed
recursive interpreter, a theoremhood oracle, or an extra safety monitor.
The Python host performs syntax manipulation only.

For example, the demo executes an interpreter and feeds it code constructed
at run time:

```
(E <E>) (M <I> <star>)    12 nodes
E (M <I> <star>)           9 nodes
E <I star>                6 nodes
I star                    3 nodes
star                      1 node
```

The code constructor M moves the two supplied trees into an application.
D adds quotation around supplied code. Neither operation duplicates its
argument. Their own instruction nodes pay for the nodes they create.

## What the self-justification terms are

The type of consistency for typed syntax is inhabited by

```
H0 = E : Box Bot -o Bot.
```

The pair version is one finite term:

```
H1 = B (C E) E : Box A -o Box (A -o Bot) -o Bot.
```

It computes `H1 p q ->* (E q) (E p)`. Each supplied program is used once.
The theorem is uniform in the metatheoretic parameter A. There is no internal
type-code quantifier, dependent sum, or erased proof parameter in this core.

At Bot there can be no closed legal input to H0; that is what consistency
means. Its computational content is exhibited by the same E at inhabited
types, including function and code types, and by evaluation of E's own
quotation. It would be misleading to demonstrate the empty-domain H0 call
itself on an accepted contradictory certificate.

Necessitation is quotation of a closed term. D and M realize affine forms
of proof internalization and composition. G provides the recursive naming
equation through F and U. Those ingredients coexist with H0 because the
underlying logic does not let an unknown proof be used twice.

## The constructive metatheory

Count every atom, application, and quotation as one node. Count nodes
inside quotations and count repeated occurrences separately even if the
host implementation shares their memory. Type-inference work is not part
of this measure.

| Rule | Nodes removed |
| --- | --- |
| I | 2 |
| K | `size(y) + 3` |
| B | 2 |
| C | 2 |
| E | 3 |
| D | 1 |
| M | 3 |
| U/F | 4 |

Each contextual step has the same drop as its contracted subterm.
Consequently every reduction sequence has fewer than `size(t)` steps.
This even holds if reduction is allowed under quotations, a stronger
relation than the executable strategy uses. This bounds abstract rewrites;
it is not a claim that Python runs in linear time.

Every rule preserves typing, by direct inversion of its typing premises.
For example, `M <f> <x> : Box B` implies `f : A -o B` and `x : A` for some
A; its reduct `<f x>` therefore has the same type. U/F preserves typing
because the two maps use precisely the same negative recursive type.

The value forms are atoms, quotations, unsaturated K/B/C/M applications,
and `F v`, with value arguments. A typed value of code type must be a
quotation. A typed value of G must have the form `F v`. A typed function
value applied to an argument value is either another unsaturated value or
has a reduction. These observations establish progress for closed terms.
No value has type Bot.

If a Bot-typed term existed, progress would give a reduction, preservation
would leave a Bot-typed term, and size would strictly decrease. Recursion
on the original term's size therefore yields a contradiction. This proof
covers E, D, M, F and U, not merely BCKI. Pair consistency follows by applying
a purported `A -o Bot` term to a purported A term and using this theorem.

Both the size argument and the full declarative consistency proof have been
checked in Lean 4.32.1 core. Their axiom reports contain `propext` and
`Quot.sound`, no `Classical.choice`, no `sorryAx`, and no reflection axiom.
The H1 typing proof uses no axioms. The external proof establishes that the
internal consistency terms are safe; the object language is not being
credited with Lean's proof about itself.

## What happens when code can be copied

Give W the usual type `(A -o A -o B) -o A -o B` and rule
`W f x -> f x x`. Let

```
h = F (W (B U E))
omega = U h <h>.
```

The type derivation is short:

```
E       : Box G -o G
U       : G -o Box G -o Bot
B U E   : Box G -o Box G -o Bot
W(B U E): Box G -o Bot
h       : G
omega   : Bot.
```

Its complete cycle is

```
U h <h>
 -> W (B U E) <h>
 -> B U E <h> <h>
 -> U (E <h>) <h>
 -> U h <h>.
```

Tree sizes are `22, 18, 27, 25, 22`. The W step is the only increase.
Lean proves an infinite run by repeating these four steps; Python checks
the Bot type at every state and detects exact syntactic equality after
four steps. The unsafe typing derivation is written above and tested in
Python; the Lean development currently formalizes the cycle, not its
unsafe typing extension.

The source text can of course contain two literal copies of h. That does
not supply an object program that duplicates an arbitrary input tree.
The safe checker and normalizer reject W everywhere, including under
quotation. This is a structural restriction on computation, not a runtime
budget parameter or a probabilistic safety assumption.

## Relationship to the earlier work and literature

The current TSLC rationale and Tiler review correctly distinguish an
external agent checker from internal self-justification. This construction
makes no agent claim. It instead makes the consistency term a specialization
of an executable full-language evaluator, and gives the boundary a four-step
operational witness. Compared with lambda-cert, the restriction is global:
even ordinary function arguments are affine. This loses much more ordinary
programming power, but makes the model extremely small.

The proof-theoretic motivation is not a novelty claim. Beklemishev and
Shamkanov explicitly isolate contraction on provability formulas and give
a modal fixed-point system invalidating formalized G2. Their paper also
warns that suitable arithmetic can recover that contraction. This toy is
not asserted to be their formal system or an arithmetic self-justifying
theory. [Primary source, §§3–5](https://arxiv.org/pdf/1602.05728).

Gross, Gallagher and Fallenstein identify `Box X -> X` with interpreters or
unquoters, and derive Löb in settings supporting the needed self-reference
and proof operations. The present construction isolates an affine variant
and supplies an explicit duplication counterexample. This is our derivation,
not a theorem attributed to their paper.
[Primary source, §§2 and 6–8](https://jasongross.github.io/lob-paper/nightly/lob.pdf).

## Evidence and open obligations

| Claim | Evidence / status |
| --- | --- |
| Complete finite syntax, eight reductions, typed quotation | Specified above and implemented |
| All-size strict decrease and strong normalization | `Decrease.lean`, kernel-checked |
| Preservation, progress, consistency of the entire typed calculus | `Soundness.lean`, kernel-checked |
| H0 and the type-indexed H1 family | `Soundness.lean`, kernel-checked; Python type checks |
| Executing the evaluator's own code and constructed code | `python3 machine.py`, tests |
| Four-step infinite run after adding W | Lean kernel proof and Python exact trace |
| Unsafe Bot typing | Paper derivation and Python tests; not formalized in Lean |
| Python checker implements exactly Lean's Typed relation | Not proved; tested only |
| H0/H1 over an internally represented raw-code checker | Not implemented or claimed |
| An internally quantified H1 over type codes | Not implemented or claimed |
| Useful self-improving-agent advantage | No such claim |

The initial Python run failed on the absent module, as intended. After
implementation the semantic tests passed but an arbitrary expected lower
bound on the number of typable small trees was wrong (665, not over 1,000);
that assertion was corrected without reducing the enumeration domain.
A subsequent boundary probe exposed acceptance of malformed target-type
arity and `None`; a red regression preceded the fix. The final test suite
must be rerun after any further implementation change.

The next substantive issue is representation: can the raw-code checker and
the quantification required by the original H0/H1 be brought inside without
reintroducing code duplication or hiding it in an encoding? An answer must
either implement that extension and reprove soundness, or prove a precise
obstruction. This core does not settle it by definition.
