# A concrete machine with executable self-justification

Date: 2026-11-27. Branch: `research/affine-reflection`.
Continues the [affine-core experiment](2026-11-27-affine-reflection.md)
and [ADR-0011](../../nachlass/refinement/ADR-0011-affine-reflection.md).

## Result

The raw-code representation gap in the first experiment is closed for a
fully specified finite-tree machine. It has its own checker as an executable
instruction, arbitrary raw program inputs, runtime type codes, total code
evaluation, and single closed terms representing H0 and uniform H1. The
consistency theorem covers the actual executable checker for the whole
extended language. It assumes neither an older checker nor reflection.

The implementation and all-size proofs are the same Lean source:
[`RawMachine.lean`](../../code/affine-reflection/RawMachine.lean).
[`RawChecks.lean`](../../code/affine-reflection/RawChecks.lean) contains
kernel-checked behavioral examples and the axiom audit.
[`RawDemo.lean`](../../code/affine-reflection/RawDemo.lean) is runnable.

The model uses a primitive structural checker, just as it uses a primitive
unquoter. Both have fully specified, computable implementations. The checker
has not been compiled into the original BCKI basis; source-level self-hosting
of that algorithm is not claimed. No theoremhood or consistency oracle is
present.

## The representation that matters

Let `C(p,A)` be the machine's actual Boolean checker. Primitive constants
carry finite type annotations, so checking is structural recursion with
decidable type equality; there is no unverified type unifier.

There are two types of finite evidence:

```
Box A   = a raw program p accepted by C(p,A)
Opposed = a runtime type code A and raw programs p,q
          accepted by C(p,A) and C(q,A -o Bot)
```

These are specialized dependent packages. They are implemented by syntax
constructors whose typing checks the stored payloads, not by a convention
that callers must provide honest inputs. Their representation equations are
kernel-checked:

```
C(<p>, Box A)            = C(p,A)
C(opposed(A,p,q),Opposed) = C(p,A) && C(q,A -o Bot).
```

Canonical-form theorems show that every typed value of these types has the
indicated shape. The evaluator theorem shows that every accepted closed
expression terminates at a value of its declared type. The representation
claim therefore covers computed inputs as well as literal packages.

Negating the packages is constructively equivalent to the desired universal
raw-code assertions:

```
(Box Bot -o Bot)
    corresponds to forall p, C(p,Bot) -> Bot

(Opposed -o Bot)
    corresponds to forall A,p,q, C(p,A) -> C(q,A -o Bot) -> Bot.
```

`h0_reading` and `h1_reading` prove the corresponding packaging/currying
equivalences in the metatheory. The single type `Opposed` includes **all**
finite runtime type codes of this extended language, including types that
mention `Opposed`, `Raw`, and `Box`. H1 is no longer a family specialized
externally to an A chosen in advance.

The object language represents these assertions by negative package types.
It does not implement a general syntax of dependent products, arbitrary
first-order formulas, or all of lambda-cert. This is an explicit encoding of
these two assertions, with exact representation theorems, not a bounded or
changed consistency predicate.

## Concrete syntax and computation

The types are finite trees over

```
Bot Unit G Raw TypeCode Bool Opposed
A -o B     Box A     Option A.
```

Machine terms comprise annotated primitive constants, application, quotation,
raw literals, type-code literals, two Boolean values, option constructors,
opposed packages, and junk. `raw(p)` accepts any finite payload, even one
that the checker rejects as executable code. W is representable as raw data
but rejected as an executable primitive.

The eight original affine reductions remain. The new instructions have
these types:

| Instruction | Type |
| --- | --- |
| check | `Raw -o TypeCode -o Bool` |
| seal A | `Raw -o Option (Box A)` |
| checkPair | `Raw -o Raw -o TypeCode -o Option Opposed` |
| H1 | `Opposed -o Bot` |
| ifThen A | `Bool -o A -o A -o A` |
| caseOpt A B | `Option A -o B -o (A -o B) -o B` |

In particular:

```
check raw(p) type(A)
    -> C(p,A)

seal(A) raw(p)
    -> some(<p>)                  if C(p,A)
    -> none                      otherwise

checkPair raw(p) raw(q) type(A)
    -> some(opposed(A,p,q))       if C(p,A) and C(q,A -o Bot)
    -> none                      otherwise

H1 opposed(A,p,q)
    -> q p.
```

The returned Booleans in the first rule are literal machine values. The
`check_exec`, `seal_exec`, `pair_exec`, and `h1_exec` theorems establish these
execution equations for arbitrary raw inputs. They do not merely test a
few chosen examples.

Checking consists of a finite primitive-type table and a recursive walk:
an application checks both children and matches its domain; quotation checks
its payload; an opposed package checks both proofs against its stored type;
raw data returns type Raw without executing or validating its payload.
The checker contains clauses for its own instruction, seal, and H1. It does
not call the consistency theorem, execute the code being checked, or
blacklist Bot as a proposed conclusion.

## Internal self-justification

The machine accepts these closed terms:

```
H0 = E[Bot] : Box Bot -o Bot
H1         : Opposed -o Bot.
```

H0 unquotes and runs the supplied program. H1 applies the supplied
refutation to the supplied proof. Their types represent consistency for
the same checker that accepts H0, H1, their quotations, and the checker
instruction. No checker for a preceding stage is substituted.

As in any sound constructive consistency proof, no legal closed input can
reach the contradictory case. H0 and H1 cannot be demonstrated on an
accepted contradictory certificate. Executable reflection is witnessed at
inhabited types: E runs code, seal moves accepted raw syntax into executable
quotation, and an object program consumes it. At Bot the same mechanism
implements refutation.

The same finite evaluator and checker definitions are compiled and executed.
The original Python prototype is retained as an illustration of the smaller
core; the new result does not rely on Python's unification implementation.

## Why the extension remains sound

Count every term node, including payloads inside raw literals, quotations,
and opposed packages. Type annotations are erased from this measure; their
comparisons terminate separately by recursion on finite type syntax.

Every root reduction strictly decreases this count. The new cases include:

| Operation | Size before | Successful-result size |
| --- | --- | --- |
| seal A on raw(p) | `size(p)+3` | `size(p)+2` |
| checkPair on raw(p),raw(q),type(A) | `size(p)+size(q)+7` | `size(p)+size(q)+2` |
| H1 on opposed(A,p,q) | `size(p)+size(q)+3` | `size(p)+size(q)+1` |

Failed checks produce one-node values. Conditional and option elimination
discard the unused branch. No rule duplicates an input program. Thus
checking can retain a program for later execution without producing two
independent copies of it.

The checker is a primitive structural algorithm, so its invocation counts
as a macro-step. The decreasing measure bounds macro-reductions, not host
CPU time. Both checking and evaluation terminate constructively, with no
fuel supplied from outside. Memory sharing in the implementation does not
alter the expanded-tree measure.

`preservation` proves that every rule preserves acceptance by the **actual**
checker. In the seal case it uses the successful local check. In the H1
case it uses the two checks recorded by the input package. Neither case
assumes consistency. `progress` and the canonical forms exclude a Bot value.
Recursion on tree size then proves consistency for the entire extension.

The implemented step function returns its reduct together with a proof of
the operational rule used. These proofs erase at runtime. `step_complete`
shows that the implementation cannot overlook every available reduction.
`run_stops`, `run_preservation`, and `run_value` establish a terminating,
type-preserving evaluator for every accepted input.

The resulting universal theorems are:

```
checker_h0 p:     C(p,Bot) = false
checker_h1 A p q: C(p,A) && C(q,A -o Bot) = false.
```

These quantify over unbounded finite syntax and all type codes. They are
kernel proofs, not consequences drawn from enumeration.

## What was executed

The demo has five accepted object programs:

| Program | Result | Term nodes |
| --- | --- | --- |
| Check the checker's own instruction | true | 6 -> 1 |
| Seal the uniform H1 instruction | some(quote H1) | 4 -> 3 |
| Check and seal the checker, then run it to check H1 | true | 19 -> 1 |
| Use checked identity code in an option branch | unit | 12 -> 1 |
| Try to seal junk as a Bot certificate | none | 4 -> 1 |

The third program uses C to pass arguments without copying its sealed
checker. This demonstrates execution through the checked code, not merely
checking a type and then ignoring the result.

`RawChecks.lean` covers every root-rule shape, inert code/data, successful
and unsuccessful checking/sealing, a forged opposed package, both Boolean
branches, both option branches, and evaluation inside an option. The success
branch of checkPair has no valid test input: `checker_h1` proves it is
unreachable. Its preservation and size obligations are nevertheless proved
without using that fact. No numeric coverage claim substitutes for these
universal proofs.

The earlier four-step W counterexample remains in `Decrease.lean` and the
Python tests. The new machine retains the same affine guard. The unsafe
extension is not silently enabled in its checker.

## Completion audit

| Requirement | Current evidence |
| --- | --- |
| Extremely concrete executable model | Finite grammar, finite instruction table, structural checker, implemented evaluator and demo |
| Full own-syntax coverage | `infer` checks E, check, seal, H1, quotations and runtime type packages |
| Honest raw-code domain | Raw accepts junk and W as data; ordinary checking/sealing rejects them as executable code |
| H0 for the actual checker | `h0_internal`, `box_representation`, `h0_reading`, `checker_h0` |
| One uniform H1 over runtime types and both raw codes | `h1_internal`, `opposed_representation`, `h1_reading`, `checker_h1` |
| Executable reflection | Arbitrary-input execution equations plus the checked-checker demo |
| Whole-system soundness | `preservation`, `progress`, `consistency`; none inherited from the old fragment |
| Correct terminating implementation | Proof-carrying step, `step_complete`, `run_stops`, `run_preservation`, `run_value` |
| Constructive foundation | Axiom audit: propext and Quot.sound only; no Classical.choice, reflection axiom, sorry, or native-decide axiom |
| Observable failure when duplication returns | Existing exact four-step W cycle and its type derivation |

This meets the requested goal of a concrete computational model with an
executable analogue of self-justification. Scope is the finite-tree machine
specified here, with its primitive checker and specialized evidence packages.
It makes no claim of useful self-improvement, general dependent logic,
arithmetic-strength self-consistency, or source-level self-hosting of the
checker. Those are possible further questions, not assumptions needed by
this model.

## Process record

The raw-code phase was specified in ADR-0011 before implementation. Its
acceptance tests first failed because RawMachine did not exist. The full
checker and runtime were then built in one Lean source. All-size proofs
cover every new constructor and primitive.

The axiom audit found Classical.choice in an initial automated proof of
`opposed_representation`; explicit Boolean case analysis removed it. This
did not change the checker or the statement. Tests for well-founded
evaluation use kernel rewriting by the evaluator's equation, since `decide`
alone does not unfold that definition; no native-decide shortcut was used.

The final validation record is
[`validation-raw.txt`](../../code/affine-reflection/validation-raw.txt).
