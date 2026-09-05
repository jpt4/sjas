# Establishing lower: conversation record

Date: 2026-09-05 (America/Indiana/Indianapolis).

## User question (verbatim)

What is required to establish `lower`?

## Assistant response (verbatim, excluding memory-rendering metadata)

`lower` requires a **uniform conservativity theorem proved inside `S`**:

```text
L_K :=
  forall x,r.
    K(x,r) -> exists p. Prf_S(x,p)

Required:
  S |- L_K
```

The continuation encoding does not weaken this logical requirement. It avoids requiring a program that *extracts* `p`, but `S` must still prove its existence.

There are three substantive problems: specifying `K`, justifying the translation uniformly, and checking whether that uniformity would restore a forbidden proof-composition principle.

## 1. Specify exactly what `K` accepts

For the proposed PTS, `K(x,r)` must mean:

```text
r certifies a closed typing judgment

  Sigma_S |- t : Tr(A)

where x = code(A).
```

This requires concrete representations of terms, types, typing derivations, the formula translation, and the approved signature.

Two details are essential:

- **The signature must be fixed.** If arbitrary additional assumptions are permitted, the judgment `z:False |- z:False` is accepted. Such a checker plainly cannot satisfy `lower` relative to a consistent `S`.
- **Conversion and axiom membership need certificates.** A useful representation lets `r` carry finite beta-conversion witnesses and, where necessary, evidence that an axiom belongs to the recursively presented source theory. Checking supplied evidence need not assume an internally total proof-search or normalization algorithm.

Similarly, writing `Tr(x)` inside a numerical checker must not quietly assume that `S` proves the syntax-translation function total. Its graph can instead be represented with an explicit translation witness.

Until these choices are made, `K` is a specification parameter, not an implemented predicate whose `lower` theorem we can settle.

## 2. Prove uniform existence and correctness of a translation

One sufficient approach is to define a relation:

```text
EraseRun(x,r,p,w)

  w records a completed translation of accepted PTS certificate r
  into original source proof p of sentence x.
```

Then prove both statements **in `S`**:

```text
Completion:

  forall x,r.
    K(x,r) ->
      exists p,w. EraseRun(x,r,p,w)


Correctness:

  forall x,r,p,w.
    K(x,r) and EraseRun(x,r,p,w) ->
      Prf_S(x,p)
```

Together they imply `L_K`.

For the translation previously given, the external argument was approximately:

```text
PTS typing derivation
  -> normalized proof term
  -> first-order deduction
  -> original d-proof
```

To use that argument internally, each necessary transformation must be justified there. In particular:

```text
normalization terminates on the relevant accepted inputs;
normalization preserves their types;
readback preserves the source conclusion and permitted axioms;
the resulting certificate exists in the source coding;
the original checker accepts that certificate.
```

The fourth condition is not merely an implementation detail. A finite output in our metatheory need not have a uniformly provable existence theorem in weak arithmetic. Willard explicitly distinguishes the external existence of composed proofs from their potentially enormous cut-free representations. [Lemma 7.1 and its following comment, author copy pp. 37–38](/home/jpt4/code/proflog/sjas/nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf)

A different argument might avoid normalizing or constructing an output explicitly. These are sufficient proof obligations for a compiler-based approach, not the only conceivable proof of `L_K`.

## 3. The application case exposes a possible obstruction

Consider a naive inductive proof of `lower`. At an application node:

```text
f : Tr(A) -> Tr(B)
a : Tr(A)
------------------
f a : Tr(B)
```

Suppose the induction hypotheses yield only:

```text
exists p. Prf_S(code(A -> B), p)
exists q. Prf_S(code(A), q)
```

The remaining goal is:

```text
exists s. Prf_S(code(B), s)
```

That is precisely the internally expressed proof-composition problem. “Apply the induction hypotheses” has not solved it.

A successful argument must either justify composition for the particular returned certificate class, retain a stronger invariant that makes composition possible, or take another route.

Here is a sharper conditional obstruction. Abbreviate:

```text
ProvS(A) = exists p. Prf_S(code(A), p)
ProvK(A) = exists r. K(code(A), r)
```

Suppose `S` proves these bridges uniformly over the relevant proof witnesses:

```text
up:
  ProvS(A) -> ProvK(A)

applyK:
  ProvK(A -> B) -> ProvK(A) -> ProvK(B)

lower:
  ProvK(B) -> ProvS(B)
```

Then `S` proves:

```text
ProvS(A -> B) -> ProvS(A) -> ProvS(B)
```

The derivation is simply:

```text
mpS f a =
  lower (applyK (up f) (up a))
```

Thus:

> Wherever the source lacks this internal composition principle, these three bridges cannot all be available.

This does **not** by itself prove that `lower` is impossible. `up` or `applyK` might fail to be internally provable, despite perfectly good external algorithms. Nor should one assume the same failure mechanism for every SJAS apparatus.

But this is a concrete test to perform before investing in a uniform lowering proof.

## 4. A useful intermediate target: lowering with a supplied bound

Separate two claims:

```text
B. Correctness with a sufficient supplied bound:

  forall x,r,b.
    K(x,r) and BudgetOK(x,r,b) ->
      exists p <= b. Prf_S(x,p)


T. Every accepted input has a sufficient bound:

  forall x,r.
    K(x,r) ->
      exists b. BudgetOK(x,r,b)
```

`B` and `T` imply full `lower`.

But they can differ sharply in proof-theoretic strength. If `K`, `BudgetOK`, and `Prf_S` are all expressible in the permitted bounded-formula class, then:

```text
B has a Pi_1-style form:
  universal quantifiers followed by a bounded formula.

T generally has a Pi_2-style form:
  universal quantifiers followed by an unbounded existence claim.
```

Here `b` bounds the **numerical proof code**, not merely its bit length. Replacing it with a length bound could require an exponentiation-totality assumption to recover a numerical bound.

This distinction gives a substantive restricted design:

```text
Accept only requests supplying:

  r                 a PTS certificate
  b                 an output-code bound
  evidence          BudgetOK(x,r,b)
```

If `B` is internally proved, this restricted checker has `lower` without proving `T`. The caller need not supply the original source proof itself.

`BudgetOK` must certify a genuine sufficient bound—for example, through a verified size/rank analysis—not rename the assertion that lowering succeeds. No such analysis has yet been supplied for our PTS.

The attraction is that bounded correctness lies in the kind of `Pi_1` knowledge that suitable Willard constructions can inherit from stronger theories. That does not mean our current `S` already proves this particular statement. [Willard 2011, Theorem 6.6](https://arxiv.org/pdf/1108.6330#page=23)

## What remains to be established

For the present construction, the concrete missing work is:

1. Fix the source configuration and a faithful, closed-judgment predicate `K`.
2. Give an explicit readback/translation with an internally usable correctness argument.
3. Establish its required output-existence bounds—or identify a restricted acceptance condition under which those bounds are available.
4. Test `up + applyK + lower` against the source’s unavailable composition principles.

One correction to my earlier wording: full `lower` is **sufficient**, not necessary, for transferring consistency. If the desired result is only “no conflicting accepted commands,” directly proving that prohibition for `K` may require less than translating every accepted theorem back into an `S`-proof.


## Follow-up request (verbatim)

Please log this, then complete the missing work.

The [resolution for a concrete Type-A source and PTS macro checker](2026-09-05-lower-resolution.md)
is recorded separately. It proves a paper-level obstruction to even lowering
at `0 = 1`, conditional on the cited Hilbert-incompleteness boundary. This
entry preserves the preceding analysis without retroactively changing its claims.
