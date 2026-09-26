# R4 draft — λᶜᵉʳᵗ, an affine certificate calculus

*Notes toward component R4, 2026-09-25. A proposal for a native, non-arithmetic
self-justifying calculus, built to test the **resource form** of the
self-justification criterion (`../LOG.md`, entries of 2026-09-24 and
2026-09-25).*

> **Status.** The proofs are in [`R4-metatheory.md`](R4-metatheory.md), for a
> core calculus λᶜᵉʳᵗ₀ with a complete rule table. They cover:
> - consistency, including the pair form `H₁`;
> - size soundness and termination;
> - the claims of §§4–5.
>
> Gödel's second theorem for codes is reduced there to the theorem for PA; two
> standard formalizations in that reduction are not carried out.
>
> Consistency is proved by a model, not by the four lemmas of §3. §3 is kept as
> the original plan. Where the proofs showed this draft to be wrong, it has been
> corrected, and each correction points to its proof.
>
> The core calculus is implemented as a small programming language, with an
> Ansatz-verified kernel, under
> [`ADR-0005-lcert-implementation.md`](ADR-0005-lcert-implementation.md), in
> `code/lcert` at the repository root. §2.7's example is its test oracle.
>
> Claims about Hofmann and Atkey rest on page images of the held witnesses,
> recorded in [`VERIFICATION.md`](VERIFICATION.md). Claims about Willard carry
> the registry's proof status at the point of use.
>
> **Notation hazard.** `◇` below is **Hofmann's resource type**, not the modal
> "possibly" of provability logic. Beklemishev–Shamkanov's `a = ◇!a` (§7) uses
> the modal `◇`. The two are unrelated.

---

## 0. The construction in one paragraph

Certificates are trees in a sort `R` whose every internal node costs one token
of Hofmann's resource type `◇`. `◇` has no closed terms and tokens are affine,
so a term can build at most as many certificate nodes as it holds tokens. The
calculus checks certificates symbolically, by a structurally recursive checker,
and has a constant `H` asserting that **no certificate checks as a refutation of
the calculus itself** — any refutation, with any number of tokens. Suppose some
term refutes the calculus using `n` tokens. It must hand `H` a certificate of a
refutation. That certificate has at most `n` nodes, yet it must declare every
token of the refutation it describes. So it describes a refutation using fewer
than `n` tokens, and a least counterexample cannot exist. No number is ever
computed, and nothing is larger than the tokens a program was given.

## 1. What the calculus is for

It tests three things.

1. **Whether the restriction can be confined to the certificate sort.** The
   ordinary layer has the strength of Heyting arithmetic in all finite types,
   with full induction. That is deliberate: if confining the discipline to `R`
   is unsound, this is where it should break.
2. **Whether R4's two transfer targets come apart.** The charter (ADR-0002, R4
   row, from ADR-0004) asks for a type theory in which the boxed diagonal
   `copy : □A → □A ⊗ □A` and uniform proof composition are exhibited
   *separately*. Here the boxed diagonal and D3 (`□A ⊸ □□A`) fail uniformly —
   no single budget serves every type — under every encoding that writes
   contexts out entry by entry. Each instance holds once enough tokens are
   supplied. Composition's cost, as a tree construction, depends on the
   encoding. It grows with the premises under the full-judgment encoding the
   draft uses, and could be constant only under a relative encoding that
   replaces the properties this one has. Inside the calculus, a composition
   with a budget uniform in the types is conjectured not to exist, because its
   evidence would need the checker to compute on open codes (§4).
   *An earlier state of this item said "the fourth derivability condition"; the
   condition meant is D3 in §4's table.*
3. **Whether a symbolic self-justifying calculus avoids infeasible numbers.**
   Willard's arithmetic must name proofs by numbers and cripple multiplication
   to keep short terms from naming huge ones. Here a certificate of size `N`
   costs exactly `N` tokens, so there is nothing to cripple (§6).

## 2. The calculus

### 2.0 Terminology: representation, code, certificate

- **A representation** of proofs is a sort of objects together with a checker
  that recognizes which objects stand for which proofs. It is the genus; the
  next two are species.
- **A code** is a representation in a *free* sort: ordinary data that the
  theory can build from nothing, copy, and compute over without restriction.
  Gödel numbers are the classical instance. Here, the values of the type `Syn`
  are codes.
- **A certificate** is a representation in a *resourced* sort. Here, the values
  of `R`: every internal node costs one token, and tokens are used at most once.

The two sorts represent the same derivations with the same tree shapes, and
`print : R ⊸ Syn` maps a certificate to the code of the same tree (§2.3). What
separates them is the operations the theory has on them:

| Operation | Codes (`Syn`) | Certificates (`R`) |
| --- | --- | --- |
| build from nothing | free | one token per node |
| copy | free | impossible — it would double the tokens |
| quote one inside another | free: a literal term | costs the certificate's size again, twice over (§4) |
| compose two into a third | free | tokens for the new root: constant under a relative encoding, growing with the premises under the full-judgment encoding (§4) |
| convert to the other sort | no parse into `R` | `print` |

**The distinction is relative to the theory, not intrinsic to the data.** The
same Gödel numbers are codes to PA. To Willard's `IS(A)` they play the role
certificates play here: the uniform operations Löb's derivation needs are not
available on them — composition, in his case (R6 §7.4). Willard has one sort,
so he weakens the whole arithmetic. λᶜᵉʳᵗ has both sorts, keeps the ordinary
layer at full strength, and confines the restriction to `R`.

*Naming note.* An earlier state of this draft called the free sort `Code`. It
is renamed `Syn` so that "code" is not used in two opposite senses: Willard's
Gödel numbers, which play the certificate role relative to `IS(A)`, and this
calculus's free syntax data.

### 2.1 Usages

Judgments carry usages in the style of quantitative type theory, drawn from
`{0, 1, ω}`:
- `0` — erased, usable only in types;
- `1` — at most once at runtime (the affine reading, as in Hofmann's calculus);
- `ω` — unrestricted.

Atkey (LICS 2018, p. 1) credits McBride's insight "to use the 0 of the semiring
to represent information that is erased at runtime, but is still available for
use in types". Whether Atkey's framework covers the affine reading of `1` as it
stands is **not checked here**. A term used at usage `ρ` needs its context
scaled by `ρ`. Passing an argument at `ω` therefore needs every variable it uses
at `ω`, and erased variables can never be used at runtime.

### 2.2 Types and terms

- **Base types.** `1`, `0`, `Bool`, `Nat`, and `Syn` (finite trees over a finite
  label set `L`, with constructors `sleaf : L → Syn` and
  `snode : L → Syn → Syn → Syn`, which take no `◇`).
- **Type formers.** Dependent `Π` and `Σ` with usage annotations; `⊗`; `&`.
- **Propositions.** The family `T : Bool → Type`, with `T(tt) ≡ 1` and
  `T(ff) ≡ 0`.
- **`abort_A : 0 → A`.**
- **Eliminators.** Dependent eliminators for every inductive type. Motives are
  at usage `0`. Methods are typed in `ω`-scaled contexts, because a method runs
  once per constructor. So **a method can capture no usage-1 variable**.

The layer without `◇` and `R` interprets Heyting arithmetic through `Π`, `Σ`,
`Nat`-induction and `T`.

### 2.3 Tokens and certificates

**`◇`** is Hofmann's resource type: "a special resource type ◇ which has no
constructors and hence no closed terms" (Hofmann 2003, p. 59). It has no
eliminator.

**`R`** is Hofmann's tree type (§4.3, pp. 65–66) over the passive label set `L`:
- `leaf : L ⊸ R`
- `node : ◇ ⊸ L ⊸ R ⊸ R ⊸ R`

Every internal node consumes one token. **`‖v‖` is the number of `node`s in a
canonical tree `v`.** Hofmann counts "the number of its nodes plus the sizes …
of all its labels", and the labels here are passive, of size 0.

**`R`'s eliminator** hands each destructed node's `◇` to the node method at
usage `1`. Since methods cannot capture tokens (§2.2), a method can rebuild at
most one node per node destroyed. This is the invariant Hofmann's operator
typing protects: "If h₀ or h₁ increase the size by a constant (as would be the
case if they were allowed to contain variables) then itᴺ(g,h₀,h₁) would multiply
the size by that constant, thus violating the intended invariant" (p. 63). His
operators take closed arguments ("an operator is applicable to closed terms
only", p. 61). Here usage scaling does the same job for tokens, and leaves
ordinary variables free.

**`print : R ⊸ Syn`** is defined by elimination: `node ↦ snode`, `leaf ↦
sleaf`, and each `◇` is dropped (affine weakening). It preserves shape, so the
number of internal nodes of `print v` is `‖v‖`. **There is no map in the other
direction.** A code carries no tokens, and a node needs one.

**Excluded, both for Hofmann's reasons.** Duplication at a non-passive type —
"If we would not require that P be passive then we could define a diagonal map
δ : D ⊸ D ⊗ D … We have already seen in the Introduction that this leads to
exponential growth" (p. 79) — and borrowing of `◇` (§9, "Borrowing does not
work", p. 82). Hofmann's restricted duplication with a passive result,
`dup_{D,P}` (§6, p. 78), may be added. It is not needed below.

### 2.4 The checker, and self-reference by name

**`chk′ : Syn → Syn → Bool`** is a primitive. For closed canonical codes `c`
and `d`, `chk′(c, d)` reduces to `Check(c, d)`, where `Check` is an external
algorithm.

**Explicit derivations.** An explicit derivation is a tree of rule instances.
Every node records its rule and its whole conclusion judgment: context, term,
type and usages. Every conversion is recorded as a chain of single reduction
steps. Derivations are encoded as codes over the finite label set `L`.

**`c⊥`** is a fixed closed code. `Check(c, c⊥) = tt` iff `c` encodes an
explicit derivation, **in λᶜᵉʳᵗ itself**, of a judgment
`x₁ :₁ ◇, …, x_m :₁ ◇ ⊢ t : 0` for some `m`.

**`Check` terminates.** It verifies each node locally, by structural recursion.
When a node records a step `chk′(c′, d′) ⇝ b`, `Check` calls itself on `c′`.
`c′` is written out, as a literal, inside the derivation being checked, so it
has fewer nodes than that derivation. (An earlier state said "a proper
subtree": the literal *encodes* `c′` and is at least as large; see
`R4-metatheory.md` §1.6.)

**Self-reference is by name.** `Check` checks against the calculus's finite
rule table. That table includes the rule for `H` below, whose type mentions the
constant `chk′`. The mutual reference is resolved in the metatheory's
definition of the calculus. No term of the calculus performs a diagonal
construction, and no term contains its own code.

**This differs from Willard.** His Group-3 axiom *is* a diagonal instance:
`Willard1993-TR` printed p. 38 takes `J` to be the Gödel number of the template
(A.1) and forms (A.2) through `SUBST_i(J, z)`. "The only integer z satisfying
SUBST_i(J,z)" is (A.2)'s own Gödel number (register row). Arithmetic has no
symbol meaning "provable in `IS(A)`", so the fixed point cannot be avoided
there. It is avoided here because `chk′` is a primitive. What the two share is
that neither uses a *uniform* diagonal operator. Willard proves only the fixed
instance `∃y SUBST_i(k̄, y)`, not `∀x ∃y SUBST_i(x, y)` (printed p. 37, register
row; R6 §7.1).

### 2.5 The self-consistency constant

> `H : Π(r :₁ R). Π(e :₁ T(chk′(print r, c⊥))). 0`

- The occurrence of `print r` in the type is at usage `0`, as all type-level
  occurrences are. At runtime, `H` consumes `r`.
- `H` has no reduction rule.
- **Both arguments are runtime.** An erased certificate could be built at type
  level from a single token, because erased variables may be used any number of
  times. An erased piece of evidence could hide a refutation from the argument
  of §3.

`H°` names the proposition `H`'s type asserts.

**The pair form: Willard's stronger consistency notion.** Willard's Level(1)
self-consistency axiom says that for no `Π*₁` sentence are there proofs of both
it and its negation (`Willard2002c` Theorem 2, `full`, for `IS-1(A)`;
`Willard2005`'s `IS_D(A)`, Eq. 6). Its counterpart here is a second constant:

> `H₁ : Π(r :₁ R). Π(s :₁ R). Π(c :ω Syn). T(chk′(print r, c)) ⊸ T(chk′(print s, neg c)) ⊸ 0`

(An earlier state wrote the evidence arrows as `→`. They are `⊸`, as in `H`:
evidence obtained by inspecting a certificate is at usage 1, and could not be
passed to an `ω`-argument; `R4-metatheory.md` §1.7.)

- `neg c := snode(arrow₁, c, ⌜0⌝)` is the code of the type `A ⊸ 0`, where `c`
  is the code of `A`.
- `H₁` says that no certificate proves a type while another proves its
  negation.
- Both certificates and both pieces of evidence are runtime arguments, for the
  reason given for `H`. `c` is ordinary data.
- `H₁` has no reduction rule.

**Negation must be the usage-1 arrow `A ⊸ 0`, not `A → 0`.** The consistency
argument (§3) applies the represented proof of `A ⊸ 0` to the represented proof
of `A`. That application consumes the latter's tokens, which an `ω`-arrow could
not accept.

**Scope, compared with Willard.** `H₁` ranges over *every* closed type. Willard
reaches only Level(1), sentences of class `Π*₁`. Level(2+) is closed for finite
systems with addition total: `Willard2004` Theorem 1 (`full`) — no such system
recognizes its own Level(2+) tableaux consistency. Here the argument of §3 does
not depend on the syntactic class of the sentence, only on the token budget.
Why Willard's higher levels fail and these do not is noted, not analyzed.

`H` follows from `H₁` inside the calculus, at a constant extra budget
(`R4-metatheory.md` Prop. 4.10):
- pass the refutation certificate to `H₁` as the certificate of the type `0`;
- pass a fixed certificate of `⊢ λ(x :₁ 0). x : 0 ⊸ 0` as the certificate of
  its negation, `neg ⌜0⌝ = ⌜0 ⊸ 0⌝`, whose evidence is `⋆`.

`H` is kept as a constant because it is available with no tokens. In the core
calculus it is the case `D = 0` of `reflect_D` (§8.2).

*Two earlier states:*
- The first said `H` follows "only at extra token cost". That was right in
  substance, but its method — rebuilding the refutation as a certificate of
  `1 ⊸ 0` — was the costly one.
- The second, written with the metatheory's first version, said `H` does not
  follow inside the calculus at all. That was wrong: no refutation certificate
  has to be transformed. Independent review found the construction (review
  R4-02, recorded in `R4-metatheory.md` §8).

### 2.6 Budgets

Write `Θₙ = x₁ :₁ ◇, …, xₙ :₁ ◇`. A **refutation with budget `n`** is a term `t`
with `Θₙ ⊢ t : 0`. A closed refutation has budget 0.

### 2.7 A worked example: `not`, its code, and its certificate

**The program.** In the ordinary layer:

> `not := λx:Bool. if x then ff else tt`, of type `Bool → Bool`.

`→` is `Π` at usage `ω`.

**The rules it uses.** The full rule table is fixed in `R4-metatheory.md` §1.4,
and this example uses only instances of its rules. When this example was
written, the table was not yet fixed. This is the fragment the example needs:

| Rule | Premises | Conclusion |
| --- | --- | --- |
| BoolF | — | `Γ ⊢ Bool type` |
| TT, FF | — | `Γ ⊢ tt : Bool`, `Γ ⊢ ff : Bool` |
| Var | — (the entry was checked when the context was extended) | `Γ, x :ω A, Γ′ ⊢ x : A` |
| Lam | `Γ ⊢ A type`, `Γ, x :ω A ⊢ b : B` | `Γ ⊢ λx:A. b : A → B` |
| If | `Γ ⊢ b : Bool`, `Γ ⊢ t : C`, `Γ ⊢ e : C` | `Γ ⊢ if b then t else e : C` |

`If` is taken as primitive here. It is the constant-motive case of `Bool`'s
dependent eliminator, and with only the eliminator, the certificate would also
contain the motive's formation derivation.

**The derivation.** Six rule instances:

```
D1  BoolF   ⊢ Bool type
D3  Var     x:ωBool ⊢ x : Bool
D4  FF      x:ωBool ⊢ ff : Bool
D5  TT      x:ωBool ⊢ tt : Bool
D2  If      x:ωBool ⊢ if x then ff else tt : Bool       from D3, D4, D5
D0  Lam     ⊢ λx:Bool. if x then ff else tt : Bool → Bool   from D1, D2
```

**One admissible encoding**, fixed for the example. It satisfies the
stipulation under A in §3: contexts are written entry by entry. Labels are
drawn from a finite set `L`, and variables are unary de Bruijn indices.

| Object | Encoding |
| --- | --- |
| rule instance `ρ` with judgment `J`, premises `D₁…D_k` | `node(ρ, J, list(D₁…D_k))`; `list` is a spine of `cons` nodes ending in `leaf nil` |
| `Γ ⊢ t : A` | `node(has, Γ, node(pair, t, A))` |
| `Γ ⊢ A type` | `node(isType, Γ, A)` |
| empty context; `Γ, x :ω A` | `leaf empty`; `node(ext_ω, Γ, A)` — one node per entry |
| `Bool`; `A → B` | `leaf bool`; `node(arrow, A, B)` |
| variable `i` | `node(var, i, leaf nil)`, with `0 = leaf zero`, `i+1 = node(succ, i, leaf nil)` |
| `tt`, `ff` | `leaf tt`, `leaf ff` |
| `λx:A. b` | `node(lam, A, b)` |
| `if b then t else e` | `node(if, b, node(branches, t, e))` |

Internal nodes per derivation node, computed mechanically: D1 = 2, D3 = 5,
D4 = 4, D5 = 4, D2 = 23, **D0 = 35**. The term `not` alone is 4 nodes; the rest
is the judgments, in particular the context repeated at every node.

**The code.** `c_not : Syn` is this tree built from `snode` and `sleaf`. It is
a closed ordinary term, costs nothing, and can be copied freely. Since
`chk′(c_not, ⌜Bool → Bool⌝)` reduces to `tt`, where
`⌜Bool → Bool⌝ = snode(arrow, sleaf bool, sleaf bool)`, the ordinary layer
proves

> `⊢ (c_not, ⋆) : Σ(c : Syn). T(chk′(c, ⌜Bool → Bool⌝))`

with no tokens: the instance of D1 for codes.

**The certificate.** `r_not : R` is the same tree built from `node` and `leaf`.
Its 35 internal nodes each consume a token, taken in preorder from `Θ₃₅`:

```
node x1 Lam
  node x2 has
    leaf empty
    node x3 pair
      node x4 lam
        leaf bool
        node x5 if
          node x6 var
            leaf zero
            leaf nil
          node x7 branches
            leaf ff
            leaf tt
      node x8 arrow
        leaf bool
        leaf bool
  node x9 cons
    node x10 BoolF
      node x11 isType
        leaf empty
        leaf bool
      leaf nil
    node x12 cons
      node x13 If
        node x14 has
          node x15 ext_ω
            leaf empty
            leaf bool
          node x16 pair
            node x17 if
              node x18 var
                leaf zero
                leaf nil
              node x19 branches
                leaf ff
                leaf tt
            leaf bool
        node x20 cons
          node x21 Var
            node x22 has
              node x23 ext_ω
                leaf empty
                leaf bool
              node x24 pair
                node x25 var
                  leaf zero
                  leaf nil
                leaf bool
            leaf nil
          node x26 cons
            node x27 FF
              node x28 has
                node x29 ext_ω
                  leaf empty
                  leaf bool
                node x30 pair
                  leaf ff
                  leaf bool
              leaf nil
            node x31 cons
              node x32 TT
                node x33 has
                  node x34 ext_ω
                    leaf empty
                    leaf bool
                  node x35 pair
                    leaf tt
                    leaf bool
                leaf nil
              leaf nil
      leaf nil
```

`node xk ℓ` abbreviates `node(xk, ℓ, …)`, whose two children follow indented.
So `‖r_not‖ = 35`, and

> `Θ₃₅ ⊢ (r_not, ⋆) : □(Bool → Bool)`

where `□A := Σ(r :₁ R). T(chk′(print r, ⌜A⌝))`. This is D1 for certificates,
with a budget of 35. `print r_not` reduces to `c_not`, so the evidence `⋆`
type-checks by the same computation as before.

**What the example shows.**
- *The program never sees the certificate.* `not` is ordinary; its certificate
  is a separate resource-layer value describing `not`'s typing derivation.
- *The code is free; the certificate is paid for.* 35 tokens, one per node.
  Hofmann reads `◇` as memory: "under the reading of ◇ as a certain amount of
  memory space proposed in [9]" (p. 82, register row). Under that reading,
  `r_not` occupies 35 cells, and the calculus never allocates a cell it was not
  given.
- *No parse.* A program holding `c_not` cannot turn it into `r_not` without
  being given 35 tokens. That is the missing direction of §5.
- *Size.* Explicit derivations record the whole context at every node, so a
  certificate grows with the number of rule instances times the context
  length. That is a cost, not a soundness issue. Only the budget enters the
  theorem of §3.

### 2.8 The deductive apparatus

λᶜᵉʳᵗ's apparatus is **natural deduction**, in the form of a type theory's
typing derivations. It is not analytic tableaux.

**The rules, read through Curry–Howard:**

| λᶜᵉʳᵗ rule | Natural deduction |
| --- | --- |
| Var | assumption |
| Lam | →-introduction, discharging an assumption |
| If | `Bool`-elimination: proof by cases |
| application | →-elimination: modus ponens |

**Three features of the presentation:**
- **Contexts are written out.** Every judgment carries its full context, so
  each derivation node can be checked locally.
- **Deduction is modulo computation.** A step may rely on a definitional
  equality, such as `T(tt) ≡ 1`, or `chk′(c, d) ⇝ b` computed by `Check`. The
  explicit derivation records each such step as a chain of single reductions.
- **Certificates encode derivations, not bare terms.** Checking a bare term
  would need type inference plus conversion checking by normalization, which is
  not structural recursion. Recording every judgment and conversion step keeps
  `Check` structural (§2.4).

**It is not cut-free.** `Check` accepts derivations with detours: a λ applied to
an argument, i.e. an introduction immediately followed by an elimination, which
Curry–Howard identifies with a cut. Composition by substitution is also
available.

`Willard2016` Definition 3.2 calls an apparatus Hilbert-style if it has a modus
ponens rule and satisfies Gödel's completeness theorem. The completeness clause
does not transfer to an intuitionistic type theory; by the modus ponens clause,
λᶜᵉʳᵗ sits on the **Hilbert side** of Willard's apparatus axis.

On Willard's Hilbert line, self-justification was reached only at Type-NS, with
not even successor total (codified §5.5). λᶜᵉʳᵗ keeps strong arithmetic in its
ordinary layer: a cell Willard's matrix does not contain. This does not
contradict Pudlák–Solovay. Their theorem concerns consistency stated over
numbers, and λᶜᵉʳᵗ's code consistency `Con′` is unprovable (§5).

**Who does the tableaux' two jobs.** R6 §7.4 found that in Willard, cut-freeness
does two jobs. λᶜᵉʳᵗ assigns them differently:

| Job | Willard (tableaux) | λᶜᵉʳᵗ (natural deduction) |
| --- | --- | --- |
| block the operations Löb's derivation needs | no internal composition: the second derivability condition fails | composition is harmless, since combining terms makes no certificate without tokens. What is blocked is quoting and copying certificates, by the token discipline (§4) |
| force a refutation to name its witness | the subformula property: the refutation must build `p*` as a node (`Willard1993-TR` Lemma 6.2, `full`) | no witness needs naming. The metatheory's model gives every certificate reaching `H` a footprint bounded by the tokens supplied (`R4-metatheory.md` §3). The plan of §3, normalization plus canonicity, was not needed |

The second row follows Willard's own Meta-Logic convention: "our proofs shall
*apply a cut rule at the meta-theoretical level*" (`Willard1993-TR` Remark 2,
printed p. 24; register row). Here, too, the work is done in the consistency
proof, never by the calculus. It is done by a model, not by normalization, the
natural-deduction form of cut elimination. *An earlier state of this row and
paragraph named normalization and exhibition, the plan of §3.*

Composition can stay because the danger Willard avoids by dropping cut is
absent. In arithmetic, linear-sum composition lets a short proof name a huge
number: the danger is composed proofs being too *short* (R6 §7.4, on
`Willard2020` printed p. 279; register row). Here, compressing a term never
compresses a certificate: a certificate of `N` nodes costs `N` tokens, however
it was computed.

**A tableau-faithful variant — an open alternative.** Restrict `Check` to
*normal* derivations, i.e. β-normal terms. Normal natural-deduction proofs have
the subformula property (Prawitz; standard, not verified here), so they are the
natural-deduction counterpart of cut-free tableaux. Three consequences:
- the consistency argument of §3 is unchanged, since it never needs the
  represented refutation to be normal;
- `H°` becomes weaker;
- composition fails uniformly, since composing normal derivations creates
  redexes, and eliminating them can blow up non-elementarily.

The variant would reproduce Willard's tableaux line inside a type theory. It is
the comparison case for showing where λᶜᵉʳᵗ departs from Willard.

**Relation to the constructive design of 2026-09-06.** That note, in the working
tree's docs log, paired a constructive derivation with a Willard source tableau
inside each certificate, so its consistency was inherited from Willard's
theorem. λᶜᵉʳᵗ drops the tableau. Its consistency comes from its own argument
alone (`R4-metatheory.md` §3).

## 3. The lemmas, and the theorem they would give

> **Superseded as a proof route.** `R4-metatheory.md` proves the theorem of
> this section for a core calculus without L1–L4. It uses a set-theoretic model
> in which values carry token footprints, capped at a budget `n`, and proves the
> model sound by strong induction on `n` (its §3). Of the lemmas:
> - L4 survives as size soundness (its T3);
> - L1 survives as termination of the evaluator (its T4), which the language
>   needs but the consistency proof does not;
> - L2 and L3 have no role.
>
> The step this section's descent uses — a certificate of `n` nodes declares
> fewer than `n` tokens — is the model's key step too. This section is kept as
> the original plan.

**L1 — Normalization.** Every term well-typed in a token context is strongly
normalizing. `H` is inert, and `chk′` reduces only on closed canonical codes.
*Plan:* reducibility candidates. The `◇`-free layer is a standard dependent type
theory with one large elimination (`T`). `R`'s eliminator is structural, and
`Check` is total.

**L2 — Subject reduction with usages.** Reduction preserves typing and usage
annotations. *Plan:* the standard argument. Atkey (p. 1) records that his
system fixes a bug in McBride's original, "that caused substitution to be
inadmissible", so this lemma is where care is needed.

**L3 — Canonicity in token contexts.** Let `Θₙ ⊢ v : A` be normal.
- If `A` is `Bool`, `Nat`, `Syn`, `1` or `R`, then `v` is in constructor form,
  or it contains an `H`-application at usage 1.
- The only normal terms of type `◇` are the token variables, and
  `abort_◇(u)` with `u` of type `0`.
- If `A = 0`, then `v` contains an `H`-application at usage 1.

**L4 — Affine mass (from L2 and L3).** Let `Θₙ ⊢ t : A` be normal. Let `r` be a
subterm of `t` at usage 1 that is a canonical tree containing no
`H`-application. Then `‖r‖ ≤ n`. *Argument:* each node of `r` takes its `◇`
argument at usage 1. By L3, and because `r` contains no `H`, that argument is a
token variable, not an `abort`. By affinity, distinct nodes use distinct tokens.

**A — Adequacy with strict overhead.** If `Check(print r, c⊥) = tt`, then
`print r` encodes an explicit derivation of `Θ_m ⊢ t′ : 0`, with `m < ‖r‖`. The
root judgment's context alone needs at least `m` internal nodes, because the
label set is finite, and the root needs one more. This is an external property
of `Check` and of the encoding.

**A depends on a stipulation about the encoding.** Contexts must be written out
entry by entry, with no sharing and no repetition counts. A "repeat this entry
`m` times" node, with `m` written in binary, would let `O(log m)` nodes declare
`m` tokens, and the descent in the theorem would fail. Sharing elsewhere — in
the represented term, say — is harmless, because only the token budget enters
the argument. The stipulation plays the role of `Willard2011` Definition
D.1(iv)'s requirement of at least `5J` bits for a tableau proof with `J`
function symbols (register row). Willard's own `u#`/`v#` constant pointers
(`Willard1993-TR` printed p. 15) are sharing of exactly the kind this
calculus may allow in the term and must forbid in the budget.

**Exhibition.** Let `t` be a normal refutation with budget `n`. **The
certificate `t` exhibits** is an `H`-application `H r e` inside `t` with two
properties: `r` contains no `H`-application, and `chk′(print r, c⊥)` reduces to
`tt`. It exists, and is found by descending through `t`:
1. By L3, `t` contains an `H`-application `H r e`.
2. If `r` contains an `H`-application, descend into it.
3. Otherwise `r` is a canonical tree, and `chk′(print r, c⊥)` computes a closed
   Boolean.
   - If it is `tt`, stop: `r` is the exhibited certificate.
   - If it is `ff`, then `e : T(ff) ≡ 0` is itself a normal refutation at usage
     1. Descend into `e`.
4. The descent is well-founded on subterms.

So exhibition is not an operation performed *by* `t`. It is a function on
normal refutations, defined by this descent. It is the counterpart of
`Willard1993-TR` Lemma 6.2 (`full`): an inconsistency proof must "formally
construct" a witness `p*` with `¬Prf(⊥, p*)` as one of its nodes. There, the
witness may be a tableau parameter. Here, canonicity makes it a value.

> **Theorem (conditional on L1–L4 and A).** For no `n` is there a refutation
> with budget `n`.
>
> *Proof.* Suppose otherwise, and let `n` be least. Take a refutation with
> budget `n` and normalize it (L1, L2). Let `r` be the certificate it exhibits.
> By L4, `‖r‖ ≤ n`. By A, `print r` encodes a derivation of `Θ_m ⊢ t′ : 0` with
> `m < ‖r‖ ≤ n`. So `t′` is a refutation of λᶜᵉʳᵗ with budget `m < n`,
> contradicting the choice of `n`. ∎

The case `n = 0` shows how the argument works. A closed refutation could only
exhibit a certificate with no nodes, a single leaf, and no derivation is a
leaf.

**The pair form `H₁`.** Exhibition extends to `H₁`: descend to an innermost
application `H₁ r s c e₁ e₂` whose certificates contain no `H` or
`H₁`-application and whose two checks both compute to `tt`. (If either check
computes to `ff`, the corresponding evidence has type `T(ff) ≡ 0` and the descent
continues into it.) Then:
- `print r` encodes a derivation of `Θ_{m₁} ⊢ t₁ : A`, with `m₁ < ‖r‖`. `A` is
  closed, because `Check` requires the certified type to be closed;
- `print s` encodes one of `Θ_{m₂} ⊢ t₂ : A ⊸ 0`, with `m₂ < ‖s‖`;
- renaming the tokens of `t₂` apart from those of `t₁`, the application
  `t₂ t₁` is a refutation with budget `m₁ + m₂`;
- `r` and `s` are both runtime arguments of one term, so they use disjoint
  tokens, and `‖r‖ + ‖s‖ ≤ n`;
- hence `m₁ + m₂ < n`, contradicting minimality again.

The renaming and context-addition steps are the usual weakening lemmas for
usage-annotated contexts. They are proved in
[`R4-metatheory.md`](R4-metatheory.md), which also replaces L1–L4 by a proof
that needs none of them (§3 there).

**Corollary.** λᶜᵉʳᵗ is consistent, and contains closed inhabitants, `H` and
`H₁`, of propositions asserting its own consistency. That is the two clauses of
`Willard2016` Definition 3.4, read for this calculus's apparatus and
certificate representation. The corollary is conditional here, and proved for
the core calculus in `R4-metatheory.md` (T1, T2).

## 4. What survives of the derivability conditions

Write `□A := Σ(r :₁ R). T(chk′(print r, ⌜A⌝))`, where `⌜A⌝` is `A`'s code.

| | In λᶜᵉʳᵗ | Why |
| --- | --- | --- |
`μ(A)` below is the size of the smallest certificate of `A`. The proofs are in
`R4-metatheory.md` §4.

| | In λᶜᵉʳᵗ | Why |
| --- | --- | --- |
| D1 | **with a budget** | for a certificate `v` of `A`, `Θ_{‖v‖} ⊢ (lit_v, ⋆) : □A`, where `lit_v` builds `v`'s tree from the tokens (Prop. 4.2) |
| D2 | **conjectured not derivable inside the calculus; as a tree construction, encoding-dependent** | a uniform `comp : □(A ⊸ B) ⊗ □A ⊸ □B` needs evidence that the composed tree checks. `chk′` computes only on closed codes, so no hypothesis about the premises yields that evidence (Conj. 4.6: an analysis, not a proof). As a tree construction, verified outside the calculus, `comp` adds a root node for the application. Under §2.4's encoding every node records its whole conclusion. So the new root repeats the combined context and both premises' conclusion terms and types, every premise node is re-recorded with the combined context, and the budget grows with the premises. Under no encoding with the metatheory's properties E2 and E3 does one new node suffice: the new root must record its whole conclusion, including an explicit context of `m₁ + m₂` entries. A *relative* encoding — conclusions reconstructed by `Check` from the premises, contexts split between premises, each token declared once where it is used — would have to replace E2 and E3, and re-prove strict overhead and the quotation-cost bound. None is specified. The conjecture concerns a budget uniform in `A` and `B`; per instance, a certificate of `B` can be built from scratch. *An earlier state said one node suffices under a relative encoding, as if such an encoding could keep E2–E3; review R4-06* |
| D3 `□A ⊸ □□A` | **per instance yes; uniformly no** | every certificate of `□A` has more than `2μ(A)` nodes. Its root must declare at least `μ(A)` tokens, and its term must mention each of them (Prop. 4.3). From a minimal certificate and `k` tokens, at most `μ(A) + k` nodes can be built, so no `k` serves every `A` (Prop. 4.4). For each `A`, an instance exists with budget `μ(□A)` (Prop. 4.5) |
| boxed contraction `□A ⊸ □A ⊗ □A` | **per instance yes; uniformly no** | two certificates of `A` need `2μ(A)` nodes, and only `μ(A) + k` can be built: Hofmann's diagonal map (p. 79) recast (Prop. 4.4). For each `A`, an instance exists with budget `2μ(A)`: discard the input and build two certificates (Prop. 4.5) |
| self-reference | **by name** | §2.4 |
| G2 for codes | **applies** | no budget derives any form of `Con′` (§5). Proved by reducing it to Gödel's second theorem for PA, with two standard formalizations not carried out (`R4-metatheory.md` §6) |

*Corrections.* An earlier state of this table wrote `□(A → B)` in the D2 row.
With `→` the argument's certificate may use no tokens, so the row now reads
`⊸`. The same state presented `comp` as a term of the calculus. It argued D3
and contraction from the claim that a certificate of `□A` "must record the
literal term that rebuilds `r`". It need not: it may build any certificate of
`A`. The conclusions survive in the uniform reading, by the argument now in the
D3 row.

The split between "with a budget" and "uniformly" is Willard's
instance-versus-uniform split, recast. Every instance is available once enough
tokens are supplied. The uniform versions of D3 and contraction need a budget
proportional to the certificate, and no term has one.

**These two failures hold under every encoding** that writes contexts out entry
by entry and spends at least one node per variable occurrence
(`R4-metatheory.md` E3, E4). A certificate of `□A` must declare the tokens that
build its certificate of `A`, one node each, and its term must mention each of
them.

**D2 is different, in two ways.**
- *As a tree construction,* it could have constant cost only under a relative
  encoding that replaces E2 and E3, and none is specified.
- *Inside the calculus,* no uniform `comp` is expected under any encoding, for
  the evidence reason in its row.

So the separation R4's charter asks for — composition kept, boxed diagonal lost
— can be exhibited at most at the level of certificate trees, with correctness
proved outside the calculus, and only once a relative encoding is chosen (§7).
Under the current full-judgment encoding, composition too needs a budget that
grows with its inputs, and λᶜᵉʳᵗ breaks all three uniform conditions.

It also answers obligation **RO1** (affineness at the object level does not evade
G2: B–S's `□`-contraction can hold in affine PA). The affinity here is on the
*certificate sort*, not the object logic. `□`-contraction fails for a
quantitative reason — node count — not a structural one.

## 5. Two consistency statements, and the gap between them

- `H° = Π(r :₁ R). T(chk′(print r, c⊥)) ⊸ 0` — certificate consistency.
- `Con′ = Π(c :₁ Syn). T(chk′(c, c⊥)) ⊸ 0` — code consistency. The usage-1
  quantifier makes it the stronger form, since it implies the usage-`ω` version.

(An earlier state wrote both evidence arrows as `→`. They are `⊸`, matching
`H` in §2.5. The underivability in fact 4 holds for every variant, the weakest
included, which has `ω` throughout: `R4-metatheory.md` §6.)

Four facts.

1. **Externally, the two are equivalent,** and both are equivalent to the
   consistency of λᶜᵉʳᵗ: `print` is shape-preserving, and every code of a
   refutation is `print` of some tree.
2. **The calculus proves `Con′ ⊸ H°`:** `λf r e. f (print r) e`.
3. **The calculus proves `H°`,** as the axiom `H`.
4. **It does not prove `Con′`, nor `H° → Con′`, with any budget** — by G2 for
   codes (§4, last row), given 3. The missing direction would need a map from
   `Syn` into `R` whose output prints back to its input. Building an output of
   `N` nodes needs `N` tokens, and no closed map yields more than a leaf
   (`R4-metatheory.md` Prop. 4.1).
   - The restricted form is provable, for codes of depth at most `k`, and needs
     no tokens at all. It is a case analysis whose every case is a closed code
     that `chk′` computes to be no refutation (Prop. 4.9). Its size grows
     doubly exponentially in `k`.
   - *An earlier state proposed "parse and then apply `H`", using `k` tokens.*
     The parse itself is definable: the core has a computational destructor
     for `R`, built from `itR` and dependent pairs (`R4-metatheory.md` §4.7).
     What the route lacks is an internal proof that the parse prints back to
     its input. *A correction made on 2026-09-25 said the destructor was
     lacking; review R4-03 showed otherwise.*

So `H°` is **equivalent to consistency but internally weaker than its ordinary
formalization**, and every bounded instance of the stronger statement is
available. This is the status Willard's systems have: they prove their
tableaux-consistency axiom but cannot verify their Hilbert consistency, and
revising Group-3 to Hilbert proofs makes `IS-1(A)` inconsistent
(`Willard2002c` Remark 3, `cited`). The analogy is structural: certificates are
to codes as cut-free tableaux are to Hilbert proofs — the representation that
cannot be compressed, against the one that can. It is the substance of Willard's
own objection to himself (`Willard2016` §8, statement ###; register row). His
reply rests on `Willard2016` Corollary 8.2 (`full`), which depends on Theorem 6.7
and so on Conjecture 6.6 (`stated-only`).

## 6. Why no large numbers appear

In Willard's arithmetic a proof is a number, and a short term can denote a huge
number. Repeated squaring fixes `v_n = 2^{2^n}` in a sentence of `O(n)` symbols
(`Willard2007-APAL` Definition 6), and the systems with total multiplication
prove it in `O(n^d)` steps (`Willard2007-APAL` Lemma 2, `full`; refined-sjas
§4). Self-justification therefore needs a language
too weak to write the compressing operations. That is the growth restriction,
and every analysis of it runs through numbers like `2^{2^n}`.

In λᶜᵉʳᵗ a certificate is a tree, and **the only way to hold a certificate of
`N` nodes at runtime is to have spent `N` tokens.** "At runtime" means outside
erased positions, which an erasing evaluator never builds.
- Under the erasing evaluator, this is `R4-metatheory.md` Theorem 4′, a sketch
  of a standard argument.
- A non-erasing evaluator would build, in an erased position, an `N`-node tree
  from one token. The model allows that, since erased values are unbounded
  (review R4-04).

There is no compressed naming to forbid, so nothing else needs to be
forbidden:
- ordinary arithmetic stays strong;
- certificates compose linearly;
- the checker is symbolic.

Willard's margin — "it is impossible in log₂p − 1 bits to encode a number as
large as p" (`Willard1993-TR` printed p. 13, register row) — becomes the
statement that a certificate cannot declare more tokens than it cost.

## 7. Open decisions, and where an attack would come from

- **The full rule table.** Fixed for the core calculus λᶜᵉʳᵗ₀ in
  `R4-metatheory.md` §1. It contains §2.7's fragment. It omits `&`, which no
  claim uses. *An earlier state said the table was not yet fixed.*
- **The apparatus.** Natural deduction with detours, as now, or normal
  derivations only, the tableau-faithful variant (§2.8).
- **The encoding of derivations.** Full judgments at every node (§2.4, §2.7),
  or a relative encoding in which `Check` reconstructs conclusions.
  - The choice decides the tree cost of composition (§4).
  - A relative encoding must still declare the budget entry by entry, and must
    keep the properties the consistency proof uses (`R4-metatheory.md` E1–E5
    and Lemma 2.7).
  - Even then, composition inside the calculus is conjectured to fail for want
    of evidence (§4, D2 row).
- **The likeliest attack** was the interaction of usage `0` with dependency.
  Erased terms may use tokens without limit, and types compute. The theorem
  needs every certificate and every piece of evidence that `H` consumes to be
  at usage 1 (§2.5).
  - *Now answered:* the metatheory's model lets erased values be unbounded and
    never needs a bound on them (`R4-metatheory.md` §3.4).
  - This is also why `reflect` has no conversion rule.
- **Second:** eliminator methods and `ω`-scaling. A rule that let a method
  capture a usage-1 variable would let iteration copy tokens. *Now answered:*
  the model's `ItR` and `RecSyn` cases (`R4-metatheory.md` §3.5).
- **Third:** any definable map from `Syn` into `R`, or any closed inhabitant of
  `◇` other than `abort_◇`. *Now answered:* size soundness excludes both
  (`R4-metatheory.md` T3, Prop. 4.1).
- **Identity types** are deliberately absent. Transport is harmless, but
  equality reflection would not be.
- **`chk′` as a primitive, or defined inside the calculus.** A primitive keeps
  self-reference by name. A definition would need the checker's own code inside
  it, reintroducing a diagonal.
- **A reduction-rule form of `H` — not equivalent, and not adopted.** Instead
  of the constant, one could add `chk′(print r, c⊥) ⇝ ff`. An earlier state of
  this item called that "equivalent to `H`" given `T`; the claim is withdrawn.
  A conversion rule also fires on *erased* occurrences of `r`, and erased
  certificates may be built at type level from a single token, so they have
  no size bound. The budget argument therefore does not cover the rule, which
  is strictly stronger than `H`. The "certified constant-result optimization"
  of the 2026-09-22 note is available instead as a *runtime* use of `H`.
- **The pair form** is now `H₁` (§2.5, §3).
- **Relation to Beklemishev–Shamkanov.** Their Theorem 5 gives cut
  admissibility with a linear size bound (register row, printed p. 11). Their §6
  conjectures a counterexample to non-formalized G2 from an operator like `!`
  and a fixed point `a = ◇!a`, with the modal `◇` (register row, printed p. 14).
  λᶜᵉʳᵗ is a different route to the same target: a constant over a
  resource-sorted certificate type, rather than a fixed point in an extended
  logic.

## 8. The language this is a draft for

*A feature inventory, not a specification. "Has" means present in this draft.
"Can have" means no conflict with §3's argument is visible, under the stated
condition. "Cannot have" gives the reason.*

**In one paragraph.** A small, total, dependently typed functional language
without universes, in which programs are proofs. Its own proof checker is a
built-in, certificates of proofs are linear resources paid for with tokens, and
programs can rely on the language's own consistency. Holding a certificate is
like holding money: it has a price, it cannot be copied, and spending it
consumes it.

### 8.1 Features it has

- **A total functional core.** Dependent functions and pairs; `Bool`, `Nat` and
  `Syn`; recursion at all finite types through eliminators, at System T
  strength. Every program terminates (`R4-metatheory.md` T4).
- **Proofs as programs.** Each Boolean program `b` gives a proposition `T(b)`.
  Quantifiers are `Π` and `Σ`, and induction is available: the strength of
  Heyting arithmetic in all finite types. Types are erased at runtime, and
  evidence for `T(b)` is trivial.
- **Syntax as free data, with its own checker.** Programs build, copy, inspect
  and boundedly search `Syn` codes. The primitive `chk′` decides whether a code
  is a derivation of the language itself. A proof represented as a code costs
  nothing.
- **Certificates as resources.** Values of `R` cost one token per node. Tokens
  come only from the program's context — a budget supplied from outside —
  certificates cannot be copied, and `print` turns them into codes.
  Certificate transformations that never enlarge their input are definable.
- **Budgeted certification.**
  - A program holding tokens can turn a code it knows into a certificate, at
    one token per node (35 for `not`, §2.7).
  - It can compose certificate trees by modus ponens, at the encoding-dependent
    cost of §4. The composite's correctness is checked at runtime by `inspect`,
    since the calculus is not expected to prove it (§4, D2 row).
  - It can parse a code received at runtime into a certificate, taking tokens
    one at a time from a supply tree through a destructor definable in the core
    (`R4-metatheory.md` §4.7). That the result prints back to the code is
    verified outside the calculus, not inside it.

  *An earlier state said "turn a code into a certificate" and "compose
  certificates" without these qualifications.*
- **Built-in self-consistency.** `H`: a certificate that checks as a refutation
  yields anything, so the branch in which a certificate checks as a refutation
  is dead code. `H₁`: no certificate proves a type while another proves its
  negation. Exploiting either needs inspection without consumption, so that a
  certificate survives its own check. The core provides this as `inspect`
  (`R4-metatheory.md` §1.4). The reduction-rule form of `H` is not a
  substitute: it is stronger than `H` and not covered by the budget argument
  (§7).

### 8.2 Features it can have

- **Ordinary conveniences.** More data types, records, and pattern matching
  compiled to eliminators, provided termination (T4) survives. Identity types,
  without equality reflection.
- **A primitive destructor for `R`, with conversion rules**, splitting a node
  into its token, label and children at usage 1.
  - A *computational* destructor is already definable (`R4-metatheory.md`
    §4.7).
  - A primitive one would add judgmental equations. Those are what an internal
    proof of parsing correctness would need.
  - The model's case for it is immediate, since the parts' footprints sum to
    the whole's.
- **Universes — unification in the manner of a pure type system.** This needs a
  consistent sort structure, primitive `R`, and explicit conversions. The cost
  is certificate size: explicit derivations record type-level computation,
  which can be non-elementary in length (`../LOG.md`, 2026-09-25).
- **Classical reasoning, through a negative translation**, as the 2026-09-05
  note did for Willard's classical theory. Not as an axiom that fails to
  compute (8.3).
- **Hofmann's resource programming in general.**
  - Token-costed lists and trees, provided a node takes its children
    separately (multiplicatively), as `R` and Hofmann's `T(A)` do — not as a
    shared pair (8.3).
  - In-place update, under his reading of `◇` as memory: p. 82 cites his
    reference [9] for that reading, a paper on bounded space and functional
    in-place update.
  - Inspection without consumption: his `dup_{D,P}` into a passive result (§6,
    p. 78), and the conditional rule it justifies, which lets the guard and the
    branches share variables of datatype (p. 79). The core has one instance of
    it as a primitive, `inspect`.
  - A polynomial-time certificate fragment: if certificate transformers are
    restricted to Hofmann's own rules, his polynomial-time theorem
    (Corollary 5.5.1, p. 78) should transfer to them. **Unverified.**
- **The tableau-faithful variant**, accepting only normal derivations (§2.8).
  Willard's stronger consistency notion, formerly listed here, is now in the
  calculus as `H₁` (§2.5).
- **Runtime-supplied budgets.** `main` runs in `Θₙ`, with `n` tokens from the
  environment: a certification allowance, like a memory limit.
- **Certified self-evaluation — proved for base data types, as an evaluation
  rule; open for higher types and as a conversion rule.**
  - *The feature:* for each closed type `A`, a constant
    `reflect_A : Π(r :₁ R). T(chk′(print r, ⌜A⌝)) ⊸ A` with a reduction rule
    that *runs* the term the certificate encodes. The term's `m` token
    variables are instantiated with `m` of the certificate's own tokens, and
    the rest are discarded. `H` is the case `A = 0`.
  - *What is proved* (`R4-metatheory.md` §§3.5, 4.8, 5):
    - The core has `reflect_D` for the base data types `0, 1, Bool, Nat, Lbl,
      Syn, R`.
    - It computes in the evaluator only, not as a conversion. A conversion
      would also fire on erased certificates, which have no size bound.
    - It is sound, by the model's Reflect case.
    - It terminates, by the evaluator's induction on the budget.
  - *Why it may stay consistent* (the original argument, now superseded by
    the model's): in a least-budget refutation, replacing an innermost
    `reflect` by the term its certificate encodes gives a refutation of budget
    at most `n − ‖r‖ + m < n`, since `m < ‖r‖` by adequacy.
  - *Why it may terminate* (likewise superseded): each `reflect` step strictly
    lowers the number of tokens a program holds, and nothing creates tokens,
    so a reduction contains at most `n` such steps.
  - *What it would give:* a total self-interpreter for *certified* code. Code
    received at runtime as data can be converted to a certificate (paying
    tokens), checked, and run. The diagonal argument against total
    self-interpreters does not apply, because a program evaluating itself would
    need a certificate larger than its own budget: self-application priced out
    rather than forbidden.
  - *The caveat:* in Willard's arithmetic some local reflection is provable
    (`Willard1993-TR` Proposition 2, `full`), but local `Π₁` reflection makes
    the system inconsistent for some nice `A` (Proposition 5, `full`). The
    proved case does not contradict this: Willard's proofs are numbers, named
    compactly, while certificates here declare their budget uncompressed.
    Whether his counterexample transcribes into this calculus is still
    **unchecked** (`R4-metatheory.md` §4.8).

### 8.3 Features it cannot have

**These would make it inconsistent:**
- **General recursion**, or any non-termination inside the logic: `fix x. x : 0`
  would prove falsity. A partiality monad kept as a separate type is a
  different matter — plausible, unverified.
- **`Type : Type`**, by Girard's paradox (standard).
- **Any source of tokens besides the budget:** a closed token, duplicating
  tokens, or Hofmann's borrowing (§9 of his paper). Certificates could then
  outgrow their budget. With duplication, one token becomes unboundedly many,
  and by the route of the next item, a refutation of budget 1 follows.
- **Converting codes into certificates for free.** Then `Con′` is provable as
  `λc e. H (parse c) e`, and the second incompleteness theorem for codes (§4)
  makes the language inconsistent.
- **Copying or quoting certificates for free.** These are the operations Löb's
  derivation needs, and the ordinary layer supplies the fixed point by
  substitution on codes, so `H` would yield falsity. Standard reasoning, not
  checked in detail.
- **An axiom asserting its own consistency over codes** — Löb again.

**These would break the consistency proof** (not known to be inconsistent, but
no longer covered):
- excluded middle at `◇` with affine negation, `◇ + (◇ ⊸ 0)`. Once `n ≥ 1`,
  the model gives neither disjunct footprint 0. With `◇ → 0` as the negation,
  the model validates it vacuously, since no footprint-0 value has type `◇`
  (review F-05);
- equality reflection, or anything else letting erased terms reach runtime;
- certificates that declare their budget in compressed form, which breaks
  adequacy (strict overhead, `R4-metatheory.md` Lemma 2.7);
- a certificate type whose `node` takes its two children as a shared
  (cartesian) pair. Hofmann's variant `T′(A)` does this, and there "l_e is an
  upper bound on the depth of t rather than its number of nodes"; a full binary
  tree of depth `|n|` costs only `|n|` tokens (p. 84, register row). Size
  soundness, the former L4, would fail;
- implicit conversions for `chk′` steps, which make the checker's definition
  circular.

**Excluded middle at `◇`- and `R`-free types** would *not* break the
consistency proof, once sums are added: the model is classical and validates
it. It would break termination (T4), since the evaluator cannot decide it. *An
earlier state listed excluded middle, unqualified, as breaking "canonicity and
with it exhibition" — true of the plan of §3, not of the proof that replaced
it.*

**These are impossible whatever the design** (standard results):
- a total interpreter for all of its own *uncertified* code, by the diagonal
  argument — certified, budgeted evaluation is the item of 8.2, proved for base
  data types;
- a proof of its own consistency over codes, by the second incompleteness
  theorem.

### 8.4 What programming in it would be like

- **Certificates behave like money.** Using a lemma certificate twice means
  holding, and paying for, two copies. Certifying code costs tokens in
  proportion to its derivation, which can be large, since derivations repeat
  contexts and record computations.
- **Certification is opt-in.** Programs that never handle certificates pay
  nothing, and run as ordinary total programs.
- **There is no global polynomial-time guarantee, by design.** The ordinary
  layer computes everything System T does.

## 9. Sources and verification

- **Hofmann**, "Linear types and non-size-increasing polynomial time
  computation", Information and Computation 183(1) (2003) 57–85, DOI
  10.1016/S0890-5401(03)00009-9. Witness held:
  [`lit/hofmann2003_linear_types_non_size_increasing_ic183.pdf`](lit/hofmann2003_linear_types_non_size_increasing_ic183.pdf),
  hash in `lit/SHA256SUMS`. Text layer PDF pp. 1–29; page images of printed
  pp. 57, 59, 60, 61, 62, 63, 65, 66, 78, 79, 82 and 84. The calculus uses only
  the size discipline. Hofmann's polynomial-time theorem (Corollary 5.5.1)
  enters only as the unverified transfer conjectured in §8.2.
- **Atkey**, "Syntax and Semantics of Quantitative Type Theory", LICS 2018, DOI
  10.1145/3209108.3209189. Witness held:
  [`lit/atkey2018_syntax_semantics_quantitative_type_theory_lics.pdf`](lit/atkey2018_syntax_semantics_quantitative_type_theory_lics.pdf).
  Text layer read at the introduction; page image of p. 1 only. The body of
  the paper has not been read, and nothing here rests on it.
- **Not held.** Hofmann's LICS 1999 preliminary version; his POPL 2002 "The
  strength of non size-increasing computation"; McBride's 2016 system that QTT
  reformulates. Nothing rests on them.
- **Willard.** `Willard1993-TR` printed pp. 13, 37 and 38 and Lemma 6.2
  (`full`); `Willard2002c` Remark 3 (`cited`); `Willard2007-APAL` Lemma 2
  (`full`); `Willard2016` Definition 3.4, §8, and Corollary 8.2 (`full`,
  conditional as stated in §5).
