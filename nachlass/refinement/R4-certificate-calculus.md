# R4 draft — λᶜᵉʳᵗ, an affine certificate calculus

*Notes toward component R4, 2026-09-25. A proposal for a native, non-arithmetic
self-justifying calculus, built to test the **resource form** of the
self-justification criterion (`../LOG.md`, entries of 2026-09-24 and
2026-09-25).*

> **Status.** Nothing here is proved. §3 states four lemmas and derives the
> consistency theorem **from** them; the lemmas are open. It is a design, not
> an implementation, and no code exists. Claims about Hofmann and Atkey rest on
> page images of the held witnesses, recorded in
> [`VERIFICATION.md`](VERIFICATION.md). Claims about Willard carry the
> registry's proof status at the point of use.
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
   *separately*. Here the boxed diagonal and the fourth derivability condition
   fail under every encoding of derivations. Composition's cost depends on the
   encoding: constant under a relative encoding, growing with the premises
   under the full-judgment encoding the draft currently uses (§4).
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
When a node records a step `chk′(c′, d′) ⇝ b`, `Check` calls itself on `c′`,
which is written out inside the derivation being checked and so is a proper
subtree.

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

### 2.6 Budgets

Write `Θₙ = x₁ :₁ ◇, …, xₙ :₁ ◇`. A **refutation with budget `n`** is a term `t`
with `Θₙ ⊢ t : 0`. A closed refutation has budget 0.

### 2.7 A worked example: `not`, its code, and its certificate

**The program.** In the ordinary layer:

> `not := λx:Bool. if x then ff else tt`, of type `Bool → Bool`.

`→` is `Π` at usage `ω`.

**The rules it uses.** The full rule table is not yet fixed (§7). This is the
fragment the example needs:

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
| force a refutation to name its witness | the subformula property: the refutation must build `p*` as a node (`Willard1993-TR` Lemma 6.2, `full`) | the metatheory normalizes the refutation, and canonicity of normal forms plays the subformula property's role (exhibition, §3) |

The second row is Willard's own Meta-Logic convention, applied systematically:
"our proofs shall *apply a cut rule at the meta-theoretical level*"
(`Willard1993-TR` Remark 2, printed p. 24; register row). Here, too,
normalization — the natural-deduction form of cut elimination — is carried out
in the consistency proof, never by the calculus.

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
theorem. λᶜᵉʳᵗ drops the tableau. Its consistency is meant to come from §3's
argument alone.

## 3. The lemmas, and the theorem they would give

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

**Corollary (conditional).** λᶜᵉʳᵗ is consistent, and contains a closed
inhabitant, `H`, of a proposition asserting its own consistency. That is the two
clauses of `Willard2016` Definition 3.4, read for this calculus's apparatus and
certificate representation.

## 4. What survives of the derivability conditions

Write `□A := Σ(r :₁ R). T(chk′(print r, ⌜A⌝))`, where `⌜A⌝` is `A`'s code.

| | In λᶜᵉʳᵗ | Why |
| --- | --- | --- |
| D1 | **with a budget** | for a derivation `d` of `⊢ u : A`, `Θ_{‖d‖} ⊢ (lit_d, ⋆) : □A`, where `lit_d` builds `d`'s tree from the tokens |
| D2 | **encoding-dependent** | `comp : □(A → B) ⊗ □A ⊸ □B` adds a root node for the application. Under §2.4's encoding every node records its whole conclusion, so the new root repeats both premises' conclusion terms and types, and the budget grows with them. Under a *relative* encoding — conclusions reconstructed by `Check` from the premises, contexts split between premises, variables scoped to their own subderivation — one new node suffices, a constant budget |
| D3 | **no, for any fixed budget** | a certificate for `□A` built from `r` must record the literal term that rebuilds `r`, together with its `‖r‖` tokens, so it has more than `2‖r‖` nodes. From `r` and a fixed `k` tokens, at most `‖r‖ + k` nodes can be built |
| boxed contraction `□A ⊸ □A ⊗ □A` | **no, for any fixed budget** | it doubles the nodes: Hofmann's diagonal map (p. 79) |
| self-reference | **by name** | §2.4 |
| G2 for codes | **applies** | the `◇`-free layer interprets Heyting arithmetic, and `chk′` is a primitive recursive checker, so the calculus cannot prove its code consistency (§5). *Standard; not verified here* |

The split between "with a budget" and "for any fixed budget" is Willard's
instance-versus-uniform split, recast. Every instance is available once enough
tokens are supplied. The uniform versions of D3 and contraction need a budget
proportional to the certificate, and no term has one.

**These two failures hold under every encoding.** A certificate for `□A` built
from `r` must contain the literal rebuilding `r`: one constructor application,
hence at least one derivation node, per node of `r`. It must also declare the
literal's `‖r‖` tokens, one node each (the stipulation under A). Copying `r`
yields `2‖r‖` nodes outright.

**D2 is different:** its uniform version holds at constant cost only under a
relative encoding. So the separation R4's charter asks for — composition kept,
boxed diagonal lost — is exhibited only once a relative encoding is chosen
(§7). Under the current full-judgment encoding, composition too needs a budget
that grows with its inputs, and λᶜᵉʳᵗ breaks all three uniform conditions.

It also answers obligation **RO1** (affineness at the object level does not evade
G2: B–S's `□`-contraction can hold in affine PA). The affinity here is on the
*certificate sort*, not the object logic. `□`-contraction fails for a
quantitative reason — node count — not a structural one.

## 5. Two consistency statements, and the gap between them

- `H° = Π(r :₁ R). T(chk′(print r, c⊥)) → 0` — certificate consistency.
- `Con′ = Π(c :₁ Syn). T(chk′(c, c⊥)) → 0` — code consistency. The usage-1
  quantifier makes it the stronger form, since it implies the usage-`ω` version.

Four facts.

1. **Externally, the two are equivalent,** and both are equivalent to the
   consistency of λᶜᵉʳᵗ: `print` is shape-preserving, and every code of a
   refutation is `print` of some tree.
2. **The calculus proves `Con′ → H°`:** `λf r e. f (print r) e`.
3. **The calculus proves `H°`,** as the axiom `H`.
4. **It does not prove `Con′`, nor `H° → Con′`** — by G2 for codes (§4, last
   row), given 3. The missing direction would need a map from `Syn` into `R`
   whose output prints back to its input, and building an output of `N` nodes
   needs `N` tokens. With `k` tokens the restricted form is provable: for codes
   of at most `k` nodes, parse and then apply `H`.

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
`N` nodes at runtime is to have spent `N` tokens.** There is no compressed
naming to forbid, so nothing else needs to be forbidden:
- ordinary arithmetic stays strong;
- certificates compose linearly;
- the checker is symbolic.

Willard's margin — "it is impossible in log₂p − 1 bits to encode a number as
large as p" (`Willard1993-TR` printed p. 13, register row) — becomes the
statement that a certificate cannot declare more tokens than it cost.

## 7. Open decisions, and where an attack would come from

- **The full rule table is not yet fixed.** §2.7 fixes only the fragment its
  example needs. Fixing the rest comes before any of L1–L4 can be proved.
- **The apparatus.** Natural deduction with detours, as now, or normal
  derivations only, the tableau-faithful variant (§2.8).
- **The encoding of derivations.** Full judgments at every node (§2.4, §2.7),
  or a relative encoding in which `Check` reconstructs conclusions. The choice
  decides whether composition is constant-cost, and so whether R4's separation
  is exhibited (§4). A relative encoding must still declare the budget entry by
  entry.
- **The likeliest attack** is the interaction of usage `0` with dependency.
  Erased terms may use tokens without limit, and types compute. The theorem
  needs every certificate and every piece of evidence that `H` consumes to be
  at usage 1 (§2.5); L2 must guarantee that no reduction moves an erased term
  into a runtime position.
- **Second:** eliminator methods and `ω`-scaling. A rule that let a method
  capture a usage-1 variable would let iteration copy tokens.
- **Third:** any definable map from `Syn` into `R`, or any closed inhabitant of
  `◇` other than `abort_◇`.
- **Identity types** are deliberately absent. Transport is harmless, but
  equality reflection would not be.
- **`chk′` as a primitive, or defined inside the calculus.** A primitive keeps
  self-reference by name. A definition would need the checker's own code inside
  it, reintroducing a diagonal.
- **A reduction-rule form of `H`.** Instead of the constant, add
  `chk′(print r, c⊥) ⇝ ff`. Given `T`, this is equivalent to `H`, and connects
  to the "certified constant-result optimization" of the 2026-09-22 note. Not
  adopted, since it complicates L1.
- **The Level(1) pair form**
  `Π(r s :₁ R)(c :₁ Syn). T(chk′(print r, c)) → T(chk′(print s, neg c)) → 0`
  is left for later.
- **Relation to Beklemishev–Shamkanov.** Their Theorem 5 gives cut
  admissibility with a linear size bound (register row, printed p. 11). Their §6
  conjectures a counterexample to non-formalized G2 from an operator like `!`
  and a fixed point `a = ◇!a`, with the modal `◇` (register row, printed p. 14).
  λᶜᵉʳᵗ is a different route to the same target: a constant over a
  resource-sorted certificate type, rather than a fixed point in an extended
  logic.

## 8. Sources and verification

- **Hofmann**, "Linear types and non-size-increasing polynomial time
  computation", Information and Computation 183(1) (2003) 57–85, DOI
  10.1016/S0890-5401(03)00009-9. Witness held:
  [`lit/hofmann2003_linear_types_non_size_increasing_ic183.pdf`](lit/hofmann2003_linear_types_non_size_increasing_ic183.pdf),
  hash in `lit/SHA256SUMS`. Text layer PDF pp. 1–29; page images of printed
  pp. 57, 59, 60, 61, 62, 63, 65, 66, 78, 79 and 82. Only the size discipline
  is used here, not Hofmann's polynomial-time theorem (Corollary 5.5.1).
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
