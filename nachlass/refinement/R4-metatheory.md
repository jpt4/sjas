# R4 metatheory — the core calculus λᶜᵉʳᵗ₀: rules, consistency, termination

*Proofs for the draft [`R4-certificate-calculus.md`](R4-certificate-calculus.md),
2026-09-25. It fixes a core calculus, λᶜᵉʳᵗ₀, with a complete rule table, and
proves what the draft left open. The central result is a consistency proof that
needs neither normalization nor canonicity. It uses a set-theoretic model with
footprints, capped at a token budget `n`, and proves the model sound by strong
induction on `n`. Where the draft turned out to be wrong, §9 lists the
correction and the section that proves it.*

> **Status.** These are paper proofs. The inequality the consistency argument
> turns on (Lemma 2.7, strict overhead) is meant to be machine-checked in
> Ansatz as well (§7).
> - Results taken from the literature are marked *standard*, and are not
>   verified here.
> - P5 (§6) cites three standard results: Gödel's second theorem for PA, PA's
>   Σ₁-completeness, and the conservativity of E-PA^ω over PA. Its two
>   formalizations inside arithmetic are given at the level of their
>   obligations.
> - One claim is a conjecture (§4.5).
> - Everything else is proved here.
>
> Independent adversarial review is recorded in §8. Round 1 found no refutation
> of T1–T3. It raised twelve findings, all accepted. Two of them withdraw
> corrections this document had itself introduced into the draft. Round 2
> again found no refutation of T1–T3. Its thirteen findings led to P5 being
> relabelled an outline, and to a typed parser. P5 was then completed (§6).
> Round 3 reviewed the completion and found it a proof. Its eleven findings
> are minor, and all were accepted. Round 4 reviewed Theorem 5.2, which
> proves what had been an expectation. It found the argument sound and the
> write-up incomplete; its findings, two major and three minor, are fixed.

---

## 0. Results

| # | Statement | Where |
| --- | --- | --- |
| **T1** | **Consistency.** For no `n` is there a derivable `Θₙ ⊢ t :¹ 0` | §3.8 |
| **T2** | **Self-justification.** λᶜᵉʳᵗ₀ has closed inhabitants of its own consistency propositions `H°` and `H₁°`. With T1, both clauses of `Willard2016` Definition 3.4 hold, for this calculus's apparatus and certificate representation | §3.9 |
| **T3** | **Size soundness.** A runtime term's value has footprint at most the tokens its context supplies, and at most the number of distinct tokens it mentions | §3.7 |
| **T4** | **Termination and adequacy.** The budgeted evaluator terminates on every term and computes the model's value. An erasing evaluator, which the resource reading needs, agrees with it on data. Neither evaluates `abort`, `H₁` or `H` in a typed program (Theorem 5.2) | §5 |
| **P5** | **The second incompleteness theorem applies to codes.** λᶜᵉʳᵗ₀ derives no form of `Con′`, with any budget | §6; by reduction to Gödel's theorem for PA. It cites three standard results (§6.1). The templates of its step 1 are also tested mechanically, on random instances |
| **§4** | The draft's derivability-condition table, settled: D1 per instance; D3 and boxed contraction per instance only; a uniform D2 conjectured not derivable; `Con′ → H°`; `H°` from `H₁` at constant budget; bounded `Con′`; certified evaluation for base data types | §4 |

The draft's lemmas L1–L4 are **not needed for T1**:
- L1 (normalization) survives only as T4, which the programming language
  needs.
- L4 (affine mass) becomes T3.
- L2 and L3 have no role.

## 1. The core calculus λᶜᵉʳᵗ₀

### 1.1 Usages and contexts

Usages are `0` (erased), `1` (at most once at runtime) and `ω` (unrestricted),
with

```
0 + ρ = ρ     1 + 1 = ω     ω + ρ = ω
0 · ρ = 0     1 · ρ = ρ     ω · ω = ω
```

Both operations are commutative, so also `ω · 0 = 0` and `ω · 1 = ω`:
scaling a usage-1 entry by `ω` makes it `ω`.

A context is `Γ = x₁ :ρ₁ A₁, …, xₖ :ρₖ Aₖ`. For contexts with the same
variables and types, `Γ₁ + Γ₂` adds usages pointwise and `ρΓ` scales them.

There are two typing modes, following Atkey's `σ ∈ {0, 1}`:
- `Γ ⊢ t :¹ A` — **runtime**, usages enforced;
- `Γ ⊢ t :⁰ A` — **type level**, usages ignored.

Types are formed at type level, `Γ ⊢ A type`. In type-level judgments, the
usages recorded in the context carry no meaning.

`Θₘ` denotes `x₁ :₁ ◇, …, xₘ :₁ ◇`. A **refutation with budget `m`** is a
derivable `Θₘ ⊢ t :¹ 0`.

### 1.2 Types

```
A, B ::= 0 | 1 | Bool | Nat | Lbl | Syn | ◇ | R | T(b)
       | Π(x :ρ A). B | Σ(x :ρ A). B              (ρ ∈ {0, 1, ω})
```

- `A ⊸ B := Π(x :₁ A). B`, `A → B := Π(x :ω A). B`, and
  `A ⊗ B := Σ(x :₁ A). B`, each with `x` not free in `B`.
- `Lbl` is a fixed finite label set `L`.
- `Syn` holds finite trees over `L`: the codes.
- `R` holds the same trees with a token at every internal node: the
  certificates.
- **Base data types** are `0, 1, Bool, Nat, Lbl, Syn` and `R`.

### 1.3 Terms

Terms are Church-style: binders carry their types.

```
t ::= x | ⋆ | abort_A t
    | tt | ff | if t then t else t | elimBool_{x.P}(t, t, t)
    | zero | succ t | recN_{x.P}(t, x y. t, t)
    | ℓ (ℓ ∈ L) | caseLbl_{x.P}(t, (t_ℓ)_{ℓ ∈ L})
    | sleaf t | snode t t t | recSyn_{x.P}(a. t, a c₁ c₂ y₁ y₂. t, t)
    | leaf t | node t t t t | itR_X(t, t, t) | print t
    | λ(x :ρ A). t | t t | (t, t)_{Σ(x:ρA).B} | let_C (x, y) = t in t
    | chk′ t t | H₁ t t t t t | reflect_D t t | inspect_X t t (x e. t) (x e. t)
```

- `H := reflect₀`, that is `reflect_D` at `D = 0`.
- `node d a r₁ r₂` takes its token `d` first.
- `if` is the non-dependent `Bool` eliminator the draft's §2.7 takes as
  primitive, and `elimBool` is the dependent one.
- Derived operations:
  - `not b := if b then ff else tt`;
  - `b₁ && b₂ := if b₁ then b₂ else ff`;
  - `c⊥ := ⌜0⌝` and `neg c := snode arrow₁ c ⌜0⌝`, the codes of `0` and of
    `A ⊸ 0` when `c = ⌜A⌝` (§1.6).
- `print` is definable as an `itR`. It is primitive here, so that types mention
  it directly.

### 1.4 Typing rules

Axioms accept any context, which is the affine reading: unused variables are
discarded. In the rules below:
- `σ(ρ)` is `0` if `ρ = 0` and `1` otherwise;
- a premise at `σ(ρ) = 0` is typed in the conclusion's context, usages
  ignored;
- `P`, `C` and `X` are types.

**Axioms.**

| Rule | Conclusion |
| --- | --- |
| Var | `Γ, x :ρ A, Γ′ ⊢ x :¹ A`, for `ρ ∈ {1, ω}` |
| Unit, TT, FF, Zero | `Γ ⊢ ⋆ :¹ 1`; `Γ ⊢ tt :¹ Bool`; `Γ ⊢ ff :¹ Bool`; `Γ ⊢ zero :¹ Nat` |
| Lbl | `Γ ⊢ ℓ :¹ Lbl` |

**Functions and pairs.**

| Rule | Premises | Conclusion |
| --- | --- | --- |
| Lam | `Γ ⊢ A type`; `Γ, x :ρ A ⊢ t :¹ B` | `Γ ⊢ λ(x :ρ A). t :¹ Π(x :ρ A). B` |
| App | `Γ₁ ⊢ f :¹ Π(x :ρ A). B`; `Γ₂ ⊢ u :^σ(ρ) A` | `Γ₁ + ρΓ₂ ⊢ f u :¹ B[u/x]` |
| Pair | `Γ ⊢ Σ(x :ρ A). B type`; `Γ₁ ⊢ a :^σ(ρ) A`; `Γ₂ ⊢ b :¹ B[a/x]` | `ρΓ₁ + Γ₂ ⊢ (a, b) :¹ Σ(x :ρ A). B` |
| Let | `Γ₁ ⊢ p :¹ Σ(x :ρ A). B`; `Γ₂ ⊢ C type`; `Γ₂, x :ρ A, y :₁ B ⊢ t :¹ C` | `Γ₁ + Γ₂ ⊢ let (x, y) = p in t :¹ C` |
| Abort | `Γ ⊢ t :¹ 0`; `Γ ⊢ A type` | `Γ ⊢ abort_A t :¹ A` |
| Conv | `Γ ⊢ t :¹ A`; `Γ ⊢ B type`; `A ≡ B` | `Γ ⊢ t :¹ B` |

**Data.**

| Rule | Premises | Conclusion |
| --- | --- | --- |
| If | `Γ₁ ⊢ b :¹ Bool`; `Γ₂ ⊢ t :¹ C`; `Γ₂ ⊢ e :¹ C` | `Γ₁ + Γ₂ ⊢ if b then t else e :¹ C` |
| ElimBool | `Γ₁ ⊢ b :¹ Bool`; `Γ, x :₀ Bool ⊢ P type`; `Γ₂ ⊢ t₁ :¹ P[tt/x]`; `Γ₂ ⊢ t₂ :¹ P[ff/x]` | `Γ₁ + Γ₂ ⊢ elimBool_{x.P}(b, t₁, t₂) :¹ P[b/x]` |
| Succ | `Γ ⊢ n :¹ Nat` | `Γ ⊢ succ n :¹ Nat` |
| RecN | `Γ₁ ⊢ n :¹ Nat`; `Γ, x :₀ Nat ⊢ P type`; `Γ₂ ⊢ z :¹ P[zero/x]`; `ωΓ₃, x :ω Nat, y :₁ P ⊢ s :¹ P[succ x/x]` | `Γ₁ + Γ₂ + ωΓ₃ ⊢ recN_{x.P}(z, x y. s, n) :¹ P[n/x]` |
| CaseLbl | `Γ₁ ⊢ a :¹ Lbl`; `Γ, x :₀ Lbl ⊢ P type`; `Γ₂ ⊢ t_ℓ :¹ P[ℓ/x]` for each `ℓ ∈ L` | `Γ₁ + Γ₂ ⊢ caseLbl_{x.P}(a, (t_ℓ)) :¹ P[a/x]` |
| SLeaf | `Γ ⊢ a :¹ Lbl` | `Γ ⊢ sleaf a :¹ Syn` |
| SNode | `Γ₁ ⊢ a :¹ Lbl`; `Γ₂ ⊢ c₁ :¹ Syn`; `Γ₃ ⊢ c₂ :¹ Syn` | `Γ₁ + Γ₂ + Γ₃ ⊢ snode a c₁ c₂ :¹ Syn` |
| RecSyn | `Γ₁ ⊢ c :¹ Syn`; `Γ, x :₀ Syn ⊢ P type`; `ωΓ₂, a :ω Lbl ⊢ t_l :¹ P[sleaf a/x]`; `ωΓ₃, a :ω Lbl, c₁ :ω Syn, c₂ :ω Syn, y₁ :₁ P[c₁/x], y₂ :₁ P[c₂/x] ⊢ t_n :¹ P[snode a c₁ c₂/x]` | `Γ₁ + ωΓ₂ + ωΓ₃ ⊢ recSyn_{x.P}(a. t_l, a c₁ c₂ y₁ y₂. t_n, c) :¹ P[c/x]` |

**Tokens and certificates.** `◇` has no introduction rule and no eliminator.

| Rule | Premises | Conclusion |
| --- | --- | --- |
| Leaf | `Γ ⊢ a :¹ Lbl` | `Γ ⊢ leaf a :¹ R` |
| Node | `Γ₁ ⊢ d :¹ ◇`; `Γ₂ ⊢ a :¹ Lbl`; `Γ₃ ⊢ r₁ :¹ R`; `Γ₄ ⊢ r₂ :¹ R` | `Γ₁ + Γ₂ + Γ₃ + Γ₄ ⊢ node d a r₁ r₂ :¹ R` |
| ItR | `Γ ⊢ X type`; `ωΓ₁ ⊢ g :¹ Lbl → X`; `ωΓ₂ ⊢ h :¹ ◇ ⊸ Lbl → X ⊸ X ⊸ X`; `Γ₃ ⊢ r :¹ R` | `ωΓ₁ + ωΓ₂ + Γ₃ ⊢ itR_X(g, h, r) :¹ X` |
| Print | `Γ ⊢ r :¹ R` | `Γ ⊢ print r :¹ Syn` |

**The checker, and the self-reference constants.**

| Rule | Premises | Conclusion |
| --- | --- | --- |
| Chk | `Γ₁ ⊢ c :¹ Syn`; `Γ₂ ⊢ d :¹ Syn` | `Γ₁ + Γ₂ ⊢ chk′ c d :¹ Bool` |
| H₁ | `Γ₁ ⊢ r :¹ R`; `Γ₂ ⊢ s :¹ R`; `ωΓ₃ ⊢ c :¹ Syn`; `Γ₄ ⊢ e₁ :¹ T(chk′ (print r) c)`; `Γ₅ ⊢ e₂ :¹ T(chk′ (print s) (neg c))` | `Γ₁ + Γ₂ + ωΓ₃ + Γ₄ + Γ₅ ⊢ H₁ r s c e₁ e₂ :¹ 0` |
| Reflect | `D` a base data type; `Γ₁ ⊢ r :¹ R`; `Γ₂ ⊢ e :¹ T(chk′ (print r) ⌜D⌝)` | `Γ₁ + Γ₂ ⊢ reflect_D r e :¹ D` |
| Inspect | `Γ₁ ⊢ r :¹ R`; `ωΓ₀ ⊢ c :¹ Syn`; `Γ ⊢ X type`; `Γ₂, x :₁ R, e :₁ T(chk′ (print x) c) ⊢ t₁ :¹ X`; `Γ₂, x :₁ R, e :₁ T(not (chk′ (print x) c)) ⊢ t₂ :¹ X` | `Γ₁ + ωΓ₀ + Γ₂ ⊢ inspect_X r c (x e. t₁) (x e. t₂) :¹ X` |

**Type formation.**
- `Γ ⊢ A type` for every base type and `◇`;
- `Γ ⊢ T(b) type` if `Γ ⊢ b :⁰ Bool`;
- `Γ ⊢ Π(x :ρ A). B type` and `Γ ⊢ Σ(x :ρ A). B type` if `Γ ⊢ A type` and
  `Γ, x :ρ A ⊢ B type`.

**Type level.** Every runtime rule has a twin at `σ = 0`:
- all term premises are at `σ = 0`;
- all premises carry the conclusion's context;
- `Var⁰` also allows `ρ = 0`.

**The context convention** (review F-04):
- All contexts displayed in one rule share one underlying telescope — the same
  variables with the same types — and differ only in usages. Binder premises
  extend it.
- Formation premises and type-level premises are recorded in the *all-zero*
  version of the conclusion's context, written `0Γ`. The unannotated `Γ` of a
  formation premise in the tables means that version.
- So a type-level derivation records usage `0` everywhere.

Three remarks:
- **Subusaging is admissible.** Raising a usage in the conclusion can be
  pushed into a premise that is not `ω`-scaled; every rule has one, or is an
  axiom.
- **`inspect` is the draft's "inspection without consumption".** It consumes
  `r` and hands it back, as `x`, to whichever branch runs.
- **§2.7's derivation of `not` uses only instances of these rules**: `Var` at
  `ρ = ω`, `TT`, `FF`, `If` with all usages `ω`, `Lam` at `ρ = ω`, and the
  formation rule for `Bool`.

### 1.5 Conversion

`≡` is the equivalence generated, on terms and on types, by the compatible
closure of the steps below, taken only between **skeleton-typed** expressions
(Lemma 2.5): every intermediate expression of a conversion must be simply typed
at the skeleton of its endpoints. Without that restriction,
`T(tt) ≡ T((λ(z :₁ Nat). tt) ff) ≡ T(tt)` would pass through an expression the
model cannot interpret (review R4-01). The steps are:

| Kind | Step |
| --- | --- |
| β | `(λ(x :ρ A). t) u ⇝ t[u/x]`; `let (x, y) = (a, b) in t ⇝ t[a/x, b/y]` |
| ι | the computation rules of `if`, `elimBool`, `recN`, `caseLbl`, `recSyn`, `itR` and `print` on constructor forms. For example `itR_X(g, h, node d a r₁ r₂) ⇝ h d a itR_X(g, h, r₁) itR_X(g, h, r₂)` and `print (node d a r₁ r₂) ⇝ snode a (print r₁) (print r₂)` |
| δ | `chk′ c d ⇝ Check(c, d)`, when `c` and `d` are closed canonical codes: terms built from `sleaf`, `snode` and label constants only |
| T | `T(tt) ≡ 1` and `T(ff) ≡ 0` |

**`H₁`, `reflect` and `inspect` have no conversion rule.** The reason differs
by constant:
- *`reflect`:* a conversion rule would also fire at type level, where
  certificates are unbounded. The model's cap (§3.2) could not follow it
  there. This is the draft's §7 point about the reduction-rule form of `H`.
- *`inspect`:* its rule is left out only to keep `Check` small.
- *`H₁`:* it has nothing to compute.

`reflect` and `inspect` compute in the evaluator (§5).

### 1.6 Explicit derivations, the encoding, and `Check`

**An explicit derivation** is a finite tree of rule instances. Each node
records its rule and its whole conclusion judgment: mode, context with usages,
term and type. Each `Conv` node records its chain as a list of types,
`A = A₀, A₁, …, A_j = B`:
- consecutive types differ by one step of §1.5, taken in either direction;
- every `Aᵢ` is skeleton-well-formed in the skeleton of the node's context.

**The encoding.** `⌜·⌝` maps derivations, judgments, contexts, terms and types
to codes over `L`:
- each syntactic constructor of arity at least 1 is an internal node labelled
  with its name, with nesting for arities above 2;
- nullary constructors are leaves;
- variables are unary de Bruijn indices;
- runtime and type-level judgments carry distinct labels;
- the draft's §2.7 table is an instance, whose label `arrow` is `arrow_ω`.

The encoding has these properties:

| | Property |
| --- | --- |
| **E1** | Injective, and decodable by structural recursion |
| **E2** | A derivation node is an internal node whose label is its rule and whose first child encodes its conclusion |
| **E3** | A judgment's encoding contains the encodings of its context and of its term as disjoint subtrees. A context of `m` entries has at least `m` internal nodes: no repetition counts, no sharing |
| **E4** | A term's encoding has at least one internal node per occurrence of a variable, and per constructor application of arity at least 1. So a closed canonical code `c′`, written as a term, encodes to at least `nodes(c′)` internal nodes |
| **E5** | `⌜Π(x :₁ A). 0⌝ = snode arrow₁ ⌜A⌝ ⌜0⌝` for closed `A`, so `neg ⌜A⌝ = ⌜A ⊸ 0⌝` |

**`Check(c, d)` is `tt` iff:**
1. `c` decodes to an explicit derivation all of whose nodes are valid
   instances of §§1.4–1.5;
2. its root judgment is `Θₘ ⊢ t :¹ A` for some `m ≥ 0`;
3. `A` is closed;
4. `⌜A⌝ = d`.

**Validity is decidable.** At a `Conv` node, a δ-step `chk′ c′ d′ ⇝ b` is
valid iff `b = Check(c′, d′)`. By E4, the literal `c′` lies inside `c`'s
encoding, so `nodes(c′) < nodes(c)`. Every other local test is decidable by
structural recursion: syntactic comparisons, and simple-type checking of chain
elements. **So `Check` is defined by recursion on `nodes(c)`. It is total and
primitive recursive.**

The typing relation is well defined: `Γ ⊢ t :σ A` holds iff a valid explicit
derivation of it exists. `≡` refers to `Check` only through single steps on
smaller codes, so nothing is circular. No term contains its own code: the
self-reference is by name, through the primitive `chk′` (draft §2.4).

`budget(c)` is the number of context entries of the root judgment, and
`nodes(c)` the number of internal nodes of `c`.

### 1.7 Relation to the draft's calculus

- **Omitted.** The draft lists `&` among the type formers, and no claim uses
  it. The model would take `Vₖ(A & B) = Vₖ(A) × Vₖ(B)`.
- **Evidence arrows.** The draft writes the evidence arrows of `H₁`, and of
  `H°` and `Con′` in its §5, as `→`. They are `⊸` here, as in the draft's own
  `H`. The evidence `inspect` provides is at usage 1, and could not be passed
  to an `ω`-argument. §4.6 and §6 show the choice changes nothing else.
- **Motives.** The draft puts motives "at usage 0". Here they are types formed
  at type level, which is the same thing.

## 2. Syntactic lemmas

**Lemma 2.1 (weakening and exchange).** Derivability is preserved by adding a
context entry at usage `0` anywhere, shifting indices, and by permuting
independent entries. *Proof:* induction on derivations. Context operations
are pointwise. ∎

**Lemma 2.2 (renaming).** Derivability is preserved by injective renaming of
variables. *Proof:* induction on derivations. ∎

**Lemma 2.3 (strengthening).** If `Γ, x :ρ A, Γ′ ⊢ t :¹ B` and `x ∉ FV(t)`,
then `Γ, x :₀ A, Γ′ ⊢ t :¹ B`.

*Proof.* Set `x`'s usage to `0` in every judgment of the derivation. No `Var`
instance is for `x`, because `x ∉ FV(t)`. The axioms accept any usages. Sums
and scalings of `0` are `0`. Type-level premises ignore usages. ∎

**Lemma 2.4 (composition).** Suppose `Θₘ₁ ⊢ t₁ :¹ A` and
`Θ′ₘ₂ ⊢ t₂ :¹ A ⊸ 0`, with `A` closed and the tokens of `Θ′` renamed apart.
Then `Θₘ₁, Θ′ₘ₂ ⊢ t₂ t₁ :¹ 0`.

*Proof.*
1. By Lemma 2.1, weaken the first judgment to `Θₘ₁, 0Θ′ₘ₂` and the second to
   `0Θₘ₁, Θ′ₘ₂`.
2. Apply App at `ρ = 1`. The contexts add to `Θₘ₁, Θ′ₘ₂`.
3. The result type is `0[t₁/x] = 0`. ∎

**Lemma 2.5 (skeletons).** Define `skel`:
- `skel(0) = skel(1) = skel(T(b)) = Unit`;
- `skel(Π(x :ρ A). B) = skel(A) → skel(B)`;
- `skel(Σ(x :ρ A). B) = skel(A) × skel(B)`;
- every other base type, and `◇`, is its own skeleton.

Two facts:
- If `A ≡ B` then `skel(A) = skel(B)`. Steps change only terms inside
  `T(·)`, and `T(tt) ≡ 1`, `T(ff) ≡ 0` relate types with skeleton `Unit`.
- If `Γ ⊢ t :σ A`, then `t` is simply typed at `skel(A)` in `skel(Γ)`.

*Proof:* the first as stated, the second by induction on derivations. Church
annotations fix every binder's skeleton. ∎

**Lemma 2.6 (Check is sound).** If `Check(c, d) = tt`, then the decoded root
judgment `Θₘ ⊢ t :¹ A` is derivable, `A` is closed, and `⌜A⌝ = d`. *Proof:*
the definition, and E1. ∎

**Lemma 2.7 (strict overhead).** If `Check(c, d) = tt`, then
`budget(c) < nodes(c)`.

*Proof.* By E2, `c` is an internal node whose first child `j` encodes the root
judgment. By E3, `j` contains a context encoding with at least `budget(c)`
internal nodes. So `nodes(c) ≥ 1 + budget(c)`. ∎

**Lemma 2.8 (print).** On closed canonical trees, `print` preserves labels and
internal nodes: `nodes(print v) = ‖v‖`. *Proof:* induction on `v`, by the ι-rules
of `print`. ∎

## 3. The budget-stratified model, and consistency

### 3.1 Carriers

Each skeleton gets a set, by induction:
- `C(Unit) = {⋆}`, `C(Bool) = {tt, ff}`, `C(Nat) = ℕ`, `C(Lbl) = L`;
- `C(Syn)` is the set of finite trees over `L`;
- `C(◇) = {◇}`;
- `C(R)` is the set of finite trees over `L` whose internal nodes carry `◇`;
- `C(σ → τ)` is the set of all functions `C(σ) → C(τ)`, and
  `C(σ × τ) = C(σ) × C(τ)`.

Every carrier is non-empty. **Defaults** `dflt_σ` are chosen explicitly and
computably:
- `⋆`, `ff`, `0`, and the first label `ℓ₀` of `L`, at the corresponding base
  skeletons;
- `leaf ℓ₀` at `R`, `sleaf ℓ₀` at `Syn`, and `◇` at `◇`;
- the pair of defaults at `σ × τ`;
- the constant function with value `dflt_τ` at `σ → τ`.

Each default is a computable value of the evaluator of §5. It is not always
the value of a closed program, since `◇` has none, and that is not needed:
the evaluator only has to return a value related to it (review RR2-08). *An earlier state let `dflt_σ` be any element,
which may be uncomputable at function type (review R4-05).*

For `v ∈ C(R)`, `‖v‖` is its number of internal nodes. `print` erases tokens,
and is a bijection from `C(R)` onto `C(Syn)`.

*This replaces an earlier state of this section,* in which one domain `𝔻`
contained all partial functions `𝔻 ⇀ 𝔻`. By Cantor's theorem no set does. The
skeleton carriers are why terms are Church-style.

### 3.2 Denotation

Fix a budget `n`. For a simply typed term `t` and an environment `η` of
matching carrier values, `⟦t⟧ⁿη` is defined by well-founded recursion on the
pair `(n, t)`, ordered lexicographically:
- Constructors, `λ`, application, pairs, `let` and the eliminators have their
  set-theoretic meaning. `recN`, `recSyn` and `itR` iterate over the value of
  their scrutinee.
- `⟦abort_A t⟧ = dflt_{skel A}` and `⟦H₁ …⟧ = ⋆`.
- `⟦chk′ c d⟧ = Check(⟦c⟧, ⟦d⟧)`, and `⟦print r⟧` erases the tokens of `⟦r⟧`.
- `⟦inspect_X r c (x e. t₁) (x e. t₂)⟧η` computes
  `b = Check(print ⟦r⟧η, ⟦c⟧η)`. If `b = tt` it is `⟦t₁⟧(η, x ↦ ⟦r⟧η, e ↦ ⋆)`;
  otherwise the same with `t₂`.
- `⟦reflect_D r e⟧ⁿη`: let `v = ⟦r⟧η`.
  - **If `‖v‖ ≤ n` and `Check(print v, ⌜D⌝) = tt`,** decode the root judgment
    `Θₘ ⊢ t′ :¹ D`. By Lemma 2.7, `m < ‖v‖ ≤ n`. The value is
    `⟦t′⟧ᵐ(x₁ ↦ ◇, …, xₘ ↦ ◇)`, which lies in `C(skel D)` by Lemma 2.5.
  - **Otherwise** it is `dflt_{skel D}`.

  The recursion is to a strictly smaller budget.

**The cap is on the tree's footprint, `‖v‖ ≤ n`, not on the budget `m` it
declares.** Inside the soundness proof the cap is always met. Outside it — at
type level, where certificates are unbounded — it leaves finitely many trees to
evaluate. The evaluator applies the same test (§5).

**Lemma 3.1 (substitution).** `⟦t[u/x]⟧ⁿη = ⟦t⟧ⁿ(η, x ↦ ⟦u⟧ⁿη)`, and likewise
for types. *Proof:* induction on `t`. The term a `reflect` decodes is closed
except for its own tokens, so substitution never reaches it. ∎

**Lemma 3.2 (conversion invariance).** If `t ≡ t′`, then `⟦t⟧ⁿη = ⟦t′⟧ⁿη` for
every `n` and `η`.

*Proof.* By §1.5, `≡` only passes through skeleton-typed expressions, so both
sides of every step have denotations. Each step preserves them:
- β, by Lemma 3.1;
- ι, by the definitions;
- δ, because `⟦chk′⟧` is `Check` and closed canonical codes denote themselves.

Compatible closure follows by compositionality. No step involves `reflect`,
`inspect` or `H₁`, so the budget never enters. ∎

### 3.3 Semantic types

For `k ≤ n`, define `Vⁿₖ(A)η ⊆ C(skel A)` by induction on `A`, with `T(b)`
atomic. The superscript `n` is left implicit below.

```
V(0) = ∅    V(1) = {⋆}    V(Bool) = {tt, ff}    V(Nat) = ℕ    V(Lbl) = L    V(Syn) = C(Syn)
Vₖ(◇) = {◇} if k ≥ 1, and ∅ if k = 0
Vₖ(R) = { v ∈ C(R) : ‖v‖ ≤ k }
V(T(b))η = {⋆} if ⟦b⟧ⁿη = tt, and ∅ otherwise
Vₖ(Π(x :₀ A). B)η = { f : ∀a ∈ C(skel A).         f(a) ∈ Vₖ(B)(η, x↦a) }
Vₖ(Π(x :₁ A). B)η = { f : ∀j ≤ n−k. ∀a ∈ Vⱼ(A)η. f(a) ∈ Vₖ₊ⱼ(B)(η, x↦a) }
Vₖ(Π(x :ω A). B)η = { f : ∀a ∈ V₀(A)η.            f(a) ∈ Vₖ(B)(η, x↦a) }
Vₖ(Σ(x :₀ A). B)η = { (a, b) : a ∈ C(skel A), b ∈ Vₖ(B)(η, x↦a) }
Vₖ(Σ(x :₁ A). B)η = { (a, b) : ∃j ≤ k. a ∈ Vⱼ(A)η, b ∈ Vₖ₋ⱼ(B)(η, x↦a) }
Vₖ(Σ(x :ω A). B)η = { (a, b) : a ∈ V₀(A)η, b ∈ Vₖ(B)(η, x↦a) }
```

A type without a subscript has the same set for every `k`.

**Lemma 3.3 (monotonicity, conversion, substitution).**
- **Monotonicity:** `Vₖ(A) ⊆ Vₖ′(A)` for `k ≤ k′ ≤ n`. *By induction on `A`:*
  raising `k` shrinks the range of `j` in the `Π₁` clause and enlarges every
  target.
- **Conversion:** if `A ≡ B` then `V(A)η = V(B)η`. *By induction on the
  conversion:* inside `T(·)` use Lemma 3.2; `V(T(tt)) = V(1)` and
  `V(T(ff)) = V(0)`; the rest is congruence.
- **Substitution:** `V(B[u/x])η = V(B)(η, x ↦ ⟦u⟧η)`, by Lemma 3.1. ∎

**Lemma 3.4 (base data types across budgets).** For a base data type `D` and
`m ≤ k ≤ n`, `Vᵐₘ(D) ⊆ Vⁿₖ(D)`. *Proof:* these sets do not depend on the cap,
and `R`'s depends on the footprint only through its bound. ∎

This is why `reflect` is restricted to base data types. `T(b)` is not one: its
set depends on `⟦b⟧ⁿ`, and a `reflect` inside `b` makes that value
budget-dependent.

### 3.4 Semantic environments

`η ⊨ⁿₖ Γ` holds iff:
- every entry has `η(x) ∈ C(skel A)`;
- each usage-1 entry has `η(x) ∈ Vⁿₖₓ(A)η`, where `Σₓ kₓ ≤ k ≤ n`;
- each usage-`ω` entry has `η(x) ∈ Vⁿ₀(A)η`.

Usage-0 entries are unconstrained. For example, `(x₁ ↦ ◇, …, xₘ ↦ ◇) ⊨ᵐₘ Θₘ`.

**Lemma 3.5 (splitting).**
- If `η ⊨ₖ Γ₁ + Γ₂`, then `η ⊨ₖ₁ Γ₁` and `η ⊨ₖ₂ Γ₂` for some `k₁ + k₂ ≤ k`.
- If `η ⊨ₖ ωΓ`, then `η ⊨₀ Γ`.

*Proof.* Take the entries of `Γ₁ + Γ₂` by usage:
- a usage-1 entry is `1` in exactly one summand and `0` in the other;
- a usage-`ω` entry has a value in `V₀`, which lies in every `Vₖ′` and adds
  `0`;
- a usage-0 entry is `0` in both. ∎

### 3.5 The fundamental lemma

> **Lemma 3.6 (soundness at budget `n`).** If `Γ ⊢ t :¹ A` is derivable and
> `η ⊨ⁿₖ Γ`, then `⟦t⟧ⁿη ∈ Vⁿₖ(A)η`. For type-level derivations,
> `⟦t⟧ⁿη ∈ C(skel A)`.

*Proof.* The type-level clause is Lemma 2.5. The runtime clause is by strong
induction on `n`, and for fixed `n` by induction on the derivation. "IH" means
the inner hypothesis unless it says *outer*. Contexts are split by Lemma 3.5.

- **Var, Unit, TT, FF, Zero, Lbl.** Immediate. Var uses monotonicity; the other
  entries contribute nothing.
- **Conv.** Lemma 3.3.
- **Lam.**
  - At `ρ = 1`: for `j ≤ n − k` and `a ∈ Vⱼ(A)`, the environment
    `(η, x ↦ a)` satisfies the extended context with footprint `k + j`.
  - At `ρ = ω`: `a ∈ V₀(A)`, so the footprint is `k`.
  - At `ρ = 0`: `a` is any carrier value, and the entry is unconstrained.

  In each case the IH puts the body in the required set.
- **App.** The IH gives `⟦f⟧ ∈ Vₖ₁(Π…)`.
  - At `ρ = 1`: `⟦u⟧ ∈ Vₖ₂(A)` with `k₂ ≤ n − k₁`.
  - At `ρ = ω`: `⟦u⟧ ∈ V₀(A)`.
  - At `ρ = 0`: `⟦u⟧` is any carrier value.

  Lemma 3.3's substitution clause turns the result into
  `Vₖ(B[u/x])`.
- **Pair and Let.** The same bookkeeping. A `Σ₁` value's footprint splits as
  `j + (k₁ − j)` between its components.
- **Abort.** `V(0) = ∅`, so no environment satisfies the premise's context.
  The case is vacuous.
- **If, ElimBool, CaseLbl.** The scrutinee's value selects one branch. The
  branches share `Γ₂`, and the motive is instantiated at that value.
- **Succ, SLeaf, SNode, Chk, Print.** Numbers, labels, codes and Booleans have
  no footprint. `Check` is total (§1.6), so `⟦chk′ c d⟧ ∈ {tt, ff}`.
- **RecN.** By induction on the value `i` of the scrutinee. The base case has
  footprint at most `k₂`. The step's context is `ω`-scaled, so it passes on at
  most the footprint of `y`.
- **RecSyn.** Both methods run once per constructor, in `ω`-scaled contexts. By
  induction on the code, every result has footprint `0`.
- **Leaf, Node.** A node adds its token's footprint, `1`, to those of its
  subtrees.
- **ItR.** The methods have footprint `0`. By induction on the tree
  `v = ⟦r⟧`, `⟦itR_X(g, h, r)⟧ ∈ V_{‖v‖}(X)`. At a node,
  `h(◇) ∈ V₁(Lbl → X ⊸ X ⊸ X)`; then the two recursive results add their
  footprints; and `1 + ‖v₁‖ + ‖v₂‖ = ‖v‖ ≤ k₃ ≤ n`. This is Hofmann's
  non-size-increasing iteration (Hofmann 2003, p. 63).
- **Inspect.** `⟦r⟧ ∈ Vₖ₁(R)`, and `b = Check(print ⟦r⟧, ⟦c⟧) ∈ {tt, ff}`.
  - If `b = tt`, then `⋆ ∈ V(T(chk′ (print x) c))` at `x ↦ ⟦r⟧`.
  - If `b = ff`, then `⋆ ∈ V(T(not …))`.

  Either way the chosen branch's environment has footprint at most
  `k₁ + k₂ ≤ k`, and the IH applies.
- **H₁.** The IH gives trees `v = ⟦r⟧`, `w = ⟦s⟧` with `‖v‖ ≤ k₁`,
  `‖w‖ ≤ k₂` and `k₁ + k₂ ≤ n`, together with `⟦e₁⟧ ∈ V(T(chk′ (print r) c))`
  and `⟦e₂⟧ ∈ V(T(chk′ (print s) (neg c)))`. Both sets must be non-empty, so
  both checks compute to `tt`.
  - By Lemmas 2.6–2.8, `print v` encodes a derivation of `Θₘ₁ ⊢ t₁ :¹ A`, with
    `A` closed, `⌜A⌝ = ⟦c⟧` and `m₁ < ‖v‖ ≤ k₁`.
  - Likewise `print w` encodes one of `Θₘ₂ ⊢ t₂ :¹ B`, with `B` closed,
    `⌜B⌝ = neg ⟦c⟧` and `m₂ < k₂`.
  - By E5 and E1, `B = A ⊸ 0`.
  - By Lemma 2.4, `Θₘ₁₊ₘ₂ ⊢ t₂ t₁ :¹ 0` is derivable, and
    `m₁ + m₂ < k₁ + k₂ ≤ n`.
  - The **outer** hypothesis at budget `m₁ + m₂`, with the all-token
    environment, puts `⟦t₂ t₁⟧` in `V(0) = ∅`. That is a contradiction.

  So no environment satisfies the premises, and the case is vacuous.
- **Reflect.** The IH gives `v = ⟦r⟧` with `‖v‖ ≤ k₁ ≤ n`, and
  `⟦e⟧ ∈ V(T(chk′ (print r) ⌜D⌝))`, so the check computes to `tt`.
  - By Lemmas 2.6–2.8, `print v` encodes `Θₘ ⊢ t′ :¹ D` with `m < ‖v‖`.
  - The cap is met, so `⟦reflect_D r e⟧ = ⟦t′⟧ᵐ(◇, …, ◇)`.
  - The **outer** hypothesis at budget `m` puts this in `Vᵐₘ(D)`.
  - That lies inside `Vⁿₖ(D)` by Lemma 3.4, since `m ≤ k₁ ≤ k`.

  For `D = 0`, which is `H`, the outer hypothesis yields an element of `∅`, so
  the case is vacuous. ∎

### 3.6 Why the induction is not circular

- The model interprets `chk′` by the fixed total function `Check`, not by
  derivability in the model.
- Derivability enters only in the H₁ and Reflect cases. There, Lemma 2.7 lowers
  the budget strictly before the outer hypothesis is used.
- No step assumes the consistency of λᶜᵉʳᵗ₀ at the current budget.

### 3.7 Size soundness (T3)

**Theorem 3.** Let `Γ ⊢ t :¹ R`, and let `η ⊨ⁿₖ Γ`. Then `‖⟦t⟧ⁿη‖ ≤ k`.
- In `Θₘ`, the bound is at most the number of distinct token variables free in
  `t`.
- A closed term of type `R` denotes a leaf.

*Proof.* The first claim is Lemma 3.6. For the refinement, lower the usage of
every token variable not free in `t` to `0` (Lemma 2.3). The all-token
environment then has footprint at most the number of those that are free. ∎

The same argument bounds the footprint of any runtime term, not only one of
type `R`. For `R`, this is the draft's L4, proved in the model rather than on
normal forms.

### 3.8 Consistency (T1)

> **Theorem 1.** For no `n` is there a derivable `Θₙ ⊢ t :¹ 0`.

*Proof.* The all-token environment satisfies `Θₙ` with footprint `n`. By
Lemma 3.6, `⟦t⟧ⁿ ∈ V(0) = ∅`, which is impossible. ∎

**Corollary 3.7 (`Check` accepts no refutation and no contradictory pair).**
- For every code `c`, `Check(c, c⊥) = ff`.
- No codes `c₁`, `c₂` and `d` have `Check(c₁, d) = Check(c₂, neg d) = tt`.

*Proof.* Otherwise Lemma 2.6 gives a derivable refutation, directly or through
Lemma 2.4, contradicting Theorem 1. ∎

The cap is needed only inside the induction. Corollary 3.7 covers codes of
every size, and so certificates of every size, erased ones included, since
`print` is onto.

### 3.9 Self-justification (T2)

The following are closed, derivable at budget 0:
- `λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). H r e`, which inhabits
  `H° := Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0`;
- `λ(r :₁ R). λ(s :₁ R). λ(c :ω Syn). λ(e₁ :₁ …). λ(e₂ :₁ …). H₁ r s c e₁ e₂`,
  which inhabits the pair form `H₁°`.

With Theorem 1, λᶜᵉʳᵗ₀ is consistent and derives its own consistency
propositions. These are the two clauses of `Willard2016` Definition 3.4, read
for this calculus's apparatus (natural deduction, draft §2.8) and certificate
representation.

## 4. The draft's §§4–5, settled

**`μ(A)`** is the least `‖v‖` with `Check(print v, ⌜A⌝) = tt`, or `∞` if there
is none. The type `□A` is `Σ(r :₁ R). T(chk′ (print r) ⌜A⌝)`.

### 4.1 No token-free certificates

**Proposition 4.1.** A closed runtime term of type `R` denotes a leaf. A
closed `f : Syn ⊸ R` maps every code to a leaf.

*Proof.* The first is T3. For the second, take `f ∈ V₀(Π(c :₁ Syn). R)`. Since
`V₀(Syn) = V(Syn)`, choose `j = 0`: then `f(c) ∈ V₀(R)`. ∎

Given `k` tokens, every output has at most `k` internal nodes.

### 4.2 D1, per instance

**Proposition 4.2.** If `Check(print v, ⌜A⌝) = tt`, then
`Θ_{‖v‖} ⊢ (lit_v, ⋆) :¹ □A`. Here `lit_v` builds `v`'s tree from the tokens,
taken in preorder.

*Proof.*
- `lit_v` uses each token once.
- `print lit_v` ι-reduces to the closed canonical code of `v`, and `chk′` of
  that code δ-reduces to `tt`. So `T(chk′ (print lit_v) ⌜A⌝) ≡ T(tt) ≡ 1`, and
  `⋆` is typed by `Unit` and `Conv`.
- `Pair` at `ρ = 1` adds the contexts to `Θ_{‖v‖}`. ∎

### 4.3 Quotation cost

> **Proposition 4.3.** For closed `A`, every certificate `w` of `□A` has
> `‖w‖ > 2μ(A)`.

*Proof.*
1. `print w` encodes a derivation of `Θₘ ⊢ p :¹ □A`.
2. By Lemma 3.6 at budget `m`, `⟦p⟧ = (v, ⋆)` with
   `Check(print v, ⌜A⌝) = tt`. So `‖v‖ ≥ μ(A)`.
3. Let `f` be the number of distinct token variables free in `p`. By the
   argument of T3's refinement, applied to `p`, `‖v‖ ≤ f`.
4. So `m ≥ f ≥ μ(A)`, and by E4 `⌜p⌝` has at least `f` internal nodes.
5. By E2 and E3, the context and the term are disjoint subtrees of the root
   judgment. So `‖w‖ ≥ 1 + m + f ≥ 1 + 2μ(A)`. ∎

*This corrects the draft's argument.* The draft said such a certificate "must
record the literal term that rebuilds `r`". It need not: a certificate of `□A`
may build any certificate of `A`. What every such certificate must pay is the
declaration of at least `μ(A)` tokens, plus a term that mentions each of them.

### 4.4 D3 and boxed contraction: per instance yes, uniformly no

Take `A_j := 1 ⊸ ⋯ ⊸ 1 ⊸ 1`, with `j` arrows. It is certifiable, by
`λ(x₁ :₁ 1). ⋯ λ(x_j :₁ 1). ⋆`. Its root judgment encodes `A_j`, so
`μ(A_j) ≥ j`.

**Proposition 4.4 (no uniform budget).** For every `k`:
1. no `q` with `Θₖ, x :₁ R ⊢ q :¹ R` maps every certificate of every closed
   `A` to a certificate of `□A`;
2. there is no family `(q_A)` with `Θₖ ⊢ q_A :¹ □A ⊸ □□A` for every
   certifiable `A`;
3. there is no family `(c_A)` with `Θₖ ⊢ c_A :¹ □A ⊸ □A ⊗ □A` for every
   certifiable `A`.

*Proof.* Take `A = A_{k+1}` and a minimal certificate `v`, so
`‖v‖ = μ(A) > k`. Evaluate in the model at budget `k + μ(A)`.
- For (1) and (2), by T3 the output has at most `μ(A) + k < 2μ(A)` nodes. By
  Proposition 4.3, a certificate of `□A` needs more than `2μ(A)`.
- For (3), the two outputs together have at most `μ(A) + k` nodes, and two
  certificates of `A` need at least `2μ(A)`. ∎

**Proposition 4.5 (per instance).** For each certifiable closed `A`:
- **boxed contraction** is derivable with budget `2μ(A)`: ignore the input,
  which affinity allows, and build two minimal certificates by Proposition 4.2;
- **D3** (`□A ⊸ □□A`) is derivable with budget `μ(□A)`, which is finite by
  Proposition 4.2.

The draft's "no, for any fixed budget" is therefore right only in the uniform
reading: no single budget serves every type. Per instance, both hold. Both
results need E3 and E4, and nothing else about the encoding.

### 4.5 D2: conjectured not derivable internally

**Conjecture 4.6.** There is no budget `k` such that for all closed
certifiable `A` and `B` some term `Θₖ ⊢ comp_{A,B} :¹ □(A ⊸ B) ⊗ □A ⊸ □B`
exists. *Its motivation* is that the application node over the two input
certificates seems unable to receive its evidence. That is the heuristic
below, and the conjecture does not imply it at type-dependent budgets (review
RR2-13).

*An earlier state* said that no term of that type "computes its output
certificate from its inputs". That is false as stated. At `A = B = 1`,
`λz. let (p, q) = z in q` returns an input certificate (review R4-07). The
conjecture is about a budget uniform in `A` and `B`, which per-instance terms
— with budget `μ(B)` — do not supply.

*Why it is expected.* The evidence would inhabit `T(chk′ (print r″) ⌜B⌝)`,
where `r″` is built from the input certificates, which are variables. Evidence
for `T(b)` comes from one of four places, and none reaches this `b`:
- *conversion of `b` to `tt`:* `chk′` reduces only on closed canonical codes,
  and this one is open;
- *hypotheses:* they mention `chk′` on *other* codes;
- *`inspect`:* its failure branch must still produce `□B`;
- *case analysis on a Boolean inside `b`:* it cannot reach inside `chk′`.

**Why this is not a proof.** In the standard model the type of the missing
evidence is *true*. So refuting its derivability needs a model in which `chk′`
can behave differently on open codes. None is given.

What does hold:
- **Per instance.** For certifiable `B`, build a certificate of `B` from
  scratch, with budget `μ(B)`.
- **Checked at runtime.** Build the tree and `inspect` it. The result type must
  allow failure.
- **As a tree transformation, verified outside the calculus.**
  - Under the full-judgment encoding, the new root repeats the combined context
    (`m₁ + m₂` entries), both conclusion terms and the type. Every premise node
    must also be re-recorded with the combined context. So the budget grows
    with the premises.
  - **No encoding with E2 and E3 composes in one node** (review R4-06). E2
    makes the new root's first child its whole conclusion. E3 makes that
    conclusion's context an explicit list of `m₁ + m₂` entries, which cannot be
    shared with the contexts inside the premises. So the new material grows
    with `m₁ + m₂`.
  - A relative encoding therefore has to replace E2 and E3, for instance by
    declaring each token once, where it is used. It must then re-prove the
    properties T1 and §4.3 use: strict overhead (Lemma 2.7), and the
    quotation-cost bound. **None is specified.** *An earlier state said such an
    encoding could keep E1–E5; that was wrong.*

The draft's D2 row wrote `□(A → B)`. With `→`, the argument's certificate may
use no tokens, because `ωΘ` cannot be weakened back to `Θ`. The row is
corrected to `⊸`.

### 4.6 The two consistency statements

- `H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0` is derivable (T2).
- `Con′ = Π(c :₁ Syn). T(chk′ c c⊥) ⊸ 0` is the code form.
- The weakest code form is `Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0`. `Con′`
  implies it, by `λ(c :ω Syn). λ(e :ω …). f c e`.

**Proposition 4.7.**
`λ(f :₁ Con′). λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). f (print r) e` has
type `Con′ ⊸ H°`.

*Proof:* App twice. `print r` uses `r` once, and `e`'s type is syntactically
the domain of `f (print r)`. ∎

**Proposition 4.8.** No budget derives `H° → Con′_ω`, nor `H° ⊸ Con′_ω`.

*Proof:* either would yield `Con′_ω` from the closed inhabitant of `H°`,
contradicting P5. ∎

### 4.7 Bounded code consistency

**Proposition 4.9.** Define `depthLeq` by `recSyn`. For each `k`, a closed term
(budget 0) inhabits
`Π(c :ω Syn). T(depthLeq c k) ⊸ T(chk′ c c⊥) ⊸ 0`.

*Proof.* Nested case analysis on `c` to depth `k`, by `recSyn` and `caseLbl`,
threading the evidence through `elimBool` on `&&`.
- In a branch where the depth bound computes to `ff`, the first hypothesis has
  type `T(ff) ≡ 0`.
- In a branch where `c` is fully known, `T(chk′ c c⊥)` δ-reduces to `T(ff)`,
  by Corollary 3.7.

The term is finite, and free of tokens. Its size grows doubly exponentially in
`k`. ∎

*On the draft's method,* which was to "parse and then apply `H`".

- **The parse is definable** (review R4-03, correcting an earlier state of this
  paragraph). The core has a computational destructor for `R`. Let
  - `K := ◇ ⊗ (R ⊗ R)`;
  - `V := Σ(b :ω Bool). Σ(a :ω Lbl). (T(b) ⊸ K)`;
  - `L(a) := (ff, (a, λ(e :₁ T(ff)). abort_K e))`;
  - `N(d, a, u, v) := (tt, (a, λ(e :₁ T(tt)). (d, (u, v))))`.

  A closed `roll : V ⊸ R` unpacks the triple, and eliminates `b` dependently at
  the motive `(T(b) ⊸ K) ⊸ R`. On `tt`, it applies the function to `⋆` and
  builds a node. On `ff`, it discards the function and returns `leaf a`.

  Then `out r := itR_V(L, λd a u v. N(d, a, roll u, roll v), r)` satisfies
  `roll (out r) = r` and `out (node d a r₁ r₂) = N(d, a, r₁, r₂)`, as evaluated
  values, by induction on the tree.
- **A typed parser is definable** (review RR2-10: the destructor alone does not
  show this).
  - *Its type:* `parse : Π(c :ω Syn). R ⊸ R ⊗ R`, by `recSyn` on the code.
    The second argument is a *supply*, a certificate read along its right
    spine for its tokens.
  - *A leaf* of the code costs nothing, and returns the supply unchanged.
  - *A node* applies `out` to the supply and spends the top token on the
    node. It discards the supply's left child, and threads the right spine
    through the two subcodes, left first. Each recursive result is used once.
  - *An exhausted supply* makes the node a leaf.
  - *Adequacy,* by induction on `c`, outside the calculus: with a supply
    whose right spine has at least `nodes(c)` nodes, `print` of the result is
    `c`, and exactly `nodes(c)` spine nodes are used.
  - *Implemented and tested* (ADR-0005, `lcert.examples`). It is closed, and
    it prints back with enough supply, and visibly not otherwise.
- **What remains is correctness inside the calculus.** An internal proof that the parse prints
  back to its input is not given: `out`'s equations hold of values, not
  judgmentally of open trees. So the parse route does not yet prove the
  restricted form inside the calculus; the case analysis above does.

The claim itself stands: every bounded instance of `Con′` is available.

### 4.8 Certified evaluation for base data types

`reflect_D` is sound: see the Reflect case of Lemma 3.6. It computes in the
evaluator, by running the decoded term with `m < ‖v‖` of the certificate's
tokens (§5). This settles the draft's §8.2 conjecture for base data types, as
an evaluation rule. It does **not** settle it as a conversion rule, which the
model cannot follow (§1.5). For higher types, Lemma 3.4 fails and the question
stays open.

The draft raised a caveat: `Willard1993-TR` Proposition 5 (`full`), by which
local `Π₁` reflection is inconsistent for some nice `A`. The result above does
not contradict it:
- Willard's reflection ranges over proofs coded as numbers, which the
  arithmetic names compactly.
- Here every certificate declares its budget uncompressed (E3), and the Reflect
  case uses exactly that.

Whether Willard's counterexample can be transcribed into λᶜᵉʳᵗ₀ at all is
**not checked**.

### 4.9 `H` and `H₁`

**Proposition 4.10.** `H°` follows from `H₁` inside the calculus, at a constant
budget (review R4-02).
- Let `v` certify the closed derivation `⊢ λ(x :₁ 0). x :¹ 0 ⊸ 0`, and let
  `K = ‖v‖`.
- Then `Θ_K ⊢ λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). H₁ r lit_v c⊥ e ⋆ :¹ H°`.

*Proof.* The refutation certificate `r` is `H₁`'s first certificate, at the
type `0`. `neg c⊥` is `⌜0 ⊸ 0⌝`, and `chk′ (print lit_v) ⌜0 ⊸ 0⌝` converts to
`tt`, so `⋆` is the second piece of evidence. Every affine argument is used
once. ∎

*This corrects an earlier state of this section.* That state claimed that
inside the calculus "the evidence obstacle of §4.5 recurs". It does not: no
refutation certificate has to be transformed. Its first certificate is used as
it is, and the second is fixed. The draft's own sentence — "`H` follows from
`H₁` only at extra token cost" — was right in substance; its method, rebuilding
the refutation as a certificate of `1 ⊸ 0`, was the costly one. `H` is kept as
a constant because it is available at budget 0. In the core it is `reflect₀`.

## 5. Termination and adequacy of evaluation (T4)

**The budgeted evaluator `evalₙ`.** It runs a program in `Θₙ`, with `n` known.
It is call-by-value and does not erase: it evaluates every term position,
usage-0 arguments included. Its rules:
- Token variables are bound to the runtime token `◇`.
- `chk′` calls `Check`.
- `inspect` branches on `Check(print v, c)` and rebinds `v`.
- `reflect_D v e`:
  - if `‖v‖ ≤ n` and `Check(print v, ⌜D⌝) = tt`, decode `Θₘ ⊢ t′ :¹ D` and
    return `evalₘ(t′)`, with `m` tokens;
  - otherwise return `dflt`.
- `H₁ …` and `abort_A t` return the default value of the skeleton.

The evaluator mirrors the model clause for clause, so the proof is Tait's, over
simple types.

> **Theorem 4.** For every simply typed term `t : σ` and every environment of
> values related to `η`, `evalₙ(t)` terminates with a value related to
> `⟦t⟧ⁿη`. Values are related at base skeletons by equality, at `σ × τ`
> componentwise, and at `σ → τ` by sending related arguments to terminating,
> related results.

*Proof.* By strong induction on `n`, and within `n` by induction on `t`.
- **Standard cases.** For `λ`, application, pairs, the eliminators, and `recN`
  and `recSyn` over their scrutinee's value, this is Tait's argument for
  System T. *Standard, not verified here.*
- **`chk′`, `print` and `inspect`** terminate because `Check` is total.
- **`itR`** is structural in the tree.
- **`reflect`** calls `evalₘ` with `m < ‖v‖ ≤ n`. This is covered by the outer
  induction, and matches the model's clause.
- **`H₁` and `abort`** return the default, as the model does. ∎

**Corollary 5.1.** For `Θₙ ⊢ t :¹ D` with `D` a base data type, `evalₙ(t)`
terminates. Its result is the canonical form of an element of `Vⁿₙ(D)`. So
closed terms of data type evaluate to canonical values, which is the draft's
L3 for data.

That typed programs never evaluate `abort`, `H₁` or `H` — the draft's "dead
code" — is Theorem 5.2 below. *An earlier state asserted it without proof
(review F-03). The next state recorded it as an expectation.*

**The erasing evaluator `evalᴱₙ`** (review R4-04).

*Why it is needed.* `evalₙ` does not honour the resource reading. With one
token `d`, `d :₁ ◇ ⊢ (r_N(d), ⋆) :¹ Σ(r :₀ R). 1` is derivable for every `N`:
the erased component may use `d` any number of times. So `evalₙ` physically
builds an `N`-node tree from one token. The model permits this, because the
`Σ₀` clause constrains nothing, so T3 is not contradicted. But the draft's
operational reading — a program holds no more certificate nodes than it was
given tokens — fails for `evalₙ`.

*The definition.* `evalᴱₙ` first erases the program, using the typing
derivation. It replaces every argument of an application at `Π₀`, and every
first component of a pair at `Σ₀`, by `⋆`. It then evaluates the result with
`evalₙ`'s rules, which never consult types, and treats a program decoded by
`reflect` the same way. Type annotations are never evaluated by either
evaluator. The erased program is not simply typed: a `Π₀` argument has become
`⋆`. So Theorem 4 does not apply to it, and Theorem 4′ uses a relation of its
own (review RR2-01).

> **Theorem 4′.** For derivable `Θₙ ⊢ t :¹ D` with `D` a base data type,
> `evalᴱₙ(t)` terminates with the same result as `evalₙ(t)`. In particular, an
> `R` result has at most `n` internal nodes.

*Proof.* A relation `E(A)` between runtime values and carrier values, defined
by induction on the type `A`.

**The definition.**
- *Base data types:* `E` is equality, a runtime tree with tokens being matched
  with the carrier tree of the same shape.
- *`◇`:* every runtime token is related to `◇`.
- *`0`, `1` and every `T(b)`:* `E` relates `⋆` to `⋆`.
- *`Π(x :₀ A). B`:* `(f, φ) ∈ E` iff for every `α ∈ C(skel A)`, `f ⋆`
  terminates and `(f ⋆, φ(α)) ∈ E(B)`.
- *`Π(x :ρ A). B` with `ρ ∈ {1, ω}`:* `(f, φ) ∈ E` iff for every
  `(a, α) ∈ E(A)`, `f a` terminates and `(f a, φ(α)) ∈ E(B)`.
- *`Σ(x :₀ A). B`:* `E` relates `(⋆, b)` to `(α, β)` when `(b, β) ∈ E(B)`.
- *`Σ` at `1` or `ω`:* componentwise.

**What `E` does not depend on.** The terms inside a type: `T(b)` has the same
clause for every `b`. So `E(A)` is determined by `A`'s skeleton and usage
annotations. It is invariant under `≡`, whose steps change only terms inside
`T(·)` and `T(tt) ≡ 1`, `T(ff) ≡ 0`. It is invariant under substitution into
types.

**Defaults.** The runtime default of each skeleton (`lcert.eval`) is related
by `E` to the carrier default (§3.1). This holds by the choice made there.

**The fundamental property.** Take a runtime derivation `Γ ⊢ t :¹ A` and a
runtime environment related by `E` to a carrier environment `η`, entry by
entry. An entry of usage `1` or `ω` is related. An entry of usage `0` may hold
any runtime value, since the erased program never reads it, and is arbitrary
in `η`. Then evaluating the erasure of `t` terminates, and its value is
`E`-related to `⟦t⟧ⁿη`.
- *Restriction:* an entry with nonzero usage in a premise has nonzero usage in
  the conclusion, because usages add and scale without cancelling. So an
  environment related for the conclusion is related for each runtime premise.
  *An earlier state required usage-0 entries to be `⋆`, which a premise's
  environment need not satisfy (review T52-04).*

The proof is by strong induction on `n`, and within `n` on the derivation:
- **Var.** The variable has usage `1` or `ω`, so its runtime value is related.
  An erased variable is never a runtime subterm: Var excludes usage 0, and
  erasure removes the type-level positions where it can occur.
- **App.**
  - At `Π₀`, the erased argument `⋆` meets the `Π₀` clause, whatever `⟦u⟧η`
    is.
  - At `1` and `ω`, the inner hypothesis relates the argument.
  - `B[u/x]` has the same `E` as `B`, by invariance.
- **Pair, Let.** The same, at `Σ`.
- **Conv.** By invariance under `≡`.
- **The eliminators, `print`, `chk′`, `inspect`.** As in Theorem 4. The
  scrutinee's runtime value equals its carrier value, so both sides take the
  same branch and iterate the same number of times.
- **Reflect.** The runtime tree equals the carrier tree. So the cap test and
  `Check` agree, and both sides decode the same derivation of budget `m < n`.
  The runtime erases it and runs it on `m` of the tree's tokens. The outer
  hypothesis at `m`, applied to that derivation, relates the result to
  `⟦t′⟧ᵐ`. At a base data type, "related" means equal.
- **H₁ and abort.** Both sides return their defaults, which are related.

**The conclusion.** At a base data type `D`, `E(D)` is equality, so
`evalᴱₙ(t) = ⟦t⟧ⁿ`. By Theorem 4, `evalₙ(t) = ⟦t⟧ⁿ` too. An `R` result is
therefore the carrier tree `⟦t⟧ⁿ`, which has at most `n` internal nodes by
Lemma 3.6. ∎

*An earlier state called this a sketch.* Written out, the relation needs no
dependency on terms, and the argument is short.

**`abort`, `H₁` and `H` are never evaluated.** Both evaluators give these
nodes a default, as the model does. In a typed program the default is never
used:

> **Theorem 5.2.** Let `Θₙ ⊢ t :¹ D` be derivable, with `D` a base data type.
> Then neither `evalₙ(t)` nor `evalᴱₙ(t)` evaluates an `abort`, `H₁` or `H`
> node: not in `t`, and not in any program that `reflect` decodes and runs.

*Why Lemma 3.6 is not enough.* An evaluator reaches a node in some
environment. At a runtime node, Lemma 3.6 makes the context of these nodes
unsatisfiable: their cases are vacuous. But `evalₙ` also evaluates type-level
subterms, about which Lemma 3.6 says nothing, since an erased certificate can
exceed every footprint (the example of `r_N(d)` above). What is needed is the
vacuity of these cases without footprints. Corollary 3.7 supplies it, because
it holds for codes of every size.

*The relation.* Fix `n`. For a carrier environment `η`, define a set `S(A)η`
of pairs — a runtime value and a carrier value — by induction on the syntax of
`A`:
- `S(0) = ∅`, and `S(1)` relates `⋆` to `⋆`;
- at `Bool`, `Nat`, `Lbl` and `Syn`, equality; at `◇`, every runtime token to
  `◇`; at `R`, a runtime tree to the carrier tree of the same shape and
  labels;
- `S(T(b))η` relates `⋆` to `⋆` if `⟦b⟧ⁿη = tt`, and is empty otherwise;
- at `Π(x :ρ A). B`, for every `ρ`: the `(f, φ)` such that for every
  `(a, α) ∈ S(A)η`, `f a` evaluates *safely* to some `v` with
  `(v, φ(α)) ∈ S(B)(η, x ↦ α)`;
- at `Σ(x :ρ A). B`, for every `ρ`: componentwise, the second component at
  `(η, x ↦ α₁)`.

An evaluation is **safe** if it terminates and its whole trace enters no
`abort`, `H₁` or `H` node. The trace includes the evaluation of every
subterm, of every closure applied, and of every program that `reflect` runs.
Safety is a property of traces, and does not refer to `S`. `S` records truth,
as `V` does, but no footprints. Two facts, each by induction on the type:
- *Conversion:* if `A ≡ B`, then `S(A)η = S(B)η`. At `T(b)` this is Lemma 3.2,
  with `S(T(tt)) = S(1)` and `S(T(ff)) = S(0)`. At `Π` and `Σ` it is
  congruence, since safety does not read the type.
- *Substitution:* `S(B[u/x])η = S(B)(η, x ↦ ⟦u⟧ⁿη)`. At `T(b)` this is
  Lemma 3.1. At `Π` and `Σ` it is congruence, with bound variables renamed
  apart.

*The fundamental property.* Let `Γ ⊢ t :σ A` be derivable, **in either
mode**. Let `ρ` be a runtime environment with `(ρ(x), η(x)) ∈ S(A_x)η` for
every entry, whatever its usage. Then `evalₙ` evaluates `t` in `ρ` safely, to
some `v` with `(v, ⟦t⟧ⁿη) ∈ S(A)η`.

*Proof.* By strong induction on `n`, and within `n` on the derivation.
- *Both modes alike.* Neither `evalₙ` nor `S` reads usages, so a type-level
  premise is handled like a runtime one. Type annotations are not evaluated.
- *How traces compose.* In every case but the vacuous ones, a node's trace
  consists of the evaluations of its premises, in related environments; of
  applications of related closures; and, at `reflect`, of one nested run. The
  hypotheses make each of these safe. The node itself is not `abort`, `H₁` or
  `H`, so the whole trace is safe.

The cases:
- **Var and the constants.** The value is `ρ(x)`, related by assumption, or a
  constant.
- **Lam.** The value is a closure over `ρ`. For `(a, α) ∈ S(A)η`, the
  environment `(ρ, x ↦ a)` is related to `(η, x ↦ α)`. So the hypothesis for
  the body gives the `Π` clause.
- **App.** The hypotheses relate the function to `⟦f⟧ⁿη` at the `Π` type, and
  the argument, in either mode, to `⟦u⟧ⁿη` at `S(A)η`. The `Π` clause makes
  the application safe, with a result related at
  `S(B)(η, x ↦ ⟦u⟧ⁿη) = S(B[u/x])η`.
- **Pair.** Componentwise. The second component's type is `B[a/x]`, and
  substitution applies.
- **Let.** The components of the pair are related at `S(A)η` and at
  `S(B)(η, x ↦ α₁)`. So the body's environment is related. The body's
  hypothesis gives a result in `S(C)`, and `C` mentions neither `x` nor `y`.
- **Conv.** By the conversion fact.
- **If, ElimBool, CaseLbl.** The scrutinee has equal runtime and carrier
  values, so both sides take the same branch, in a related environment. At
  `ElimBool` and `CaseLbl`, substitution turns the branch's type, for example
  `S(P[tt/x])η`, into `S(P[b/x])η`, since `⟦b⟧ⁿη = tt`.
- **Succ, SLeaf, SNode, Leaf, Node, Print, Chk.** The premises are related.
  The constructors preserve the relation, `print` forgets tokens on both
  sides, and `Check` is total and gets the same arguments on both sides.
- **RecN.** The scrutinee has the same value `i` on both sides. By an inner
  induction on `j ≤ i`, the `j`-th accumulator is related at
  `S(P)(η, x ↦ j)`:
  - at `0`, by the base's hypothesis and substitution;
  - from `j` to `j + 1`, by the step's hypothesis, in the environment extended
    by `x ↦ j` and the accumulator, both related.
- **RecSyn.** Likewise, by induction on the code, which is the same on both
  sides.
- **ItR.** By induction on the tree, which has the same shape on both sides.
  At a leaf `g` is applied, and at a node `h` is applied to the token, the
  label and the two related recursive results. Each application is safe by
  the `Π` clause of `g`'s or `h`'s relation.
- **Inspect.** Both sides branch on the same `Check` result. In the branch
  taken, the Boolean under `T` is `tt`. So `(⋆, ⋆)` lies in
  `S(T(chk′ (print x) c))`, or in `S(T(not …))`, at `x ↦ ⟦r⟧ⁿη`, and the
  branch's hypothesis applies.
- **Abort.** The hypothesis for its premise yields an element of
  `S(0) = ∅`. So no environment of the stated kind exists, and the case holds
  vacuously.
- **H₁.** The hypotheses for `e₁` and `e₂` give
  `Check(print ⟦r⟧ⁿη, ⟦c⟧ⁿη) = tt` and `Check(print ⟦s⟧ⁿη, neg ⟦c⟧ⁿη) = tt`.
  Corollary 3.7 excludes this, at every size. Vacuous.
- **H, that is `reflect₀`.** Likewise `Check(print ⟦r⟧ⁿη, c⊥) = tt`, which
  Corollary 3.7 excludes. Vacuous.
- **`reflect_D` with `D ≠ 0`.** `r` and `e` are evaluated first, safely, by
  their hypotheses. The runtime tree and the carrier tree have the same shape
  and labels, so the same node count and the same `print`. So both sides make
  the same cap test, and the same `Check`.
  - If both pass, Lemmas 2.6–2.8 give a derivable `Θₘ ⊢ t′ :¹ D` with
    `m < ‖v‖ ≤ n`. The **outer** hypothesis at `m`, with the tokens related
    to `◇`, makes `evalₘ(t′)` safe, with a result related to `⟦t′⟧ᵐ` at
    `S(D)`. At a base data type `S(D)` does not depend on the budget, just as
    `V(D)` does not (Lemma 3.4).
  - Otherwise both return defaults, which are related (§5's defaults
    paragraph). ∎

At the root, the runtime tokens are related to `◇, …, ◇`, so `evalₙ(t)` is
safe.

*The erasing evaluator.* `evalᴱₙ` evaluates the erasure of a runtime
derivation. Change `S` at usage 0, as Theorem 4′'s `E` does, and relax the
environments:
- at `Π(x :₀ A). B`: `(f, φ)` is related when for every `α ∈ C(skel A)`,
  `f ⋆` evaluates safely to some `v` with `(v, φ(α)) ∈ S(B)(η, x ↦ α)`;
- at `Σ(x :₀ A). B`: `(⋆, b)` is related to `(α, β)` when
  `(b, β) ∈ S(B)(η, x ↦ α)`;
- an environment is related to `η` when every entry of usage `1` or `ω` is
  related. An entry of usage `0` may hold any runtime value, since the erased
  program never reads it, and any carrier value in `C(skel A)`.

The property is then stated for runtime derivations only. The cases that
change:
- **Restriction.** An entry with nonzero usage in a premise has nonzero usage
  in the conclusion, because usages add and scale without cancelling. So an
  environment related for the conclusion is related for each runtime premise.
- **Lam at usage 0.** For every `α ∈ C(skel A)`, the body runs with `x ↦ ⋆`
  at runtime and `x ↦ α` in the model. That entry has usage 0, so the
  environment is related, and the body's hypothesis applies. The `Π₀` clause
  quantifies over the whole carrier, not over `S(A)`, so this case differs
  from the non-erasing one even when `S(A)` is empty.
- **App and Pair at usage 0.** The argument is not evaluated. Its denotation
  `⟦u⟧ⁿη` lies in `C(skel A)`, by the type-level clause of Lemma 3.6. So the
  `Π₀` clause, or the `Σ₀` clause, applies at `α = ⟦u⟧ⁿη`.
- **Let at usage 0.** The pair is `(⋆, b)`, related to `(α₁, β)` with `α₁`
  arbitrary in the carrier. The body runs with `x ↦ ⋆` and `x ↦ α₁`, an entry
  of usage 0, and with `y` related.
- **Reflect.** The decoded derivation is erased, and run by `evalᴱₘ`. The
  outer hypothesis is the erasing property at `m`, for that derivation.
- **The vacuous cases** use only runtime premises: `abort`'s argument, and the
  evidence of `H₁` and `H`. Erasure keeps these. `S(0) = ∅` does not depend
  on `η`, and Corollary 3.7 holds for every code, so arbitrary carrier values
  at usage-0 entries change nothing.
- **Every other case** is as before, with the restriction fact supplying the
  premises' environments. ∎

*What made it provable.* The earlier state paired Theorem 4's relation with
Lemma 3.6's, and Lemma 3.6 cannot follow evaluation into erased positions.
Consistency at every size, which Corollary 3.7 gives once T1 is proved,
removes the need for footprints.

*Checked in the implementation.*
- `lcert.eval` reports to a probe every `abort`, `H₁` or `H` node it enters,
  before evaluating the node's arguments.
- The tests run typed programs in which such a node lies on a branch the run
  does not take, so that an evaluator taking a wrong branch would enter it.
  The programs are:
  - the tutorial's guard;
  - a check of a type and then of its negation, holding `H₁`;
  - the destructor, and the parser that uses it;
  - PA's transport axiom;
  - a dependent elimination;
  - that elimination certified, and run through `reflect`.
- They run each program under both evaluators, and require that no such node
  is entered.
- *By hand, not in the suite:* swapping the branches of `inspect`, or of
  `elimBool`, in the evaluator makes these tests fail. With `inspect` swapped,
  the probe records `H₁`.

**Not proved at all:** that `evalᴱₙ` never duplicates a token object, so that
the tokens in a value are distinct. The model counts nodes and does not tell
tokens apart. The implementation checks this property at runtime and tests
it.

## 6. The second incompleteness theorem applies to codes (P5)

> **Proposition 5.** For no `n` and `t` is `Θₙ ⊢ t :¹ Con′_ω` derivable, where
> `Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0`. So no stronger form is derivable
> either: not `Con′` with its usage-1 quantifier, and not the `⊸` variants.

**The idea.** Suppose `Δ` derives `Θₙ ⊢ t :¹ Con′_ω`. Two translations then
make PA prove its own consistency. The first is checked inside PA. The second
is carried out in arithmetic in all finite types, and only its arithmetic
conclusion is brought back to PA:
- **Step 1 (§6.2):** λᶜᵉʳᵗ₀ interprets PA, verifiably. A PA-proof of `0 = 1`
  becomes a λᶜᵉʳᵗ₀-refutation, by a primitive recursive translation that PA
  proves correct. So PA proves `Con_λ → Con_PA`.
- **Step 2 (§6.3):** arithmetic in all finite types interprets `Δ`, including
  its self-reference constants at the fixed budget `n`. So it proves `Con_λ`,
  and so, being conservative, does PA.

Gödel's second theorem forbids the result (§6.4).

### 6.1 What is cited

Three standard results, not verified here; no source for them is held.

| | Result |
| --- | --- |
| **S1** | Gödel's second incompleteness theorem for PA, with the standard provability predicate of the Hilbert system `H_PA` below: if PA is consistent, PA does not prove `Con_PA` |
| **S2** | PA proves every true closed equation between primitive recursive terms (a case of its Σ₁-completeness) |
| **S3** | E-PA^ω, extensional Peano arithmetic in all finite types, is conservative over PA for arithmetic sentences |

S3 is usually obtained in two steps:
1. E-HA^ω is conservative over HA, by a *formal* interpretation of E-HA^ω in
   HA — for instance the hereditarily effective operations, formalized in HA.
2. The negative translation carries this from E-HA^ω and HA to E-PA^ω and PA.

A semantic model would not do: it shows that theorems are true, not that PA
proves them. Only Π₁-conservativity is used here, since `Con_λ` is Π₁.

`Con_λ` is the arithmetic sentence `∀c (IsSyn(c) → CHECK(c, c⊥) ≠ 1)`. Here
`CHECK` is the primitive recursive function `Check` of §1.6, and `IsSyn`
recognizes the numeric codes of codes (§6.3).

### 6.2 Step 1: λᶜᵉʳᵗ₀ interprets PA, provably in PA

**The system `H_PA`.**
- *Language:* `0`, `S`, `+`, `·`, `=`, variables `x, y, z, …`, and the
  connectives `→`, `⊥`, `∀`.
- *Abbreviations:* `¬φ := φ → ⊥`, and `∧`, `∨`, `∃` classically.

Its axiom schemes, for formulas `φ, ψ, χ`, variables `x, y, z`, a term `t`
and an atom `α`:

| | Scheme |
| --- | --- |
| A1 | `φ → (ψ → φ)` |
| A2 | `(φ → (ψ → χ)) → ((φ → ψ) → (φ → χ))` |
| DN | `((φ → ⊥) → ⊥) → φ` |
| A4 | `∀x φ → φ[t/x]`, where `t` is free for `x` in `φ` |
| A5 | `∀x (φ → ψ) → (φ → ∀x ψ)`, where `x` is not free in `φ` |
| E1 | `x = x` |
| E2 | `x = y → (α[x/z] → α[y/z])` |
| Q1–Q6 | `S x = 0 → ⊥`; `S x = S y → x = y`; `x + 0 = x`; `x + S y = S(x + y)`; `x · 0 = 0`; `x · S y = x · y + x` |
| IND | `φ[0/x] → (∀x (φ → φ[S x/x]) → ∀x φ)` |

Its rules are modus ponens (MP) and generalization (Gen).
- A1, A2, DN and MP give classical propositional logic in `→` and `⊥`.
- A4, A5 and Gen give the quantifier rules; E1 and E2, equality; Q1–Q6 and
  IND, arithmetic.
- So `H_PA` is an axiomatization of PA (*standard*).
- `0 = 1` means `0 = S 0`.

**Every formula is negative.** In `→`, `⊥`, `∀`, with decidable atoms, no
formula needs the double negations of the Gödel–Gentzen translation. So the
translation below is direct. *Earlier states of this proof* translated `∧`
and `∨`, which brought in pairing and projection obligations (review RR2-03).
Choosing the connectives removes them.

**The translation.** A variable `x` becomes the λᶜᵉʳᵗ₀ variable `v_x`, and:

| PA | λᶜᵉʳᵗ₀ |
| --- | --- |
| `0`, `S t`, `t + u`, `t · u` | `zero`, `succ ⟦t⟧`, `PLUS ⟦t⟧ ⟦u⟧`, `TIMES ⟦t⟧ ⟦u⟧` |
| `t = u` | `T(EQ ⟦t⟧ ⟦u⟧)` |
| `⊥` | `0` |
| `φ → ψ` | `⟦φ⟧ → ⟦ψ⟧`, a `Π` at `ω` |
| `∀x φ` | `Π(v_x :ω Nat). ⟦φ⟧` |
| a line `φ`, free variables `x₁ < … < x_k` in a fixed order | `⟦∀ᶜˡ φ⟧ := Π(v_{x₁} :ω Nat) ⋯ Π(v_{x_k} :ω Nat). ⟦φ⟧` |

`EQ`, `PLUS` and `TIMES` are closed terms, defined with two helpers:

```
ISZERO := λ(b :ω Nat). recN(tt, k y. ff, b)
PRED   := λ(b :ω Nat). recN(zero, k y. k, b)
EQ     := λ(a :ω Nat). recN(ISZERO, k h. λ(b :ω Nat). if ISZERO b then ff else h (PRED b), a)
PLUS   := λ(a :₁ Nat). λ(b :ω Nat). recN(a, k y. succ y, b)
TIMES  := λ(a :ω Nat). λ(b :ω Nat). recN(zero, k y. PLUS y a, b)
```

Two points about usages:
- *In `EQ`,* the recursive result `h` is used once, inside an ordinary `λ`,
  never inside a method.
- *`PLUS` takes its first argument at usage 1.* That argument is the
  recursion's base, which runs once. `TIMES`'s step passes it the recursive
  result `y`, which `recN` supplies at usage 1, and an `ω`-argument would
  scale `y` to `ω`. The implementation found this (§6.2, the mechanical
  check).

**They compute as the axioms need.** Each of the following holds between open
terms, by β and ι, through a common reduct:

```
EQ zero zero ≡ tt              EQ zero (succ b) ≡ ff            EQ (succ a) zero ≡ ff
EQ (succ a) (succ b) ≡ EQ a b  PLUS a zero ≡ a                  PLUS a (succ b) ≡ succ (PLUS a b)
TIMES a zero ≡ zero            TIMES a (succ b) ≡ PLUS (TIMES a b) a
```

**Substitution.** If `t` is free for `x` in `φ`, then `⟦φ[t/x]⟧` is
`⟦φ⟧[⟦t⟧/v_x]`, as syntax. *By induction on `φ`:* "free for" is exactly what
keeps `⟦φ⟧`'s binders from capturing `⟦t⟧`.

**Four closed lemmas**, all at budget 0:

1. **Reflexivity.** `REFL : Π(x :ω Nat). T(EQ x x)`:

       REFL := λ(x :ω Nat). recN_{x. T(EQ x x)}(⋆, k y. y, x)

   - The base: `T(EQ zero zero) ≡ 1`.
   - The step: `y : T(EQ k k) ≡ T(EQ (succ k) (succ k))`.
2. **Transport.** `TRANSPORT : Π(x :ω Nat). Q(x)`, where
   `Q(x) := Π(y :ω Nat). Π(f :ω Nat → Bool). T(EQ x y) → T(f x) → T(f y)`.
   It recurses on `x` at the *packaged* motive `!Q(x) := Σ(q :ω Q(x)). 1`:

       TRANSPORT := λ(x :ω Nat). let (q, u) = recN_{x. !Q(x)}((BASE, ⋆), x₁ h. let (q, u) = h in (STEP, ⋆), x) in q
       BASE := λy. recN_y(λf e a. a,        y₁ r. λf e a. abort e,                          y)
       STEP := λy. recN_y(λf e a. abort e,  y₁ r. λf e a. q y₁ (λz. f (succ z)) e a,       y)

   - *The zero cases:* the evidence has type `T(EQ zero (succ y₁))` or
     `T(EQ (succ x₁) zero)`, both `≡ 0`.
   - *STEP's successor case:*
     - `e : T(EQ (succ x₁) (succ y₁)) ≡ T(EQ x₁ y₁)`;
     - `a : T(f (succ x₁)) ≡ T((λz. f (succ z)) x₁)`;
     - the result, `T((λz. f (succ z)) y₁)`, `≡ T(f (succ y₁))`.
   - *Why the packaging:* the inner case analyses are `recN`s, and their
     methods are `ω`-scaled. They may use `q`, which the packaging returns at
     `ω`, but not `h`, which `recN` supplies at usage 1. That is the defect of
     the earlier outline (review RR2-04).
3. **Stability.** `STAB(φ) : ((⟦φ⟧ → 0) → 0) → ⟦φ⟧`, by recursion on `φ`,
   every binder at `ω`:

       STAB(s = t)  := elimBool_{z. ((T z → 0) → 0) → T z}(EQ ⟦s⟧ ⟦t⟧, λk. ⋆, λk. k (λw. w))
       STAB(⊥)      := λk. k (λw. w)
       STAB(φ → ψ)  := λk. λa. STAB(ψ) (λnb. k (λf. nb (f a)))
       STAB(∀x φ)   := λk. λv_x. STAB(φ) (λnb. k (λf. nb (f v_x)))

   For an atom, the branches use `T(tt) ≡ 1` and `T(ff) ≡ 0`.
4. **Induction.** It uses the packaged motive `Σ(p :ω ⟦φ⟧). 1`:

       λb. λst. λv_x. let (p, u) = recN_{x. Σ(p :ω ⟦φ⟧). 1}((b, ⋆), v_x h. let (p, u) = h in (st v_x p, ⋆), v_x) in p

   - The step uses `h` once.
   - It recovers `p` at `ω`, as often as `st` needs.
   - By the substitution fact, the base and the step have the motive's
     instances *as syntax*.

**The templates.** Each proof line `i` becomes a term `τ_i` of its translated
closure: a closed term for an axiom, and a term in the context of the earlier
lines for MP and Gen. Below, the leading `λ` over the closure's variables is
omitted, and every template is ascribed its closure type: conversion does the
rest.

| Line | Term |
| --- | --- |
| A1 | `λa b. a` |
| A2 | `λp q a. p a (q a)` |
| DN | `STAB(φ)` |
| A4 | `λp. p ⟦t′⟧`, where `t′` is `t` with every variable not free in the instance replaced by `0`; if `x` is not free in `φ`, the result type does not mention the argument |
| A5 | `λp a v_x. p v_x a` |
| E1 | `REFL v_x` |
| E2, with `α = (s = t)` | `λe p. TRANSPORT v_x v_y (λv_z. EQ ⟦s⟧ ⟦t⟧) e p` |
| Q1, Q2 | `λe. e` — the conversions give `T(EQ (succ x) zero) ≡ 0` and `T(EQ (succ x) (succ y)) ≡ T(EQ x y)` |
| Q3–Q6 | `REFL v_x`, `REFL (succ (PLUS v_x v_y))`, `REFL zero`, `REFL (PLUS (TIMES v_x v_y) v_x)`, by the conversions |
| IND | lemma 4 |
| MP, from lines `i : φ` and `j : φ → ψ` | `(L_j ā) (L_i ā′)`. Premise variables absent from `ψ` are instantiated at `zero`, since the premises are universally closed |
| Gen, from line `i : φ`, giving `∀x φ` | `λv_x. L_i v⃗` |
| a proof `φ₁ … φ_m` | `B₁`, where `B_i := (λ(L_i :ω ⟦∀ᶜˡ φ_i⟧). B_{i+1}) τ_i` and `B_{m+1} := L_m`. Each `τ_i` has type `⟦∀ᶜˡ φ_i⟧` in the context `L₁ :ω ⟦∀ᶜˡ φ₁⟧, …, L_{i−1} :ω ⟦∀ᶜˡ φ_{i−1}⟧`. The binders are at `ω` because a line may be cited more than once |
| a proof of `0 = 1` | its term, ascribed `0`: `T(EQ zero (succ zero)) ≡ T(ff) ≡ 0` |

**The mechanical check.** The translation is implemented (`lcert.pa`, ADR-0005;
its tests run in the extended suite).
- *Every scheme:* on randomly generated instances, it type-checks at budget
  0, at exactly the translated closure type, and Check accepts the
  derivation.
- *Whole proofs* translate too, including a proof of `∀x (x = x)` by the
  induction axiom.
- *The closing ascription* is checked on a hypothetical proof of `0 = 1`: a
  variable of type `T(EQ zero (succ zero))`, since PA has no such proof to
  translate.
- *What this is not:* a proof of the schemes, or of Proposition 6.1's
  statement inside PA. It is evidence that the templates are right.
- *Two defects found and fixed,* in this document's first version of the
  templates:
  - `PLUS`'s first argument had to be at usage 1;
  - A4 had to instantiate the variables of an irrelevant term.

> **Proposition 6.1.** The translation `tr`, from `H_PA`-proofs to codes, is
> primitive recursive, and
>
>     PA ⊢ ∀p (Prf_{H_PA}(p, ⌜0 = S 0⌝) → IsSyn(tr p) ∧ CHECK(tr p, c⊥) = 1).

*Proof.*
- **`tr` builds each derivation from fixed templates.** The judgments at the
  nodes are computed from the parameters — the formulas and terms — by the
  translation. At each Conv node, `tr` records the fixed chain its template
  prescribes: the steps of the conversions displayed above, or one β-step,
  as in E2.
  - The recursive parts — `STAB`, the formation subderivations of translated
    types, the closures — are defined by recursion on formulas.
  - So `tr` is primitive recursive.
  - The implementation computes chains by normalization instead. That yields
    different chains, equally valid.
- **PA proves correctness by induction on the length of `p`.** The statement:
  for every line `i`, the derivation `tr_i(p)` is valid, and concludes
  `L₁ :ω ⟦∀ᶜˡ φ₁⟧, …, L_{i−1} :ω ⟦∀ᶜˡ φ_{i−1}⟧ ⊢ τ_i :¹ ⟦∀ᶜˡ φ_i⟧`. It is a
  primitive recursive property of `p` and `i`. The cases:
  - **An axiom instance.** Validity is a conjunction, over the template's
    nodes, of Check's local conditions (§1.6). Each is an equation between
    primitive recursive functions of the parameters: the formulas and terms.
    - *A closed derivation with no parameters* — those of `EQ`, `PLUS`,
      `TIMES`, `REFL` and `TRANSPORT` — is valid by one true closed
      equation, which PA proves by S2.
    - *At a template's fixed nodes,* the equations hold by the definitions
      of the translation and the encoding. PA verifies them by unfolding
      those definitions.
    - *The parts that depend on a term or formula* — the typing of `⟦t⟧`,
      the formation of `⟦φ⟧` and of the closure, and `STAB(φ)` — PA proves
      valid by induction on the term or formula. For `STAB`, the inductive
      step has one case per clause displayed above:
      - an atom `s = t`: one `ElimBool` node on `EQ ⟦s⟧ ⟦t⟧`, whose branches
        use the fixed chains `T(tt) ⇝ 1` and `T(ff) ⇝ 0`;
      - `⊥`: a fixed closed derivation;
      - `φ → ψ` and `∀x φ`: fixed nodes around the derivation for `ψ`, or for
        `φ`, which the induction hypothesis makes valid.
    - *The substitution fact,* which A4, E2 and IND use, PA proves by
      induction on `φ`.
  - **MP.** The two instantiations agree. Both substitute `v_y` for each
    variable `y` of `ψ`, and `zero` for the others. So `⟦φ⟧[ā] = ⟦φ⟧[ā′]` as
    syntax, by the substitution fact. The new nodes' local conditions are then
    equations between primitive recursive functions of the premises'
    conclusions, which PA verifies.
  - **Gen.** Likewise: one `Lam` node, and the applications to `v⃗`.
  - **The binding of the lines.** `Lam` and `App` at `ω`. Each `τ_i`'s
    context holds only earlier lines, at `ω`, so scaling it by `ω` changes
    nothing.
  - **The last line of a proof of `0 = S 0`.** One more Conv node, with the
    fixed chain `T(EQ zero (succ zero)) ⇝ ⋯ ⇝ T(ff) ⇝ 0`.
- **Every derivation built is closed, at budget 0,** so its root context is
  `Θ₀`. ∎

The formalization is given at the level of its obligations. Each is an
unfolding of primitive recursive definitions, or an induction on codes, in PA.
The implementation exhibits every template node that the obligations are
about.

### 6.3 Step 2: arithmetic in finite types interprets `Δ`

**E-PA^ω.** Its types are built from `0` (the naturals) by `→` and `×`. Its
terms are Gödel's System T. It has equality at type 0, extensional equality
above it, full induction, and classical logic.

**Coding.**
- A finite tree over `L` is coded by a natural number, by a primitive
  recursive pairing in which subtrees have smaller codes.
- A certificate and its print have the same code: in the model the token
  carries no information, since `C(◇) = {◇}`. So `PRINT` is the identity on
  codes.
- The primitive recursive functions `NODES`, `IsSyn` (a tree code), `CHECK`,
  `NEG` (for `neg`) and `ROOTTYPE` are fixed. `ROOTTYPE(p)` is the type code
  at the root judgment of the derivation `p` encodes, or `0` if there is none.
- PA proves their recursion equations on tree codes. Among them:
  - `NODES` is `0` at a leaf, and
    `NODES(node(a, v₁, v₂)) = 1 + NODES(v₁) + NODES(v₂)`. This carries the
    footprint arithmetic of the `Node` and `ItR` cases, `1 + ‖v₁‖ + ‖v₂‖ = ‖v‖`,
    into E-PA^ω;
  - `CHECK` takes only the values `0` and `1`.

**The interpretation.**
- *Skeletons:* `Unit`, `Bool`, `Nat`, `Lbl`, `Syn`, `◇` and `R` become type
  `0`, each with a range predicate (`{0}`, `{0, 1}`, all, `{0, …, |L|−1}`,
  `IsSyn`, `{0}`, `IsSyn`). Then `σ → τ` becomes `σ̂ → τ̂`, and `σ × τ` becomes
  `σ̂ × τ̂`.
- *Terms:* a simply typed term `t` becomes a System T term `⟦t⟧`,
  compositionally. The clauses that are not the obvious ones:
  - `recN` is Gödel's recursor.
  - `recSyn` and `itR` are structural recursion on tree codes, definable in
    System T by course-of-values recursion. Their recursion equations are
    provable. At a node, `itR`'s equation applies the method to the token's
    value, `0`.
  - `caseLbl` is definition by cases on the label's number.
  - `node d a r₁ r₂` drops the token.
  - `print` is the identity, and `chk′` is `CHECK`.
  - `inspect_X r c (x e. t₁) (x e. t₂)` becomes: if `CHECK(⟦r⟧, ⟦c⟧) = 1`,
    then `⟦t₁⟧` at `x ↦ ⟦r⟧, e ↦ 0`, else `⟦t₂⟧` likewise.
  - **`reflect_D r e` becomes the default `dflt_D` for every `D`**,
    including `D = 0`. `H₁ …` becomes `0`, and `abort_A t` becomes
    `dflt_{skel A}`.
- *Why the defaults are allowed:* `reflect`, `H₁` and `abort` have no
  conversion rule (§1.5), so no equation constrains them (review F-02).

**Semantic types.** For the fixed `n`, and each type `A` occurring in `Δ`, the
E-PA^ω formula `V_k(A)[η](x)` transcribes §3.3:

| Type | Formula |
| --- | --- |
| `0`, `1`, `Bool`, `Nat`, `Lbl`, `Syn` | `⊥`; `x = 0`; `x ≤ 1`; `⊤`; `x < |L|`; `IsSyn(x)` |
| `◇` | `x = 0 ∧ k ≥ 1` |
| `R` | `IsSyn(x) ∧ NODES(x) ≤ k` |
| `T(b)` | `x = 0 ∧ ⟦b⟧[η] = 1` |
| `Π` at `0`, `1` and `ω`; `Σ` at `0`, `1` and `ω` | the clauses of §3.3, with `∀a ∈ C(σ)` read as a quantifier over `σ̂` restricted by the range predicate, and `∃j ≤ k` bounded |

The environment formula `Env_k(Γ)[η]` transcribes §3.4. Everything is a
formula of E-PA^ω:
- the budget `n` is a numeral;
- the footprints are number variables, bounded as in `∃j ≤ k` and
  `∀j ≤ n − k`;
- the rest is quantifiers over finite types.

**Lemma 6.2 (for the types and conversions of `Δ`, in E-PA^ω).**
1. **Monotonicity:** `V_k(A) → V_{k′}(A)` for `k ≤ k′ ≤ n`, by induction on
   `A`.
2. **Substitution:** `⟦s[u/x]⟧` is the System T term `⟦s⟧[⟦u⟧/x]`, by
   induction on `s`. So `V(B[u/x])[η] ↔ V(B)[η, ⟦u⟧[η]]`.
3. **Conversion:** for every step `t ⇝ t′` between skeleton-typed terms,
   E-PA^ω proves `⟦t⟧ = ⟦t′⟧` at their type.
   - β, by System T's equations.
   - ι, by the equations of the recursors, of course-of-values recursion, and
     of definition by cases.
   - δ: `⟦chk′ c d⟧` is `CHECK(c̄, d̄)`. With `b = Check(c, d)`, the
     equation `CHECK(c̄, d̄) = b̄` is true by computation. So PA proves it
     (S2), and so does E-PA^ω. `Δ` has finitely many such steps.
   - T, by `V(T(b))`'s clause.
   - Under binders, by extensionality.

   So `V(A) ↔ V(B)` along every chain of `Δ`. The chains' skeleton typing
   (§1.5) is what gives every element a translation (review R4-01). ∎

**Lemma 6.3 (the bounded facts).** PA proves:

    BF₀  ∀v (IsSyn(v) ∧ NODES(v) ≤ n → CHECK(v, c⊥) ≠ 1)
    BF₁  ∀v ∀w ∀c (IsSyn(v) ∧ IsSyn(w) ∧ NODES(v) + NODES(w) ≤ n →
                    ¬(CHECK(v, c) = 1 ∧ CHECK(w, NEG(c)) = 1))

*Proof.*
1. From Check's condition 4 (§1.6), PA proves that `CHECK(p, d) = 1` implies
   `d = ROOTTYPE(p)`. So in BF₁, `c` may be replaced by `ROOTTYPE(v)` (review
   RR2-07).
2. PA proves that the codes `v` with `IsSyn(v)` and `NODES(v) ≤ n` are the
   finitely many listed ones: `v = v̄₁ ∨ ⋯ ∨ v = v̄_M`.
   - *This sentence is Π₁, so S2 does not give it.* Its PA-proof is built by
     induction on `n`, outside PA.
   - A code with `NODES(v) = 0` is a leaf, with one of the `|L|` labels.
   - A code with `NODES(v) ≤ n + 1` is a leaf, or `node(a, v₁, v₂)` with `a` a
     label and `NODES(v₁) + NODES(v₂) ≤ n`. So `v₁` and `v₂` are listed at
     the bound `n`.
   - Each step uses only the recursion equations of `IsSyn` and `NODES`.
3. So PA derives BF₀ and BF₁ from finitely many instances, one per listed code
   or pair of codes. Since `CHECK` takes only the values `0` and `1`, each
   instance follows propositionally from equations `CHECK(v̄, d̄) = 0`:
   - for BF₀, `CHECK(v̄ᵢ, c⊥) = 0`, for each `i`;
   - for BF₁, `CHECK(v̄ᵢ, ROOTTYPE(v̄ᵢ)) = 0` or
     `CHECK(w̄ⱼ, NEG(ROOTTYPE(v̄ᵢ))) = 0`, whichever is true, for each pair.
4. Those equations are true, by Corollary 3.7: no code checks as a
   refutation, and no two codes check as a type and its negation. They are
   closed equations between primitive recursive terms, so PA proves them by
   S2. ∎

**Lemma 6.4 (the fundamental lemma for `Δ`, in E-PA^ω).** For every node
`Γ ⊢ s :¹ A` of `Δ`:

    E-PA^ω ⊢ ∀η ∀k⃗ (Env_k⃗(Γ)[η] → V_{Σk⃗}(A)[η](⟦s⟧[η])).

*Proof.* By induction on `Δ`, at the meta level. Each case is the
corresponding case of Lemma 3.6, carried out in E-PA^ω with Lemma 6.2, except
for these:
- **Reflect at `D ≠ 0`.** `dflt_D` satisfies `V(D)`: `⋆` at `1`, `ff` at
  `Bool`, `0` at `Nat`, `ℓ₀` at `Lbl`, `sleaf ℓ₀` at `Syn`, and `leaf ℓ₀` at
  `R`, which has no nodes and so lies in every `V_k(R)`.
- **Reflect at `D = 0`, which is `H`.** The premises give
  `NODES(⟦r⟧) ≤ k₁ ≤ n` and `CHECK(⟦r⟧, c⊥) = 1`, contradicting BF₀. The case
  is vacuous.
- **H₁.** The same, with BF₁.
- **No outer induction on budgets** is needed, since no case descends to a
  smaller budget. `RecN`, `RecSyn` and `ItR` use induction in E-PA^ω on the
  scrutinee: on numbers, or by course-of-values induction on tree codes.
- **Inspect** splits on whether `CHECK(⟦r⟧, ⟦c⟧) = 1`. The evidence `0`
  satisfies `V(T(chk′ (print x) c))` in the first branch, and
  `V(T(not …))` in the second.

The environment and footprint bookkeeping are those of §3.4–3.5, with the
footprints as number variables and `n` a numeral. ∎

**Corollary 6.5.** From `Δ`, E-PA^ω proves `Con_λ`.

*Proof.* Apply Lemma 6.4 at `Δ`'s root, with every token mapped to `0` and
footprint `n`. The conclusion `V(Con′_ω)(⟦t⟧)` unfolds to
`∀c (IsSyn(c) → ∀e (e = 0 ∧ CHECK(c, c⊥) = 1 → ⊥))`, which gives `Con_λ`. ∎

### 6.4 The conclusion

1. By S3, Corollary 6.5 gives PA ⊢ `Con_λ`.
2. By Proposition 6.1, PA ⊢ `Con_λ → Con_PA`: a proof `p` of `0 = S 0` would
   give `IsSyn(tr p) ∧ CHECK(tr p, c⊥) = 1`.
3. So PA ⊢ `Con_PA`.
4. PA is consistent, being true in ℕ, so this contradicts S1.

Hence no `Δ` exists. ∎

**Consequences.** Proposition 4.8, that no budget derives `H° → Con′_ω`, is
now proved as well.

**What changed, and why the proof is now complete.**
- **An outline until 2026-09-26.** Two review rounds found its obligations
  incompletely stated (RR2-03 to RR2-07).
- **The completion:**
  - it chooses PA's connectives so that no negative-translation machinery is
    needed;
  - it gives every template of Step 1 explicitly, and checks them
    mechanically;
  - it moves Step 2 from a hand-built realizability model inside PA to E-PA^ω,
    which interprets the calculus directly. The price is citing S3;
  - it interprets `reflect` by defaults throughout, so that Step 2 needs no
    induction on budgets.
- **Its level of detail.** It is complete as a paper proof: every obligation
  is stated, and proved or reduced to S1–S3. Two formalizations inside
  arithmetic are given at the level of their obligations, not symbol by
  symbol: Proposition 6.1, and Lemma 6.4. Nothing is formalized in a proof
  assistant.

## 7. What is mechanized

The implementation (ADR-0005, directory `code/lcert` at the repository root)
mechanizes two layers.

**The Ansatz kernel.** Ansatz is a Lean-4-compatible kernel for Clojure. It
checked the following, with its bundled Init environment only, offline:

| Theorem | Statement |
| --- | --- |
| `ctxlen_le_nodes` | for every code tree `t`, the length of its first-child spine is at most its number of internal nodes (E3's counting fact) |
| `fstlen_le_nodes` | the same for the first child's spine |
| `strict_overhead` | **Lemma 2.7**: every derivation-shaped code `cell l true j p` has `budget < nodes` |
| `h1_budget` | `m₁ < k₁`, `m₂ < k₂` and `k₁ + k₂ ≤ n` give `m₁ + m₂ < n`: the descent of the H₁ case of Lemma 3.6 |
| `not_cert_nodes`, `not_cert_budget` | by kernel computation: the draft's §2.7 certificate of `not` has 35 nodes and budget 0 |

Codes are an inductive type over a field-less terminator, because Ansatz does
not accept leaf constructors with fields. So the theorems are about codes
rendered as `CT` trees. `lcert.kernel` makes the rendering, and a test checks
it against the plain measures on every code the implementation produces.

**The language.** The rest of the metatheory is not mechanized in a proof
assistant. It is implemented and tested:
- every rule of §1.4, in a type checker that builds explicit derivations, and
  again, independently, in `Check`;
- the encoding, whose properties E3–E5 are tested;
- the erasing evaluator of §5, with Theorem 5.2 checked by a probe on
  programs that carry `abort`, `H₁` and `H`;
- Propositions 4.2, 4.9 at depth 0 and 4.10, and the destructor of §4.7, as
  programs that type check and run.

What the tests establish is agreement between two independent checkers, and
with the draft's worked example. That is evidence for the rule table, not a
proof of the theorems of §3.

## 8. Review record

**Round 1, 2026-09-26.** Two independent read-only reviews of this document
and the draft, as of commit `9931914`:
- codex, model gpt-6-astra at maximum reasoning effort;
- Cursor's agent, model gpt-5.6-sol-xhigh.

Their reports are in the session scratchpad and are not committed; the
findings are summarized here.

**What neither found:** a refutation of T1, T2 or T3. Both checked, and
reported sound:
- the model's carriers and footprint clauses, monotonicity and splitting;
- every case of Lemma 3.6, including H₁ and Reflect;
- the non-circularity of the outer induction;
- `Check`'s well-foundedness;
- Propositions 4.1–4.5, 4.7 and 4.9.

| ID | Severity | Finding | Disposition |
| --- | --- | --- | --- |
| R4-01 | major | Conversion chains may pass through untyped expressions, which the model cannot interpret | fixed: chains must be skeleton-typed (§1.5, §1.6), and Lemma 3.2 uses it |
| R4-02 | major | `H°` follows from `H₁` inside the calculus at constant budget, refuting this document's earlier §4.9 | accepted: Proposition 4.10; the draft's §2.5 is corrected back |
| R4-03 | major | A computational destructor for `R` is definable, refuting the earlier §4.7 | accepted: §4.7 gives the construction; internal correctness of parsing stays open |
| R4-04 | major | The non-erasing evaluator builds `N`-node trees from one token in erased positions | accepted: §5 adds the erasing evaluator and scopes the resource reading to it |
| R4-05 | major | Defaults must be computable for Theorem 4 | fixed: explicit defaults (§3.1) |
| R4-06 | major | A one-node composition contradicts E2–E3 | accepted: §4.5 corrected; a relative encoding must replace E2–E3 |
| R4-07 | minor | Conjecture 4.6 as stated has a counterexample (`A = B = 1`) | fixed: restated with a budget uniform in `A` and `B` |
| R4-08, F-01 | major | PA induction by direct `recN` violates the usage-1 hypothesis | fixed: the `!P` packaging (§6, Step 1) |
| R4-09, F-02 | major | The two bounded facts do not verify `reflect_R` in the PA model | fixed: `reflect_D` for `D ≠ 0` is interpreted by defaults in that model (§6, Step 2) |
| F-03 | major | "`H₁` and `abort` are dead code" was asserted without proof | downgraded to an expectation (Corollary 5.1); proved on 2026-09-26 (Theorem 5.2) |
| F-04 | minor | Formation premises' contexts were unrelated to the conclusion's | fixed: the context convention (§1.4) |
| F-05 | minor | Excluded middle at `◇` depends on which negation is meant | fixed in the correction table (§9) and the draft's §8.3 |

**Two of these refute corrections made in round 0**, the writing of this
document: R4-02 and R4-03. Round 0 had "corrected" the draft on the relation
between `H` and `H₁`, and on the need for a destructor. Both corrections are
withdrawn.

**Round 2, 2026-09-26.** Cursor's agent (gpt-5.6-sol-xhigh), on the text as
of commit `0a51bf1`. A codex run stopped early: its workspace ran out of
credits.

**What it did not find:** a refutation of T1–T3. It confirmed that round 1's
fixes close their findings, among them the skeleton-typed chains,
Proposition 4.10, the destructor, the `!P` packaging and the default
interpretation of `reflect` in P5.

| ID | Severity | Finding | Disposition |
| --- | --- | --- | --- |
| RR2-01 | major | The erasing evaluator's result is not simply typed, so Theorem 4's relation cannot cover it | already addressed: commit `31d9fd0`, made before this review arrived, proved Theorem 4′ with a type-indexed relation of its own; the definition now says so |
| RR2-02 | minor | Theorem 4's relation omitted products | fixed |
| RR2-03 | major | P5's conjunction and stability steps ignored that `Let` gives a `Σ`'s second component at usage 1 | fixed in the outline: re-eliminating projections, and stability lifted structurally |
| RR2-04 | major | P5's transport by nested `recN` was not usage-correct | fixed in the outline: `!P` for both inductions, symmetry, structural lifting |
| RR2-05 | major | P5's claim that PA proves `tr` correct was unsupported | accepted: P5 is now labelled an outline, with the obligations listed |
| RR2-06 | major | P5's PA model is a new realizability model, not "routine" | accepted: restated, with its obligations |
| RR2-07 | minor | The bounded `H₁` fact needs target uniqueness proved in PA | accepted: stated as an obligation |
| RR2-08 | minor | `◇` has no closed program, so a default need not be one | fixed (§3.1) |
| RR2-09 | minor | The draft's §7 still said a relative encoding keeps E1–E5 | fixed in the draft |
| RR2-10 | major | The destructor alone does not show that runtime codes can be parsed | resolved constructively: a typed parser is given (§4.7), implemented and tested |
| RR2-11 | major | The draft's §8.1 called the `H` branch "dead code" | fixed in the draft: semantically impossible; operationally unproved. Since proved (Theorem 5.2) |
| RR2-12 | minor | The draft's §6 said certificates "compose linearly" | fixed in the draft |
| RR2-13 | minor | Conjecture 4.6's "in particular" does not follow from it | fixed: recast as motivation |

**Implementation review, 2026-09-26.** Grok 4.7, through Cursor, reviewed
the implementation (ADR-0005) against §§1.4–1.6 and §5. Two earlier attempts
did not complete:
- one on gpt-5.6-sol was stopped by the provider's safety filter, over the
  prompt's wording;
- a reworded one hit that model's monthly usage limit.

**What it found:**
- *Nothing critical.* `Check` accepts no code the rule table rejects. The type
  checker, the evaluator and the kernel measures follow the specification.
- *Three minor mismatches*, all fixed with tests first:
  - `abort`, `H₁` and `reflect`'s evidence now evaluate their arguments, as
    call-by-value requires;
  - default tokens are now distinct objects, and the runtime check's blind
    spot — closures — is documented;
  - a pathologically deep code is now rejected instead of overflowing the
    stack.

**Round 3, 2026-09-26.** Grok 4.7, through Cursor, reviewed the completed §6
(P5), read-only, with `lcert.pa` and its tests. The codex CLI and gpt-5.6-sol
were unavailable, for the reasons above.

**What it did not find:** a flaw in the reduction. It concluded that
"Proposition 5 is proved". It checked `H_PA` as an axiomatization of PA, the
usages of every template against §1.4 and against `lcert.pa`, the default
interpretation in E-PA^ω, Lemmas 6.2–6.4, and the direction in which S1–S3
are applied.

| ID | Severity | Finding | Disposition |
| --- | --- | --- | --- |
| P5-61-CASES | minor | Proposition 6.1's proof names the inductions on formulas without writing them out: `STAB`'s cases, and MP's instantiation at `zero` | fixed: written out. A closed derivation with no parameters is one S2 instance |
| P5-63-ENUM | minor | Lemma 6.3: the enumeration of small codes is not an instance of S2, and BF₁'s instances are not equations | fixed, with one correction to the review, which called the enumeration a true Δ₀ sentence. It is Π₁, and is proved by an induction on `n` outside PA. The instances follow propositionally from equations `= 0`, since `CHECK` takes only the values `0` and `1` |
| P5-S3-ROUTE | minor | S3's usual route was described through a semantic model, which shows truth, not provability | fixed: a formal interpretation in HA, then the negative translation |
| P5-INTRO | minor | "Two translations, each checkable inside PA" misdescribes step 2, which is carried out in E-PA^ω | fixed |
| P5-LET | minor | The proof row used `let`, which in the calculus is pair elimination. Lines are bound by `λ` at `ω` | fixed. This document's own re-reading also found MP and Gen lines called closed, though they refer to earlier lines; fixed with it, here and in `lcert.pa` |
| P5-SEMI | minor | §1.1's product table omits `ω · 0` and `ω · 1` | fixed: both operations are commutative. The implementation already computed them so |
| P5-NODES | minor | The equation `NODES(node(a, v₁, v₂)) = 1 + NODES(v₁) + NODES(v₂)`, which `ItR`'s footprint arithmetic needs in E-PA^ω, is not stated | fixed |
| P5-BOUNDS | minor | "The bounds are numerals", though the footprints are variables | fixed |
| P5-S2-WORD | minor | Lemma 6.2's δ clause credited S2 with the truth of a computation | fixed |
| P5-DFLT | minor | The defaults listed for `reflect` did not name `sleaf ℓ₀` | fixed |
| P5-MECH | minor | "Step 1 is mechanically checked" overstates what the tests check | fixed: §0 and §6.2 say what the tests check, and what they do not |

**Round 4, 2026-09-26.** Grok 4.7, through Cursor, reviewed Theorem 5.2,
read-only, with the evaluator and its tests.

**What it did not find:** a counterexample. Its verdict: "Theorem 5.2 is not
shown false." It found the new vacuity argument sound — `abort` by
`S(0) = ∅`, `H₁` and `H` by Corollary 3.7 at every size — and confirmed that
erasure keeps every premise the vacuous cases use. It also confirmed that the
probe is placed and tested as described.

| ID | Severity | Finding | Disposition |
| --- | --- | --- | --- |
| T52-01 | minor | Invariance of `S` under conversion and substitution needs an induction on the type, not only Lemmas 3.1–3.2 | fixed |
| T52-02 | major | The "standard cases" were delegated to Theorem 4, whose relation has no safety conjunct, and how safety composes through a node's trace was not said | fixed: every case written out, with traces |
| T52-03 | minor | In `reflect`, the trees are related, not equal; Lemmas 2.6–2.8 were not cited; `r` and `e` are evaluated first | fixed |
| T52-04 | major | For the erasing evaluator, "the other cases are unchanged" is false: `Lam` and `Let` at usage 0, the split of contexts, and `reflect`'s erased run all differ; one citation overshot | fixed: those cases written out, with a restriction fact. Theorem 4′ had the same gap in its environments, and has the same repair |
| T52-05 | minor | Two test programs did not put their node where a faulty evaluator would reach it; no test ran a dead `abort` through `reflect`; the mutation claim was not a test | fixed: replaced by a check of a type and its negation holding `H₁`, and by a certified program with a dead `abort` run through `reflect`. The mutation checks are stated as done by hand |

## 9. Corrections this document makes to the draft

| Draft location | Correction | Where |
| --- | --- | --- |
| §2.5 `H₁`; §5 `H°` and `Con′` | evidence arrows `⊸`, not `→` | §1.7 |
| §2.5 "`H` follows from `H₁` only at extra token cost" | right in substance; the cost is constant, through a fixed certificate of `0 ⊸ 0`. *Round 0 wrongly corrected this to "only outside the calculus"; review R4-02* | §4.9 |
| §4 D2 row, `comp : □(A → B) ⊗ □A ⊸ □B` | `→` should be `⊸`. A budget uniform in `A` and `B` is conjectured not to exist. One-node composition is impossible under E2–E3; a relative encoding must replace them | §4.5 |
| §4 D3 and contraction rows, "must record the literal term that rebuilds `r`" | the argument is replaced: any certificate of `□A` has more than `2μ(A)` nodes. The conclusion holds uniformly; per instance, both are derivable | §§4.3–4.4 |
| §4 G2 row, "Standard; not verified here" | proved by reduction to G2 for PA, citing three standard results. The reduction was an outline until 2026-09-26 | §6 |
| §5 fact 4, "parse and then apply `H`" | the restricted form holds with no tokens, by case analysis. The parse is definable, through a computational destructor. What is missing is an internal proof that it prints back to its input. *Round 0 wrongly said a destructor was lacking; review R4-03* | §4.7 |
| §3 L1–L4, conditional theorem | the theorem is proved without them | §3 |
| §6 "the only way to hold a certificate of `N` nodes at runtime is to have spent `N` tokens" | true of runtime positions under the erasing evaluator (Theorem 4′). Under the non-erasing one, an erased position can hold an `N`-node tree built from one token | §5 |
| §8.2 certified self-evaluation, "conjectural" | proved for base data types, as an evaluation rule | §4.8, §5 |
| §8.3 "excluded middle … breaks canonicity and with it exhibition" | at `◇`- and `R`-free types, the model validates it once sums are added, so T1's proof survives; T4 does not, since the evaluator cannot decide it. At `◇`, the answer depends on the negation. With `¬A = A ⊸ 0`, neither disjunct of `◇ + ¬◇` has footprint 0 once `n ≥ 1`, so T1's proof does not cover it. With `¬A = A → 0`, the model validates it, vacuously (review F-05) | §3, §5 |
