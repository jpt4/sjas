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
> - P5 contains two steps marked *not carried out*; they are standard in kind.
> - One claim is marked as a conjecture (§4.5).
> - Everything else is proved here.
>
> Independent adversarial review is recorded in §8.

---

## 0. Results

| # | Statement | Where |
| --- | --- | --- |
| **T1** | **Consistency.** For no `n` is there a derivable `Θₙ ⊢ t :¹ 0` | §3.8 |
| **T2** | **Self-justification.** λᶜᵉʳᵗ₀ has closed inhabitants of its own consistency propositions `H°` and `H₁°`. With T1, both clauses of `Willard2016` Definition 3.4 hold, for this calculus's apparatus and certificate representation | §3.9 |
| **T3** | **Size soundness.** A runtime term's value has footprint at most the tokens its context supplies, and at most the number of distinct tokens it mentions | §3.7 |
| **T4** | **Termination and adequacy.** The budgeted evaluator terminates on every term and computes the model's value | §5 |
| **P5** | **The second incompleteness theorem applies to codes.** λᶜᵉʳᵗ₀ derives no form of `Con′`, with any budget | §6; reduction to Gödel's theorem for PA, with two standard steps not carried out |
| **§4** | The draft's derivability-condition table, settled: D1 per instance; D3 and boxed contraction per instance only; D2 conjectured not derivable internally; `Con′ → H°`; bounded `Con′`; certified evaluation for base data types | §4 |

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

`≡` is the reflexive, symmetric, transitive and compatible closure, on terms and
on types, of these steps:

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
`A = A₀, A₁, …, A_j = B`. Consecutive types differ by one step of §1.5, taken
in either direction.

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
encoding, so `nodes(c′) < nodes(c)`. Every other local test is a syntactic
comparison. **So `Check` is defined by recursion on `nodes(c)`. It is total
and primitive recursive.**

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

Every carrier is non-empty. Let `dflt_σ` be a fixed element of each. For
`v ∈ C(R)`, `‖v‖` is its number of internal nodes. `print` erases tokens, and
is a bijection from `C(R)` onto `C(Syn)`.

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
type level, where certificates are unbounded — the footprint cap leaves
finitely many trees to evaluate, and §6 relies on that.

**Lemma 3.1 (substitution).** `⟦t[u/x]⟧ⁿη = ⟦t⟧ⁿ(η, x ↦ ⟦u⟧ⁿη)`, and likewise
for types. *Proof:* induction on `t`. The term a `reflect` decodes is closed
except for its own tokens, so substitution never reaches it. ∎

**Lemma 3.2 (conversion invariance).** If `t ≡ t′`, then `⟦t⟧ⁿη = ⟦t′⟧ⁿη` for
every `n` and `η`.

*Proof.* Each step preserves denotation:
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

**Conjecture 4.6.** Under every encoding, no term of type
`□(A ⊸ B) ⊗ □A ⊸ □B` computes its output certificate from its inputs. In
particular, the application node over the two input certificates cannot be
given its evidence.

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
  - Under a relative encoding, one new node is expected to suffice, but **no
    relative encoding is specified**. Specifying one means keeping E1–E5,
    keeping `Check` structural, and keeping Lemma 2.7, which is all that T1
    uses.

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

*This corrects the draft's method.* The draft proposed to "parse and then apply
`H`". That would need two things the core lacks:
- a destructor for `R`, since `itR` cannot thread tokens into a recursion over
  a code;
- a proof that the parse prints back to its input, stated with the supply of
  tokens erased.

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

In the core, `H` is `reflect₀`, so the question does not arise there. The
draft said that `H` "follows from `H₁` only at extra token cost". That holds
outside the calculus:
- a refutation certificate yields certificates of `1 ⊸ 0` and of `1`;
- the first costs tokens growing with the certificate, because every judgment
  must be re-recorded with the new variable.

Inside the calculus the evidence obstacle of §4.5 recurs. The draft's sentence
is corrected accordingly.

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
> `⟦t⟧ⁿη`. Values are related at base skeletons by equality, and at `σ → τ`
> by sending related arguments to terminating, related results.

*Proof.* By strong induction on `n`, and within `n` by induction on `t`.
- **Standard cases.** For `λ`, application, pairs, the eliminators, and `recN`
  and `recSyn` over their scrutinee's value, this is Tait's argument for
  System T. *Standard, not verified here.*
- **`chk′`, `print` and `inspect`** terminate because `Check` is total.
- **`itR`** is structural in the tree.
- **`reflect`** calls `evalₘ` with `m < ‖v‖ ≤ n`. This is covered by the outer
  induction, and matches the model's clause.
- **`H₁` and `abort`** return the default, as the model does. ∎

**Corollary 4.10.** For `Θₙ ⊢ t :¹ D` with `D` a base data type, `evalₙ(t)`
terminates. Its result is the canonical form of an element of `Vⁿₙ(D)`. So
closed terms of data type evaluate to canonical values, which is the draft's
L3 for data.

`H₁` and `abort` are moreover dead code in typed programs. Wherever evaluation
reaches one, the model would give its context a satisfying environment, and
the vacuous cases of Lemma 3.6 exclude that. Making this precise needs a
relation that pairs Theorem 4's with Lemma 3.6's. It is not written out.

**What is not proved:** that an *erasing* evaluator, which skips usage-0
positions, agrees with `evalₙ` on data results. That is the standard
erasure-correctness property of QTT, *standard, not verified here*. The
implementation uses `evalₙ` itself.

## 6. The second incompleteness theorem applies to codes (P5)

> **Proposition 5.** For no `n` and `t` is `Θₙ ⊢ t :¹ Con′_ω` derivable, where
> `Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0`. So no stronger form is derivable
> either: not `Con′` with its usage-1 quantifier, and not the `⊸` variants.

*Proof.* Suppose `Δ` derives `Θₙ ⊢ t :¹ Con′_ω`. We show that PA proves its own
consistency, which Gödel's second theorem forbids (*standard*), PA being
consistent.

**Step 1: PA embeds into λᶜᵉʳᵗ₀ at budget 0.**
- Take PA's Gödel–Gentzen negative translation, into the `∀ → ∧ ⊥`-fragment
  with decidable atoms (*standard*).
- Map `∀` to `Π(x :ω Nat)`, `→` to `→`, `∧` to `Σ` at `ω`, `⊥` to `0`, and
  `s = t` to `T(eqNat s t)`.
- `+`, `×` and `eqNat` are defined by `recN` at higher type.
- **Stability of atoms,** `((T(b) → 0) → 0) → T(b)`, is proved by `elimBool`
  on `b`.
- **Induction** is `recN` with a dependent motive.
- **Transport along `eqNat`**, for formulas, reduces to transport for
  `T(f x)` with `f :ω Nat → Bool`. That is proved by double induction,
  generalized over `f`.

This gives a primitive recursive translation `tr` of PA-proofs into budget-0
derivations. For proofs of `0 = 1`, it yields refutations, since
`T(eqNat 0 1) ≡ T(ff) ≡ 0`. PA proves:

    ∀p. Prf_PA(p, ⌜0=1⌝) → Check(tr p, c⊥) = tt

*Standard in kind; the formalization is not carried out here.*

**Step 2: PA proves `∀c. Check(c, c⊥) ≠ tt` from `Δ`.**
1. Formalize §3's model at the single budget `n`, for the finitely many types
   occurring in `Δ`. Use realizers — indices of recursive functions,
   hereditarily extensional at higher type — in place of set-theoretic
   functions.
2. Interpret `reflect` through a finite table over the trees with `‖v‖ ≤ n`.
   These are finitely many, and this is where the footprint cap (§3.2) is used.
3. PA verifies the H₁ and Reflect cases through two bounded statements, each a
   finite conjunction of closed primitive recursive equations:
   - no tree `v` with `‖v‖ ≤ n` is a refutation certificate;
   - no pair `v, w` with `‖v‖ + ‖w‖ ≤ n` is a contradictory pair. Their type
     code is a subtree of `print v`, so it is bounded too.

   Both are **true**, by Corollary 3.7, and PA proves true closed p.r.
   equations by computation.
4. The rest of the fundamental lemma, for `Δ`'s finitely many rule instances,
   is a routine induction inside PA. *Standard in kind; not carried out here.*
5. The conclusion at the type `Con′_ω` is
   `∀c. Check(c, c⊥) = tt → ⊥`, since `Syn` codes carry no footprint.

**Step 3.** Steps 1 and 2 give PA ⊢ `Con_PA`. Contradiction. ∎

*Why PA and not PRA, as the plan of 2026-09-25 had it:* λᶜᵉʳᵗ₀ defines every
function of Gödel's System T, and PRA cannot interpret it.

**What P5 rests on.** Two standard-in-kind formalizations, in Steps 1 and 2.
The new ingredient is proved here: self-reference constants at a fixed budget
reduce to finitely many true bounded facts. That is what keeps G2 from
colliding with T2.

## 7. What is mechanized

*To be filled when the Ansatz development lands.* The target is what Ansatz
supports today:
- proofs about Nat- and Bool-valued functions of user-defined trees go
  through;
- equation lemmas for tree-returning functions are not generated.

So the plan is:
- the code tree type;
- `nodes`, and the context length at the root, as Nat-valued functions;
- **Lemma 2.7** as a kernel-checked theorem;
- the `not` certificate's count of 35, by kernel computation.

The implementation's runtime uses the compiled, verified functions.

## 8. Review record

*To be filled with the independent adversarial reviews — codex and Cursor,
read-only — and each defect they find, with its disposition.*

## 9. Corrections this document makes to the draft

| Draft location | Correction | Where |
| --- | --- | --- |
| §2.5 `H₁`; §5 `H°` and `Con′` | evidence arrows `⊸`, not `→` | §1.7 |
| §2.5 "`H` follows from `H₁` only at extra token cost" | true only outside the calculus; inside, the evidence obstacle recurs | §4.9 |
| §4 D2 row, `comp : □(A → B) ⊗ □A ⊸ □B` | `→` should be `⊸`. A uniform internal `comp` is conjectured not derivable under any encoding. Constant cost under a relative encoding is conjectural until one is specified | §4.5 |
| §4 D3 and contraction rows, "must record the literal term that rebuilds `r`" | the argument is replaced: any certificate of `□A` has more than `2μ(A)` nodes. The conclusion holds uniformly; per instance, both are derivable | §§4.3–4.4 |
| §4 G2 row, "Standard; not verified here" | proved by reduction to G2 for PA, with two standard formalizations not carried out | §6 |
| §5 fact 4, "parse and then apply `H`" | the restricted form holds with no tokens, by case analysis; the parse route needs a destructor for `R` and an erased lemma | §4.7 |
| §3 L1–L4, conditional theorem | the theorem is proved without them | §3 |
| §8.2 certified self-evaluation, "conjectural" | proved for base data types, as an evaluation rule | §4.8, §5 |
| §8.3 "excluded middle … breaks canonicity and with it exhibition" | at `◇`- and `R`-free types the model validates it, once sums are added, so T1's proof survives; T4 does not, since the evaluator cannot decide it. At `◇` neither disjunct has footprint 0 in the model, so T1's proof does not cover it | §3, §5 |
