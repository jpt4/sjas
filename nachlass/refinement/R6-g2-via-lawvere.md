# Gödel's Second Incompleteness Theorem inside the Lawvere–Yanofsky scheme

*R6 of the Refinement stage, second document. Charter:
[`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md),
whose binding methodological order reads: "First prove G2 using Lawvere /
Yanofsky techniques — producing an explicit, numbered hypothesis list. Only then
determine where Willard's constructions invalidate particular hypotheses of that
limitative theorem." This document is the first half of that order.*

*Companion to [`lawvere-sjas.md`](lawvere-sjas.md), which **assumes** a selected
Gödel fixed point as a hypothesis of its Theorem 1.1. Here the fixed point is
produced by the scheme's own machinery, and the limitative conclusion is drawn
from its **absence**.*

> **Status, stated first because the contribution is easy to overstate.**
> The mathematics of §4 is **Löb's theorem**, restated in a Heyting algebra.
> Nothing about Löb's theorem is new here and none of it is claimed as new.
> What is new is its **placement**: Lemma 4.1 is exactly the supply of
> *fixed-point-free endofunctions* that Yanofsky's Theorem 1 needs at the G2
> slot and does not have, and supplying it puts G2 into the same shape as
> Cantor, Russell, Tarski and Turing in that paper — rather than leaving it,
> as Yanofsky does, on a list of things that "seem amenable."
>
> Every Yanofsky quotation below is verified against a rendered page image;
> register rows are in [`VERIFICATION.md`](VERIFICATION.md). Witness held at
> [`lit/yanofsky2003_universal_approach_self_referential_paradoxes_arxiv_math0305282.pdf`](lit/yanofsky2003_universal_approach_self_referential_paradoxes_arxiv_math0305282.pdf).
> §6's Kreisel route is **`unverified`**: no witness for it is held, and it is
> recorded because it is the route Yanofsky actually named, not because this
> stage has checked it.

---

## 0. Answer

**G2 is an instance of Yanofsky's Theorem 1 — the "Cantor" form of Lawvere's
fixed point theorem — applied to the endofunction**

`α(P) = ¬□P` on `Lind⁰`,

**which is the very endofunction his paper already uses for Gödel's *First*
theorem.** The two theorems are the two readings of one square. G1 reads it
forwards (Theorem 3: representability gives a fixed point, and external
consistency then shows the fixed point is undecided). G2 reads it backwards
(Theorem 1: no fixed point gives non-representability). What flips the reading
is precisely the assumption to be refuted:

> **Lemma 4.1.** If `□` satisfies the three derivability conditions and
> `□c ≤ c` for some `c ≠ ⊤`, then `y ↦ □y ⇒ c` has **no fixed point**.

At `c = ⊥` the hypothesis `□c ≤ c` *is* `⊢ Con`, and `c ≠ ⊤` *is* consistency.
So a consistent theory that proved its own consistency would make Yanofsky's
`α` fixed-point-free, Theorem 1 would declare the constructed `g` not
representable by `f`, and the diagonalization lemma exhibits its representative
`G(x) = ¬Prov(D(x))` anyway. That collision is G2.

Three things this buys that `lawvere-sjas.md` Theorem 1.1 did not:

1. **The fixed point is no longer a hypothesis.** It enters and leaves through
   the scheme's own Theorem 3, so the arithmetic strength it costs is visible
   and separately numbered (H-C-fn and H-C-pt below) rather than folded into
   "suppose `G ↔ ¬□G`".
2. **A numbered hypothesis list** (§5), with the point of consumption of each
   hypothesis marked in the proof — including the single step at which
   Beklemishev–Shamkanov's boxed contraction is used, and the single step at
   which Willard's Type-A condition (2) is used twice.
3. **Proposition 7.1**, the payoff for the SJAS question. The diagonal enters
   the list **twice**, and the two entries are different statements. **H-C-fn**
   — substitution is a definable *total function*, so that Yanofsky's own
   representative `G(x) = E(D(x))` can be written — is what `IS(A)` destroys.
   **H-C-pt** — for each formula separately, a provable biconditional at a fixed
   numeral — is what the scheme actually consumes, and Proposition 7.1 reduces
   even that to a *single* index. `IS(A)` retains the fixed-numeral regime by
   design, and `Willard2001` Theorem A.1's footnote 16 exhibits the one instance
   required. So the Lawvere hypothesis and the Willard failure **are not the
   same statement**, which is why breaking the latter does not touch the former.
   That is why the "attack the fixed point" cell of `R3-the-margin.md` is empty,
   and it is now a proposition rather than an observation.

Departure from the source, declared: Yanofsky's `Φ_E : Lind⁰ ⟶ Lind⁰` and his
`f : Lind¹ × Lind¹ ⟶ Lind⁰` are **not well defined as stated** (§2.3). The
repair is his own Theorem 2. The modal instance used here is well defined, and
what makes it so is the derivability conditions themselves (Lemma 3.2).

---

## 1. What Yanofsky leaves open, exactly

The paper's closing section is "Future Directions". Under the heading "The
following ideas are a little more 'spacey,'" the first bullet reads, in full
(printed p. 22, image-verified; the misspelling of Kreisel and the plural
"proofs" are the source's):

> "Gödel's second incompleteness theorem about the unprovability within
> arithmetic of the consistency of arithmetic. This theorem is a simple
> consequence of the first incompleteness theorem. However Kreisal has a direct
> model theoretic proofs that uses a diagonal method (see, e.g., page 860 of
> Smoryński's article in [1].) This proof seems amenable to our scheme."

Two things in that bullet are worth separating, because the second is the whole
difficulty and the first conceals it.

**"A simple consequence of the first incompleteness theorem" is not available
inside the scheme.** The scheme's G1 is the *positive* reading of the square: a
fixed point of `E(x) ≡ (∀y)¬Prov(y,x)` exists (Theorem 4), and it is then
argued *about the theory, from outside*, that a consistent and ω-consistent
theory can neither prove nor refute it. The step from there to G2 is the
formalization of that outside argument inside the theory — and the scheme as
Yanofsky leaves it has **no vocabulary for the inside/outside distinction at
all**. `Lind⁰` is a set of Lindenbaum classes; "provable" is its top element;
there is no operator on `Lind⁰` corresponding to `⊢` in which to run the
argument a second time. Supplying that operator, with exactly the properties the
second run needs, is what §3 does, and it is the entire content of the
enrichment.

**The Kreisel pointer is a different route, and a more expensive one.** It is
recorded and reconstructed in §6. It replaces the third derivability condition
by the arithmetized completeness theorem, which costs `IΣ₁`-or-more strength —
the exact cost `Willard2001` Theorem A.1 exists to avoid. For the SJAS purpose,
the route of §5 is the one that transfers and the Kreisel route is the one that
does not.

**One further bullet, on the same page, is the SJAS question in Yanofsky's own
words** — or nearly. The last bullet of the paper reads:

> "We worked out Gödel's first incompleteness theorem which showed that (using
> the language of the introduction) arithmetic can not completely talk about its
> own provability. What about Gödel's completeness theorem? Certain weak systems
> can completely talk about their own provability. Can this be stated as some
> type of fixed point theorem?"

*Read in context this is about the completeness theorem, not about
self-verifying systems; the SJAS reading is this document's, not Yanofsky's.*
With that declared: the sentence "Certain weak systems can completely talk about
their own provability" is, verbatim, the claim `Willard1993-TR` was written to
establish, and "Can this be stated as some type of fixed point theorem?" is
ADR-0004's question. §7 answers the half of it that this document reaches: it
can be stated as the **failure of a named hypothesis** of a fixed point theorem,
and the failure is not where a first guess puts it.

---

## 2. The scheme, as Yanofsky states it

Three results are used. All are quoted from page images.

### 2.1 The two forms

> **Theorem 1 (Cantor's Theorem)** *If `Y` is a set and there exists a function
> `α : Y ⟶ Y` without a fixed point (for all `y ∈ Y`, `α(y) ≠ y`), then for all
> sets `T` and for all functions `f : T × T ⟶ Y` there exists a function
> `g : T ⟶ Y` that is not representable by `f` i.e. such that for all `t ∈ T`
> `g(−) ≠ f(−,t)`.* (printed p. 5)

> **Theorem 3 (Diagonal Theorem)** *If `Y` is a set and there exists a set `T`
> and a function `f : T × T ⟶ Y` such that all functions `g : T ⟶ Y` are
> representable by `f` (there exists a `t ∈ T` such that `g(−) = f(−,t)`,) then
> all functions `α : Y ⟶ Y` have a fixed point.* (printed p. 14)

The proof of each is the same square: `g(t) := α(f(t,t))`, and evaluation of a
representation of `g` at its own representative. Theorem 3's hypothesis is
weakened at once by the source itself:

> **Remark 5** *It is important to note that the theorem uses a stronger
> hypothesis than the proof actually uses. The theorem asks that **all**
> `g : T ⟶ Y` be representable, however the proof only uses the fact that any
> `g` constructed in such a manner is representable. In the future, we shall use
> this fact and only require that constructed `g` be representable.* (printed
> p. 15)

Remark 5 is the hinge of §7. It is weakened once more there, from "every
constructed `g`" to "the one constructed `g` this proof builds".

### 2.2 The `β`-form

> **Theorem 2** *Let `Y` be a set, `α : Y ⟶ Y` a function without a fixed point,
> `T` and `S` sets and `β : T ⟶ S` a function that is onto (i.e., has a right
> inverse `β̄ : S ⟶ T`,) then for all functions `f : T × S ⟶ Y` the function
> `g_β : T ⟶ Y` constructed as follows … is not representable by `f`.*
> (printed p. 6; `g_β(t) = α(f(t, β(t)))`)

Yanofsky motivates Theorem 2 as a curiosity — the diagonal need not be *the*
diagonal, only every row need be touched. §2.3 shows it is not a curiosity: it
is what makes his own §5 instantiation well typed.

The positive reading of Theorem 2 is not stated in the source. It is needed
below, and is one line:

> **Theorem 3′ (Diagonal Theorem, `β`-form).** Let `β : T ⟶ S` be onto with
> right inverse `β̄`, let `f : T × S ⟶ Y`, let `α : Y ⟶ Y`, and suppose
> `g_β(t) := α(f(t,β(t)))` is representable by `f`, say `g_β(−) = f(−,s₀)`. Then
> `y₀ := f(β̄(s₀), s₀)` is a fixed point of `α`.
>
> *Proof.* `f(β̄(s₀),s₀) = g_β(β̄(s₀)) = α(f(β̄(s₀), β(β̄(s₀)))) = α(f(β̄(s₀),s₀))`,
> the last step by `β ∘ β̄ = Id_S`. ∎

(Yanofsky's printed proof of Theorem 2 writes `β(β̄(t₀))` for `β(β̄(s₀))` in the
middle line. A typographical slip; nothing depends on it.)

### 2.3 Departure Y1: the source's arithmetic instantiation is not well typed

Yanofsky's §5 sets `Lind^i` = the Lindenbaum classes of well-formed formulas
with `i` free variables, and defines (printed pp. 15–16)

`f : Lind¹ × Lind¹ ⟶ Lind⁰`, `f(B(x), H(y)) = H(⌜B(x)⌝)`, and
`Φ_E : Lind⁰ ⟶ Lind⁰`, `P ↦ Φ_E(P) = E(⌜P⌝)`.

**Neither is a function on Lindenbaum classes.** Both apply `⌜·⌝` to a class,
and `⌜·⌝` depends on the representative. Explicitly, in `PA` with
`E(x) ≡ "x is even"`: `0=0` and `(0=0) ∧ (0=0)` are the same element of `Lind⁰`
and have different Gödel numbers, so `Φ_E` sends one element of `Lind⁰` to two.
The same defect sits in `f`'s **first** argument; its second argument is sound,
since `⊢ ∀y(H(y) ↔ H'(y))` gives `⊢ H(⌜B⌝) ↔ H'(⌜B⌝)`.

**The repair is Theorem 2.** Take `T = Form¹`, the *formulas* with one free
variable; `S = Lind¹`; `β : Form¹ ⟶ Lind¹` the quotient map, onto, with right
inverse `β̄` = least-Gödel-number representative; and

`f : Form¹ × Lind¹ ⟶ Lind⁰`,  `f(B, [H]) := [H(⌜B⌝)]`,

which is well defined. Yanofsky's `△ = ⟨Id, Id⟩` becomes `⟨Id, β⟩`, and every
statement below is an instance of Theorems 1, 2 and 3′ rather than of Theorems
1 and 3. **This is a repair of the instantiation, not of the theorems**, and it
is made with the source's own generalization.

What remains to be checked, case by case, is that the `α` used is well defined
on `Lind⁰`. For the `E` this paper actually uses it is — but that is a property
of those `E`, not a general fact, and in the modal case it is exactly the
derivability conditions that supply it (Lemma 3.2).

---

## 3. The modal enrichment

Yanofsky twice warns that the set-level statement is too coarse and that the
objects carry structure the functions must preserve — "Perhaps `Y` and `T` are
sets with extra (algebraic) structure and functions between them are intended to
preserve that extra structure" (Remark 1, printed p. 5), and again at Remark 4.
The enrichment below is that warning taken literally at `Y = Lind⁰`.

### 3.1 The value object is a Heyting algebra

Let `α₀` be a theory with a classical (or intuitionistic) consequence relation
and let `Lind⁰ = Lind⁰(α₀)` be its Lindenbaum algebra of sentences: `[P] ≤ [Q]`
iff `α₀ ⊢ P ⊃ Q`; `⊤ = [0=0]`; `⊥ = [0=1]`; `∧`, `⇒` induced by the
connectives. The only structural fact used below is **residuation**,

`a ∧ b ≤ c` ⟺ `a ≤ b ⇒ c`,

which holds in any Heyting algebra and hence in any Lindenbaum algebra of a
theory with `⊃`-introduction and modus ponens. Classical negation is not needed
anywhere; `¬a` below abbreviates `a ⇒ ⊥`.

`⊤ ≤ a` says exactly `α₀ ⊢ a`. `⊥ ≠ ⊤` says exactly that `α₀` is consistent.

### 3.2 The modality

Fix a proof predicate and write `Prov(x) :≡ ∃y Prf(y,x)`. Define

`□ : Lind⁰ ⟶ Lind⁰`,  `□[P] := [Prov(⌜P⌝)]`.

`□` is `Φ_E` for `E(x) ≡ Prov(x)`, so Departure Y1 applies to it and
well-definedness must be earned. The three conditions are:

> **M1 (necessitation; Willard's condition (1); B–S L3).** `⊤ ≤ a ⟹ ⊤ ≤ □a`.
> **M2 (composition; Willard's condition (2); B–S L1; HBL 2).** `□(a ⇒ b) ∧ □a ≤ □b`.
> **M3 (four; Willard's condition (3); B–S L2; HBL 3).** `□a ≤ □□a`.

M2 is written **conjunctively**, matching `Willard2001` Theorem A.1's
condition (2) — `α ⊢ {Der(⌜Φ⌝) ∧ Der(⌜Φ ⊃ Ψ⌝)} ⊃ Der(⌜Ψ⌝)`, as displayed in
[`refined-sjas.md`](refined-sjas.md) §2.2. Written that way it needs a single
element to feed both conjuncts, and that is where contraction enters Lemma 4.1 —
at exactly one step.

**Lemma 3.1 (derived structure).** M1 and M2 give, for all `a, b ∈ Lind⁰`:
(i) *monotonicity*, `a ≤ b ⟹ □a ≤ □b`;
(ii) `□⊤ = ⊤`;
(iii) *lax meet preservation*, `□a ∧ □b ≤ □(a ∧ b)`.

*Proof.* (i) `a ≤ b` means `⊤ ≤ a ⇒ b`, so `⊤ ≤ □(a ⇒ b)` by M1, so
`□a = ⊤ ∧ □a ≤ □(a ⇒ b) ∧ □a ≤ □b` by M2. (ii) M1 at `a = ⊤`. (iii) `M1` on the
tautology `⊤ ≤ a ⇒ (b ⇒ (a ∧ b))` and M2 twice. ∎

**Lemma 3.2 (`□` is well defined; Departure Y1 discharged for this `α`).** Under
M1 and M2, `□` is a function on `Lind⁰`, and so is `α_c(y) := □y ⇒ c` for each
fixed `c ∈ Lind⁰`.

*Proof.* If `[P] = [Q]` then `[P] ≤ [Q]` and `[Q] ≤ [P]`, so `□[P] ≤ □[Q]` and
`□[Q] ≤ □[P]` by Lemma 3.1(i); hence `□[P] = □[Q]`. `⇒` and the constant `c` are
operations of the algebra, so `α_c` is well defined too. ∎

**This is the point of the enrichment.** For a general `E`, `Φ_E` is not a
function on `Lind⁰` at all (§2.3). For `E = Prov` it is, and what makes it one
is M1 and M2 — the same conditions that make the second, boxed run of the
diagonal argument go through. The scheme does not merely tolerate the modality;
the modality is what repairs the scheme at the one place it is applied.

---

## 4. The Löb obstruction

This is the lemma the scheme was missing. It is Löb's theorem (1955); the
statement below is its algebraic form, and the proof below is the standard one
with the point of consumption of each hypothesis marked.

> **Lemma 4.1 (Löb obstruction).** Let `Y` be a Heyting algebra and
> `□ : Y ⟶ Y` satisfy M1, M2, M3. Let `c ∈ Y` satisfy `□c ≤ c`. Then the
> endofunction
>
> `α_c : Y ⟶ Y`,  `α_c(y) = □y ⇒ c`
>
> has a fixed point **only if** `c = ⊤`.
>
> Equivalently — the form Theorem 1 consumes: **if `□c ≤ c` and `c ≠ ⊤`, then
> `α_c` has no fixed point.**

*Proof.* Suppose `γ ∈ Y` with `γ = □γ ⇒ c`.

| # | step | consumes |
| --- | --- | --- |
| 1 | `γ ∧ □γ ≤ c`, hence `⊤ ≤ (γ ∧ □γ) ⇒ c` | residuation (H-A) |
| 2 | `⊤ ≤ □((γ ∧ □γ) ⇒ c)` | **M1** |
| 3 | `□(γ ∧ □γ) ≤ □c` | **M2**, with 2 |
| 4 | `□γ ∧ □□γ ≤ □(γ ∧ □γ)` | Lemma 3.1(iii), i.e. **M1 + M2** |
| 5 | `□γ ≤ □γ ∧ □□γ` | **M3** and **contraction** (H-K) |
| 6 | `□γ ≤ □c ≤ c` | 3, 4, 5 and the hypothesis `□c ≤ c` |
| 7 | `⊤ ≤ □γ ⇒ c = γ`, i.e. `γ = ⊤` | residuation, 6 |
| 8 | `⊤ ≤ □γ` | **M1** again, on 7 |
| 9 | `⊤ ≤ c` | 6, 8 |

∎

Three observations about the proof, each of which is used later.

**(a) M1 is used twice, and the second use is the formalization step.** Step 2
runs the argument inside `□`; step 8 brings the result back out. This is the
inside/outside distinction §1 says the unenriched scheme cannot express. In
categorical terms, steps 2–4 are the image under `□` of the arrow that step 1
produces: **the derivability conditions are the statement that `□` transports
the Lawvere square**, and G2 is what you get by running that square once in the
algebra and once in its `□`-image.

**(b) Step 5 is the only place contraction is used**, and it is used because M2
is stated conjunctively: the single element `□γ` has to feed both arguments of
M2 at step 3–4. This is the same step that `lawvere-sjas.md` §1.4 isolates as
`BoxContr_G`, and the same one Beklemishev–Shamkanov's Proposition 3.8 consumes.
In a Heyting algebra `a ≤ a ∧ a` is free; in a symmetric monoidal (affine,
linear) consequence relation it is a separate structural assumption, and
[`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md)
§§1 and 2.3 records that `□`-contraction is a rule of the ambient consequence
relation, underneath all three derivability conditions rather than one of them.

**(c) M3 is used once, at a single instance** — `□γ ≤ □□γ` for the one element
`γ` produced by the diagonal, not for all `a`. §6's route removes even that.

**Corollary 4.2 (Löb's theorem).** Under M1–M3, `□c ≤ c ⟹ c = ⊤`, *provided*
`α_c` has a fixed point. The proviso is what §5 supplies from the scheme.

---

## 5. G2 as an instance of the scheme

### 5.1 The hypotheses, numbered

| tag | hypothesis | where it lives |
| --- | --- | --- |
| **H-A** | `Y = Lind⁰` is a Heyting algebra: `∧`, `⇒`, `⊤`, `⊥` with residuation | §3.1 |
| **H-B** | a set `Form¹` of one-variable formulas, a coding `⌜·⌝`, the quotient `β : Form¹ ⟶ Lind¹` with right inverse `β̄`, and the evaluation `f(B,[H]) = [H(⌜B⌝)]` | §2.3 |
| **H-C-fn** | *(one route to H-C-pt, and not itself consumed)* the diagonal `d(B) = ⌜B(⌜B⌝)⌝` is a definable **total function** `D` of the theory, so that Yanofsky's representative `G(x) = E(D(x))` can be written as a term | §2.1, §5.2 |
| **H-C-pt** | for every `B ∈ Form¹`, `⊢ G(⌜B⌝) ↔ ¬Prov(d̄(B))` — a *family of provable biconditionals, each at a fixed numeral*, which is what "`g_β` is representable by `f`" means in `Lind⁰` | §2.1 Remark 5, §5.2 |
| **H-D** | `□[P] = [Prov(⌜P⌝)]` is a well-defined endofunction of `Lind⁰` | Lemma 3.2 — *derived from M1, M2* |
| **H-M1** | `⊤ ≤ a ⟹ ⊤ ≤ □a` | §3.2 |
| **H-M2** | `□(a ⇒ b) ∧ □a ≤ □b` | §3.2 |
| **H-M3** | `□a ≤ □□a`, needed only at `a = γ` | §3.2 |
| **H-K** | contraction: `a ≤ a ∧ a`, needed only at `a = □γ` | Lemma 4.1 step 5 |
| **H-N** | `⊥ ≠ ⊤`, i.e. the theory is consistent | §3.1 |

Note what is **not** on the list: ω-consistency, Σ₁-soundness, and any
completeness or reflection principle. Yanofsky's G1 application is stated "in a
consistent and ω-consistent theory" (printed p. 16); the G2 instance needs only
H-N. The negative reading is the cheaper of the two.

**Why H-C is split, and why the split is the whole of §7.** Representability in
Yanofsky's sense is the equation `g_β(−) = f(−,s₀)` — a *family of equalities in
`Lind⁰`*, one per formula, each of them a provable biconditional at a fixed
numeral. It is a metatheoretic condition on a family. It is **not** the
internally quantified sentence `∀x ∃z Subst(x,z)`, and a theory may satisfy the
first while refuting the second. H-C-fn is one convenient way to secure H-C-pt —
Yanofsky's way, and the only way his §5 offers — but it is strictly stronger
than what the square consumes. Conflating the two is the error §7.1 exists to
prevent, and an earlier draft of this document committed it.

### 5.2 The theorem

> **Theorem 5.1 (G2 in the scheme, positive form).** Assume H-A, H-B, **H-C-pt**,
> H-M1, H-M2, H-M3, H-K. If `□⊥ ≤ ⊥` then `⊥ = ⊤`.
>
> Equivalently, with H-N: **a consistent theory satisfying the derivability
> conditions, and able to name its diagonal instances one at a time, does not
> prove `Con`.** H-C-fn is not assumed.

*Proof.* Take `α := α_⊥`, i.e. `α(y) = □y ⇒ ⊥ = ¬□y`. By Lemma 3.2 this is an
endofunction of `Lind⁰`. It is `Φ_E` for

`E(x) ≡ ¬Prov(x) ≡ (∀y)¬Prf(y,x)`,

**which is verbatim the `E(x)` Yanofsky uses for Gödel's First Incompleteness
Theorem** (printed p. 16).

Build Yanofsky's square in the repaired form of §2.3, writing `d(B) := ⌜B(⌜B⌝)⌝`
for the diagonal number of `B` and `d̄(B)` for its numeral:

`g_β : Form¹ ⟶ Lind⁰`,  `g_β(B) := α(f(B, β(B))) = α([B(⌜B⌝)]) = [¬Prov(d̄(B))]`.

What is needed now is a single `G ∈ Form¹` with `g_β(B) = f(B,[G]) = [G(⌜B⌝)]`
for every `B`. There are two ways to get one, and they are the two halves of the
split hypothesis.

- **Term form (H-C-fn), Yanofsky's own.** If the theory has a definable total
  function `D` with `⊢ D(⌜B⌝) = d̄(B)`, put `G(x) :≡ ¬Prov(D(x))`.
- **Relational form (H-C-pt).** If substitution is available only as a
  predicate `Subst(x,z)` — not as a function symbol — put

  `G(x) :≡ ∀z (Subst(x,z) ⊃ ¬Prov(z)) ∈ Form¹`.

  Then `[G(⌜B⌝)] = [¬Prov(d̄(B))]` follows for a *given* `B` from two
  fixed-numeral facts, `Subst(⌜B⌝, d̄(B))` and
  `∀z (Subst(⌜B⌝,z) ⊃ z = d̄(B))`. No totality statement is needed, because the
  square asks for one equation per `B`, never for a sentence quantifying over
  `B`.

**The relational route is Willard's own device, one level down.** It is not an
artifact of this document's reading of the corpus: the same move is what the
object language of the SJAS systems is built on, and `Willard2005` printed p. 5
performs it explicitly for *multiplication*. The U-Grounding signature has no
multiplication function symbol, and Willard says why in the vocabulary of §5.1's
split:

> "since our U-Grounding language has no function symbol for multiplication, our
> U-Grounding based axiom systems `α` **do not implicitly assume** that
> multiplication is a total function. However, they do certainly retain an
> ability to encode the graph `M(x,y,z)` of multiplication as a `Δ*₀` formula"

His Equation (4) is that graph,

`[ (x = 0 ∨ y = 0) ⇒ z = 0 ] ∧ [ (x ≠ 0 ∧ y ≠ 0) ⇒ ( z/x = y ∧ (z−1)/x < y ) ]`,

with `/` the total, **non-growth** Division grounding function: it says that `z`
is the least number whose quotient by `x` is `y`. Every operation in it
*inspects* a candidate `z`; none *produces* one. And the closing gloss names the
one thing given up:

> "Equation (4)'s formal encoding of `M(x,y,z)`'s graph as a `Δ*₀` formula will
> add a substantial level of semantic depth to our U-Grounding axiom systems `α`
> because it will imply that that these logics will have an understanding of
> multiplication's main generic properties (except of course for the
> *deliberately omitted* totality condition of "`∀x ∀y ∃z M(x,y,z)`".)"

**Replace a function by its graph, retain every instance, decline the totality.**
Equation (4) is that move in the object language; `G(x) :≡ ∀z (Subst(x,z) ⊃
¬Prov(z))` is the same move in the metalanguage. The correspondence is exact:
**H-C-fn is the totality condition, H-C-pt is the graph.** A function symbol
cannot be had without its totality — `f(x)` is a term and `∀x ∃y (y = f(x))` is
logically valid — which is why declining the symbol, and not merely declining an
axiom, is what these systems do. Willard's `Π*_n`/`Σ*_n` classes exist for the
same reason: on the same page he notes that conventional `Π_n`/`Σ_m` notation
"(with its multiplication function symbol) is suitable only for axiom systems
which recognize multiplication as a total function", because the bound `t` of a
bounded quantifier must be a **term**.

Either way `g_β` is representable by `f` at `s₀ := [G] ∈ Lind¹`, which is
H-C-pt. Theorem 3′ then yields a fixed point of `α` at

`γ := f(β̄(s₀), s₀) = [G(⌜G⌝)]`,  i.e.  `⊢ G(⌜G⌝) ↔ ¬Prov(⌜G(⌜G⌝)⌝)`.

Now apply Lemma 4.1 with `c = ⊥`. Its hypothesis `□⊥ ≤ ⊥` is the assumption;
`γ` is a fixed point of `α_⊥`; therefore `⊥ = ⊤`. ∎

### 5.3 The same theorem in Theorem 1's form

The point of §4 is that G2 can also be stated in the shape the rest of
Yanofsky's paper uses — a fixed-point-free `α`, and a `g` that consequently is
not representable.

> **Corollary 5.2 (G2 in the scheme, Cantor form).** Assume H-A, H-B, H-M1,
> H-M2, H-M3, H-K, H-N, and suppose `⊢ Con`, i.e. `□⊥ ≤ ⊥`. Then by Lemma 4.1
> the function `α(y) = ¬□y` has **no fixed point in `Lind⁰`**, and therefore, by
> Theorem 2, for every `f : Form¹ × Lind¹ ⟶ Lind⁰` the function
> `g_β(B) = α(f(B,β(B)))` is **not representable by `f`**.
>
> But for the evaluation `f` of H-B, `g_β` *is* representable, at `[G]`,
> whenever H-C-pt holds — by either route of §5.2. Hence H-C-pt, H-N and
> `⊢ Con` are jointly contradictory: **`⊬ Con`.**

The two corollaries are contrapositives of one another, exactly as Theorems 1
and 3 are contrapositives in the source. Corollary 5.2 is the one that places
G2 in the family: Cantor, Russell, Grelling, Turing, Tarski and now G2 are all
"exhibit a fixed-point-free `α`; conclude non-representability."

**What the family membership costs, said plainly.** For Cantor the
fixed-point-free `α` is free (`¬` on `2`). For Tarski it is free (`¬` on
`Lind⁰`, using consistency). For G2 it is **not** free: producing it is
Lemma 4.1, which is Löb's theorem, which itself consumes a fixed point supplied
by the same square. That is not circular — Lemma 4.1 is applied under the
reductio hypothesis and its own fixed point is the one Theorem 3′ produces — but
it is the honest reason G2 sat on Yanofsky's "spacey" list rather than in §3
with Russell and Grelling. **The scheme does not make G2 cheap. It makes the
price legible**, as a list of nine hypotheses of which exactly four are modal.

---

## 6. The Kreisel route Yanofsky named

> **Status: `unverified`.** No witness is held for Kreisel's proof, for
> Smoryński's Handbook article, or for the arithmetized completeness theorem in
> any form. Nothing below is image-verified. The reconstruction is standard
> textbook material and is recorded because it is the route the source actually
> points at, and because the trade it makes is decisive for §7.

The arithmetized completeness theorem (ACT) says, for `T` r.e. extending
`IΣ₁`: `T ⊢ Con(T) ⟶ "there is a definable model M ⊨ T"`, with a definable
satisfaction predicate `Sat_M` whose Tarski clauses `T` proves, and with the
standard part embedded as an initial segment. Two consequences, both provable
in `T`:

- *internal soundness*: `□π ⟶ Sat_M(⌜π⌝)`;
- *`Π₁` transfer down an initial segment*: `Sat_M(⌜π⌝) ⟶ π` for `π ∈ Π₁`.

Composing under `Con(T)` gives **provable local `Π₁`-reflection**:
`T ⊢ Con(T) ⟶ (□π ⟶ π)` for every `Π₁` sentence `π`.

Now the fixed point `γ = [G(⌜G⌝)]` of §5.2 is `Π₁` (`Prov` is `Σ₁`). Assuming
`T ⊢ Con(T)`, reflection gives `□γ ≤ γ`; with `γ = ¬□γ` this gives `□γ ≤ ¬□γ`,
hence `□γ = ⊥`, hence `γ = ⊤`, hence by M1 `□γ = ⊤` — so `⊥ = ⊤`.

**What changed.** M3 is gone, and M2 is used only inside ACT's own
formalization; reflection has taken over the work of step 5–6 of Lemma 4.1. What
paid for it is `IΣ₁`-or-more, plus the formalized completeness theorem. So the
Kreisel route does land in the scheme, in Theorem 1's form, with `α_⊥` made
fixed-point-free by reflection instead of by Four — and Yanofsky's "seems
amenable" is correct.

**And it is the wrong route for this repository.** `Willard2001` Theorem A.1
exists precisely to state the limitative result *without* assuming the theory
extends Peano Arithmetic — Willard's own gloss, image-verified in
`VERIFICATION.md`, is that it "differs from the classic Hilbert-Bernays theorem
only by not requiring `α` to be an extension of Peano Arithmetic". A route that
buys the removal of M3 with the arithmetized completeness theorem buys it at a
price no self-verifying system can pay. §5's route is the transferable one.

---

## 7. Which hypothesis a self-verifying system must break

This is ADR-0004's second half, and only its first move is made here: reading
the corpus's known breaches against §5.1's list. It is not a new claim about
Willard; it is the existing claims of `lawvere-sjas.md` and `refined-sjas.md`
relocated onto a hypothesis list that now includes the fixed point's own
provenance.

### 7.1 The substitution *function* is not a hypothesis of the theorem

> **Proposition 7.1 (fixed-point economy).** In the proof of Theorem 5.1:
>
> (i) **H-C-fn is not consumed at all.** It appears only as one of two routes to
> H-C-pt, and §5.2's relational route reaches H-C-pt without it.
>
> (ii) **H-C-pt is consumed at a single index.** Theorem 3′'s proof evaluates
> the representability equation only at `t = β̄(s₀)`; every other member of the
> family is inert. H-C-pt may therefore be replaced by the single hypothesis
>
> **H-C-1**: some `γ ∈ Lind⁰` satisfies `γ = ¬□γ`, i.e. `⊢ γ ↔ ¬Prov(⌜γ⌝)`,
>
> and the proof is unchanged.
>
> *Proof.* (i) Inspect §5.2: the two routes are alternatives and the relational
> one assumes no function symbol. (ii) Inspect Theorem 3′: its chain is
> `f(β̄(s₀),s₀) = g_β(β̄(s₀)) = α(f(β̄(s₀), s₀))`, which uses the
> representability equation at `β̄(s₀)` and nowhere else; §4 then takes `γ` as
> given. ∎

This is Yanofsky's Remark 5 taken two steps further than he takes it: from
*every* `g : T ⟶ Y` is representable, to *every constructed* `g` is (his step),
to *this one* `g` is, to *this one `g` at this one argument* is.

**A correction to this document's own earlier draft, recorded rather than
quietly amended.** The first version of this section said "`IS(A)` fails H-C and
supplies its conclusion anyway", with H-C stated as "the diagonal function `D`
… is representable in the theory". That sentence conflated two different
statements — the theory's internal `∀x ∃z Subst(x,z)` and the metatheorist's
family of fixed-numeral biconditionals — and so made the argument look like a
lucky escape from a hypothesis that had failed. It is not an escape. **The
hypothesis that fails in `IS(A)` was never on the list.**

**What `IS(A)` removes, from the page.** `Willard1993-TR` Appendix A introduces
substitution as a **relation** and never as a function — item (iii) of printed
p. 37 reads "`SUBST_i(x,y)` denotes Godel's classic substitution *relation*
which yields TRUE when …". The Point of Clarification on the same page then
states the failure and its scope in one breath:

> "Since it does not recognize multiplications as a function, `IS(A)` will
> clearly be too weak to prove that `∀x∃y SUBST_i(x,y)`. However, `IS(A)` will
> be able to prove for *any fixed integer* `k̄` that `∃y SUBST_i(k̄,y)`. It will
> turn out that the latter will be sufficient for `IS(A)` to formally define the
> *particular instance of the reflection principle* needed to define H."

That is **exactly** H-C-fn failing and H-C-pt surviving, in Willard's own words,
and the last sentence says he intends to use the survivor.

**And the diagonal number itself is constructed, named, and load-bearing.**
Printed p. 38 does the diagonalization in the ordinary way: `J` is the Gödel
number of Equation (A.1) — the formula carrying the free variable — and the
self-substitution instance is (A.2), the Group-3 axiom. Willard states its
defining property twice: the parenthesis at (A.3), "the only integer `z`
satisfying `SUBST_i(J,z)` in equations (A.2) & (A.3) is (A.2)'s Godel number",
and Clarifying Comment 1, "`K` denotes the *unique* Godel number satisfying
`SUBST_i(J,K)`". `K` is then the threshold separating proofs that use Group-3
from proofs that do not, which is what makes (A.3) a correct `Δ₀` definition of
`Prf_{IS(A)}`. Substitution appears throughout only under a **bounded**
quantifier with a fixed first argument — `∃z<y SUBST_i(J,z)` — which is the
relational form of §5.2 with the bound supplied by the encoding.

So the situation is not that Willard removes the diagonal number. He constructs
it, he needs it, and his system is built on its uniqueness. What he removes is
the *function*.

`Willard2001` Theorem A.1's footnote 16 then exhibits the H-C-1 instance in the
same relational shape: for `Theta(z) := ∀x ∀y (Subst(z,x) ⊃ ¬Derive(x,y))` and
`N = code(Theta)`, `α` proves `Theta(N) ↔ ¬Der(code(Theta(N)))` (image-verified,
`VERIFICATION.md`; Theorem A.1 itself is **`stated-only`**, and that status
governs every use of it here). Note the `∀`-relational `Theta`, not a
substitution term — the same device. The same footnote records that this
fixed-point identity is the *only* step of the argument said to need PA's `Π⁻₁`
theorems.

> **Corollary 7.2.** Losing the substitution **function** does not evade G2,
> because the scheme never asked for one. On the Lawvere side, the "attack the
> fixed point" cell of `R3-the-margin.md` is empty for a sharper reason than
> "removing the diagonal is hard": the only diagonal hypothesis the argument
> consumes is H-C-1, a single fixed-numeral biconditional, and the fixed-numeral
> regime is precisely what the weak theories keep — deliberately, because they
> need it to build their own Group-3 axiom.

**H-C-pt is not free, and the corpus's size analysis is the price.** It would be
easy to read "substitution is only a relation" as a cheap disclaimer. It is the
opposite. Once multiplication is not a total function, *existence of a number is
no longer free*, so every object the theory must be able to name — a proof code,
a tableau, the diagonal number `K` — has to be shown to lie inside the range the
theory can prove inhabited. **The bit-accounting is the existence argument**,
carried out object by object; there is no other way to secure a fixed-numeral
instance. `Willard1993-TR` printed p. 15 says so at the point where the encoding
is designed:

> "The symbol `ĉ[j]` will also have a quite standard meaning, it will designate
> a constant that represents the integer `j`. **If `j` is a very large number,
> then its repeated appearance in a proof could cause the proof to grow to an
> unacceptably large size.** Therefore, our notation will employ two special
> symbols to reduce the bit length of a proof."

and **Lemma 5.2** (status **`full`**) states the reason outright, in a
parenthesis with an exclamation mark:

> "(The reason for the new "`v#`" notation is that the two analogous mappings
> *onto* `⌜Φ(ĉ[j])⌝` *are unknown by* `IS(PA+)` *to be total functions* !)"

Expanding a long constant in place is precisely the operation whose totality is
unavailable, so the encoding never performs it: `u#`/`v#` are **pointers**, the
same page fixes integers at `⌊log₃₂ i⌋ + 3` bytes, and the whole scheme is built
"so as to achieve the ideal 'linear' bit compression". Linear is what a theory
without multiplication can certify. The exactitude about growth throughout the
corpus — the `θ` constants, `Log(q_β) ≥ 5J`, the `ω_i`/`ϖ_i` hierarchies — is
this same obligation discharged in later settings, and `Willard2001` Remark 3.6
records how little room there is: "the margin between the attainable and the
impossible is exactly one" (`Willard2001#Rem3.6`, status `cited`). A margin of
one cannot be measured loosely.

So the relation between §7.1 and the corpus's arithmetic is not that Willard
waived a hypothesis. **He paid for its surviving half by hand, instance by
instance**, and the payment schedule is the size analysis.

### 7.2 The list, against the three systems

| hypothesis | who breaks it | evidence and status |
| --- | --- | --- |
| **H-A** (cartesian/Heyting value object) | nobody in the comparison | — |
| **H-C-fn** (substitution as a total function) | **Willard `IS(A)`** | `Willard1993-TR` printed p. 37, image-verified: `∀x∃y SUBST_i(x,y)` unprovable, substitution used only as a relation. **But H-C-fn is not consumed by Theorem 5.1** (Prop. 7.1(i)), and it shares its mechanism with the H-M2 row — see §7.4 |
| **H-C-pt** (fixed-numeral biconditionals) | nobody in the comparison | `IS(A)` keeps it by design — `∃y SUBST_i(k̄,y)` "for any fixed integer `k̄`", printed p. 37 — and needs it to define its own Group-3 axiom |
| **H-M1** (necessitation) | nobody | — |
| **H-M2** (uniform composition) | **Willard Type-A `IS(A)`** | `Willard1993-TR` printed p. 12, image-verified: composition holds "only in the *degenerate case* where `x` and `y` are *fixed constants*". This is the breach, and §7.4 exhibits its mechanism — the composed proof's code is bounded by the **product** of the two input codes. |
| **H-M3** (four) | nobody in the comparison; §6's route dispenses with it | — |
| **H-K** (contraction on `□γ`) | **Beklemishev–Shamkanov `S`** | `lawvere-sjas.md` §§0, 3.3 and `R2-beklemishev-shamkanov-assessment.md` §§1, 2.3: `S` satisfies L1–L3 and lacks the boxed diagonal |
| **H-N** (`⊥ ≠ ⊤`) | nobody — it is the theorem's point | Willard proves external consistency separately |

The table reproduces `lawvere-sjas.md` §0's verdict with two rows added — the
split halves of the diagonal — and those rows carry the finding: **the half
`IS(A)` breaks is not a hypothesis of the theorem, and the half that is one it
keeps.** Willard's own frame requires the breach to be in the derivability
conditions ("every self verifying system must breach in some way one of the
three fundamental Hilbert-Bernays conditions", `Willard1993-TR` printed p. 1,
image-verified), and this document's hypothesis list is the first one in the
Refinement broad enough to have let the alternative be tested rather than
assumed.

### 7.3 An observation for follow-up, not a claim

`Willard2001` **Theorem 7.2** (proof status **`full`** in
`../codification/registry/results.md`) states that no consistent `α` with a
`Δ⁻₀` proof predicate, proving all `PAX`'s `Π⁻₁` theorems, can prove the
Canonical Reflection Principle for every `Π⁻₁` sentence — with fixed point
`Γ(g) = ∀x ∀h ≤ x SUBST(g,h) ⊃ ¬Prf^D_α(h,x)` and `Θ = Γ(n̄)`. Willard's stated
difference from Löb is that "we do not assume that `α` recognizes either
Addition or Multiplication as total functions", and his **Lemma 7.1**
(status `full`) replaces the derivability conditions outright with a *semantic*
closure property available to any `α`-sound and `α`-complete deduction method.

Read against §5.1: Theorem 7.2 has the shape of Lemma 4.1's conclusion —
`□c ≤ c ⟹ c = ⊤` — restricted to `Π⁻₁` `c`, with **H-M1–H-M3 replaced by a
semantic hypothesis** and **H-C-1 again supplied at a fixed numeral `n̄`**. If that
reading survives checking, Willard's §7 is a *fourth* row of §7.2's table:
neither the Kreisel substitution (§6) nor the Willard Type-A breach, but a third
way of paying for the fixed-point-free `α`.

**This is an observation about shapes, not an identification.** It has not been
checked against the page images, the quantifier structure of (62) has not been
compared with `α_c`, and no claim is made that the two theorems are the same
theorem. It is recorded as the obvious next question the hypothesis list raises.

### 7.4 One product, four sites — the mechanism, checked

*Directed check, 2026-09-07, against `Willard1993-TR` §§4, 5, 7 and Appendix A.
The question: does the HBL (2) breach asserted at printed p. 12 have a stated
mechanism, and is it the **same** mechanism as the substitution and expansion
failures? It does, and it is.*

**Lemma 7.1's proof (printed p. 24, imaged) internalises one instance of
proof-composition, and shows what the uniform version would cost.** The `Π₁`
theorem of `PA+` that Group-2 transfers into `IS(PA+)` reads:

> "`∀t ∀p ∀y` if `t` is a terse proof of `¬Φ` and `p` is a semantic tableaux
> proof of `Φ` and **`y = 64 · ⌜¬⊥⌝ · t · p`** then `∃q<y` satisfying
> `Prf_{IS(A)}(⊥,q)`"

— the bound on the glued proof is the **product of the two proof codes**.
`IS(PA+)` can use it only after `t` is instantiated to a constant `g`:

> "While `IS(PA+)` can not recognize multiplication as a function, it certainly
> can verify **the scalar product `kp = p+p+p+…+p`** is a function"

which is Eq. (7.1), `∀p ∃y { y = kp = 64·⌜¬⊥⌝·g·p }`. Lemma 5.3's proof (printed
p. 18, imaged) is the identical step at a different site: `T(p,k) ≤ L_k·p`, and
"`IS(PA+)` will recognize multiplication by any *scalar constant* as a total
function because `L_k p = p+p+p+…+p`".

**So the four failures are one product, rescued by one device:**

| site | the product | how the corpus rescues it |
| --- | --- | --- |
| Lemma 5.2 — expansion vs pointers | `m·ℓ` (occurrences × constant length) | fix `j`, or fix `Φ` |
| Lemma 5.3 — the transformation `T(p,k)` | `L_k · p` | fix `k` |
| **Lemma 7.1 — proof composition, i.e. H-M2** | `64·⌜¬⊥⌝·t·p` | fix `t` at the constant `g` |
| Appendix A — substitution, i.e. H-C-fn | `∀x ∃y SUBST_i(x,y)` | fix `k̄` |

The device is always the same: **fixing one factor turns a product into a scalar
multiple, and a scalar multiple unfolds into a fixed-length sum** — the same
fixed-length unfolding as Lemma 5.1's `2λ` tupling. That is what "only in the
*degenerate case* where `x` and `y` are *fixed constants*" means mechanically,
and it is why §7.2's H-C-fn and H-M2 rows have **one** mechanism between them
rather than two.

*Correction to the form in which this was first conjectured* (`../LOG.md`,
2026-09-06): the product was guessed to arise from concatenation, as
`N_s · 32^{|t|} + N_t`. Willard's own bound is the straight product of the two
codes times a constant. The kinds agree, and concatenation is why a product is
intrinsic — `Glue(t,p)` has `|t| + |p| + O(1)` symbols, so its code is about
`32^{|t|+|p|}` — but the corpus's bound is the one to cite.

**The licensing passage for `lawvere-sjas.md` §4.2 is Remark 2**, printed p. 24,
immediately after Lemma 7.1:

> "our proofs shall *apply a cut rule at the meta-theoretical level*. … suppose
> it is known that a *cut-free* `IS(PA+)` system can prove the two statements
> `α` and `α ⊃ β`. Then by the Cut Elimination Theorem …, `IS(PA+)` will also
> support a cut-free proof of `β`. … we will *often omit constructing formally
> the cut-free proofs* of such theorems `β`: Instead, *their theoretical
> existence* will be established implicitly via the Cut Elimination Theorem"

Willard names this **Meta-Logic** and says it "will be used frequently in the
remainder of this paper". This is HBL (2) in the open: **modus ponens over
`IS(PA+)`'s theorems is available to the metatheorist by cut elimination, and is
exactly what the system does not have internally and uniformly.**
`lawvere-sjas.md` §4.2 asserted that division of labour without citing a
passage; Remark 2 is the passage. It is also Codification obligation **O19**
("Flag the Meta-Logic convention"), one of the twenty-four still open under gap
**G40** — so this check discharges the Refinement's use of O19 and leaves the
Codification's half of it open.

**Is the product an artifact of the coding?** No, and `Willard2011` settles it by
promoting the encoding from a construction to a **condition**. Definition D.1(iv)
(printed p. 38, imaged) allows

> "`ξ*`'s Gödelized method `g` for encoding a semantic tableaux proof can be
> **essentially any natural method** that satisfies the minor stipulation that at
> least `5J` bits are required to encode a semantic tableaux proof that has `J`
> function symbols"

and footnote 23 gives the reason it is a *minor* one: `J` function symbols force
at least `2J` logical symbols, "and thus employ at least `5J` bits", so "all the
usual methods for generating the Gödel codes satisfy this criteria".

**The arithmetic of that "thus", supplied because the source does not show it.**
The step counts a *sequence*, not a *count*: a string of `2J` symbols drawn from
an alphabet of size `k` has `k^{2J}` possibilities and so needs `2J·log₂k` bits.
TR printed p. 15 fixes the alphabet at "the standard six logical connectives:
`∧, ∨, ¬, ⊃, ∀`, and `∃`", and `2J · log₂ 6 ≈ 5.17J ≥ 5J`. The constant 5 is
then `log₂ 32`, which is why the criterion reads `32^J`. *This reconstruction is
this document's; Willard states the conclusion without the computation.* It is
recorded in [`VERIFICATION.md`](VERIFICATION.md)'s **Computed claims**.

**How far the criterion reaches, stated more carefully than an earlier draft of
this section did.** Gluing proofs with `J₁` and `J₂` function symbols yields one
with `J₁+J₂`, hence — *for any encoding D.1(iv) admits* — a code at least
`32^{J₁+J₂} = 32^{J₁}·32^{J₂}`, a product of quantities each of which lower-bounds
an input code. So the product is invariant across the admitted class, and the
counting above explains why that class is the natural one. **It does not show
that no encoding whatever could do better**, and the earlier draft's claim that
"what forbids a compacter encoding is an information-theoretic count, not a
convention" is withdrawn: counting bounds the *aggregate* — most proofs with `J`
function symbols cannot be compressed below `2^{5J}`, by pigeonhole — while
D.1(iv) demands it of *every* one. Willard's own word for it is the right one:
a **stipulation**, satisfied by the usual methods, motivated by the count, not
forced by it. The 1993 ancestor is Lemma 5.1's corollary on *linearly
equivalent* encodings: their `Δ₀` classes "must be essentially isomorphic", so
"the absence of a formal multiplication function in `IS(A)` does not raise any
new complications, with regards to linearly equivalent encodings" — again an
invariance across a class, not across all codings.

**What "the product" concretely is.** Concatenating byte-strings multiplies:
if `p` occupies `|p|` bytes then the string `t` followed by `p` has value
`N_t · 32^{|p|} + N_p`. `Glue(t,p)` is not literal concatenation — it is tree
surgery, a new root `{¬⊥}` with `t` below it and `p` spliced onto `t`'s pivotal
branch — but its encoding is the two codes plus `O(1)` symbols, so its value is
`Θ(N_t·N_p)`, and Willard's bound `64·⌜¬⊥⌝·t·p` is exactly a
constant-times-product cover for it (computation in **Computed claims**). The
operation actually missing is therefore **left shift by a variable amount**,
`x · 32^y`. It is worth noting that `G₀` contains the *other* shift:
`Shift(x,y) = x/2^y` (TR printed p. 3), which shrinks, and is total.

**Which level the multiplication lives at, since it is easy to misplace.**
`Glue(t,p)`'s *size* is `|t| + |p| + O(1)` — **additive**. Its *code* is
`Θ(N_t·N_p)` — multiplicative — only because codes are exponential in size. So
Willard's product comes from an exponential encoding applied to an additive size
increase. This is **not** the multiplication contraction introduces into cut
elimination, where the *derivation size itself* grows, towering in the cut depth.
The two share an axis — what a composite costs relative to its parts — and not a
mechanism, and conflating them would put Beklemishev–Shamkanov's structural
result and Willard's arithmetic one in one currency when they are in two.
Recorded as obligation **RO2**,
[`RO2-contraction-growth-rate.md`](RO2-contraction-growth-rate.md).

Note also that Willard has **both** obstructions, not one. `Glue` builds a
legitimate tableau directly, with no normalization, so its only barrier is the
code arithmetic. *General* modus ponens is different: it needs cut elimination,
and Remark 2 below is Willard taking that from the metatheory rather than
internalising it.

And an encoding compact enough to defeat the criterion would defeat the
consistency proof with it: printed p. 13's argument is that an inconsistency
proof `p` cannot embed a witness `p* ≥ p` because "Addition grows so slowly that
`log(x + y) ≤ MAX(log x, log y) + 1`", so "it is impossible in `log₂p − 1` bits to
encode a number as large as `p`". A coding under which composition were additive
on codes would be exactly a coding in which a short string names a long proof.
*That last inference is this document's, not Willard's.*

**Cut-freeness does both jobs, and this is the structural point of §7.** The
same property that blocks H-M2 is what makes Group-3 safe. `Willard1993-TR`
printed p. 20 (imaged) defines it: "Every semantic tableaux or resolution proof
of `⊥` from an axiom system `A` must have every sentence of the proof tree
(except possibly its root) constitute a **subcomponent** of some axiom of `A`.
This characteristic … will be called their "**cut-free**" property", and it
"trivially generalizes to resolution, as well as **any formal 'cut-free'
deduction method**". Then **Lemma 6.1** (**`sketch`**) shows the inconsistency
sentence `¬∀y ¬Prf_{IS(A)}(⊥,y)` is not a subcomponent of any Group-1 or Group-2
axiom, and **Lemma 6.2** (**`full`**) draws the consequence:

> "If the axiom system `A` is nice then a semantic tableaux proof `p` of `⊥` from
> `IS(A)` is impossible without `p` **formally constructing a parameter or
> constant element `p*`**, such that one node of `p`'s proof tree is the sentence
> `¬Prf_{IS(A)}(⊥, p*)`."

So Group-3's safety is not a size fact alone. It is the conjunction of three
things: the axiom is **universally quantified over proof codes**; the deduction
method's subformula property means it can only be contradicted through an
**instantiated** subcomponent, so the witness must be *named in the tree*; and
self-reference makes the named witness another inconsistency proof of the same
system, hence subject to the same minimality and the same growth bound. The
sentence is arranged so that refuting it demands naming an object at least as
large as the refutation — and printed p. 13's `log(x + y) ≤ MAX(log x, log y) + 1`
is what makes that impossible.

**One property, two roles.** Cut-freeness blocks internal proof composition, so
H-M2 fails and G2 is evaded; and cut-freeness forces every use of Group-3 to name
its witness, so the growth bound bites and Proposition 1 goes through. For the
computational-analogue programme this matters more than the arithmetic: in the
`Tab`/`Xtab` reading, working in `Tab` rather than `Xtab` is not merely where the
breach lives — it is also what buys the consistency argument.

**What this does not establish, stated because the temptation is real.** It does
**not** prove H-M2 underivable. It exhibits the mechanism that was previously
only asserted; it shows every positive internalisation in the paper dodging that
mechanism the same way; and it shows Willard supplying composition from outside
by declared convention.

A proof of underivability needs a model of `IS(A)` in which the uniform
composition sentence is false. **An earlier draft of this section named
`Willard2001` Lemma 3.2's finite models `M_i = {0,…,i}` as the candidate and was
wrong to.** No finite structure models `IS(A)` at all: Group-1 contains the
totality axiom for Addition, and it fails in `M_i` at `x = y = i` — in the 1993
and 2001 presentations, where that axiom is `Π₂`/`Π⁻₂`, and equally in
`Willard2011`'s sharpened `Π*₁` form `∀x ∀y ∃z ≤ x+y : {z = x+y}` (Eq. 46). The
`M_i` are a device for checking the *validity of `Π⁻₁` sentences*, not models of
the system. The right candidate is a **cut** in a nonstandard model — closed
under Addition, not under the product — which is the semantic content of what
Proposition 1 establishes syntactically. That route is still unwalked, and this
document does not walk it.

---

---

## 8. What has and has not been proved

**Proved here.** Theorem 5.1 and Corollary 5.2, from H-A, H-B, H-C-pt,
H-M1–H-M3, H-K, H-N — **H-C-fn is not among them**. Lemma 4.1 (= Löb, algebraic
form) with the consumption of each hypothesis marked. Lemma 3.1, Lemma 3.2,
Theorem 3′. Proposition 7.1 and Corollary 7.2. Departure Y1: the source's `Φ_E`
and `f` are ill defined on Lindenbaum classes, with a counterexample, and are
repaired by the source's own Theorem 2.

**Not new.** Lemma 4.1 is Löb's theorem. Theorem 5.1 is Gödel's second
incompleteness theorem in the Hilbert–Bernays–Löb form. The *route* — through
Yanofsky's Theorems 1/2/3′, with the fixed point produced and destroyed inside
the scheme, and with a numbered hypothesis list — is what this document
contributes, together with Proposition 7.1.

**Not claimed.**

- No claim that G2 follows from Lawvere's fixed point theorem *alone*. It does
  not: LFPT supplies H-C-1 and nothing else. The modal hypotheses H-M1–H-M3 are
  irreducibly extra, and §4(a) says exactly what they add — the ability to run
  the same square a second time inside `□`.
- No claim that `IS(A)` satisfies H-C-pt as a *schema*. What is established from
  the page is that `IS(A)` proves `∃y SUBST_i(k̄,y)` for any fixed `k̄`, that
  Willard says this suffices for the instance he needs, and that `Willard2001`
  footnote 16 exhibits an H-C-1 instance under Theorem A.1's `Π⁻₁` hypothesis
  (**`stated-only`**). The uniqueness half of §5.2's relational route,
  `∀z (Subst(k̄,z) ⊃ z = d̄)`, is asserted by Willard metatheoretically at
  `Willard1993-TR` printed p. 38 ("the *unique* Godel number satisfying
  `SUBST_i(J,K)`"); **whether `IS(A)` itself proves that uniqueness at a fixed
  numeral has not been checked here**, and Theorem 5.1's application to `IS(A)`
  is conditional on it.
- No claim that Yanofsky's own suggestion (§6) has been carried out to this
  stage's evidentiary standard. §6 is `unverified` and marked so.
- No claim that Willard's Theorem 7.2 is an instance of Lemma 4.1 (§7.3).
- **No claim that H-M2 is underivable in `IS(A)`.** §7.4 establishes its
  *mechanism* — the composed proof's code is bounded by the product of the two
  input codes — and that the same product governs three other sites. What is
  proved in the corpus is weaker and differently shaped: `IS(A)` is consistent
  and proves its own consistency, so by Theorem A.1 (**`stated-only`**) *some*
  one of the three conditions must fail. That is a disjunction. The
  identification of the failing condition as (2) is Willard's, asserted at
  printed pp. 1, 2, 6 and 12 and now mechanically explained, not derived. §7.4
  names what a proof of underivability would still need.
- No claim about Pakhomov's `H_{<ω}`, which remains outside this comparison for
  the reason `lawvere-sjas.md` §6 records.
- Proposition 7.1 says that failing H-C-fn does not *evade* G2. It says nothing
  about whether failing H-C-fn is *necessary* for anything, and nothing about
  self-justification, which needs an internal consistency point *and* external
  consistency (`lawvere-sjas.md` §1.3).

**Open, and sharpened by this document.** ADR-0004's "empty cell" question
becomes: is there a system in which `α_⊥` has *no* fixed point at all — not
merely one where substitution is not internally total? Corollary 7.2 says the
second is not enough, and §7.1's correction says the second was never a
hypothesis of the theorem. The first is not known to be occupied by any system
in the corpus.

**Also open, and raised by the correction.** H-C-1 is the weakest form of the
diagonal hypothesis this document could find. Is it the weakest there is? The
argument needs one sentence `γ` with `⊢ γ ↔ ¬□γ`; a system that could not name
even one would be the occupant of the empty cell, and no mechanism for denying a
single fixed-numeral instance while retaining a usable proof predicate is known
here.

---

## 9. Coverage and anchors

**Yanofsky witness.** `lit/yanofsky2003_universal_approach_self_referential_paradoxes_arxiv_math0305282.pdf`,
24 pages, arXiv:math/0305282 (2003; PDF stamped 2008).
SHA-256 recorded in `lit/SHA256SUMS`.

| Read | Pages |
| --- | --- |
| Text layer (`pdftotext -layout`) | 1–24 |
| Rendered page images (`pdftoppm -r 130 -png`) | 5, 6, 14, 15, 16, 17, 22 |

Every quotation and every source statement used above lies in the imaged range.
Pages 1–4, 7–13, 18–21, 23–24 have been read as text only and nothing in this
document rests on them.

**Sources.**

- N. S. Yanofsky, *A Universal Approach to Self-Referential Paradoxes,
  Incompleteness and Fixed Points*, Bulletin of Symbolic Logic 9(3) (2003),
  362–386; [arXiv:math/0305282](https://arxiv.org/abs/math/0305282). Local
  witness:
  [`Yanofsky 2003 PDF`](lit/yanofsky2003_universal_approach_self_referential_paradoxes_arxiv_math0305282.pdf).
  Theorems 1, 2, 3, 4, Remarks 1, 4, 5, and the Future Directions bullets.
- F. W. Lawvere, [*Diagonal Arguments and Cartesian Closed
  Categories*](https://tac.mta.ca/tac/reprints/articles/15/tr15.pdf), 1969;
  reprint TAC 15 (2006), 1–13.
- M. H. Löb, *Solution of a Problem of Leon Henkin*, JSL 20(2) (1955), 115–118.
  **Not held**; Lemma 4.1 is proved here from M1–M3 and does not cite it.
- C. Smoryński, "The Incompleteness Theorems", in *Handbook of Mathematical
  Logic* (Barwise, ed.), North-Holland 1977 — the p. 860 pointer Yanofsky gives
  for the Kreisel route. **Not held; not verified.**
- Willard anchors, each carrying its
  [`results registry`](../codification/registry/results.md) status at the point
  of use: `Willard1993-TR` printed pp. 1, 12, 37; `Willard2001` Theorem A.1 and
  footnote 16 (**`stated-only`**), Lemma 7.1 and Theorem 7.2 (**`full`**).
- Companion Refinement documents: [`lawvere-sjas.md`](lawvere-sjas.md) §§0–1,
  [`refined-sjas.md`](refined-sjas.md) §2.1,
  [`R3-the-margin.md`](R3-the-margin.md) §5.3,
  [`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md) §§1, 2.3.
