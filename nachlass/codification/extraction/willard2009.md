# Extraction: Willard2009

> Tier B extraction (component C10). **Anchoring**: PDF page = printed page of
> the author's copy (1–33); Information & Computation pagination is 207 (2009)
> pp. 1078–1093 and is not used here.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2009` |
| Title | Some Specially Formulated Axiomizations for IΣ₀ Manage to Evade the Herbrandized Version of the Second Incompleteness Theorem |
| Venue | Information and Computation 207 (2009) pp. 1078–1093 |
| Witness | `nachlass/papers/willard2009_i_sigma0_herbrand_author_inf9.pdf` (33 pp.) |
| MSC | 03B52; 03F25; 03F45; 03H13 |
| Conference form | `Willard2006-WoLLIC` (15 pp.) — see [`willard2006-wollic.md`](willard2006-wollic.md) |

## 2. Role in corpus — a different kind of evasion

Every other affirmative result in the corpus weakens a **theory** — dropping
multiplication-totality, or successor-totality, or restricting the apparatus.
This one changes **only the axiomatization**. `Ax-1`, `Ax-2` and `Ax-3` prove
*the same set of theorems* (Theorem 1), and yet an extension of `Ax-3` can
recognise its own Herbrand consistency while the conventional axiomatizations
cannot. Obligation **O61**.

It is also one of the four papers `Willard2011` claims to unify ([68] there),
and the source of `Willard2020`'s remark that Type-Almost-M systems "verify
their Herbrand but not also semantic tableaux consistency".

**Provenance.** The paper answers a question put to Willard by **L. A.
Kolodziejczyk in an email of 16 November 2005**, itself "an outgrowth out of
some conversations he had with **Zofia Adamowicz and Konrad Zdanowski**".
Kolodziejczyk's observation was that "logically equivalent axiom systems, such
as `Ax-1` and `Ax-2`, **do not necessarily have the same properties** with
regards to the semantic tableaux and Herbrandized versions of the Second
Incompleteness Theorem", and that there are bounded arithmetics in which
Herbrand and tableaux consistency are **provably not equivalent** ([18,19]).

**Relation to Paris–Wilkie 1981.** Their open question had two halves —
whether `IΣ₀` satisfies G2 for semantic tableaux and for Herbrand deduction.
`Willard2002a` Theorem 6.4 answers the **tableaux** half affirmatively. This
paper shows the **Herbrand** half fails for a suitably chosen axiomatization.
The two results go opposite ways on one question. Obligation **O62**.

## 3. Machinery

**`Δ^R₀`**: like `Δ₀` except that the terms `T` bounding quantifiers
`∀v ≤ T`, `∃v ≤ T` may use **only `Max`** — arithmetic functions remain allowed
elsewhere in the body. `Π^R₁` is a `∀`-block over `Δ^R₀`. Eq. (3) is `Δ^R₀`;
Eq. (4) is `Δ₀` but not `Δ^R₀`. **An eighth class notation across the corpus.**

**`Q₀`**: nine `Π^R₁` axioms (Eqs. 5–13) fixing `+`, `·`, successor, `Max`, `=`
and `≤`, common to all three axiomatizations.

| System | Definition |
| --- | --- |
| `Ax-1` | `Q₀` + the standard `Δ₀` induction scheme (1) |
| `Ax-2` | `Q₀` + the bounded induction scheme (2) |
| `IΔ^R₀` | `Q₀` + scheme (2) with `Δ^R₀` `φ` |
| **`Ax-3`** | **`IΔ^R₀` + Trivial-R** |
| `Ax-3*` | `Ax-3` ∪ {`Diag(Ax-3)`} |

**Split Representation** (Eq. 14): `(a₀, a₁, …, a_N)` represents `x` when
`x = Σ aᵢ·(a₀+1)^{i−1}` with every `aᵢ ≤ a₀` — a base-`(a₀+1)` digit tuple, so
each digit is bounded by `a₀` and `Max` suffices for the quantifier ranges.
**Trivial-R** supplies `Δ^R₀` predicates `Mult^{I,J,K}`, `Add^{I,J,K}`,
`Maxim^{I,J,K}`, `Eq^{I,J}`, `LTE^{I,J}` simulating the operations on split
integers, with `Π^R₁` axioms (15)–(19) tying them to the real operations.

**Definition 1 — Threshold / Anti-Threshold.** With `α ⊇ β` meaning **axiom**
containment ("stronger than the more modest construct that α proves all β's
theorems"), `(α, D)` is a **Threshold** for the Second Incompleteness Effect iff
*every* consistent `α* ⊇ α` fails to prove its `D`-consistency, and an
**Anti-Threshold** otherwise. Note this is the same axiom-versus-theorem
distinction that defines `Willard2011`'s Type-Almost-M and drives
`Willard2020`'s LEM result (drift D29). Obligation **O63**.

**Conventional Encoding Method** (Definition 6): `Θ(P) > MinG(t)` whenever the
Herbrand proof `P` contains the term `t`. Another member of the corpus's
encoding-condition family (D25/D31).

## 4. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Definition 1 | 6 | `⊇` as axiom containment; **Threshold** and **Anti-Threshold** for the Second Incompleteness Effect | n/a |
| **Theorem 1** | 6 | **`Ax-1`, `Ax-2` and `Ax-3` prove the same set of theorems** — so `Ax-3` is a legitimate axiomatization of `IΣ₀`. Via Paris–Dimitracopoulos's 1-to-1 correspondence between `Δ₀` and `Δ^R₀` representations | sketch |
| **Remark 1** | 8 | **The paper's motivating example.** `Υ_n` (Eq. 23) is a `Δ₀` sentence of **O(n) logic symbols** asserting `v₀ = 2`, `v_i = v_{i−1}·v_{i−1}`, hence `v_i = 2^{2^i}`. Its `Δ^R₀` counterpart `Υ^R_n` over split integers in base 2 needs **at least 2^n characters**. Logical equivalence, exponential length gap — generalizing this *is* the paper | n/a |
| Definition 2 | 10 | `Υ` has a **Herbrandized Proof** from β iff β ∪ {`¬Υ`} yields a Herbrandized proof of `⊥`. Preceded by *Herbrandized Instance* (27) and *Herbrandized Proof of `⊥`* = finitely many instances plus a propositional anti-tautology proof | n/a |
| — (unnumbered, p. 10) | 10 | **Non-Growth Function**, **Arithmetic Controlled Set**, **Arithmetically Controlled Term**; `MinG(t) = 2^{C_t+F_t}`, `Val(t)`. Worked example: Eq. (28)'s term has `Val(t) = 3·4 = 12`, `MinG(t) = 2^25` | n/a |
| Lemma 1 | 10 | `t` arithmetically controlled with `Val(t) ≥ 4` ⟹ `Val(t) < MinG(t)`. Proof: for `Val(t) = 2^k` the maximally compressed form is `2·2·…·2`, so `MinG(t) = 2^{2k−1} > 2^k`; non-powers of 2 are never more compressed than the greatest power of 2 they exceed | sketch |
| Definition 3 | 11 | **`B`-Bounded Arithmetic Set**: contains `+`, `·`, successor; every other `G` obeys `G(x̄) ≤ Max(x̄)` **when `Max(x̄) < B`** (29). A **`B`-Bounded Arithmetic Term** uses only the constants 0, 1, 2 and functions from such a set | n/a |
| Lemma 2 | 11 | `t` a `B`-bounded arithmetic term with **`MinG(t) < B`** and **`Val(t) ≥ 4`** ⟹ `Val(t) < MinG(t)`. Proof **omitted** as an easy generalization of Lemma 1's; footnote 2: the two term classes grow identically until construction reaches `MinG(t) ≥ B` | sketch |
| Definition 4 | 11 | **`B`-Bounded Valid `Π^R₁` sentence**: (30) `∀a₁…∀a_n φ̃(ā)` is so called iff its restriction (31) to `a_i < B` holds in the standard model | n/a |
| Definition 5 | 11 | **Canonical Arithmetic Condition**: every axiom of α is `Π^R₁` **and** `Q₀`'s nine axioms (5)–(13) are among them | n/a |
| Definition 6 | 12 | **Conventional Encoding Method**: `Θ(P) > MinG(t)` for every Herbrand term `t` in `P` | n/a |
| **Theorem 2** | 12 | For canonical α of `B`-Bounded Valid `Π^R₁` sentences and conventional `Θ`, every Herbrand proof `P` of `⊥` from α satisfies `Θ(P) > B` | full (Appendix A) |
| **Theorem 3** | 12 | `Diagonal(α,D)` and `α^D` are well defined, and `Diag(Ax-3)` has a `Π^R₁` encoding — `Γ(N̄)` for `Γ(g) = ∀p ¬SubstPrf^D_α(g, ⊥, p)` (33) | sketch |
| **Theorem 4** | 14 | **`Ax-3*` is consistent**, so `Ax-3` is an **anti-threshold** for the Herbrandized Second Incompleteness Theorem | full |
| **Remark 2** | 15 | **Theorem 4 survives widening `Δ^R₀`.** Permitting **Addition** as well as `Max` in the bounded quantifiers' terms leaves Theorem 4 intact; the restriction was made "for the sake of simplifying the presentation". A dial explicitly *not* at its threshold — obligation **O68** | n/a |
| Remark 3 | 15 | Appendix B added **at the anonymous referee's suggestion**; it generalizes [55]'s `Ax-1`/`Ax-2` results to `Ax-3` for "the polar opposite result" | n/a |
| **Definition 7** | 19 | **`Herb−k` proof**: `(t₁,p₁)…(t_n,p_n)`, each `p_i` a Herbrand proof of `t_i` whose axioms come from α or from `t₁…t_{i−1}`, every intermediate `t_i` in **Level-k** (`= Π^R_k ∪ Σ^R_k`). "An extension of Herbrand deduction that contains a type of Gentzen-like deductive cut rule for Level-k sentences" — a **fourth name** for the TabList apparatus (drift **D33**) | n/a |
| **Item I** | 19 | **Negative half of the `Herb−k` dial.** A logically valid `Π^R₁` sentence `Ψ` exists such that **no** consistent system containing `Ψ` as an axiom proves its own consistency under **`Herb−2`** | stated-only |
| **Item II** | 20 | **Positive half.** For each consistent `A` there is a consistent `I(A)` proving all `A`'s `Π^R₁` theorems and recognizing its own consistency under **`Herb−1`**. "We will not prove results I and II here because each has a rather long proof" — obligation **O67**, drift **D38** | stated-only |
| — (unnumbered, p. 20) | 20 | **`Tab−k`** defined as `Herb−k` with tableaux proofs and `Π*_k`/`Σ*_k` intermediates, and the gloss that `Π*_k`/`Σ*_k` are "roughly analogous" to `Π^R_k`/`Σ^R_k` **except that they contain no multiplication function symbol**, using `M(x,y,z)` instead | n/a |
| Definition 8 | 21 | **`B`-Bounded Good Skolemization**: (36)/(37)'s Skolem functions satisfy Definition 3's `B`-Bounded requirement *and* (38) — validity for all `x_i < B` — in the standard model. Footnote 6: `f_i^Ψ(x₁…x_i) ≤ Max(x₁…x_i)` whenever `Max(x₁…x_n) < B` | n/a |
| Lemma 3 | 22 | Every `B`-Bounded Valid `Π^R₁` sentence has a logically equivalent form with a `B`-Bounded Good Skolemization. "Follows immediately from the definitions" | full |
| **Remark 4** | 22 | **Why Definition 3 is the hinge.** Such Skolem functions never grow faster than the multiplication primitive, and that slow growth **characterizes `Ax-3` but not `Ax-1`/`Ax-2`** — Willard's own one-paragraph answer to why the *axiomatization* decides the result | n/a |
| **Lemma 4** | 22 | **Appendix A's workhorse.** For canonical α with `B`-Bounded Good Skolemizations and conventional `Θ`, any Herbrand proof `P` of `⊥` satisfies `Θ(P) > B`. By contradiction: Definition 6 + Lemma 2 force `Val(T) < B` for every term of `P` (39); (38) then makes every Herbrandized instance (40) standard-model valid; and by footnote 7 such a conjunction can never prove `⊥` | full |
| **Lemma 5** | 24 | **`Ax-4` = `Ax-3` + (41)** `∀z ∀q ≤ z [ q·q ≤ z ⇒ ∃r ≤ z (r = q·q) ]`, logically equivalent to `Ax-3`. It is a **threshold** under tableaux: `n` rounds of elimination rules (43), (45), (48) build `U₀ = 2`, `U_{i+1} = U_i·U_i` along one **pivotal branch** of a fragment `F` of only **O(n) nodes**, establishing `U_n = 2^{2^n}` whose binary encoding has length `2^n` | sketch |
| Lemma 6 | 26 | `Ax-4` is **also** an anti-threshold for Herbrand deduction, since Theorem 4's proof generalizes to any extension of `Ax-3` by finitely many logically valid `Π^R₁` sentences. With Lemma 5, the polar-opposite pair already holds of `Ax-4` | sketch |
| **Lemma 7** | 27 | **The translation lemma.** `Ψ` (50) `= ∀v ψ̃(v)` is `Ax-4`'s only non-`Ax-3` axiom, but its "cousin" `Ψ*` (52) `= ∀z ψ̲(z)` — the induction instance (51), an analogue of [55]'s **"passive induction"** — *is* an `Ax-3` axiom. Each tableaux use of `Ψ` becomes a four-part subtree of exactly **`k = k₁ + k₂ + 3`** nodes whose final sentence (58) is identical to the one it replaced (53), so `T*` exceeds `T` by at most **`kn`** nodes | full |
| **Theorem 5** | 29 | **`Ax-3`, like `Ax-4`, satisfies the semantic tableaux version** of the Second Incompleteness Theorem. Since `U_n`'s binary length `2^n` dwarfs Lemma 7's `kn` overhead, Lemma 5's argument survives the translation. "The remaining details … are omitted for the sake of brevity" | sketch |

**Theorem 5 is the counterweight to Theorem 4** and the reason the paper does
not overclaim: the *same* `Ax-3` that evades the Herbrandized effect **obeys**
the semantic-tableaux one. Willard flags this in §2: "Ax-3's evasion of the
Second Incompleteness Theorem under a Herbrandized definition of consistency
**does not generalize for semantic tableaux deduction**." So the paper exhibits
a system where the two consistency notions come apart, concretely realising
Kolodziejczyk's [18,19] separation. Obligation **O64**.

**Theorem 4's proof** is the corpus's standard minimal-witness architecture:
take `P` minimising `Θ(P)`; all `Ax-3*` axioms are `Π^R₁`; Theorem 2 forces some
axiom to fail `B`-Bounded validity for `B < Θ(P)`; every `Ax-3` axiom is
`B`-Bounded valid for all `B`, so `Diag(Ax-3)` is the culprit; from that
construct `R` with `Θ(R) < Θ(P)`, contradicting minimality.

## 5. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Read of pp. 1–17 in full plus the Appendix A/B statements; uncapped, case-insensitive item sweep across all 33 pp. | 8 Definitions, 7 Lemmas, 5 Theorems, 4 Remarks |

| 2 | 2026-08-27 | **Visual control pass**, pp. 4, 11, 12, 14 | 0 new items; Split Representation, `Q₀`'s nine axioms, Definition 6 and Theorems 2–3 confirmed as recorded; **Lemma 1's statement recorded properly** as `Val(t) ≥ 4 ⟹ Val(t) < MinG(t)` |
| 3 | 2026-08-27 | **Completion read of pp. 18–33** (§6 and Appendices A and B) with a visual pass on pp. 8, 10, 19, 22, 25, 26, 27, 28 | **2 new items** — §6's Items I and II; four combined inventory rows split into their eleven constituents; **six proof statuses corrected** (Lemmas 1, 2, 5, 6 and Theorem 5 all printed as *Proof Sketch* or *proof omitted*, not full) |

Coverage **complete**: pp. 1–33 (pp. 30–33 are the reference list). Eq. (14)'s
constraint is `a₁ ≤ a₀ ∧ … ∧ a_N ≤ a₀`, with `a₀` the base and not itself
constrained.

**What the completion read changed.** Pass 1 read the appendices at statement
level and recorded four combined rows, all marked `full`. In fact the appendices
carry two *new results* Willard states without proof (Items I and II, the
`Herb−1`/`Herb−2` dial — obligation **O67**), and of the four Appendix B items
only **Lemma 7** is proved in full; Lemmas 5 and 6 and Theorem 5 are printed as
*Proof Sketch*. Lemmas 1 and 2 in the main body were likewise overstated. This
is the second time in this component that a statement-level pass produced
`full` statuses that the page contradicts.

**A new instance of hazard H2, and a severe one.** Appendix B uses two
differently decorated `ψ`s: `ψ̃` (overtilde — the `Δ^R₀` open formula of Eq. 49)
and `ψ̲` (underbrace — the induction instance of Eq. 51). `pdftotext` renders
the overtilde as a stray `f` on the preceding line and the underbrace as
`|{z}`, so **both appear in the text layer as bare `ψ`**. Lemma 7 turns entirely
on `Ψ = ∀v ψ̃(v)` being an `Ax-4` axiom while `Ψ* = ∀z ψ̲(z)` is an `Ax-3`
axiom, so the text layer makes the lemma unreadable. The same pages render the
successor prime `y′` as `y 0`, and p. 10 renders `MinG(t) = 2^{C_t+F_t}` as
`2Ct+Ft` and the example value `2^25` as `225`.
