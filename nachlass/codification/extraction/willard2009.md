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
| Remark 1 | 8 | On `Ax-3` as an anti-threshold for the Herbrandized effect | n/a |
| Definition 2 | 10 | The revised notation for a theorem `Υ` | n/a |
| Lemma 1 | 10 | For an arithmetically controlled term `t` satisfying the stated condition; valid for all terms with `Val(t) ≥ 4` | full |
| Definition 3 | 11 | `B`-bounded sets of functions | n/a |
| Lemma 2 | 11 | For a `B`-bounded arithmetic term `t` with `MinG(t) < …` | full |
| Definition 4 | 11 | The `Π^R₁` sentence `Φ` and `B`-Bounded Validity | n/a |
| Definition 5 | 11 | The **Canonical** arithmetic axiom system property | n/a |
| Definition 6 | 12 | **Conventional Encoding Method**: `Θ(P) > MinG(t)` for every Herbrand term `t` in `P` | n/a |
| **Theorem 2** | 12 | For canonical α of `B`-Bounded Valid `Π^R₁` sentences and conventional `Θ`, every Herbrand proof `P` of `⊥` from α satisfies `Θ(P) > B` | full (Appendix A) |
| **Theorem 3** | 12 | `Diagonal(α,D)` and `α^D` are well defined, and `Diag(Ax-3)` has a `Π^R₁` encoding — `Γ(N̄)` for `Γ(g) = ∀p ¬SubstPrf^D_α(g, ⊥, p)` (33) | sketch |
| **Theorem 4** | 14 | **`Ax-3*` is consistent**, so `Ax-3` is an **anti-threshold** for the Herbrandized Second Incompleteness Theorem | full |
| Remark 2 | 15 | On the assumption about terms | n/a |
| Remark 3 | 15 | Appendix B's further reason Theorem 4 is of interest; added at the anonymous referee's suggestion | n/a |
| Definition 7 | 19 | A sequence `(t₁,p₁), (t₂,p₂), …` — the TabList construct in this paper's notation | n/a |
| Definition 8, Lemma 3, Lemma 4, Remark 4 | 21–22 | Appendix A's machinery for Theorem 2's proof | full |
| Lemma 5, Lemma 6, Lemma 7, **Theorem 5** | 24–29 | **Appendix B**: `Ax-4` and then `Ax-3` satisfy the **semantic tableaux** version of the Second Incompleteness Theorem | full |

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

Coverage **partial**: pp. 1–17 read in full, pp. 18–33 (Appendix A's proof of
Theorem 2 and Appendix B's proofs of Lemmas 5–7 and Theorem 5) read at statement
level only — gap **G31**, which no longer covers the visual pass. Eq. (14)'s
constraint is `a₁ ≤ a₀ ∧ … ∧ a_N ≤ a₀`, with `a₀` the base and not itself
constrained.
