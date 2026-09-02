# Refinement Verification Registry

The Refinement was opened without the Codification's machinery — no coverage
registry, no `audit.sh` participation, no unconditional imaging rule. The
2026-09-02 review found that **every Tier-1 defect was of a kind that machinery
exists to prevent**, committed in the one stage not covered by it. This file
closes that gap, and `../codification/audit.sh` now checks it (checks R-A–R-D,
in `../codification/audit-r.sh`).

## The rules

Four inherited from ADR-0001, one added by this stage's own failures.

1. **Every quotation containing mathematics is image-verified.** `pdftotext`
   silently strips mathematical typography from these PDFs. It stripped every
   `□` from a Beklemishev–Shamkanov block quote reproduced in three Refinement
   documents (finding #3), and stripped the `□` from Theorem 4's conclusion
   `⊠⊤ =_S □⊥`, which a draft then reproduced as `⊠⊤ =_S ⊥`.
2. **Every claim about a Willard result carries that result's `Proof` status**
   from `../codification/registry/results.md`, at the point of *use*.
3. **Every claim about what the corpus does or does not say is checked against
   the extraction records first.** Finding #1 was a claim that Willard never
   names a derivability condition, contradicted by an extraction section
   titled "Which derivability condition fails".
4. **Departures are declared** (ADR-0002), and so is the reverse — Willard's own
   statements are not presented as the Refinement's inferences.
5. **Arithmetic is computed, never read off an author's informal gloss.**
   `Willard2006a` calls the Hybrid convention "midway between the additive and
   multiplicative"; a draft took that as exact and built a criterion on it. The
   actual growth is `Θ(n (log n)^H)`, which refutes the criterion. Every growth
   or size claim in this stage is now recorded in **Computed claims** below with
   the computation that produced it.

## Quotation register

`img` = verified against a rendered page image. `txt` = text layer only —
permitted **only** for prose containing no mathematics. Check **R-B** enforces
both the enum and the no-mathematics-in-`txt` rule.

| Quote / claim | Source | Anchor | Verified |
| --- | --- | --- | --- |
| "every self verifying system must breach … one of the three fundamental Hilbert-Bernays conditions" | `Willard1993-TR` | printed p. 1 | img (C5) |
| "may arise because of either the axiom system employed or the choice of deduction method, but not necessarily due to both" | `Willard1993-TR` | printed p. 1 | img (C5) |
| **"only in the *degenerate case* where `x` and `y` are *fixed constants*"** — the condition-(2) failure | `Willard1993-TR` | printed p. 12 | img (C5); recorded at `../codification/extraction/willard1993-tr.md` §3.7c |
| Theorem A.1's three conditions; status `stated-only`, "no formal proof given" | `Willard2001` | p. 44 | txt — prose and a status read from `results.md` row `Willard2001#ThmA.1` |
| Lemma 7.1 "replaces the Hilbert-Bernays derivability conditions with a semantic argument" | `Willard2001` | p. 37 | txt — prose, quoted from the `results.md` digest |
| "Conventional generic configurations `ξ` will satisfy the Hilbert-Bernays derivability conditions" | `Willard2011` | printed p. 21 fn. 11 | txt |
| Tight, Def. 4.5 — refutation length at least envelope plus two | `Willard2011` | printed p. 13–14 | img (C6, 2026-08-21) |
| Defs. 5.1 / 5.3 / 5.5 — A-Stable, E-Stable, EA-stable; Thm 5.9's dependencies are 5.5 and 5.7, not 5.1 | `Willard2011` | printed pp. 16–18 | img (C6); statuses from `results.md` |
| Lemma 4.6 — Tightness suffices for a consistent self-justifying system; status `sketch`, proof "kept brief and informal" | `Willard2011` | printed p. 14 | txt — status read from `results.md` |
| Conventional Tableaux Encoding Requirement, `32^J` / `5J` bits — a **lower** bound | `Willard2011` | printed p. 39 fn. 23 | img (C16 Q1) |
| the two sequences and their encoding lengths | `Willard2020-LFCS` | p. 11 | img (C10) |
| `U-Height(p)`, `θ` values | `Willard2005` | pp. 19, 26 | img (C10) |
| Remark 4 — the strong half attributed: "a Level(0-) tableaux generalization **in [68] implies that** … no useful analog of Definition 5 can be found …" | `Willard2005` | printed p. 21 | img (C10); `remark`, no proof status |
| `ℑ(H) = Σ χ(p_i)` | `Willard2004` | printed p. 363 | img (C16 Q18) |
| Hybrid(H) recurrence | `Willard2006a` | p. 30 | img (C16 Q20) |
| `ISINF(A)` Infinitely Far-Reaching, Theorem 6 | `Willard2006a` | printed p. 8 | img (C11) |
| Lemma 5 — `O(n)` nodes, "whose binary encoding has a `2^n` length that is much larger than `F`'s length"; status `sketch`, "We obviously have omitted many details here" | `Willard2009` | printed p. 24 | img (C11) |
| "the true reason … Lemma 8 … collapses entirely" | `Willard2000-TAB` | §5 | img (C10) |
| Def. 3.7 plain contraction and weakening; Prop. 3.8's C3 step **with `□`**; Thm 3; Thm 4 (contraction **and** weakening; conclusion `⊠⊤ =_S □⊥`); Rem. 3.9's `□`-contraction and `□`-weakening | Beklemishev–Shamkanov | p. 8 | **img (2026-09-02)** |
| "the system `S` does not provide a counterexample to the non-formalized version of G2, since `⇒ ¬□⊥` is not provable" | Beklemishev–Shamkanov | p. 14 | **img (2026-09-02)** |
| "we are still missing convincing examples of mathematical theories based on weak logics for which G2 would fail" | Beklemishev–Shamkanov | p. 14 | **img (2026-09-02)** |
| superexponentiation, the cut `S`, the finite model of size at most the tower, and `Con^pred` | Pakhomov | p. 4 | **img (2026-09-02)** |
| "both `H` and `H_{<ω}` could not prove totality of successor function" | Pakhomov | pp. 3–4, 21 | txt — prose |
| footnote 3 (Beklemishev's simplification "still unpublished"); Acknowledgments | Pakhomov | p. 25 | txt — prose |
| "The theories in his examples are not completely natural … Diagonal Lemma" | Pakhomov | p. 3 | txt — prose |

## Computed claims

Rule 5. Every growth or size claim, with the computation.

| Claim | Where used | Computation |
| --- | --- | --- |
| additive naming: `Log₂ C_n = n` | `refined-sjas.md` §§4–5 | `C_i = 2C_{i−1}` |
| multiplicative naming: `Log₂ C_n = 2^n` | `refined-sjas.md` §§4–5 | `C_i = C_{i−1}²` |
| **Hybrid(H): `Log₂ C_n = Σ_{i≤n} (Log i)^H = Θ(n (log n)^H)`** | `refined-sjas.md` §5, `R3-the-margin.md` §3.1 | summed numerically for `H ∈ {1,2}`, `n ∈ {10,100,1000}`: `H=1` gives 21.8 / 524.8 / 8529.4 against `n` = 10 / 100 / 1000. Hybrid(1) is **not** `O(n)`, which refutes the withdrawn `O(L)` criterion |
| encoding requirement gives `Log(q_β) ≥ 5J` | `R3-the-margin.md` §1.1 | `Gödel number ≥ 32^J` and `log₂ 32 = 5`. A **lower** bound: it excludes over-compressed encodings, not wasteful ones |

## Corrections applied 2026-09-02

| # | Defect | Where it was |
| --- | --- | --- |
| 1 | Claimed Willard never names the breached condition; he names (2) for `IS(A)`, as a uniformity failure | `refined-sjas.md` §2.3 |
| 2 | Claimed Pakhomov's `H_{<ω}` proves successor totality and so falsifies R1; it cannot, and does not | `R1-review.md` §2, `refined-sjas.md` §1, `ADR-0002` |
| 3 | Block quote with every `□` stripped by the text layer | `refined-sjas.md` §2.5, `R2` §1, `R3` §5 |
| 4 | Listed Beklemishev–Shamkanov's `S` as a route to self-verification; their §6 says it is not | `R3` §5, `R2` §2.3 |
| 5 | `Willard2020` Thm 4.5 called "Established"; it is `sketch`, with a dropped hypothesis on `β` | `refined-sjas.md` §2.3, `R3` §§3.2, 4 |
| 6 | `Willard2001` Thm A.1 said to be "proved"; it is `stated-only` | `R2` §2.1 |
| 7 | The `O(L)` naming criterion misclassifies Hybrid(1), the positive case | `refined-sjas.md` §5 |
| 8 | Hybrid(H) assigned to the envelope side without argument; withdrawn | `R3` §3.1, `refined-sjas.md` §§5.1, 9 |
| 9 | Level(n) said not to change proof cost, then said to lower it | `R3` §3.3 |
| 10 | EA-stability given as Def. 5.1 and as the `Π` half only; it is Def. 5.5, both halves, quantified over possibly-false R-Views | `R3` §1 |
| 11 | Encoding requirement said to guard against *wasteful* encodings; it is a lower bound and guards the other direction | `R3` §1.1 |
| 12 | `ℜ` and `Z` treated as interchangeable — the merge codified §8.1 forbids (D34, O46, G22) | `R3` §3.2 |
| 13 | BS Thm 4 said to need weakening "not contraction"; it needs **both**, plus Löb's conditions | `R2` §1 |
| 14 | Dial-status accounting said "two of five are sketch", counting one dial's halves as two dials and omitting `Z` | `R3` §4, `refined-sjas.md` §9 |
| 15 | "Self-verification is the failure of G2's argument" — a biconditional `S` refutes | `R3` §5, `R2` §2.3 |
| 16 | "Four things are given up", followed by five | `refined-sjas.md` §7 |
| 17 | `Willard2005` Remark 4's attributed clause ("in [68] implies that") elided; status not carried | `refined-sjas.md` §7 |
| 18 | `Willard2009` Lemma 5 called "the cleanest available form"; it is `sketch` | `refined-sjas.md` §4 |
| 19 | §9 still instructed a continuation to acquire G7, closed by `R1-review.md` §1 | `refined-sjas.md` §9 |
| 20 | §1's roadmap off by one from §6 onward | `refined-sjas.md` §1 |
| 21 | Pakhomov's model bound glossed as small; it is superexponential, on the cut, and the sentence is `Con^pred` | `refined-sjas.md` §1, `R3` §5.1 |
| 22 | Conditions (1) and (2) called "permissions to use a proof again"; reuse is (3) | `R2` §2.1 |
