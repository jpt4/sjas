# Result Matrix (C13)

The (language profile × apparatus) projection of the five axes, with the
consistency **level** and the **proof status** recorded in every cell. Each
cell says whether a system of that profile, under that apparatus, **can** or
**cannot** recognise its own consistency — and on what evidence.

Statuses are the registry's own: `full` | `sketch` | `cited` | `stated-only`.
A cell is only as strong as its weakest supporting status, and several
famous-sounding results in this corpus are `sketch`.

Legend: **+** affirmative (a self-justifying system exists) · **−** negative
(the Second Incompleteness Effect holds) · **?** open.

---

## 1. The main grid

| Profile ↓ / Apparatus → | Hilbert-Frege | Semantic tableaux | TabList / Tab-1 | Herbrand | Xtab |
| --- | --- | --- | --- | --- | --- |
| **Type-M** (×, +, S total) | **−** Pudlák 1985 / Solovay `∗` | **−** `Willard2002a` Thm 6.4 · *sketch* | — | **−** `Willard2007-APAL` Thm 6 · *full* — **but see §3** | — |
| **Type-A** (+, S total) | **−** by Solovay `∗` a fortiori | **+** Level(1) · `Willard2005` Thm 3 · *full* | **+** Level(1) · `Willard2005` Thm 5 · *full*; `Willard2004` Thm 3 · *sketch* | **+** Level(0) · `Willard2011` `ξ^R` | **−** `Willard2020` Thm 4.5 · *sketch* |
| **Type-S** (S total only) | **−** Solovay `∗` · *cited, never published* | *not studied* | — | — | — |
| **Type-NS** (nothing total) | **+** — see §2 for the naming-convention split | — | — | — | — |

**The empty and the absent are informative.** Type-S under Hilbert is the cell
Solovay's theorem closes, and closing it is what forces the corpus's two
branches. Type-NS is studied *only* under Hilbert, because dropping totality is
the price of keeping the cut; Type-A is studied *only* under cut-free
apparatuses, because keeping totality is the price of dropping the cut.

---

## 2. Inside Type-NS × Hilbert: the naming convention is the variable

The system barely changes across this row (see [`genealogy.md`](genealogy.md)
§4). What changes is how the constants `C_i` are named — which is to say, how
fast the system's integers grow.

| Naming convention | `C_i` | Verdict | Source | Status |
| --- | --- | --- | --- | --- |
| **incremental** | `C_{i−1} + 1` | **+**, but *not* Continuously Expanding | `Willard2001` Thm 3.4 (`ISREF`) | *full* |
| **additive** | `C_{i−1} + C_{i−1}` | **+** and Continuously Expanding | `Willard2006a` Thm 3 (`ISCE`) | *full* |
| **Hybrid(H)** | `⌈2^{[Log i]^H}⌉·C_{i−1}` | **+** at `H = 1`; **−** for `H > 1` | `Willard2006a` p. 30 | *stated-only* |
| **multiplicative** | `C_{i−1}·C_{i−1}` | **−** | `Willard2006a` Thm 4 · *full*; Thm 4\* · *sketch* | |
| **θ Up-Walking** | three constants only | **+** *conditionally* | `Willard2016` Thm 6.7 · *full* — **conditional on Conjecture 6.6**, which is *stated-only* | |

**Hybrid(H) is the corpus's only exactly located threshold.** Every other
boundary is bracketed between a positive and a negative result; here the
transition point is named — `H = 1` works, `H > 1` fails — though only as a
remark, without proof.

**`Willard2006a` p. 7 says why the ordering is what it is**: the incremental
convention "grows too slowly" to be Continuously Expanding; the multiplicative
one is too fast to be self-justifying; the additive one is "simultaneously
*sufficiently slow* to satisfy Theorem 3's self-justification property while
also *sufficiently fast* to satisfy the continuous expansion property."

---

## 3. The axiomatization row: `Ax-3` breaks the grid

`Willard2009` is the one place where the *theory* is held fixed and only the
**axiomatization** varies. `Ax-1`, `Ax-2` and `Ax-3` prove **the same
theorems** (Theorem 1), and yet:

| System | Herbrand | Semantic tableaux |
| --- | --- | --- |
| `Ax-1`, `Ax-2` (= `T-1`, `T-2`) | **−** threshold · `Willard2007-APAL` Thm 6 · *full* | **−** threshold · `Willard2007-APAL` Thm 6 · *full* |
| **`Ax-3`** | **+ anti-threshold** · `Willard2009` Thm 4 · *full* | **−** threshold · `Willard2009` Thm 5 · *sketch* |
| `Ax-4` | **+ anti-threshold** · Lem. 6 · *sketch* | **−** threshold · Lem. 5 · *sketch* |

This is the corpus's sharpest single fact: **one theory, three axiomatizations,
opposite incompleteness properties** — because the systems cannot prove that
they are equivalent. `Willard2006-WoLLIC` p. 10 states the consequence most
sharply, and the journal version drops it: `Diag(1)` and `Diag(2)` are
**provably logically invalid**, `Diag(3)` is valid (drift **D47**, obligation
**O69**).

**Theorem 5 is the counterweight** that stops Theorem 4 being read as a general
evasion (obligation **O64**) — and it is `sketch`, resting on
`Willard2007-APAL`'s eight elimination rules (gap **G32**, now closed by that
paper's extraction).

---

## 4. The level dimension (Axis 3) as a boundary

Fix Type-A and tableaux; vary what the Group-3 axiom quantifies over.

| Level | Verdict | Source | Status |
| --- | --- | --- | --- |
| Level(0−) | **+** | `Willard2001` `IS(A)`, `SJAS-Origin` | *full* |
| **Level(1)** | **+** | `Willard2002c` `IS-1(A)`; `Willard2005` Thm 3 | *full* |
| Level(2) / **Level(2+)** | **−** | `Willard2004` Thm 1 | *full*, modulo gap **G35** |
| `Π*₂`, `Σ*₂` as TabList intermediates | **−** | `Willard2004` Thm 2 | *sketch* |

`Willard2004` §1 calls the gap between Level(1) and Level(2+) "**very narrow**".
It is the only boundary in the corpus whose negative half is *proved* rather
than sketched or asserted — and even that proof defers its `T* ≤ N²` step to a
"longer version of this paper" that does not exist (**G35**).

---

## 5. The restricted-cut dial

Four parameterisations of "how much cut may I have", none related to the others
by any paper (drift **D38**).

| Dial | Positive | Negative | Status of both halves |
| --- | --- | --- | --- |
| **ℜ** (`Tab-ℜ-List` intermediates) | `Π*₁ ∪ Σ*₁` — `Willard2004` Thm 3 | `Π*₂`, `Σ*₂` — Thm 2 | *sketch* / *sketch* |
| **Level(n)** | Level(1) | Level(2+) | *full* / *full* (G35) |
| **Hybrid(H)** | `H = 1` | `H > 1` | *stated-only* / *stated-only* |
| **`Herb−k`** | `k = 1` (Item II) | `k = 2` (Item I) | *stated-only* / *stated-only* |
| **`Z`** (LEM instances as logical axioms) | `Δ*₀` | `Π*₂` and above | *stated-only*; **`Π*₁` open** |

**The one genuinely open question in the corpus** is the `Π*₁` cell of the `Z`
dial: `Willard2020` §7 conjectures that the `Δ*₀` evasion continues at `Π*₁`
"but this fact has not yet been formally proven". Drift **D34** sharpens it —
on `Willard2004`'s ℜ dial the `Π*₁ ∪ Σ*₁` case is **proved**, so the conjecture
is exactly the claim that the result **transfers from the ℜ dial to the `Z`
dial**.

---

## 6. What the matrix is missing, and why

| Cell | Why empty |
| --- | --- |
| Type-A × cut-free sequent calculus | claimed five times, proved never — obligation **O80** |
| Type-A × resolution | same; `Willard2002a` p. 5 (2002) and `Willard2020` §8 |
| Type-NS × cut-free apparatuses | not studied; the Hilbert line exists *because* cut is retained |
| Real-valued / floating-point | `Willard2005-TAB`, `Willard2006b` sit outside the integer grid: `IS_D(A′)` recognises multiplication as total **over simulated reals**. Its primary witness, the ASL-2005 technical report, is unlocated — gap **G36** |
| `Willard1997` | no witness — gap **G2** |

---

## 7. Reading the matrix honestly

Counting by status across the governing results above: the affirmative side is
mostly `full`, the negative side is mostly `sketch`, and the two dials with
exactly located thresholds are `stated-only` on both halves. A codified
statement that presents the boundary as sharply located everywhere would be
overstating what the corpus proves.

Three of Willard's own limiting statements belong beside this table —
obligations **O24** (self-justification gives "essentially a 1-line proof"),
**O65** (the arithmetics are "weaker than traditional arithmetics", and G2's
refutation of Hilbert's original programme is "undeniable"), **O72** (the
Q-1/Q-2 split), and **O79** (statement `###`, "is it not *almost cheating*").
