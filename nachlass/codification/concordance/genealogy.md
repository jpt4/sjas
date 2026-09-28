# Genealogy and Canonical Naming (C13)

The systems registry has carried **provisional** canonical names since C4.
This file **freezes** them and gives the map from every paper's own notation to
the frozen name. From here on, `registry/systems.md` and `codified-sjas.md`
use the frozen names; the paper notations survive only in the map below and in
the per-paper extraction records.

Freezing does two things the registry alone could not. It records where Willard
**renamed one construct** across papers without saying so, and it records where
two constructs **share a name** and are not the same thing.

---

## 1. The two branches, and why there are exactly two

`Willard1993-TR` p. 2 derives the architecture from a theorem it attributes to
Solovay: no consistent system can have **both** a cut-permitting apparatus
**and** Addition-totality and still prove its own consistency. That leaves two
directions of retreat, and the corpus is the exploration of both:

| Branch | Retreat | Origin | Mature form |
| --- | --- | --- | --- |
| **Tableaux line** | keep totality, drop the cut | TR Proposition 1 | `SJAS-Tableaux-Level1`, `SJAS-TabList-Level1`, `SJAS-Add-Level1` |
| **Hilbert line** | keep the cut, drop totality | TR Proposition 7 | `SJAS-NoAddition-Hilbert`, `ISCE`, `IQFS` |

`Willard2016` Example 3.3 item (b) restates the same split twenty-three years
later in the **Type** vocabulary — Type-NS under Hilbert, or Type-A under
tableaux, "whose natural hybridizations are precluded by `++`" — without
mentioning the 1993 derivation. Drift **D49**; obligation **O71** requires the
codified statement to give the split once and record both namings.

A third line appears in 2009 and belongs to neither: `Ax-3` changes **only the
axiomatization**, holding the theory fixed.

---

## 2. Frozen names

### 2.1 Tableaux line

| Frozen name | Papers and their notations | Consistency level |
| --- | --- | --- |
| **`SJAS-Origin`** | `Willard1993-TR` `IS(A)`, full form `IS^s_d(A,G)`; `Willard1993` `IS(A)` | primitive (Level 0−) |
| **`SJAS-Tableaux-Level0`** | `Willard2001` `IS(A)` | Level(0−) |
| **`SJAS-Tableaux-Level1`** | `Willard2001` `IS^λ(A)` (0.01 < λ < 1); `Willard2002c` `IS-1(A)`; `Willard2011` `ξ*`; `Willard2020` `IS_Tab(β)`; `Willard1998` `IS^λ(A)` | Level(1) |
| **`SJAS-TabList-Level1`** | `Willard2002c` `IS-1*(A)` under **`R(1,1)`**; `Willard2004` `IS-1*(A)` under **`Tab₁List`**, and the α of Theorem 3 | Level(1) |
| **`SJAS-Tab1-Level1`** | `Willard2020` `IS_{Tab-1}(β)` | Level(1) |
| **`SJAS-Add-Level1`** | `Willard2005` `IS_D(A)`; `Willard2011` `ξ**` | Level(1) |
| **`SJAS-Add-Level1-Finite`** | `Willard2014` `IS_D^#(β)`, `IS_D^#(β_{A,i})` | Level(1) |
| **`SJAS-SimReal-Level1`** | `Willard2005-TAB` `IS-1(A′)` | Level(1) |
| **`SJAS-SimReal-Tab1`** | `Willard2005-TAB` `IS_D(A′)`; `Willard2006b` `IS_D(A′)` | Level(1) |

**The five-name construct.** `SJAS-Tableaux-Level1` is one system with five
notations across five papers spanning 1998–2020. No paper says so. Nothing in
the corpus is more likely to be read as five results than this.

**`R(1,1)` is `Tab₁List`.** `Willard2004` §6 states the rename in as many
words — "the deductive rule of inference that had been called **'R(1,1)
Hierarchy Deduction'** in our prior paper [34] has now been **renamed**, and it
is called instead **'Tab₁List'** deduction". Drift **D33**, **D46**.

### 2.2 Hilbert line

| Frozen name | Papers and their notations | Naming convention |
| --- | --- | --- |
| **`SJAS-NoAddition-Hilbert`** | `Willard2001` `ISREF(A)`; `Willard1998` `ISREF(A)`; `Willard2011` `ξ⁻` | incremental `C_i = C_{i−1}+1` |
| **`SJAS-BitwiseOr-Hilbert`** | `Willard2001` `ISTR(A)`; `Willard1998` `ISTR(A)` | incremental |
| **`ISCE`** | `Willard2006a` `ISCE(A)` | **additive** `C_i = C_{i−1}+C_{i−1}` |
| **`ISINF`** | `Willard2006a` `ISINF(A)` | additive; Infinitely Far-Reaching |
| **`SJAS-Hybrid-H`** | `Willard2006a` `Hybrid(H)` | `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}` |
| **`SJAS-Extender`** | `Willard2006a` `IS.Extender(A)` | additive |
| **`IQFS`** | `Willard2016` `IQFS(β)`, `IQFS(PA+)` | **θ Up-Walking axioms**, three constants only |
| **`IQFS-star`** | `Willard2016` `IQFS*` | as IQFS, Dag-oriented ground terms |
| **`IQFS-R`** | `Willard2016` `IQFS^R`, `IQFS*^R` | as IQFS plus `S_j` as axioms |

**`ISCE` is `ISREF` with the naming convention upgraded.** `Willard2006a`
states it: `Willard2001`'s Theorem 3.4 (`ISREF(A)`) "is the degenerate
incremental-naming version; `ISCE(A)` upgrades it to additive naming and
thereby gains the **Continuous Expansion** property." So the Hilbert line has
one system and three naming conventions, and the conventions are the variable.

### 2.3 Herbrand line and the axiomatization line

| Frozen name | Papers and their notations | Note |
| --- | --- | --- |
| **`SJAS-Herbrand-Level0`** | `Willard2011` `ξ^R` | Level(0) only |
| **`Ax-3`** | `Willard2009` `Ax-3 = IΔ^R₀ + Trivial-R`; `Willard2006-WoLLIC` `Ax-3` | *anti-threshold* for Herbrand G2 |
| **`Ax-3-star`** | `Willard2009` `Ax-3*` | `Ax-3 ∪ {Diag(Ax-3)}` |
| **`Ax-4`** | `Willard2009` `Ax-4` | `Ax-3` + Eq. (41); stepping stone to Theorem 5 |
| **`T-1`, `T-2`, `Type-i-j`** | `Willard2007-APAL` | `IΣ₀` under Type-1/Type-2 induction × `Q₀/Q₁/Q₂` |

**`T-1`/`T-2` are `Ax-1`/`Ax-2`.** `Willard2007-APAL`'s Type-1 and Type-2
induction schemes are `Willard2009`'s `Ax-1` and `Ax-2`. But **`Q₀` is not the
same system in the two papers** — eight axioms without `Max` in
`Willard2007-APAL`, nine with `Max` in `Willard2009`. Drift **D51**.

### 2.4 Negative controls

Every affirmative result in the corpus is paired with a control that fails, and
the controls are as load-bearing as the theorems.

| Frozen name | Notation | What it isolates |
| --- | --- | --- |
| `SJAS-Add-Level1-NegControl` | `Willard2005` `NS^{k,m}_D(A)` | **axiom vs theorem**: both prove `Υ(k,m)`; only NS has it as an axiom |
| `SJAS-Add-Level1-PsiControl` | `Willard2005` `N_D(A,Ψ)` | the `Σ*₁`/`Π*₁` asymmetry |
| `SJAS-Mult-NegControl` | `Willard2001` `ISMULT(A)` | multiplication-totality |
| `SJAS-Mult-Reflection-NegControl` | `Willard2001` `XIS^λ(PAX)` | reflection + multiplication |
| `SJAS-Xtab-NegControl` | `Willard2020` `IS_Xtab(β)` | **LEM as logical axioms** |
| `SJAS-Tableaux-Mult-NegControl` | `Willard2020` `IS^M_Tab(β)` | multiplication under Tab |
| `SJAS-Origin-MultControl` | `Willard1993-TR` `IS^{UΔ₀}(A,G_m)` | the origin-era control |

### 2.5 Frameworks, not systems

| Frozen name | Notation | Role |
| --- | --- | --- |
| `GenericConfig` | `Willard2011` `ξ = (L^ξ, Δ^ξ₀, B^ξ, d^ξ, g^ξ)` | the parameterisation the other systems instantiate |
| `GenAC` | `Willard2020` `(α, D)`, `Willard2016` "Generalized Arithmetic" | axiom basis + apparatus as an ordered pair |
| `PA-UGrounding` | `Willard2005`, `Willard2016` `PA+` | base theory, not an SJAS |
| `QplusV`, `QplusW` | `Willard2002a` `Q + V`, `Q + W` | the negative-side thresholds |
| `WZF`, `Support-ZFC` | `dew-2008-zcf-group` | nachlass set-theory programme, unpublished |

---

## 3. Name collisions to keep apart

These are cases where one string means two things. Each is a place a careless
reading of the corpus produces a false identification.

| String | Sense A | Sense B |
| --- | --- | --- |
| **"cut"** | Gentzen's sequent-calculus **cut rule** — absent from the cut-free apparatuses, recovered by Xtab, TabList/Herb−k and Passive Induction | a **Definable Cut** — a formula closed downward and under successor. `Willard2009` p. 16 and `Willard2004` p. 348: "**formally unrelated** … despite their very similar sounding names". Obligation **O78** |
| **`Level-k`** | `Willard2009` Definition 7: `Π^R_k ∪ Σ^R_k`, the Herb−k intermediate class | `Willard2002c`/`Willard2004`: Level(n) over `Π*ₙ`, Level(n+) over `Q*ₙ` — the class the *consistency statement* quantifies over |
| **`Q₀`** | `Willard2007-APAL`: eight `Π₁` axioms, no `Max` | `Willard2009`: nine `Π^R₁` axioms, including `Max` (D51) |
| **"Consistency Preserving"** | three inequivalent definitions — `Willard2005` (A's `Π*₁` theorems true in ℕ), `Willard2016` (β consistent with Groups 0–1), `Willard2020` (all β's axioms true in ℕ). Drift **D55** | |
| **`IS-1*(A)`** | `Willard2002c`: under `R(1,1)` | `Willard2004`: under `Tab₁List` — the same system, after the rename |
| **`℧`** | `Willard2007-APAL`: the `Π₁` sentence with the Trivial properties | `Willard2004` `℧_n`: the growth-ladder conjunction; `Willard2020`: an arbitrary sentence in `℧ ∨ ¬℧`; `Willard2005`: the Gödel diagonalization sentence. Four uses, one glyph, and `pdftotext` renders it as the digit `0` in all four |

---

## 4. What the genealogy shows

1. **The corpus has far fewer systems than notations.** Sixty-two registry rows
   collapse to roughly twenty distinct systems. `SJAS-Tableaux-Level1` alone
   accounts for five.
2. **The variable is rarely the system.** Between `ISREF`, `ISCE` and `IQFS`
   the *system* barely changes; what changes is the **naming convention** —
   incremental, additive, Hybrid(H), θ. The Hilbert line is a study of growth
   rates wearing three system names.
3. **Every branch point is a growth rate.** Incremental is too slow to be
   Continuously Expanding; multiplicative is too fast to be self-justifying;
   additive is the compromise. That single observation, in `Willard2006a` p. 7,
   organises the whole Hilbert line — and it is the same observation that
   `Willard2000-TAB` §5 says the *negative* proofs consume (obligation **O76**).
