# Extraction: Willard2014

> Tier C **motivation** extraction (component C11), with §§5–6 taken at
> **full core depth** per gap **G33** / obligation **O73**. Framing sections
> (§§1–4, 7–10) use the lighter motivation template. Visual control: all 16
> published pages rendered and read as images (`pdftoppm -r 130`); archive
> §§5–6 checked for identity (typography only). Page images govern formulas.
> Printed pagination LNCS 8652 pp. 221–236 = PDF 1–16.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2014` |
| Title | On the Broader Epistemological Significance of Self-Justifying Axiom Systems |
| Venue | WoLLIC 2014, LNCS 8652, pp. 221–236 |
| Primary witness | `nachlass/papers/willard2014.pdf` (16 pp.) |
| Archive witness | `nachlass/papers/willard2014_broader_epistemological_author_archive.pdf` (20 pp.; author preprint, §§5–6 mathematically identical) |
| Funding | NSF Grant CCR 0956495 |
| Self-description | "extended abstract" (Theorem 2's proof omitted as "almost identical to [40]'s proof of Theorem 1") |
| Role claim (resume) | Willard's recommended entry-point overview |

## 2. Role in corpus — finite Group-2 sits inside a motivation paper

The abstract's load-bearing promise is technical: "one of which will transform
our previous infinite-sized self-verifying logics into formalisms of purely
finite size." That transformation is §§5–6's `IS_D^#(β)`.

`Willard2016` Remark 7.3 cites this paper as [59] and calls the finite system
**`ISD(β)`**. The printed notation here is **`IS_D^#(β)`** (superscript `#`,
subscript `D`) — a finite-Group-2 variant of the infinite-schema `IS_D(A)`
reviewed in §4 from [40] = `Willard2005`. Drift **D76**. The finite-axiomatization
result is load-bearing for every Hilbert-line system (ISCE / IQFS / IQFS\*) and
bears on `Willard2011` Appendix G / GlobSim (O23). Gap **G33** closed by this
hybrid extraction; obligation **O73** discharged.

## 3. Systems defined

### 3.1 Framing systems (§4; motivation depth)

**`IS_D(A)`** — four-group review of `Willard2005`:

- **Group-Zero**: constants `c̄₀`, `c̄₁`; addition and `Double(x)=x+x`; any
  `n≥2` named in fewer than `3·⌈log n⌉` symbols.
- **Group-1**: finite `Π₁*` set `F` proving every true `Δ₀*` sentence.
- **Group-2**: for each `Π₁*` sentence `Φ`, axiom (4)
  `∀p { HilbPrf_A(⌜Φ⌝, p) ⇒ Φ }`.
- **Group-3**: Level-1 self-consistency (5)
  `∀x∀y∀p∀q ¬[ Pair(x,y) ∧ Prf_{IS_D(A)}(x,p) ∧ Prf_{IS_D(A)}(y,q) ]`.

**Theorem 1** (cited from [40]): `IS_D(•)` is consistency-preserving for
semantic tableaux and Tab-1.

### 3.2 The finite system (§§5–6; full depth)

**`IS_D^#(β)`** (Definition 1): like `IS_D(A)`, but Group-2 is the finite set
`β` of `Π₁*` axioms rather than the infinite schema (4). Group-3's "I am"
fragment is updated to reflect the finite Group-2.

## 4. Numbered-item inventory — §§5–6 at full depth

| Label | p. (PDF / printed) | Digest | Proof |
| --- | --- | --- | --- |
| **Definition 1** | 6 / 226 | **`IS_D^#(β)`**: `β` any finite set of `Π₁*`-encoded axioms; Group-2 = `β`; Group-3 updated accordingly | n/a |
| **Theorem 2** | 6 / 226 | For `D` = tableaux or Tab-1, `IS_D^#(β)` is consistent whenever every axiom of `β` is true in the standard model — the finite-Group-2 analog of Theorem 1's consistency preservation | cited ([40] Thm 1; "will not be repeated in this extended abstract") |
| **Definition 2** | 6 / 226 | **`Test_i(t,x)` is a Kernelized Formula** iff PA proves, for every `Π₁*` sentence `Ψ`, the identity (6): `Ψ ⇔ ∀x Test_i(⌜Ψ⌝, x)`. Infinitely many such `Δ₀*` predicates exist; their enumeration is a **Kernel-List** | n/a |
| Example 1 | 7 / 227 | Constructs one kernel: true `Σ₁*` sentences are r.e., so a `Δ₀*` `Probe(g,x)` exists with `∃x Probe(g,x) ∧ x≥g` (7); with `Pair(t,g)`, truth of a `Π₁*` sentence is `∀x ¬[∃g≤x Pair(t,g) ∧ Probe(g,x)]` (8); that matrix is `Test_0` | n/a |
| **Definition 3** | 7 / 227 | **`Ψ`'s i-th Kernel Image** is the `Π₁*` sentence (9) `∀x Test_i(⌜Ψ⌝, x)` | n/a |
| Example 2 | 7 / 227 | PA proves `Ψ` equivalent to its kernel image, but a weak system proving (9) may not equate them — the limitation Theorem 3 inherits and §6 partially repairs | n/a |
| **Theorem 3** | 7 / 227 | For any standard-model-valid `A` and any kernel index `i`, there is a **finite** `Π₁*` set `β_{A,i}` such that if `Ψ` is a `Π₁*` theorem of `A` then `IS_D^#(β_{A,i})` proves `Ψ`'s i-th kernel image (9) | sketch |
| Proof of Thm 3 | 7–8 / 227–228 | Notation: `Check(t)` (`Δ₀*` "t codes a `Π₁*` sentence"); `HilbPrf_A(t,q)`; **`GlobSim_i`** = (10) `∀t∀q∀x { [HilbPrf_A(t,q) ∧ Check(t)] ⇒ Test_i(t,x) }`. Any `IS_D^#(β)` whose finite Group-2 contains (10) works (minimal case: `β = {(10)}`). Group-1 proves the true `Δ₀*` facts `HilbPrf_A(⌜Ψ⌝, p̄)` and `Check(⌜Ψ⌝)`, whence (10) yields (11) `∀x Test_i(⌜Ψ⌝, x)` | sketch |
| §6 L-fold | 8–9 / 228–229 | For fixed `L`, let `β` contain `L` copies of (10) for distinct kernels `Test_1…Test_L`. Then each `Π₁*` theorem of `A` maps to `L` distinct kernel images. Multiplication as 3-way `Δ₀*` predicate via (12) `[…] ∧ [(x≠0∧y≠0) ⇒ (z/x = y ∧ (z−1)/x < y)]` | n/a |
| Remark 1 | 9 / 229 | Engineering conjecture: large-but-finite `β` with the L-fold strategy may meet application needs | n/a |
| Remark 2 | 9 / 229 | Second Incompleteness remains robust; computers may still benefit from delicately chosen finite `β`; human-consistency analogy | n/a |

### 4.1 Distilled theses from framing sections (motivation depth)

1. **Self-justifying** = Part-i (proves own `d`-consistency) + Part-ii (is consistent); Kleene-style `SelfRef(α,d)` typically kills Part-ii (§2).
2. **Type-M / A / S / NS** taxonomy by totality of successor, addition, multiplication (§2); Type-A works for tableaux Level-1; Type-NS for Hilbert; Type-S and Type-M block the corresponding apparatuses (§§2–3, 7–8).
3. **Fragmentary vs Utopian consistency**: exceptions are fringe from a Utopian view, material from an engineering view (§§7, 10).
4. **Theorem 4**: `IS_D(PA)` and `IS_D^#(β_{PA,i})` formalize total `Left`/`Right` half-product functions (double-precision multiplication analog) (p. 11 / 231).
5. **Theorem 5**: extend `IS_D` / `IS_D^#` so the resulting `S` verifies Tab-1 reflection (16) for all `Δ₀*`/`Σ₁*` sentences and the root-diluted `Π₂*` principle (17) (pp. 12–13 / 232–233).

## 5. Notation table (§§5–6)

| Symbol | Meaning | Canonical | Anchor |
| --- | --- | --- | --- |
| `IS_D(A)` | Infinite-Group-2 Level-1 system (review of Willard2005) | SJAS-Add-Level1 | §4, p. 5 |
| `IS_D^#(β)` | Finite-Group-2 variant; Group-2 = finite `β` | SJAS-Add-Level1-Finite | Def 1, p. 6 |
| `β`, `β_{A,i}` | Finite `Π₁*` axiom set; the Theorem-3 instance for kernel `i` | finite-group2-base | Def 1, Thm 3 |
| `Test_i(t,x)` | Kernelized `Δ₀*` formula | kernel-test | Def 2 |
| Kernel-List | Enumeration of all kernels | kernel-list | Def 2 |
| i-th Kernel Image | `∀x Test_i(⌜Ψ⌝, x)` (9) | kernel-image | Def 3 |
| `GlobSim_i` | Global Simulation Sentence (10) | GlobSim | Thm 3 proof |
| `Check(t)`, `HilbPrf_A` | `Δ₀*` coding predicates | Check, HilbPrf | Thm 3 proof |

## 6. Replicated context

- `IS_D(A)` is `Willard2005`; Theorem 1 cites it.
- `GlobSim_i` is the ancestor of `Willard2011` Definition 6.8's `GlobSim` / TestList finite simulation of Group-2.
- `Willard2016` Rem 7.3 says the same finite reduction "will routinely generalize" to ISCE / IQFS / IQFS\*.
- Type-M/A/S/NS taxonomy reappears in `Willard2016` and `Willard2018`.

## 7. Discrepancies and errata

- **D76 / O73**: `Willard2016` Rem 7.3 writes `ISD(β)`; the printed system is `IS_D^#(β)`. Do not merge the infinite and finite names.
- Theorem 2's proof is omitted ("extended abstract"); status `cited`, not `full`.
- Archive witness is 20 pp. vs published 16; §§5–6 content matches (spelling/`Δ` glyph differences only).

## 8. Saturation record

| Pass | Date | Scope | New numbered items |
| --- | --- | --- | --- |
| 1 | 2026-08-29 | Full text-layer read; lighter template on §§1–4, 7–10; full depth on §§5–6 | Defs 1–3, Thms 1–5, Remarks 1–3, Examples 1–2, Eqs (4)–(17) |
| 2 | 2026-08-29 | Visual control: PDF pages 1–16; archive §§5–6 cross-check | zero new; confirmed `IS_D^#(β)` glyph (superscript `#`) |

**Coverage**: Read 1–16; Images 1–16; state `complete`.
