# Composition Obligations

The C14–C15 checklist. Every finding that the **codified statement itself must
act on** is appended here as it is discovered, in addition to wherever else it
is recorded (drift ledger, fidelity check, extraction record). Findings
recorded only in those places do not surface reliably at composition time —
this register exists so none is lost between discovery and composition.

Statuses: `open` (must still be acted on), `discharged:YYYY-MM-DD` (with a
pointer to where the codified statement handles it).

Kinds: `side-condition` (implicit hypothesis to state), `distinguish`
(quantities or systems that must not be conflated), `two-readings` (variants to
present with provenance rather than merge), `proof-status` (claim whose status
must be labelled), `notation` (glyph or convention to fix), `provenance`
(citation chain to preserve).

| Id | Kind | Obligation | Source | Status |
| --- | --- | --- | --- | --- |
| O1 | side-condition | **Theorem 1's closing inequality needs `m ≥ 3`.** `g > h` unfolds to `m − 1 > m^{2θ}`, which for θ = 1/4 requires `m > (3+√5)/2 ≈ 2.618`. Willard (p. 20) and the affine-tree re-derivation both gloss it as immediate. Harmless — `m` is a proof's Gödel number — but the codified Theorem 1 must discharge it explicitly rather than inherit the gloss | `../extraction/willard2005-fidelity-check.md` (Observation); drift D6 | open |
| O2 | distinguish | **Three constants must stay separate** in any presentation of the compactification argument: **6** bits per language symbol (Willard2005 Appendix A, base-64 bytes); **5** in Eq. (20) bounding U-Height relative to `Log₂ p` (function symbols along a branch only, not all symbols); **1/4** the exponent θ actually proven. Conflating them is what produced drift D6 | fidelity check F2; drift D3 | open |
| O3 | two-readings | **Density bounds θ from below, not above.** State the relation as the window `1/d ≤ θ < 1/3` (non-empty iff `d > 3`), with Willard's choice θ = 1/4 at density 5 shown as a selection within it — not as an equality `θ = 1/d` and not as a ceiling `θ ≤ 1/d` | drift D6 | open |
| O4 | proof-status | **Label the stated-only apparatus claims.** Willard2005 Remark 1 asserts consistency preservation for cut-free sequent calculus, Herbrand deduction, Tab−Q\*₁−List, and Q\*₁-restricted Hilbert with the case analysis explicitly omitted for space; Remarks 6 and 8 are likewise stated-only. The result matrix must mark these cells as asserted-not-proved, distinctly from the proved tableau and Tab−U\*₁−List cells | extraction §4; gap G8 | open |
| O5 | provenance | **Theorem ∗ is cited and unpublished.** Solovay's generalization of Pudlák's Theorem 2.3 was never published; its only expositions are Willard's own (`Willard2001` Appendix A, 4 pp.; `Willard2005` §4). The same April-1994 telephone-communication thread is cited in `Willard2020-LFCS` ref. 41. The codified statement must present Theorem ∗ with this provenance rather than as ordinary published background | drift D4 | open |
| O6 | notation | **Fix the glyphs the text layer destroys.** The consistency-preserving map is Fraktur **ℑ** (not `=`); the Gödel diagonalization sentence uses a mho-like script glyph (not `f`); numeral **overbars** are semantic — `Γ(n̄)` vs `Γ(n)` is numeral-substitution vs variable-substitution, i.e. the fixed point itself, and Eq. (9) reads `∃z Log^k(z) ≥ m̄` | extraction §8.1; notation registry | open |
| O7 | distinguish | **`IS(A)` vs `IS_D(A)`.** `Willard2001`'s system (Level(0-), semantic tableaux) and `Willard2005`'s (Level(1), apparatus-parameterized) differ by one subscript, share the group architecture, and delegate Group-1 and the ∆\*₀ encodability appendices from the latter to the former. Keep them typographically distinct and state the delta once | drift D5 | open |
