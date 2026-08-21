# Notation Registry (symbol concordance)

One row per symbol/notation as used in a specific paper, mapped to the
canonical symbol chosen for the Codified SJAS. The canonical column stays
**provisional** until the C13 concordance freezes it.

Columns — **Paper**: corpus key. **Symbol**: the paper's notation (plain text;
described rather than drawn where markdown cannot render it). **Meaning**:
one-line gloss. **Canonical**: codification-wide symbol (provisional).
**Anchor**: page of first use/definition. **Notes**: drift pointers,
collisions with other papers' uses.

| Paper | Symbol | Meaning | Canonical | Anchor | Notes |
| --- | --- | --- | --- | --- | --- |
| Willard2005 | IS_D(A) | The self-justifying system built over base A with apparatus D | SJAS-Add-Level1 | p. 8 | Close to [67]'s IS(A); keep typographically distinct (drift D5) |
| Willard2005 | NS^{k,m}_D(A) | The negative control adding Upsilon(k,m) to Group-0 | SJAS-Add-Level1-NegControl | p. 11 | — |
| Willard2005 | N_D(A,Psi) | Negative control generalized to an arbitrary added sentence Psi | SJAS-Add-Level1-PsiControl | p. 37 | — |
| Willard2005 | A(x,y,z), M(x,y,z) | Three-way relations for x+y=z and x\*y=z | Add-rel, Mult-rel | p. 1 | M is Delta\*0-definable via Eq. (4) using division |
| Willard2005 | U-Grounding functions | The eight total function symbols: subtraction, division, Root, Maximum, Logarithm, Count, addition, Double | U-signature | p. 4 | Six non-growth "Grounding" functions plus the two growth functions |
| Willard2005 | n-bar | U-Grounded binary representation of n; at most 2 Log2 N occurrences of addition and Double applied to 1 | quote-numeral | pp. 4-5 | Example Eq. (3) encodes 25 |
| Willard2005 | Delta\*0, Pi\*n, Sigma\*n, Q\*n | U-Grounding-language analogues of the arithmetic hierarchy classes | Delta0-T, Pin-T, Sigman-T, Qn-T | p. 5 | Starred forms are essential: unstarred forms presuppose multiplication is total |
| Willard2005 | Level(n) consistency | No Pi\*n sentence provable together with its negation | Level-n | p. 5 | Level(0-) = no proof of 0=1 |
| Willard2005 | Introspectively Unified Logic | Pair (alpha, D) with alpha proving D-consistency of alpha, and alpha in fact consistent | self-justifying-pair | p. 2 | Part-i / Part-ii terminology used throughout |
| Willard2005 | Prenex\* Normalized | Sentence that is Pi\*i or Sigma\*i for some i >= 0 | prenex-star | p. 6 | Remark 5 says the convention is dispensable |
| Willard2005 | Tab-R-List | Tableau proof list with modus ponens restricted to the class R | TabList(R) | p. 7 | Variants Tab-Q\*k, Tab-U\*k, Tab-Sigma\*k, Tab-Pi\*k; U\*k = Sigma\*k union Pi\*k |
| Willard2005 | App-forall(a), App-exists(b) | Envelope conditions restricting unbounded quantifiers to <= a or <= b | AppAll(a), AppEx(b) | p. 18 | Bounded quantifiers explicitly unaffected (Eqs. 16-17) |
| Willard2005 | Normed(a,b) | Axiom-envelope class plus the eight-function-symbol signature restriction | Normed(a,b) | p. 19 | Definition 4 |
| Willard2005 | theta-Compactified | Proof of magnitude p pushes the validity envelope by at most p^theta | theta-compact | p. 19 | Definition 5; theta < 1/3 |
| Willard2005 | U-Length, U-Depth Delta(s), U-Height | Function-symbol count per sentence; accumulated along ancestors; maximized over leaves | u-length, u-depth, u-height | p. 22 | — |
| Willard2005 | VAL(u), Positive(a,b) | Parameter valuation with Val(u) <= b\*2^Delta(s); branch/node validity conditions I and II | valuation, positive-branch | pp. 22-23 | Eq. (19) |
| Willard2005 | Z-Based Deduction Tree | Tree rooted at an axiom of Z, branches not required to close | deduction-tree | pp. 21-22 | Contrasted with a tableau proof, which is rooted at a negated target and fully closed |
| Willard2005 | Subst(g,h), ExPrf, SubstPrf | Godel substitution; proof-from-union-with-added-axiom; their hybridization | Subst, ExPrf, SubstPrf | pp. 9-10 | All Delta\*0; encodability delegated to [67] Appendices C and D via LinH theory |
| Willard2005 | Pair(x,y) | x codes a Pi\*1 sentence and y codes its negation | NegPair | p. 9 | — |
| Willard2005 | Definable Cut phi(x), Thinning | Eq. (10) cut conditions; Eq. (11) thinning relation | cut, thinning | pp. 12-13 | Survey apparatus, not used by IS_D(A) |
| Willard2005 | Cut-Localized D-consistency | Eq. (15): no D-proof of 0=1 with Godel number inside the cut | cut-local-consistency | p. 13 | The prior literature's approach, contrasted with IS_D(A)'s global Group-3 |
| Willard2005 | Upsilon(k,m) | The Sigma\*1 sentence "exists z with Log^k(z) >= m" (Eq. 9) | Upsilon(k,m) | p. 11 | Definition 2 |
| Willard2005 | 2^m_k | Least z with Log^k(z) >= m | tower(k,m) | p. 11 | Definition 2 |
| Willard2005 | mho-like script glyph, written here as U(s,b) | Godel diagonalization sentences for NS; U(s,b) bounded to proofs below 2^b_s | diag, diag(s,b) | pp. 32-33 | Pi\*1 via Gamma(n-bar), Eq. (24). **pdftotext substitutes the letter f**; visually verified p. 33 |
| Willard2005 | Fraktur capital I | Definition 1's consistency-preserving mapping symbol | preserve-map | p. 10 | **pdftotext substitutes "=" for it**, which reads as nonsense ("call this function = Consistency Preserving"); visually verified p. 11 |
| Willard2005 | overbar on a numeral, e.g. m-bar, b-bar, n-bar | The U-Grounded binary representation term denoting that integer | quote-numeral | p. 5 | **pdftotext silently drops the overbar.** Load-bearing: Gamma(n-bar) vs Gamma(n) is numeral-substitution vs variable-substitution, i.e. the fixed point itself; Eq. (9) is "exists z Log^k(z) >= m-bar" |
| Willard2005 | CheckProof(t,p,q), F(t,p) | Proof-verification predicate and its minimal-witness function | CheckProof, F | p. 33 | Corollary 1 slips the argument order to F(p,t) |
| Willard2005 | zeta(p) | Maps an NS-proof to the shortest IS-proof of the same theorem | zeta | p. 31 | Length gap bounded by C > 2^m_k |
| Willard2005 | byte | Six bits; proofs are base-64 integers over 24 language symbols | byte | p. 39 | Appendix A. Distinct from Eq. (20)'s constant 5 and from theta = 1/4 (fidelity check F2) |
