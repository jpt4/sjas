# Systems Registry (canonical genealogy)

One row per formal system (or system family) Willard defines, keyed to the
paper that defines it. `Canonical` assigns the codification-wide name;
several papers' notations may map to one canonical system, and one paper may
define several systems. The genealogy narrative and the variation axes live in
`../concordance/`. Canonical names are **provisional** until C13 freezes them.

Columns — **Canonical**: codification-wide system name. **Paper**: corpus key
(`corpus.md`) of the defining source. **Paper notation**: the system's name in
that paper. **Language profile**: total-function signature class. **Apparatus**:
deduction method the system is paired with. **Groups**: axiom-group structure /
finite vs infinite axiomatization. **Anchor**: definition location + page.
**Notes**: admission conditions, base-theory assumptions, variant deltas.

| Canonical | Paper | Paper notation | Language profile | Apparatus | Groups | Anchor | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| SJAS-Add-Level1 | Willard2005 | IS_D(A) | U-Grounding: eight total function symbols (six non-growth grounding functions plus addition and Double); multiplication only as a Delta\*0 relation | Parameter D; proven for semantic tableaux (Thm 3) and Tab-U\*1-List (Thm 5); claimed for cut-free sequent, Herbrand, Tab-Q\*1-List, Q\*1-restricted Hilbert (Rem 1, stated-only) | Group-0 (4 axioms: constants, addition, Double); Group-1 (finite Pi\*1 set F proving all true Delta\*0 sentences); Group-2 (infinite schema, one axiom per Pi\*1 sentence of A); Group-3 (one self-referential Pi\*1 sentence) | section 3, pp. 8-10 | Level(1) self-consistency. Admission condition for preservation: all A's Pi\*1 theorems true in the standard model. Group-3 built by Kleene fixed point over SubstPrf (Eqs. 7-8); Prf_{IS_D(A)}(t,p) is itself Delta\*0 as SubstPrf(n-bar,t,p). Willard stresses the Pi\*1 (not Pi\*2) encoding is essential |
| SJAS-Add-Level1-NegControl | Willard2005 | NS^{k,m}_D(A) | Identical to SJAS-Add-Level1 | Identical to SJAS-Add-Level1 | Identical except Group-0 gains the Sigma\*1 axiom Upsilon(k,m) | Definition 3, p. 11 | The single-parameter negative control: both systems prove Upsilon(k,m), but only NS has it as an axiom, which shortens proofs by C > 2^m_k and changes the referent of "this" in Group-3. Inconsistent for k > k0 (Thm 6) |
| SJAS-Add-Level1-PsiControl | Willard2005 | N_D(A,Psi) | Identical to SJAS-Add-Level1 | Identical to SJAS-Add-Level1 | Group-0 gains an arbitrary sentence Psi | section 6, p. 37 | Generalization of the negative control. Sigma\*1/Pi\*1 asymmetry: inconsistent for valid Sigma\*1 Psi (Thm 6 case), but always consistent for valid Pi\*1 Psi (Thm 5 generalizes) |
| PA-UGrounding | Willard2005 | PA+ | Peano Arithmetic extended with the U-Grounding function symbols | n/a (base theory) | n/a | section 6.2, p. 32 | Footnote 7: PA+ may use multiplication internally, provided the Pi\*1 theorems fed to Group-2 contain no multiplication function symbol |
