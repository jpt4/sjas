# Review scope: ADR-0006 trust surface

This branch is a **projection** of `adr-0006-formalization` at `ccb1164`.
That branch formalizes the λᶜᵉʳᵗ₀ metatheory (`nachlass/refinement/R4-metatheory.md`,
`R4-certificate-calculus.md`) in Ansatz, a Lean-4-compatible kernel embedded
in Clojure. The full branch adds 80k lines, mostly proof scripts, which
exceeds the ultrareview limit. This projection keeps the parts the kernel
cannot vouch for. **It is for review only: it is not merged, and the
projected files do not load.**

## What was projected, and how

`review-tools/surface.clj` (on the base branch `review/base`) copies each
formal file verbatim except for proofs.
- A headline constant is one named in the ADR-0006 statement index
  (`review-tools/keep.txt`). Its statement is kept, and its proof body
  becomes `;; ‹proof elided: lines a–b of the full file; kernel-checked›`.
- Every other lemma becomes `;; ‹lemma NAME: statement and proof elided,
  lines a–b›`.

Definitions (`a/inductive`, `kdef`, `a/defn`, Clojure helpers), docstrings
and comments are unchanged. `review-tools/readcheck.clj` confirms that each
projected file still reads as Clojure. For every file, the original's form
count equals the projection's form count plus the number of collapsed lemmas.
Line numbers in the markers refer to the full file at `ccb1164`.

Proof bodies are checked by the kernel when a namespace loads, and a
rejected declaration aborts the load. `kdef` installs definitions through the
kernel's `check-constant` (`base.clj`). No Lean axiom is declared anywhere in
the formalization. Large derivations use proof by reflection,
`(exact (dtCheck_sound chkf tree … (Eq.refl Bool.true)))`, where
`dtCheck_sound` (`check_dt.clj`) is itself a kernel theorem. The Clojure
generators that build those trees (e.g. `dtgen.clj`) are therefore untrusted,
and they are left out.

## Files

- **Object calculus:** `syntax`, `usage`, `judgment` (`Tl`, `Rt`), `conv`,
  `skel`.
- **Checker and encoding:** `check`, `check_spec`, `check_dt`, `encode`.
- **Models and semantics:** `model`, `eval` (`Ev`), `erase` (`Er`, `EvE`,
  `Ok`, `OkE`).
- **PA:** `pa`, `paassembly`, `palcert`, `paconsistency`.
- **Headline statements:**
  - Theorem 4: `theorem4`.
  - Theorem 4.6: `theorem46`, `thm46f7`.
  - D2/D3: `cor46`.
  - Theorem 5.2: `s52theorem`, `s52univ`.
  - Proposition 4.9: `p49depth`, `p49all`.
  - Lemma 6.4: `dflt64`.
  - Proposition 5: `p5`.
  - Self-justification: `selfjust`.
- **Documents and tooling:** `ADR-0006-formalization.md` (its statement index
  maps each paper statement to its constants), corrections to R4, and the
  `bin/` suite runner.

Left out are carriers/denotation (`carrier`, `den`, `sem`), the
fundamental lemma, the §4 propositions other than those above, and every
auxiliary module. They are at the same paths on `adr-0006-formalization`.

## What to look for

1. **Faithfulness.** Do the definitions match R4? This covers the typing
   rules, conversion, `Check`/`CheckSpec`, the encoding, `Ev`, erasure and PA.
2. **Statements.** Does each headline statement say what the ADR-0006 index
   claims? Look for vacuous or trivializing hypotheses, wrong quantifiers, and
   "concrete" instances that are not at the real checker `Check decCert`.
3. **Trust path.** Could anything reach the environment without a kernel
   check?
4. **Suite.** Does `bin/` load every formal namespace and fail on a rejection?
5. **Claims.** Do ADR-0006's status lines overstate what is proved? Also
   check consistency claims: every one must be relative to the metatheory or
   to named hypotheses, never absolute.
