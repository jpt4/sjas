# 2026-10-04 — The two-tape machine model: proposal, self-attack, adversarial review

**Context.** The user observed that the token discipline resembles a bounded
automaton (certificates on a bounded, perhaps tree-structured tape; the family
Turing complete; no universal machine). The author elaborated a *two-tape*
reading (free scratch tape = `Syn`; bounded notarized tape = `R`), attacked
it, and repaired it to "unary resource declaration is the operative fact".
The user then predicted an imperatively executed implementation of
self-justification and asked for an adversarial review by Opus 5.5 at X-High
effort. The reviewer ran headless (`claude -p --model claude-opus-5-5
--effort xhigh`, read-only tools) from the brief
`code/lcert/test-runs/review-twotape-prompt.md`. Its report, verbatim:

---

# Adversarial review: the two-tape model of λᶜᵉʳᵗ₀

Paths are relative to the worktree root unless noted. The formal Clojure files are in `code/lcert/formal/lcert/formal/` and the Willard texts in `nachlass/codification/sources-text/`. I ran nothing; every verdict comes from reading or from a derivation given here.

## (B) First elaboration

- **B1. Two tapes: free scratch (Syn) and a bounded linear notarized tape (R). REPAIRABLE.**
  - The store is affine, not linear. Weakening discards tokens anywhere, `print` destroys all of a certificate's tokens, and `reflect` destroys ‖v‖−m of them (`eval.clj:206-209`).
  - Leaves are free: `leaf ℓ` costs no token.
  - The bound is the number of token names in the program text, and it holds only for runtime positions under erasure (R4 §1.1a:111-121, §5:1272-1278).
- **B2. Writing a cell costs a token. HOLDS** for internal nodes (Node rule, R4:237).
- **B3. Only notarized content can be executed (reflect) or trusted (H, H₁, inspect). REPAIRABLE.**
  - It holds for `reflect`, H and H₁ (R4:246-247).
  - `inspect` trusts nothing. It runs Check at runtime and hands the certificate back (R4:248, `eval.clj:211-214`). A code-level version is already definable as `elimBool(chk′ c d, …)`.
  - Free-tape data can be executed. The ordinary layer has `recN` at higher-type motives, which is System T strength (EQ, R4:1632), so it can interpret code fragments. What only notarized content gets is the *primitive* interpreter that covers the whole language up to a size bound.
- **B4. §4's uniform failures as space lower bounds.**
  - **Boxed contraction needs 2N cells: BREAKS.** Counterexample: `c_A := λ(p :₁ □A). (p, (lit_v, ⋆))` types at `Θ_{μ(A)}`. It passes the input through and builds one minimal certificate (Prop 4.2). Prop 4.4(3)'s proof gives only k ≥ μ(A) (R4:752-753). Prop 4.5's 2μ(A) (R4:756) is a loose upper bound, read here as a lower bound.
  - **D3 needs more than 2N: HOLDS** (Cor 4.6″, R4:788-790). The factor 2 rests on E3 (Prop 4.3 step 5, R4:726).
  - **D2 needs μ(B): HOLDS** (Thm 4.6).
  - All three are bounds on how many token names the program text contains (§1.1a:129-134), not on runtime store.
- **B5. reflect is a universal machine for strictly smaller members, and m < ‖v‖ is the cost of simulation. BREAKS.**
  - reflect is not universal: it runs closed programs at base data types with no input (R4:247, §1.6 Check clause 2).
  - It has no simulation overhead. The inner program runs System-T computations at zero token cost.
  - The gap m < ‖v‖ is the size of the stored description, not a cost of simulating.
- **B6. Willard has one sort, so one tape bounded everywhere. BREAKS.**
  - Willard's restriction is that multiplication is a "3-way relation (rather than … a total function)" (`willard2005.txt:12-13`). That limits provable growth. Nothing is stored, consumed or used linearly, and proofs can be reused freely.
  - His margin is information-theoretic: "impossible in log₂p − 1 bits to encode a number as large as p" (`willard1993-tr.txt:1509-1511`).
  - The record itself says the mechanisms differ: "proof-theoretic there, a resource here" (R4:1173-1176), and "There is no compressed naming to forbid … ordinary arithmetic stays strong" (draft:784-791).
  - The closest λᶜᵉʳᵗ₀ counterpart of a growth restriction is Hofmann non-size-increase on R (the ItR case, R4:596-600). Willard allows additive growth; λᶜᵉʳᵗ₀ allows no growth on R at all. The tape picture conflates growth with possession.

## (C) The author's attack and repair

- **C1. "Bounded automaton" is the wrong class. HOLDS, and understates.** The ordinary layer is at least System T (P5 interprets PA, §6), and computes codes of 2ⁿ nodes at zero tokens (Prop 4.12). No space class fits.
- **C2. The family is not Turing complete inside the theory. HOLDS.** Any total language plus an unbounded outer loop is Turing complete. Tokens play no part in that.
- **C3. The Pudlák mapping fails. The verdict holds, but the reason given is insufficient.**
  - "H° is uniform" does not refute a finite-consistency reading. At budget n, H° can only be *applied* to certificates of at most n nodes. The metatheory's proof is a ladder indexed by n (Lemma 3.6, strong induction on n, with reflect capped at ‖v‖ ≤ n). That ladder is finite-consistency-shaped, which C3 ignores.
  - The real reasons:
    - **Source side.** In the held texts Willard uses Pudlák for the second incompleteness theorem over extensions of Q with Hilbert proofs, via definable cuts and successor totality (`willard2000-tab.txt:21-27`, `willard2001.txt:94, 2679-2680`). He does not use finite consistency. Pudlák's paper is not held (gap G39, refined-sjas:854-866), so (A)'s premise cannot be checked here.
    - **Target side.** λᶜᵉʳᵗ₀'s internal finite consistency is Prop 4.9 (R4:948-960): bounded Con′ on codes, at budget 0, with proof size doubly exponential in depth. It needs no tokens. Tokens are not λᶜᵉʳᵗ₀'s finite-consistency mechanism.
- **C4. The tape bound holds only under erasure; type-level certificates are unbounded. HOLDS** (R4:1272-1278; draft:776-782). Two additions:
  - The bound on intermediate states during evaluation is never stated as a theorem. Theorem 4′ covers only the final data result (R4:1289-1291).
  - Token distinctness is explicitly unproved (R4:1535-1538).
- **C5. reflect is not universal. HOLDS.** "No input" can be worked around by writing the input as a literal, which costs at least one certificate node per literal node (E4).
- **C6. Two rows of the space table import encoding facts. HOLDS.** Theorem 4.6's own list of what it uses omits E3 (R4:875-885), which matters for the conclusion below.
- **C-conclusion. "The load-bearing fact is unary resource declaration (E3); binary budgets would break descent, consistency and no-universal-member." BREAKS.**
  - **Derivation.**
    - Let m′ be the number of distinct token variables free in the decoded term t′.
    - Then m′ ≤ the number of variable occurrences ≤ internal nodes of ⌜t′⌝ (E4) < nodes(c), since the root is an extra internal node (E2). No context-declaration format enters.
    - By Lemma 2.3, strengthen the derivation to a context with exactly those m′ tokens at usage 1.
    - Change §3.2's reflect clause to evaluate ⟦t′⟧^{m′} (or cap at ‖v‖−1).
    - The Reflect case then goes through: outer hypothesis at m′, then Lemma 3.4 with m′ < ‖v‖ ≤ k₁.
    - The H₁ case goes through: compose the strengthened contexts and use the outer hypothesis at m′₁+m′₂ < k₁+k₂ ≤ n.
    - Under erasure, unnamed tokens are never read anyway (§1.1a:111-121).
    - So binary or even one-node budget declarations break none of the three.
  - **Formal record.**
    - `Theorem_1` is proved for *every* checker satisfying `CheckSpec` (`model.clj:106-151`). The only size clause in `CheckSpec` is "m < nodes(c)", and nothing requires unary declaration.
    - The concrete Check *tests* m < ‖c‖, 2f < ‖c‖ and ‖⌜A⌝‖ < ‖c‖ "for every decoder" (`check_spec.clj:23-27, 218-220`; ADR-0006:134-136, 146, 154-158).
    - The verified certificate format declares m as a single leaf, `encN m = Code.sl m`, with zero internal nodes (`certenc.clj:83, 385`). Completeness *pads* undersized certificates (`certenc.clj:29-30`).
    - So the inequality is a size tax that padding nodes pay as well as meaningful ones. That directly refutes "inseparability of machine and tape".
  - **Willard's compression.** The true analogue of Willard's compression is a *compressed program*: a certificate whose term is generated rather than written out, which violates E4. Then a small certificate could name more tokens than it has nodes, and descent fails. The analogue is not a binary budget. The record's own E3 attribution (R4:1014; draft:793-795) makes the same mistake.

## (D) The prediction

- **D1. Linear store of n cells, never duplicated. UNDECIDED.**
  - The store is affine, not linear.
  - Distinctness of tokens is unproved (R4:1535). The model has `C(◇) = {◇}` and cannot see duplication.
  - The runtime check sees only node construction and the final value, not closures or intermediate states (`eval.clj:56-65, 177-182`).
- **D2. Its own verifier Check. HOLDS, and is trivial.** Every proof-carrying-code VM has one.
- **D3. reflect verifies once, then no runtime checks. BREAKS by D's own fourth falsifier.**
  - reflect's evidence `e : T(chk′ (print r) ⌜D⌝)` already guarantees that Check accepts (Theorem 5.2's S relation).
  - Under erasure the cap ‖v‖ ≤ n cannot fail (Thm 3 and §1.1a).
  - So the runtime Check and cap at `eval.clj:204` are exactly checks "the calculus says are unnecessary". Verification belongs at `inspect`, or statically by δ (Prop 4.2).
- **D4. Traps unreachable, read as self-justification. BREAKS.**
  - Theorem 5.2 covers `abort` alongside H and H₁. It is "well-typed programs don't go wrong", and holds identically for a machine without H.
  - Willard's Def 3.4 (`willard2016.txt:310-314`) requires (i) a theorem or axiom stating the apparatus's consistency and (ii) actual consistency. Example 3.5 calls (i) "almost trivial" by adding an axiom (`:315-321`).
  - In λᶜᵉʳᵗ₀, (i) is the axiom H (the T2 inhabitant is η-expanded H, R4:675-679), and (ii) is a Π₁ metatheorem, T1.
  - Neither has a runtime signature. Programs that typecheck both with and without H run identically. The only difference is the verifier's *acceptance set*.
- **Proposed falsifiers.**
  - A trap reached in a certified run, and reflect failing to descend, cannot fail except by implementation bug. Theorem 5.2 is already probe-tested (R4:1517-1533), and Check enforces m < ‖c‖ by construction (it tests it in F7).
  - "More nodes than cells" tests linear typing, not self-justification.
  - "A runtime check the calculus says is unnecessary" is a hygiene rule, not a falsifier, and D3 violates it.

## Attacks 2–7

- **2 (unary load-bearing).** BREAKS; see C-conclusion. E3 is load-bearing only for Prop 4.3's factor 2, and therefore for the "2N" numbers. Without it, D3's uniform failure still follows from Thm 4.6 with B = □A, since μ(□A) > μ(A) by E4.
- **3 (token linearity). UNDECIDED: no counterexample, no proof.**
  - Paths checked and blocked:
    - ω-scaled method premises reject usage-1 variables (`typing.clj:78-82, 278, 314-316, 333`).
    - `◇ → B` closures are inapplicable, because ω-scaling a usage-1 token is impossible.
    - reflect consumes r, and the inner program gets a subset of v's tokens.
    - `inspect` rebinds x once.
    - The evidence premise has a disjoint context.
    - `print` destroys tokens, and re-certifying costs a fresh supply.
    - An itR that copies a certificate together with its code (`R ⊸ R ⊗ Syn`) is legal and duplicates nothing.
  - Implementation defect: `default-value` at `:Fn` computes one value and returns it on every call (`eval.clj:81`). A default of type `Nat → ◇` therefore returns the same phantom token every time, which contradicts the comment at `:79`. It is unreachable in typed programs, given Theorem 5.2.
- **4 (no universal member). What is precisely true:**
  - (i) reflect at budget n runs every Check-accepted closed base-typed program whose certificate has at most n nodes.
  - (ii) No program reflects its own certificate or a larger one. Its own certificate has at least 1+n+|t| nodes, and it can hold only as many nodes as it names tokens.
  - (iii) The ordinary layer has no total interpreter for its own Syn codes, by the classical diagonal (Brown–Palsberg 2016, Thm 3.1/3.2). `recSyn` computes codes of codes, so Brown–Palsberg's typed-representation escape (Thm 3.3, `(p_u p_u) ∉ F_ω`) is unavailable. λᶜᵉʳᵗ₀ also lacks the polymorphism their representation needs. Neither point involves tokens.
  - (iv) Tokens add only that reflect's diagonal needs an infinite tower of supplies, each certificate containing the next one's supply. That is (ii) again.
  - So "no universal member" says nothing beyond "untyped codes plus a diagonal" and "certificate larger than budget, plus T3".
- **5 (Willard's growth vs storage).** BREAKS; see B6.
- **6 (D versus a typed VM).** A Move-style VM with linear resources, the verifier exposed as a native, PCC-style trusted execution and typed `unreachable` already has D1–D4. What it lacks is a typing rule that *mentions its own verifier*: H's premise is "Check accepts a refutation", where Check checks programs that use H. On top of that it would need programs accepted only because of H, and a proof of T1.
  - **Falsifiers that could actually fail:**
    - (a) Strip reflect's runtime Check and cap under erasure, then fuzz typed programs, including evidence produced by `inspect` and by conversion. The erasing fundamental property and Theorem 5.2's S relation are formally unproved (ADR-0006:300).
    - (b) Check token distinctness at every intermediate state, including closure environments (R4:1535).
    - (c) Build the effective-budget / one-leaf-budget variant and look for a refutation. This decides C's claim; the formal record predicts no breakage.
    - (d) The only real test of self-justification is static: adversarial search for c with `Check(c, c⊥) = tt`, concentrated on derivations using H₁ or reflect near the cap.
- **7 (formal record).**
  - The size facts are tested and padded, not derived (above).
  - The erasure theorems D relies on are paper-only.
  - Side finding, plausible from reading but not executed: certificates declare m (and the fuel) as leaf labels `sl m`, while V(Syn)/V(R) require labels below 100 (`skel.clj:66-70`; ADR-0006:244-248). An accepted certificate with m ≥ 100 then has no lblOk R value. If label constants in the typing rules are also bounded below 100, which I did not check, no program can hold it, and reflect and H₁ are unusable above budget 99 in the verified instantiation. Corollary 3.7 on codes is unaffected.

## Overall

- **Repaired model (C): misidentified.** Its descriptive half is correct but adds nothing beyond T3, Lemma 2.7 and §1.1a: a total machine, a free scratch store, an affine store of certificates, holding required to run, store = named tokens. Its explanatory half, that unary declaration and machine/tape inseparability are operative and binary budgets break things, is false. **Defensible core:** a program can spend only tokens it names (T3's refinement), and a certified program is written inside its certificate (E2+E4). So running a certificate always leaves strictly fewer spendable tokens than holding it cost. That is a stored program plus affine naming, and it is Hofmann non-size-increase on R.
- **Prediction (D): wrong as a test of self-justification; correct but trivial as a description of a typed proof-carrying VM.** Its observables cannot tell an H machine from one without H.

## The single most damaging objection

The project's own kernel-checked `Theorem_1` holds for every checker that merely *tests* m < ‖c‖, for every decoder. The verified certificate format writes m as one leaf and meets the inequality by padding with nodes that describe nothing. Separately, E4 alone gives (free tokens of t′) < ‖c‖. Unary declaration, and the "inseparability of machine and tape", therefore carry no weight: any surplus of held nodes pays the descent, junk included. With that claim gone, the two-tape model restates T3 and Lemma 2.7. The machine it predicts runs identically with H deleted, so no execution of it can bear on self-justification.
