# ADR-0006 — Formalizing the R4 metatheory in Ansatz

**Status.** Accepted 2026-09-27. In progress: F1–F3 done (Lemma 3.6, Theorems 1–3, Corollary 3.7); F4 mostly done (4.6, 4.8 open; 4.9 in part); F5 in progress (Theorem 4′ assembly, Theorem 5.2); **F7 done** (2026-10-03: a concrete, computing checker meets CheckSpec, TokSize and TypeSize, and accepts the certificates); F6 not begun.

**Branch.** `adr-0006-formalization`, from `sjas-codification`, merged back
into it phase by phase.

**Depends on.**
- [`R4-metatheory.md`](R4-metatheory.md), the metatheory to be formalized;
- [ADR-0005](ADR-0005-lcert-implementation.md), whose Ansatz kernel
  (`code/lcert/ansatz/lcert/verified.clj`) this extends.

## Context

The goal set on 2026-09-27: "Formalize both the statements and the proofs of
the metatheory, using the Ansatz Lean-in-Clojure library."

So far Ansatz checks six small facts (ADR-0005). Everything else in
`R4-metatheory.md` is a paper proof, reviewed six times.

**What the spikes of 2026-09-27 found** (`../LOG.md`, that date):
- *Works:* inductive types, indexed inductive families in `Prop`, the tactics
  `apply`, `exact`, `induction`, `cases`, `omega`, `rfl` and others, and
  large elimination. The set-theoretic model of §3 needs large elimination: a
  `Type`-valued function on skeletons.
- *Needs a workaround:*
  - `a/defn` cannot define a `Type`-valued or dependently typed function. It
    mis-infers the recursor's universe, and its Clojure code generator cannot
    compile a type.
  - The workaround: elaborate the term with Ansatz's surface elaborator, name
    the recursor's universe explicitly (`Sk.rec.{2}`), and install the
    definition with the kernel's `check-constant`, bypassing code generation.
    This checks the definition with the kernel, like any other.
- *Surface details:* the arrow is `=>`. Inductive predicates need `:in Prop`.
  The Clojure reader needs universe levels written as `Sk.rec$2`, rewritten
  after reading.

## Decision

**1. What "formalized" means here.** Every numbered definition, lemma,
proposition and theorem of `R4-metatheory.md` gets a kernel-checked
counterpart: a statement, and a proof with no `sorry` and no axioms beyond
those listed in 2.
- A result is *formalized* when its statement and proof are accepted by the
  Ansatz kernel.
- A result is *stated* when only its statement is, with the proof pending,
  which this document then records.

**2. The trust base.** The only assumptions are these, each an explicit
hypothesis of the theorems that use it, never a global axiom:
- **The checker's specification** (`CheckSpec`). The metatheory uses `Check`
  only through Lemmas 2.6–2.8 and the encoding properties E1–E6. The model's
  theorems therefore quantify over any `chk` that meets those properties.
  Constructing the concrete `Check` in Ansatz and proving it meets them is
  phase F7.
- **The three cited results of §6** (S1–S3), for P5 only. Gödel's second
  theorem for PA, PA's Σ₁-completeness, and E-PA^ω's conservativity are not
  proved here. They enter as hypotheses of the P5 theorem.
- Lean's `Init` environment, as bundled with Ansatz.

**3. The design.**
- *A deep embedding:* one inductive type `Exp` for types and terms, as the
  encoding already treats them, with de Bruijn indices; contexts as lists of
  usage–type pairs.
- *The typing judgment:* an inductive predicate `Der chk σ Γ t A`,
  parameterized by the checker used for δ-steps; conversion as an inductive
  predicate over single recorded steps.
- *The model:* carriers `Car : Sk → Type` by large elimination. Denotation is a
  relation `Den n η t v`, since a `Prop` judgment cannot be eliminated into
  `Type`. `V` is defined by recursion on types into `Prop`.

**4. The phases,** each a namespace under `code/lcert/formal/`, loaded by a
new `bin/test-formal`:

| Phase | Content | Metatheory |
| --- | --- | --- |
| F0 | the `kdef` helper, conventions, the suite | — |
| F1 | usages and their laws; `Exp`, contexts, lifting and substitution | §1.1–1.3 |
| F2 | the rule table, conversion, skeletons; the syntactic lemmas | §1.4–1.5, §2 |
| F3 | carriers, defaults, denotation, `V`, environments; the fundamental lemma; T1, Corollary 3.7, T2, T3 | §3 |
| F4 | §4: Propositions 4.1–4.12, Theorem 4.6, Proposition 4.10′ | §4 |
| F5 | the evaluators; T4, T4′, Theorem 5.2 | §5 |
| F6 | P5, relative to S1–S3 | §6 |
| F7 | the concrete encoding and `Check`, and the proof of `CheckSpec` | §1.6 |

**5. Testing.** A formal statement is its own test: the kernel rejects it
until it is proved, which is the red step. Each namespace also carries
negative checks, such as a false variant the kernel must reject, so that the
statements are not vacuous.

## F7 plan: the concrete checker

CheckSpec, TokSize and TypeSize are hypotheses until a concrete `Check` meets
them. The plan, in dependency order (namespace `check.clj` and successors):

1. **Decidable equality** of codes and expressions (`codeEq`, `expEq`, through
   the injective encoding). *Done.*
2. **Derivation trees as data.** An inductive type `DT` (in `Type`) with one
   constructor per rule of `Rt`, `Tl`, `SkJ` and conversion, carrying the
   rule's explicit fields and premise trees, and `concl : DT → Option judgment`.
3. **Checkers with soundness,** one `Bool` check per rule:
   - head steps (`Hd`, 18 kinds) and steps at a path (`getP`/`setP`);
   - skeleton typing (`SkJ`, about 40 rules) and `nbr`;
   - conversion chains (`Cv`);
   - the type-level (`Tl`) and runtime (`Rt`) rules: the side conditions
     (lookups, usage arithmetic, closedness, base codes) and that each
     premise tree concludes what the rule needs.
   Soundness: `check d = true → concl d = some J → J derivable`, by induction
   on `d`, generated per rule as the other rule-table proofs are.
4. **The encoding of derivations** (`encD : DT → Code`, as `lcert.encode`'s
   `enc-deriv`) and its decoder, with the round trip.
5. **Check and dec:** `Check c d` decodes `c` to a tree, checks it, and
   compares its conclusion's type code with `d`; `dec` returns its budget,
   term and type. Then CheckSpec's first clause (Lemmas 2.6–2.7) from
   soundness, and TokSize and TypeSize from the encoding's shape (E2–E4).

Steps 2 and 3 are the bulk; each inductive's checker is independent once
the tree type exists, so they can proceed in parallel.

**Step 3 done (2026-10-02):** `check_der.clj`'s `dtCheck chkf t J` and
`dtCheck_sound` (`dtCheck chkf t J = true → Holds chkf J`, every `chkf`, no
hypothesis), over Codex's `check_hd`/`check_skj`/`check_dt`.

**Step 5, refined: the circularity.** CheckSpec asks that an accepted code
decode to a derivation *at the checker itself*, and δ-steps inside a
derivation consult the checker. `prop410.clj`'s `rt_tr` does not break the
circle: its bound is only shown to exist, and it measures the second code.
So:
- *5a, transport for data.* `dtCheck chk1 t J = dtCheck chk2 t J` whenever
  the checkers agree on first codes smaller than `t`'s largest δ-code
  (`HdDT.delta` records both codes), by induction on `DT`.
- *5b, the checker,* by recursion on the certificate's size: decode `c` to
  a budget, term, type and two trees (typing, and the type's formation);
  check both at `Check` restricted to strictly smaller first codes; require
  every δ-code of the trees to be smaller than `c`; and test the size facts
  directly — `m < ‖c‖`, TokSize's `2f < ‖c‖`, TypeSize's `‖⌜A⌝‖ < ‖c‖`.
  CheckSpec's first clause at `Check` follows from `dtCheck_sound` and 5a;
  its other clauses as in `checkspec_sat`.
- *5c, completeness:* encoding trees as codes, the round trip, and: the
  encoding of a valid derivation, padded if necessary, is accepted. This is
  what makes `Check` a checker of the certificates rather than merely sound.

**Steps 5a–5c done (2026-10-03).** `check_agree.clj` (5a: `dtCheck_agree`),
`check_spec.clj` (5b: `Check decD`, `check_fix`, `check_full`, and
`check_spec` / `check_toksize` / `check_typesize` — CheckSpec, TokSize and
TypeSize for every decoder), `certenc.clj` (5c: the encoding of certificates,
`decCert`, round trips for every type a certificate holds, `decCert_enc`, and
`check_complete`: a typing tree and a formation tree that `Check decCert`
accepts as derivations give a certificate it accepts). The three hypotheses
of the trust base are therefore theorems about one concrete checker,
`Check decCert`; the metatheory's results, stated for any checker meeting
them, apply to it.

*Labels (2026-10-05).* Certificates first wrote numbers as single leaves
`sl n`, so an accepted certificate with a budget or fuel of 100 or more
could not be held as an `R` value (`lblOk`). Numbers are now unary, and
`check_complete_lbl` gives a certificate that is `lblOk` as well as
accepted, given label constants and raw codes below 100 in its data.

*Deviation.* The paper derives the size facts (Lemmas 2.6–2.7) from its
encoding; `Check` tests them, so they hold of every accepted code by
construction, and completeness pads a certificate that is too small. The
hypotheses CheckSpec, TokSize and TypeSize are then theorems of one
concrete checker.

**F7 part 1 (2026-10-02, `f7-checker`).** Steps 2 and the initial parts of
3 now have these kernel-checked definitions and soundness proofs:

- `check_hd.clj`: `HdDT` records all 18 `Hd` rules. `hdCheck_sound` proves
  `hdCheck chkf tree e e2 = true → Hd chkf e e2`; its 18 case lemmas are
  `hdCheck_<rule>_sound`. Both endpoints are compared with `expEq`, β
  contracta use the existing `subst1`/`substL`, and the recorded `caseLb`
  branch and δ codes are checked by `nthB` and both `codeOf` calls.
  `stepCheck_sound` proves
  `stepCheck chkf p tree e e2 = true → Step chkf e e2`, checking `getP`
  and the entire `setP` result.
- `check_skj.clj`: `SkDT` records all 40 `SkJ` rules and premise trees.
  `skjCheck_sound` proves
  `skjCheck w G e s tree = true → SkJ w G e s`, with 40 lemmas named
  `skjCheck_<rule>_sound`. The tree records otherwise undetermined
  skeletons, rather than inferring them with `skOf` (which returns `none`
  on branch lists). The context is supplied to the checker; each rule
  computes its premises' context extensions. Mode, expression, skeleton,
  lookup/base-type side conditions and every premise are checked.
- `check_dt.clj`: `DT` has all 30 `Tl`, 28 `Rt` and 3 `Cv` constructors.
  Its ordinary premise trees are recursive `DT` fields; conversion's
  skeleton and step premises reuse `SkDT` and `StepDT`. `DTJ` represents
  their conclusions as data, and `concl : DT → Option DTJ` extracts the
  claimed conclusion. Every constructed tree has a conclusion, so this
  extractor always returns `some`; it deliberately accepts malformed
  records as data. `stepDTCheck_sound` proves the stored-step wrapper
  sound. There is no soundness claim for `concl` alone.

This decomposes the planned single tree type into reusable `HdDT`, `SkDT`,
`StepDT` and `DT`. No existing definition or rule was changed, and no
completeness theorem is needed or claimed. The `Cv` checker (including
its existing `nbr` conditions), `Tl`/`Rt` checkers, derivation encoding and
decoding, final `Check`/`dec`, and the proofs of `CheckSpec`, `TokSize` and
`TypeSize` remain open. In particular the supplied `chkf` is still a
parameter, not the final self-referential concrete checker.

Each new namespace has a registered formal suite. The focused REPL runs
passed 9 tests / 318 assertions, including acceptance for every head and
skeleton rule, both δ outcomes, bad decoded-code claims, failed lookups,
wrong annotations, invalid paths and altered siblings. See the
[F7 work log](../LOG.md#2026-10-02--f7-derivation-data-and-the-first-concrete-checkers)
for the fresh full-suite result and commit preservation details.

## F6 plan: P5 at the meta level (decided 2026-10-05)

The user's decision: formalize P5 at the meta level now, with S1–S3 and the
two internalizations (R4's Proposition 6.1 inside PA, Lemma 6.4 inside
E-PA^ω) as named hypotheses. Every hypothesis that is not proved
independently gets a rigorous informal justification, with exacting paper
proofs and precise citations. Phase 2 proves them with Mathlib and
FormalizedFormalLogic/Foundation (Saitou–Noguchi, arXiv:2609.13780: G2 for
Δ₁-definable consistent T ⊇ IΣ₁).
- **F6.1** — H_PA as a deep embedding (R4 §6.2); semantics in ℕ;
  soundness, so PA is consistent (proved, not assumed).
- **F6.2** — E-PA^ω syntax and provability, enough to state S3 and Int-6.4.
- **F6.3** — the meta content of Prop 6.1: every H_PA proof of φ gives a
  budget-0 derivation of ⟦∀ᶜˡφ⟧ (the §6.2 templates); 0 = S0 gives a
  refutation.
- **F6.4** — the meta content of Lemma 6.4: the default model (reflect ↦
  dflt) is sound without budget induction, via Corollary 3.7 (BF₀, BF₁).
  *Route (2026-10-05):* an instance of Theorem 4.6's generic model
  (`f4-theorem46`, an R-interpretation `ri`) with `print` the identity and
  `riPf` constantly false, so `reflect` always returns its default and
  `PhCons` is Corollary 3.7 at every size; no second fundamental lemma.
- **F6.5** — §6.4: from S1, S3, Int-6.1, Int-6.4 and F6.1's consistency, no
  budget derives Con′_ω; then Prop 4.8. The hypotheses must fix *natural*
  presentations of CHECK and Prf_PA (G2 fails for some nonstandard
  provability predicates: Feferman 1960).
- **F6.6** — a document of the hypotheses: exact statements; verified
  citations with theorem numbers (S1: Gödel 1931, Hilbert–Bernays 1939,
  Feferman 1960; S2: Σ₁-completeness, e.g. Hájek–Pudlák 1993; S3: Troelstra
  1973, Kohlenbach 2008, Avigad–Feferman 1998); exacting paper proofs of
  Int-6.1 and Int-6.4.

## Success and failure

- **Success:** F1–F6 formalized in the sense of 1, with the trust base of 2,
  and F7 formalized, or recorded as the one open obligation with the reason.
- **Failure:** a result that cannot be formalized because it is false. The
  metatheory is then corrected and the correction recorded. Or a result that
  cannot be formalized because of Ansatz. That is recorded, with the
  limitation, as a stated-only result.
- **A review question for each phase:** does the formal statement say what the
  paper says? Differences are recorded here.

## Deviations recorded during the work

Each is a difference between design 3 above, or the paper, and what the kernel
now checks.
- **Denotation is a function, not a relation.** `den n t G s η : Car s` is
  defined as a table of levels, `denT n n`. `T (k+1) m` reuses row `k` below
  level `m` and re-evaluates at `m`. Reflect runs its decoded program at
  exactly `⟦t′⟧ᵐ` (`denT_stable`). Typing is split into `Tl` (formation) and
  `Rt` (runtime, with usage vectors), with skeleton typing `SkJ` beside them.
- **Branch lists are a constructor, `tBrs P k`,** with skeleton `Lbl → skel P`.
  Ansatz's inductive compiler cannot derive recursors for function-typed
  fields. The first skeleton given to `tBrs` broke Lemma 2.5, which Codex
  found; the present one does not.
- **`SkJ`'s reflect rule requires a base type.** Grok found that Lemma 3.1
  fails for reflect at an open type. The paper's rule has the same premise
  implicitly, since `chk′`'s type is closed. The formal rule states it.
- **Formation premises and well-formed contexts.** App₀/App, Pair₀/Pair and
  Let (and their type-level counterparts zApp, zPair, zLet) carry the
  formation premises of their Π or Σ type (fPi/fSig's), and Lemma 3.6
  assumes a well-formed context (`WFCtx`). The paper presupposes both;
  without them a branch list can reach argument position, where skeleton
  inference has nothing to give it, and the fundamental lemma fails.
- **Branch lists are read positionally.** `V(tBrs P k)` reads the branch
  for label l ≥ k at the list's position l − k, matching the denotation of
  `bcons` (first element at 0). The first version read position l, which
  made the Bcons case unprovable for k > 0.
- **Conversion requires `nbr` at every chain element.** Internal branch lists
  may occur only in `caseL` branch position or a branch-list tail; the paper
  has no branch-list terms. This excludes `conv_skj_counterexample`, whose
  skeleton-typed beta step changes denotation.
- **Codes and certificates have labels below NL.** The carrier of Syn and R
  is `Code`, trees with natural-number labels; the paper's are trees over the
  finite label set L. So `V(Syn)` and `V(R)` also require every label below
  NL (`lblOk`). Without it the recursors over codes and certificates (RecSyn,
  ItR) could not pass a tree's labels to methods expecting `Lbl`.
  *The encodings respect it (2026-10-05).* L is the 97 encoding labels plus
  3 free ones (`lcert.syntax`); the formal encodings now introduce only
  encoding labels — `tBrs` moved from label 100 to 11, and the certificate
  format's numbers became unary — and `enclabels.clj` proves, for every bound
  `nb ≥ 97`, that an encoding's labels are below `nb` whenever its data's are
  (`lblBelow_encE`, `lblBelow_encCert`; `lblOk_lblBelow` at 100). So the
  bound survives making |L| a parameter.
- **Bcons carries its motive's formation** (`hP`), as CaseL does; the
  fundamental lemma's Bcons case reads it. RecSyn carries the formation of
  its node branch's two added types (`hY1`, `hY2`), for that branch's
  well-formed context. Both are admissible in the paper, where every type in
  a derivation is formed.
- **`V` takes the skeleton explicitly,** since carriers depend on it.
- **Lemma 2.4 takes closed formation explicitly.** `lemma24` also assumes
  `Tl chkf Bool.true (List.nil Exp) A Exp.tUnit`, then weakens that formation
  judgment into the combined token context (`tl_theta_closed`). No existing
  regularity/strengthening theorem supplies it from the two runtime premises;
  `closedTy A = true` only checks variable scope. The typing definitions are
  unchanged. Discharging this extra hypothesis for the fundamental lemma's
  H₁ case remains an integration obligation.

- **H_PA uses de Bruijn variables** (F6.1, `pa.clj`). ∀ binds index 0; A4 is
  `∀φ → φ[t/0]` (substituting and lowering), A5 `∀(φ↑ → ψ) → (φ → ∀ψ)`, E1
  `i = i`, E2 renames a variable in an atom, Q1–Q6 use indices 0 and 1, and
  Gen generalizes index 0. This is R4's system up to the standard
  equivalence of named and nameless syntax; it removes the "free for" side
  conditions.

## Statement index

Each paper result, the kernel constant that states it, and where the formal
statement differs. "Proved" means kernel-checked with no axioms; every
theorem quantifies over `chkf dec encTy`, and those using the checker take
`CheckSpec` as a hypothesis.

| Paper | Constant (namespace) | Status | Differences |
| --- | --- | --- | --- |
| §1.1 usage algebra | `uadd_*`, `umul_*` (usage) | proved | — |
| §1.4 typing rules | `Tl`, `Rt` (judgment); `SkJ` (conv) | defined | `tBrs` pseudo-type for branch lists; App₀/App/Pair₀/Pair/Let carry their Π/Σ formation; Bcons, RecSyn and Inspect carry the formation of the types they add (see the deviations) |
| §1.5 conversion | `Hd`, `Step`, `Cv` (conv) | defined | steps are recorded with a position |
| §1.6 Check, via Lemmas 2.6–2.8, E1, E5 | `CheckSpec` (model); `check_spec`, `check_toksize`, `check_typesize`, `check_complete` (check_spec, certenc) | proved of `Check decCert` | discharged by F7: a concrete checker meets the trust base and accepts the certificates. The size facts are tested by the checker rather than derived from the encoding (deviation above) |
| §1.6 the encoding, E1, E5 | `encE`, `encE_inj`, `E5`, `base_enc`, `checkspec_sat` (encode) | proved | E1 holds on all expressions; CheckSpec is satisfiable (by the checker that accepts nothing), so no theorem is vacuous through it. The checker itself (CheckSpec's first clause) is F7's open part |
| §1.5–1.6 checker, part 1 | `hdCheck_sound`, `stepCheck_sound` (check-hd); `skjCheck_sound` (check-skj); `stepDTCheck_sound`, `DT`, `concl` (check-dt) | head/path/skeleton soundness proved; derivation data defined | recorded rule data and hidden skeletons; all 18 `Hd` and 40 `SkJ` rules; `DT` records all `Tl`/`Rt`/`Cv` rules but their checkers and the final `CheckSpec` construction remain open |
| Lemma 2.1, weakening | `tl_weaken`, `rt_weaken` (derivations) | proved | arbitrary insertion; runtime entry has usage 0; exchange is not covered |
| Lemma 2.3, strengthening | `rt_strengthen`, `rt_mask` (strengthen) | proved | "not free" is `freshF`; `rt_mask` lowers a set of variables at once |
| Lemma 2.4 | `lemma24` (derivations) | proved | explicit closed formation premise; terms are `lift m2 m1 t1` and `lift m1 0 t2` |
| Lemma 2.5 | `lemma25_tl`, `lemma25_rt`, `skel_subst`, `cv_skel` (skeletons) | proved | — |
| §3.1–3.2 carriers, ⟦·⟧ⁿ | `Car`, `den` = `denT n n` (carrier, den) | defined | a table of levels (reflect runs at ⟦t′⟧ᵐ: `denT_stable`) |
| Lemma 3.1 | `lemma31`, `den_subst1`, `den_substL` (substitution) | proved | hypothesis `SubOK` (substituted terms have `skOf`) — see the counterexample `lemma31_skj_counterexample` |
| §3.3 V | `V` (sem) | defined | explicit skeleton argument |
| Lemma 3.3 monotonicity | `V_mono` (mono) | proved | — |
| Lemma 3.3 substitution | `lemma33_subst`, `V_subst1`, `V_substL` (substitution) | proved | `SubOK`; stated at skeleton `skel A` |
| Lemma 3.3 conversion, Lemma 3.2 | `hd_den`, `den_step_nil` (conversion); path congruence, `cv_V`, `F_conv`, `conv_all` (convcase) | proved | `Cv` requires `nbr` (no branch list in argument position) of every element; without it Lemma 3.2 fails (`conv_skj_counterexample`) |
| Lemma 3.4 | `Lemma_3_4` (mono) | proved | — |
| Lemma 3.5 | `EnvSat_split`, `EnvSat_omega` (splitting) | proved | plus `EnvSat_one`, `EnvSat_mono` |
| Lemma 3.6 | `Lemma_3_6` (model), `Lemma_3_6_holds` (convcase); `lemma36` (lemma36); cases `F_*` (fundamental, outer, recsyn, convcase) | proved | well-formed contexts (`WFCtx`); Syn and R values have labels below NL |
| Theorem 1, Corollary 3.7 | `Theorem_1`, `Corollary_3_7` (model); `Theorem_1_holds`, `Corollary_3_7_holds` (convcase); `theorem1`, `cor37_*` (lemma36) | proved | — |
| Theorem 2 (H) | `Theorem_2_H` (model) | proved | |
| Theorem 2 (H₁) | `Theorem_2_H1` (model) | proved | — |
| Theorem 3 | `theorem3`, `theorem3_tokens` (lemma36) | proved (with `conv_all` for its Conv hypothesis) | the refinement needs the budget to reach the token count |
| Proposition 4.1, 4.7 | `prop41_closed`, `prop41_fun`, `prop47` (section4) | proved (4.1 given the Conv case) | 4.1(ii) for codes with labels below NL |
| Propositions 4.2, 4.5, 4.10 | `prop42`, `prop45_d3`, `prop45_contraction`, `prop410` (section4b) | proved | labels below NL as hypotheses (`lblOk v`, `lblOk ⌜A⌝`) |
| Propositions 4.3, 4.4 | `prop43`, `prop44_1`, `prop44_2`, `prop44_3` (prop434) | proved, given `TokSize` (4.3, 4.4 (1), (2)) and `TypeSize` (4.4) | two consequences of E2–E4, each as weak as its use: an accepted certificate is larger than twice its term's free tokens (`TokSize`; the paper's 1 + m + f implies it) and than its type's code (`TypeSize`); trust base of §2. μ(A) enters as a lower bound; 4.4 is refuted at ¬ᵏ⁺¹1 (the paper's A_j is 1 ⊸ ⋯ ⊸ 1) with a minimal certificate as hypothesis; 4.4 (3) needs only `TypeSize` |
| Proposition 4.10′ (H does not give H₁) | `prop410_prime` (prop410); `lemma36_noH1`, `rt_tr` | proved | χ accepts every code above a size bound (the paper changes Check at two pairs); "without H₁" is `noH1` of the term |
| Theorem 4, Corollary 5.1 | `theorem4`, `Theorem_4`, `corollary51`, `corollary51_conv`, `corollary51_nodes` (eval, theorem4) | proved | simply typed terms with `argsOK` (skOf defined at every argument; not a branch-list head). `corollary51` keeps `ConvCase`; `corollary51_conv` discharges it by `conv_all`. `corollary51_nodes` is Theorem 3 at Θₙ: an `R` denotation has at most `n` internal nodes |
| Erasure, evalᴱ, E, the trace | `Er`, `er_total`, `usk_skel`, `ebase_rel`, `erdflt`, `erdflt_ty`, `envE`, `e_abort`, `EvE`, `Erel`, `Ok`, `OkE` (erase); `usk_cv`, `erel_cv`, `erel_subst1`, `henv_of_usk` (uskel); `AdeqE`, `adeqE_var`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw`, `adeqE_app0`, `adeqE_app1`, `adeqE_appw`, `adeqE_pair0`, `adeqE_pair1`, `adeqE_pairw`, `adeqE_conv`, `adeqE_let0`, `adeqE_let1`, `adeqE_letw`, `adeqE_ite`, `adeqE_elimB`, `adeqE_case`, `adeqE_succ`, `adeqE_sleaf`, `adeqE_snode`, `adeqE_leaf`, `adeqE_node`, `adeqE_bnil`, `adeqE_bcons`, `adeqE_recN`, `adeqE_recs_leaf`, `adeqE_recs_node`, `adeqE_recs`, `adeqE_recS`, `adeqE_itr`, `adeqE_itR`, `adeqE_prn`, `adeqE_chk` (funde) | defaults, the environment relation, substitution invariance of E, path congruence, and the constant, variable, λ, application, pair, conversion, let, if, elimBool, caseLbl, constructor, recN, recSyn, itR, print, and chk′ cases of the fundamental property proved; inspect, abort, H₁, reflect, the induction assembling AdeqE, and Theorem 5.2 are open | erasure is a relation because `Rt` is `Prop`. `dec` returns a term, so reflect's success rule quantifies over an `Er` of the decoded judgment (uniqueness of erasure is not proved). `E` at `Σ₀` forgets the first component: the paper writes `(⋆, b)`, but the runtime default of a product is `(dflt σ, dflt τ)`, so requiring `⋆` would leave `abort` of `Σ(y :₀ Bool). 1` unrelated. `erdflt` / `erdflt_ty` are the defaults paragraph. `envE` leaves usage 0 unconstrained. `e_abort` and `e_h1` return those defaults after evaluating the premises; they are not Theorem 5.2's vacuous cases. `usk_hd` is invariance of `E` under a head step (`T(tt) ⇝ 1`, `T(ff) ⇝ 0`). Path congruence is `usk_cv` (uskel), and `usk_step_needs_wf` is the ill-typed β. Substitution invariance is `usk_subst` / `erel_subst1`; `henv_of_usk` transports a carrier environment along `usk_ctx`, and is a function because `HEnv` is a type. The fundamental property of evalᴱ is proved for constants, variables, λ, and application (`adeqE_star`, `adeqE_tt`, `adeqE_ff`, `adeqE_zero`, `adeqE_lbl`, `adeqE_var`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw`, `adeqE_app0`, `adeqE_app1`, `adeqE_appw`, `adeqE_pair0`, `adeqE_pair1`, `adeqE_pairw`, `adeqE_conv`). At Π₀ the runtime argument is ⋆; the denotation still applies ⟦f⟧ to ⟦u⟧. At Σ₀ the first component is ⋆ and is forgotten by E. Conversion transports the relation by `usk_cv` and the carrier by `denU_cv`. Let (`adeqE_let0`, `adeqE_let1`, `adeqE_letw`) evaluates the erased scrutinee to a pair and the erased body under (second, (first, ρ)); the body's judgment is at `lift 2 0 C`, brought back by `denU_lift`, and `den_let_body` is `den_letp_some` once `skOf` of the scrutinee is the product. At Σ₀ the first runtime component is unconstrained. The tail environment is `envE` at the body's usage vector. `if` (`adeqE_ite`) selects the branch at one type. `elimBool` (`adeqE_elimB`) moves the branch from `P[tt]` or `P[ff]` onto `P[b]` by `skel_subst_eq`, because a Boolean term has skeleton Unit. `caseLbl` (`adeqE_case`) is application of the branch list, and `P[a/x]` is `P` after a Unit substitution. Constructors (`adeqE_succ`, `adeqE_sleaf`, `adeqE_snode`, `adeqE_leaf`, `adeqE_node`, `adeqE_bnil`, `adeqE_bcons`) build the value from related arguments; a node evaluates its token and drops it, and `bnil` returns the related default. `recN` (`adeqE_recN`) inducts on the numeral: the step runs under the accumulator and the predecessor, `stepTy` has the skeleton of `P`, and the conclusion `P[n]` agrees with `P` because a numeral has skeleton Unit. The leaf method of recSyn (`adeqE_recs_leaf`) binds the label at usage ω; `leafTy`, `nodeTy`, `y1Ty` and `y2Ty` are substitutions of terms, so they have the skeleton of the motive. The node method (`adeqE_recs_node`) binds the two recursive results at usage 1, cast from `usk P` into `y1Ty` and `y2Ty`, and the two codes and the label at usage ω. `adeqE_recs` inducts on the code. `denU_recS` is that `Code.rec`; the cast stays outside the recursor. `adeqE_recS` is the judgment at `P[c]`, which agrees with `P` because a code term has skeleton Unit. The methods are moved off `leafTy` and `nodeTy` by `adeqE_leaf_at` and `adeqE_node_at`. The itR recursion (`adeqE_itr_leaf`, `adeqE_itr_node`, `adeqE_itr`) applies `g` to the label and `h` to the token, the label, and the two recursive results; `gTy` and `hTy` quantify the motive under lifts, so the results are cast back to `usk X`. A token is related at ◇. `denU_itR` is that `Code.rec`; the cast stays outside the recursor. `adeqE_itR` evaluates `g`, `h`, and the certificate, then folds them. `adeqE_prn` returns the certificate's code. `adeqE_chk` returns Check of the two codes. `denU` and `henv_of_usk` are functions because their types are not propositions. Induction is on `Er`, which carries the erased term; `Rt` is a proposition and cannot return it. Theorem 5.2's relation `S` is not yet proved |
| Proposition 4.11 | `prop411`, `prop411_typed`, `prop411_den`, `prop411_nodes` (section4c) | proved | cₙ is the chain of ω-lets `sh4_cn`; label ℓ is 0 (below NL). ⟦cₙ⟧ is the complete binary code `sh4_bush` (2ⁿ internal-node-count as cnodes + 1 = 2ⁿ, since Nat subtraction truncates; `prop411_nodes` is cnodes = 2ⁿ − 1). The certificate-size table is the paper's measurement, not a theorem |
| Proposition 4.12 (1) | `prop412`, `prop412_typed`, `prop412_den`, `prop412_nodes` (section4c) | proved | the motive is Σ(p :ω Syn). 1, which does not depend on the Nat; the step opens the package and rebuilds a node. Part (2), polynomial certificate size, is the paper's citation of T3 and Proposition 4.1 |
| Proposition 4.9 | `sh4_depth0_typed`, `sh4_from_ff`, `sh4_cv_ff` (section4c) | partial | k = 0 of `depthLeq` (tt on a leaf, ff on a node) is a closed budget-0 term of Π(c :ω Syn). Bool. Where that bound has computed to ff, T(ff) ▹ 0 by Hd.tTF, so the evidence inhabits 0. The closed inhabitant of Π(c). T(depthLeq c k) ⊸ T(chk′ c c⊥) ⊸ 0 is not constructed: δ needs a canonical code, so the paper's proof case-splits to depth k (100-way caseLbl, two subcodes), and that derivation was not built |
| §6.2 H_PA; §6.4 step 4 (PA is consistent) | `PT`, `PF`, `PAx`, `PPrv`, `paHolds`, `paPrv_sound`, `pa_consistent` (pa) | proved | de Bruijn variables (A4/A5/E2 without "free for"); DN is sound by stability (`paHolds_stab`), with no classical axiom |
| Proposition 5, Proposition 4.8 | `ConOmega`, `prop5`, `prop48`, `prop48_lolli`; `p5Apply_w`, `p5Apply_1` (p5) | proved from named hypotheses | at the concrete checker `Check decCert`; hypotheses S1, S3, Int-6.1, Int-6.4 and two anchors, stated and justified in [P5-hypotheses.md](P5-hypotheses.md); PA's consistency is proved (pa) |
| §4 (the rest) | — | open | the inhabitant of Proposition 4.9; Theorem 4.6 (branch `f4-theorem46`) |

## Review guide

The kernel checks every proof, so a review adds most by checking what the
kernel cannot: that each formal statement says what the paper says.
- **Definitions against the paper:** `Rt`/`Tl` against §1.4, `Cv` against
  §1.5, `den` against §3.2, `V` against §3.3, `EnvSat` against §3.4.
- **Hypotheses that could make a theorem vacuous:** `CheckSpec` (is it
  satisfiable by the real `Check`? is it all the metatheory uses?), `SubOK`,
  the `clean` premise.
- **Generators** (`tools/*.py`, and the Clojure generators in `unfold`,
  `mono`, `syntactic`, `fundamental`): do they emit the intended statements?
- **Deviations** above: do any change what the theorems mean?
Proofs need not be read. `bin/test-formal` rebuilds and re-checks everything.

## Consequences

- The metatheory's status line will change from "paper proofs" to naming what
  the kernel has checked.
- The formal suite is slow, since Ansatz elaborates every declaration at load.
  It is kept separate from the fast and extended suites.
