# ADR-0006 — Formalizing the R4 metatheory in Ansatz

**Status.** Accepted 2026-09-27. In progress: F1–F3 done (Lemma 3.6, Theorems 1–3, Corollary 3.7); F4 begun; F5 in progress; F7's encoding done, its checker open; F6 not begun.

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
| §1.6 Check, via Lemmas 2.6–2.8, E1, E5 | `CheckSpec` (model) | hypothesis | the trust base; F7 is to discharge it. Its first clause also gives the decoded type's formation |
| §1.6 the encoding, E1, E5 | `encE`, `encE_inj`, `E5`, `base_enc`, `checkspec_sat` (encode) | proved | E1 holds on all expressions; CheckSpec is satisfiable (by the checker that accepts nothing), so no theorem is vacuous through it. The checker itself (CheckSpec's first clause) is F7's open part |
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
| Propositions 4.3, 4.4 (1) | `prop43`, `prop44_1` (prop434) | proved, given `TokSize` (4.3) and also `TypeSize` (4.4) | two consequences of E2–E4, each as weak as its use: an accepted certificate is larger than twice its term's free tokens (`TokSize`; the paper's 1 + m + f implies it) and than its type's code (`TypeSize`); trust base of §2. μ(A) enters as a lower bound; 4.4 uses ¬ᵏ⁺¹1 and a minimal certificate; 4.4 (2), (3) open |
| Proposition 4.10′ (H does not give H₁) | `prop410_prime` (prop410); `lemma36_noH1`, `rt_tr` | proved | χ accepts every code above a size bound (the paper changes Check at two pairs); "without H₁" is `noH1` of the term |
| Theorem 4, Corollary 5.1 | `theorem4`, `Theorem_4`, `corollary51`, `corollary51_conv`, `corollary51_nodes` (eval, theorem4) | proved | simply typed terms with `argsOK` (skOf defined at every argument; not a branch-list head). `corollary51` keeps `ConvCase`; `corollary51_conv` discharges it by `conv_all`. `corollary51_nodes` is Theorem 3 at Θₙ: an `R` denotation has at most `n` internal nodes |
| Erasure, evalᴱ, E, the trace | `Er`, `er_total`, `usk_skel`, `ebase_rel`, `erdflt`, `erdflt_ty`, `envE`, `e_abort`, `EvE`, `Erel`, `Ok`, `OkE` (erase); `usk_cv`, `erel_cv`, `erel_subst1`, `henv_of_usk` (uskel); `AdeqE`, `adeqE_var`, `denU_lam0`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw` (funde) | defaults, the environment relation, substitution invariance of E, path congruence, and the constant, variable, and λ cases of the fundamental property, proved; the rest of the fundamental property of evalᴱ and Theorem 5.2 are open | erasure is a relation because `Rt` is `Prop`. `dec` returns a term, so reflect's success rule quantifies over an `Er` of the decoded judgment (uniqueness of erasure is not proved). `E` at `Σ₀` forgets the first component: the paper writes `(⋆, b)`, but the runtime default of a product is `(dflt σ, dflt τ)`, so requiring `⋆` would leave `abort` of `Σ(y :₀ Bool). 1` unrelated. `erdflt` / `erdflt_ty` are the defaults paragraph. `envE` leaves usage 0 unconstrained. `e_abort` and `e_h1` return those defaults after evaluating the premises; they are not Theorem 5.2's vacuous cases. `usk_hd` is invariance of `E` under a head step (`T(tt) ⇝ 1`, `T(ff) ⇝ 0`). Path congruence is `usk_cv` (uskel), and `usk_step_needs_wf` is the ill-typed β. Substitution invariance is `usk_subst` / `erel_subst1`; `henv_of_usk` transports a carrier environment along `usk_ctx`, and is a function because `HEnv` is a type. The fundamental property of evalᴱ is proved for constants, variables, and λ (`adeqE_star`, `adeqE_tt`, `adeqE_ff`, `adeqE_zero`, `adeqE_lbl`, `adeqE_var`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw`). `denU` and `henv_of_usk` are functions because their types are not propositions. Induction is on `Er`, which carries the erased term; `Rt` is a proposition and cannot return it. Theorem 5.2's relation `S` is not yet proved |
| §4 (the rest), §6 | — | open | F4 derivations and Proposition 5 |

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
