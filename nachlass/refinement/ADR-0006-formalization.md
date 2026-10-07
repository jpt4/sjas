# ADR-0006 — Formalizing the R4 metatheory in Ansatz

**Status.** Accepted 2026-09-27. Updated 2026-10-07. F1–F3 done (Lemma 3.6, Theorems 1–3, Corollary 3.7). F4 mostly done: Theorem 4.6, Lemma 4.6a and Corollaries 4.6′/4.6″ are proved at F7's checker with only the paper's hypotheses (branches `f4-theorem46` and `f7-enc46`; F7's format is now canonical and unpadded, and the padded format's failure of Enc46 is kept as a theorem); Proposition 4.8 is proved from P5's named hypotheses (F6.5); 4.9 in part. F5 done (Theorem 4′; Theorem 5.2, proved 2026-10-07: `theorem52`, `theorem52e`, `theorem52_concrete`). **F7 done** (2026-10-03; canonical certificate format 2026-10-06). F6 in progress: F6.1 (H_PA and its soundness, relative to the metatheory), F6.3a (the translation's substitution lemmas), F6.5 (Proposition 5) and F6.6 (the hypotheses document) done; F6.3 in progress; **F6.4 done** (2026-10-07: `lemma64`, `lemma64_theta`, `cor65_dflt`, `lemma64_concrete`, `cor65_concrete`, in `dflt64`).

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
- **The metatheory itself.** Every theorem here is a theorem of Lean 4's type
  theory as the Ansatz kernel implements it. So every consistency result —
  λᶜᵉʳᵗ₀'s (T1), PA's (`pa_consistent`) — is *relative*: it holds if that
  type theory is consistent (and sound for such statements). No theory proves
  its own consistency (G2); the metatheory is far stronger than PA or
  λᶜᵉʳᵗ₀. *Made explicit 2026-10-05, at the user's correction.*

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
them, apply to it. (Since 2026-10-06 `decCert`, `decCert_enc` and
`check_complete` are the canonical format's, in `certcanon.clj`; certenc.clj
keeps the padded format as `decCertPad`, `decCertPad_enc`,
`check_complete_pad`.)

*Labels (2026-10-05).* Certificates first wrote numbers as single leaves
`sl n`, so an accepted certificate with a budget or fuel of 100 or more
could not be held as an `R` value (`lblOk`). Numbers are now unary, and
`check_complete_lbl` gives a certificate that is `lblOk` as well as
accepted, given label constants and raw codes below 100 in its data.

*Deviation (2026-10-03 to 2026-10-06).* The paper derives the size facts
(Lemmas 2.6–2.7) from its encoding; `Check` tests them, so they hold of
every accepted code by construction, and completeness padded a certificate
that was too small. The hypotheses CheckSpec, TokSize and TypeSize are then
theorems of one concrete checker.

*The canonical format (2026-10-06, branch `f7-enc46`).* The padding made
Theorem 4.6's encoding fact false at F7 (`enc46f7.clj`), so the format
changed ([design note](../docs-f7-enc46-design.md)):
`encCert m t A T₁ T₂ = sn 96 ⌜(F, m, t, A, T₁, T₂)⌝ (sl 0)`, with the fuel
`F` fixed by the trees, raw codes written as literal terms
(`encC c = ⌜codeTerm c⌝`), and a decoder that accepts only the exact
encoding of what it decoded (`decCert_canon`). The size tests stay in the
checker's body (`check_spec.clj` is unchanged and generic over the
decoder), but they are now consequences of the encoding, as in the paper:
every canonical certificate passes them (`cert_size0`–`cert_size4`), and
`budget_canon`, `toksize_canon`, `typesize_canon` derive the three facts
from canonicity alone. Completeness needs no padding (`check_complete`,
`check_complete_lbl`, `certcanon.clj`). The padded format is kept as
`encCertPad`/`decCertPad` with `check_complete_pad`, as the historical
counterexample.

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
  soundness, so PA is consistent *relative to the metatheory* (the step is
  a theorem of the metatheory, not an assumption of P5).
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
  *Done 2026-10-07 (`dflt64.clj`, branch `f6-4-default-model`):* `riDflt` is that instance, and the route held. The generic assembly (`lemma36_ri`) still contains an outer induction on `n` (`outer_all_ri`), but only the phantom-free branches of Reflect and H₁ read it, and `riPf ≡ false` empties them; `F_refl_dfl` and `F_h1_dfl` are those two cases without `hout`, and `lemma64_step` is `lemma36_step_ri` with those two terms replaced. So `lemma64` is soundness at one budget by induction on the derivation only. `PhCons` is `phcons_of_spec` (Corollary 3.7, BF₀/BF₁, at every size). See the statement index and P5-hypotheses.md §5.
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
- **The phantom model of §4.5 is the model over an R-interpretation**
  (2026-10-06, branch `f4-theorem46`; design note
  [`../docs-theorem46-design.md`](../docs-theorem46-design.md)). `rint.clj`'s
  `RInt` fixes how certificates are read: the label `leaf a` builds, `print`,
  and phantom-freeness. Its laws L1–L4 are bundled in the type. The carrier
  of `R` stays `Code`. The phantoms ★ᵢ are the leaves `sl i`, i < J, and
  `leaf a` builds `sl (a + J)`, so no term builds a phantom.
  - The generic chain is `ri_den` … `ri_convcase`. It is a generated copy of
    the standard chain (`tools/gen_ri_all.sh`: `gen_ri.py` renames and adds
    the parameter, and `ri_edits.py` patches the R-specific steps). It is not
    an in-place generalization, so no existing definition or theorem changed.
  - The generic proof is one proof of the fundamental lemma for both models.
    `lemma36_via_ri` recovers the standard Lemma 3.6 from it at `stdRI`.
    Retiring the old chain is a separable follow-up: redefine
    `den`/`V`/`EnvSat` as the `stdRI` instances.
  - Where a phantom blocks the budget descent (the H₁ and Reflect cases), the
    generic proof uses `PhCons`: Corollary 3.7 for trees with a phantom.
    PhCons holds at every interpretation, by the standard Corollary 3.7
    (`phcons_of_spec`).
- **Theorem 4.6's encoding fact is a hypothesis, `Enc46`. F7's first,
  padded format refuted it; the canonical format satisfies it.**
  - Case 3 of the paper's proof argues from E2, E3, E4 and E6, and from two
    facts about the rules. Its conclusion: an accepted tree that embeds
    accepted certificates strictly inside pays, in its own nodes, at least
    the term size of one of them.
  - `Enc46 chkf dec sz` (`enc46.clj`) states exactly that, for any term
    measure `sz`. It is the theorem's only encoding fact.
  - Enc46 is satisfiable (`enc46_sat`) and not trivial (`enc46_nontrivial`).
    With CheckSpec and large certificates, it is jointly satisfiable only at
    a toy checker so far (`enc46w.clj`, `thm46_hyps_sat`).
  - F7's padded format (`Check decCertPad`, F7's checker until 2026-10-06)
    reads a certificate's padding only through its size tests. So a
    certificate stays accepted when its padding is replaced by any larger
    tree (`check_pad`), including one that holds another certificate.
  - `enc46_F7`: large certificates of any one type refute Enc46 at
    `Check decCertPad`, for every measure.
  - `thm46_F7_vacuous` (kept, as a theorem about the padded format): Theorem
    4.6's hypotheses are contradictory there at every budget
    k ≥ 2 + ‖left c₀‖ + ‖c₀‖, for any accepted c₀.
  - The canonical format (`certcanon.clj`, 2026-10-06) closes every free
    position: the padding, the unread root label and right child, an
    over-large fuel, and raw codes stored verbatim in δ-records (a δ-step on
    a literal certificate stored the certificate inside the tree).
    - Its certificate label 96 is on the root only: no encoder puts it on an
      internal node (`nlb 96`: `nlbN` … `nlbDT`, `nlb_encE`). This is F7's
      form of E6.
    - So no accepted code holds an accepted code strictly inside
      (`check_nest_free`), and Enc46 holds at `Check decCert` for every
      measure (`enc46_canon`). This is stronger than the paper's case 3,
      which must allow a sub-derivation to be a certificate: F7 wraps only
      the root, so case 3 cannot arise.
  - Hence Theorem 4.6 and Corollaries 4.6′ and 4.6″ at `Check decCert` with
    the paper's hypotheses only (`thm46_F7`, `thm46_types_F7`,
    `cor46_prime_F7`, `cor46_prime_iff_F7`, `cor46_dprime_F7`,
    `cor46_dprime_iff_F7`, `d3_gap_F7`; `thm46f7.clj`).
  - The result depends on the encoding's structure (a reserved root label,
    canonicity, literal raw codes). By the project's rule that is a finding,
    made explicit here: Theorem 4.6 holds at F7's checker because of how F7
    encodes certificates, and failed at its first encoding.
- **E1 is not used by Theorem 4.6 over codes; E3 is used by the paper's case
  3.**
  - `thm46` assumes ⌜B⌝ ≠ ⌜Aᵢ⌝. Its case 2 (`r″ = ★ᵢ`) needs only CheckSpec's
    decoding: a code accepted at two type codes makes them equal
    (`acc_same`).
  - E1 enters only in `thm46_types`, to pass from B ≠ Aᵢ to ⌜B⌝ ≠ ⌜Aᵢ⌝.
    R4 §4.5's "what the proof uses" lists E1.
  - The paper's case 3 counts ⌜fᵢ⌝'s nodes among the root judgment's. That
    needs the judgment to contain the term's encoding, which is E3, and R4's
    list omits E3. The formal proof cannot see this, because Enc46 packages
    case 3.
  - Theorem 4.6 uses no property of the type encoding `encTy` beyond
    CheckSpec.
- **Large certificates are a hypothesis.**
  - The paper builds `fᵢ := (λ(y :₀ Nat). gᵢ) N̄` with N > k. That needs the
    checker's completeness, which CheckSpec does not give.
  - `thm46` takes, for each input, an accepted certificate whose decoded
    term exceeds k in `sz` (`hbig`).
  - The corollaries take `LargeCert`: such certificates at every k.
  - At F7's canonical format the hypothesis is discharged, though not by
    completeness: Enc46 holds there for every measure, so `thm46` is applied
    at the measure that is constantly k + 1, at which every certificate is
    large. The paper's `(λ(y :₀ Nat). gᵢ) N̄` would need weakening of
    derivation trees, which F7 does not have.
- **Corollary 4.6″ assumes ⌜□A⌝ ≠ ⌜A⌝.** E1 reduces it to □A ≠ A as types.
  An injective encoding of arbitrary shape does not give that. At F7 it is
  derived by size (`box_code_ne`: ⌜□A⌝ holds ⌜codeTerm ⌜A⌝⌝, larger than
  ⌜A⌝).
- **Corollary 4.6′'s family is ¬ʲ1** (prop434's `negN`), standing in for the
  paper's A_j = 1 ⊸ ⋯ ⊸ 1, as in Proposition 4.4.
- **The upper bounds above μ.**
  - "Exists exactly when k ≥ μ(B)" needs a term at every budget at least
    μ(B). The paper types its term only at Θ_{μ(B)}.
  - In the formalization, ⋆ absorbs the extra tokens: an axiom carries any
    usages (`d2_upper_k46`, `d3_upper_k46`).

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
| §1.6 Check, via Lemmas 2.6–2.8, E1, E5 | `CheckSpec` (model); `check_spec`, `check_toksize`, `check_typesize` (check_spec); `check_complete`, `check_complete_lbl`, `decCert_canon`, `cert_size0`–`cert_size4`, `budget_canon`, `toksize_canon`, `typesize_canon` (certcanon); `tok_encE`, `nlb_encE` (encsize) | proved of `Check decCert` | discharged by F7: a concrete checker meets the trust base and accepts the certificates. The checker tests the size facts, and since 2026-10-06 they also follow from the canonical encoding (deviation above) |
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
| §4.5 the phantom model | `RInt`, `RLaws`, `stdRI`, `phRI`, `PhOk`, `PhCons` (rint); `den_ri`, `V_ri`, `EnvSat_ri`, `Sound_ri` and the generic chain (ri_den … ri_convcase) | defined | the model over an R-interpretation: the carrier of R stays `Code`, phantoms are the leaves below J, and `print`, `leaf` and `reflect` read the interpretation (see the deviations) |
| Lemma 4.6a | `lemma46a`, `lemma36_gen`, `phcons_of_spec` (lemma46a); `lemma36_ri` (ri_lemma36), `conv_all_ri` (ri_convcase) | proved | hypotheses: CheckSpec, and `PhOk` (the phantom codes' labels lie in L). No encoding fact. §4.5's "Lemma 2.8 applies without a phantom" is the interpretation's law L3. The standard Lemma 3.6 is the instance at `stdRI` (`lemma36_std`, `lemma36_via_ri`) |
| Theorem 4.6 | `thm46` (over codes), `thm46_types` (over types), `thm46_ne_needed` (theorem46); `Enc46`, `enc46_sat`, `enc46_nontrivial` (enc46); `thm46_hyps_sat`, `thm46_W_noterm` (enc46w); `check_pad`, `enc46_F7`, `thm46_F7_vacuous` (enc46f7, the padded format); `check_nest_free`, `enc46_canon`, `thm46_F7`, `thm46_types_F7` (thm46f7) | proved, relative to Enc46; at `Check decCert` with no encoding hypothesis | Hypotheses: CheckSpec; Enc46 for a term measure `sz` (case 3 only); for each input, a certificate with labels in L whose term exceeds k in `sz`; and ⌜B⌝ ≠ ⌜Aᵢ⌝ (`thm46`), or B ≠ Aᵢ for closed types (`thm46_types`, the theorem's only use of E1). The conclusion is a certificate of B with at most k nodes, so μ is a lower bound. B = A₁ must be excluded (`thm46_ne_needed`). The hypotheses are jointly satisfiable at a toy checker (enc46w). At F7's padded format they are refuted (enc46f7). At F7's canonical format Enc46 holds for every measure (`enc46_canon`), so the theorem holds there with the paper's hypotheses only: certificates of the inputs with labels in L, B ≠ Aᵢ, and the derivation (`thm46_F7`, `thm46_types_F7`) |
| Corollary 4.6′ (D2) | `d2_lower`, `d2_lower1`, `d2_upper`, `d2_upper_k46`, `cor46_prime`, `cor46_prime_iff`, `d2_no_uniform` (cor46) | proved | Lower bounds: Theorem 4.6's hypotheses, with `LargeCert` for A ⊸ B and A. Only A ≠ B is assumed; B ≠ A ⊸ B is proved (`pi_ne_cod46`). Upper bounds: any certificate of B with labels in L, at every k ≥ ‖v‖; they need neither CheckSpec nor an encoding fact. `cor46_prime_iff`: a term exists at Θₖ iff k ≥ μ(B). `d2_no_uniform` adds TypeSize and E5, at A = 1, B = ¬ᵏ⁺¹1. At F7's padded format everything but the upper bounds was vacuous (`enc46_F7`); at `Check decCert` both halves hold with A ⊸ B and A certifiable, A ≠ B and B closed (`cor46_prime_F7`, `cor46_prime_iff_F7`) |
| Corollary 4.6″ (D3) | `d3_lower`, `d3_gap`, `d3_upper_k46`, `cor46_dprime`, `cor46_dprime_iff` (cor46); `prop45_d3` (section4b) | proved | Lower bound: Theorem 4.6 at B = □A, with `LargeCert` for A and ⌜□A⌝ ≠ ⌜A⌝ as a hypothesis. Gap μ(□A) > 2μ(A): `prop43` (TokSize). Upper bound at every k ≥ ‖w‖. `cor46_dprime_iff`: a term exists at Θₖ iff k ≥ μ(□A). At `Check decCert`: `cor46_dprime_F7`, `cor46_dprime_iff_F7` (A certifiable; ⌜□A⌝ ≠ ⌜A⌝ derived) and `d3_gap_F7` |
| Proposition 4.10′ (H does not give H₁) | `prop410_prime` (prop410); `lemma36_noH1`, `rt_tr` | proved | χ accepts every code above a size bound (the paper changes Check at two pairs); "without H₁" is `noH1` of the term |
| Theorem 4, Corollary 5.1 | `theorem4`, `Theorem_4`, `corollary51`, `corollary51_conv`, `corollary51_nodes` (eval, theorem4) | proved | simply typed terms with `argsOK` (skOf defined at every argument; not a branch-list head). `corollary51` keeps `ConvCase`; `corollary51_conv` discharges it by `conv_all`. `corollary51_nodes` is Theorem 3 at Θₙ: an `R` denotation has at most `n` internal nodes |
| Erasure, evalᴱ, E, the trace | `Er`, `er_total`, `usk_skel`, `ebase_rel`, `erdflt`, `erdflt_ty`, `envE`, `e_abort`, `EvE`, `Erel`, `Ok`, `OkE` (erase); `usk_cv`, `erel_cv`, `erel_subst1`, `henv_of_usk` (uskel); `AdeqE`, `adeqE_var`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw`, `adeqE_app0`, `adeqE_app1`, `adeqE_appw`, `adeqE_pair0`, `adeqE_pair1`, `adeqE_pairw`, `adeqE_conv`, `adeqE_let0`, `adeqE_let1`, `adeqE_letw`, `adeqE_ite`, `adeqE_elimB`, `adeqE_case`, `adeqE_succ`, `adeqE_sleaf`, `adeqE_snode`, `adeqE_leaf`, `adeqE_node`, `adeqE_bnil`, `adeqE_bcons`, `adeqE_recN`, `adeqE_recs_leaf`, `adeqE_recs_node`, `adeqE_recs`, `adeqE_recS`, `adeqE_itr`, `adeqE_itR`, `adeqE_prn`, `adeqE_chk` (funde) | defaults, the environment relation, substitution invariance of E, path congruence, and the constant, variable, λ, application, pair, conversion, let, if, elimBool, caseLbl, constructor, recN, recSyn, itR, print, and chk′ cases of the fundamental property proved; inspect, abort, H₁, reflect, the induction assembling AdeqE, and Theorem 5.2 are open | erasure is a relation because `Rt` is `Prop`. `dec` returns a term, so reflect's success rule quantifies over an `Er` of the decoded judgment (uniqueness of erasure is not proved). `E` at `Σ₀` forgets the first component: the paper writes `(⋆, b)`, but the runtime default of a product is `(dflt σ, dflt τ)`, so requiring `⋆` would leave `abort` of `Σ(y :₀ Bool). 1` unrelated. `erdflt` / `erdflt_ty` are the defaults paragraph. `envE` leaves usage 0 unconstrained. `e_abort` and `e_h1` return those defaults after evaluating the premises; they are not Theorem 5.2's vacuous cases. `usk_hd` is invariance of `E` under a head step (`T(tt) ⇝ 1`, `T(ff) ⇝ 0`). Path congruence is `usk_cv` (uskel), and `usk_step_needs_wf` is the ill-typed β. Substitution invariance is `usk_subst` / `erel_subst1`; `henv_of_usk` transports a carrier environment along `usk_ctx`, and is a function because `HEnv` is a type. The fundamental property of evalᴱ is proved for constants, variables, λ, and application (`adeqE_star`, `adeqE_tt`, `adeqE_ff`, `adeqE_zero`, `adeqE_lbl`, `adeqE_var`, `adeqE_lam0`, `adeqE_lam1`, `adeqE_lamw`, `adeqE_app0`, `adeqE_app1`, `adeqE_appw`, `adeqE_pair0`, `adeqE_pair1`, `adeqE_pairw`, `adeqE_conv`). At Π₀ the runtime argument is ⋆; the denotation still applies ⟦f⟧ to ⟦u⟧. At Σ₀ the first component is ⋆ and is forgotten by E. Conversion transports the relation by `usk_cv` and the carrier by `denU_cv`. Let (`adeqE_let0`, `adeqE_let1`, `adeqE_letw`) evaluates the erased scrutinee to a pair and the erased body under (second, (first, ρ)); the body's judgment is at `lift 2 0 C`, brought back by `denU_lift`, and `den_let_body` is `den_letp_some` once `skOf` of the scrutinee is the product. At Σ₀ the first runtime component is unconstrained. The tail environment is `envE` at the body's usage vector. `if` (`adeqE_ite`) selects the branch at one type. `elimBool` (`adeqE_elimB`) moves the branch from `P[tt]` or `P[ff]` onto `P[b]` by `skel_subst_eq`, because a Boolean term has skeleton Unit. `caseLbl` (`adeqE_case`) is application of the branch list, and `P[a/x]` is `P` after a Unit substitution. Constructors (`adeqE_succ`, `adeqE_sleaf`, `adeqE_snode`, `adeqE_leaf`, `adeqE_node`, `adeqE_bnil`, `adeqE_bcons`) build the value from related arguments; a node evaluates its token and drops it, and `bnil` returns the related default. `recN` (`adeqE_recN`) inducts on the numeral: the step runs under the accumulator and the predecessor, `stepTy` has the skeleton of `P`, and the conclusion `P[n]` agrees with `P` because a numeral has skeleton Unit. The leaf method of recSyn (`adeqE_recs_leaf`) binds the label at usage ω; `leafTy`, `nodeTy`, `y1Ty` and `y2Ty` are substitutions of terms, so they have the skeleton of the motive. The node method (`adeqE_recs_node`) binds the two recursive results at usage 1, cast from `usk P` into `y1Ty` and `y2Ty`, and the two codes and the label at usage ω. `adeqE_recs` inducts on the code. `denU_recS` is that `Code.rec`; the cast stays outside the recursor. `adeqE_recS` is the judgment at `P[c]`, which agrees with `P` because a code term has skeleton Unit. The methods are moved off `leafTy` and `nodeTy` by `adeqE_leaf_at` and `adeqE_node_at`. The itR recursion (`adeqE_itr_leaf`, `adeqE_itr_node`, `adeqE_itr`) applies `g` to the label and `h` to the token, the label, and the two recursive results; `gTy` and `hTy` quantify the motive under lifts, so the results are cast back to `usk X`. A token is related at ◇. `denU_itR` is that `Code.rec`; the cast stays outside the recursor. `adeqE_itR` evaluates `g`, `h`, and the certificate, then folds them. `adeqE_prn` returns the certificate's code. `adeqE_chk` returns Check of the two codes. `denU` and `henv_of_usk` are functions because their types are not propositions. Induction is on `Er`, which carries the erased term; `Rt` is a proposition and cannot return it. Theorem 5.2 is the next row |
| Theorem 5.2: neither evaluator reaches `abort`, `H₁` or `H`; the relation `S`; safe traces | `Trace52`, `S52` and its clauses, `s52_cv`, `s52_subst`, `s52_subst1`, `s52_lift`, `s52_no_refutation`, `s52_no_contradiction` (safety52, s52facts); `Ok`, `OkE`, `ok52_eval`, `ok52_evalE` (erase, safety52); `Env52`, `env52_sub`, `var52`, `WFS` (s52env); `Result52`, the case lemmas `s52_const`, `s52_var`, `s52_conv`, `s52_abort`, `s52_h1`, `s52_H` (s52fund), `s52_lam`, `s52_lam0e`, `s52_app`, `s52_app0e` (s52bind), `s52_pair`, `s52_pair0e`, `s52_let` (s52prod), `s52_succ`, `s52_sleaf`, `s52_snode`, `s52_leaf`, `s52_node`, `s52_prn`, `s52_chk` (s52data), `s52_ite`, `s52_elimB`, `s52_bnil`, `s52_bcons`, `s52_caseL` (s52branch), `s52_recN` (s52recn), `s52_recS` (s52recs), `s52_itR` (s52itr), `s52_insp` (s52inspect), `Closed52`, `s52_refl` (s52reflect); `Reg`, `regB`, `reg_*` (s52reg); `S52JudgT`, `S52JudgR`, `s52_var_free`, `s52_tl_step`, `s52_rt_step` (s52assembly); `S52JudgE`, `s52e_lam`, `s52_er_step`, `s52_closed_step`, `s52_closed_all` (s52erased); `theorem52`, `theorem52_base`, `theorem52e`, `theorem52_both`, `theorem52_concrete` (s52theorem) | proved | Hypothesis: `CheckSpec` only; `theorem52_concrete` has none (`Check decCert`). **Statement.** For a derivable `Θₙ ⊢ t :¹ X`, an `Ok` trace of `evalₙ(t)` exists (`Ok` is `Ev` without the `abort`, `H₁` and `H` rules, reflect's nested runs included; `ok52_eval` makes it an `Ev` derivation), and for every erasure `e` of the derivation an `OkE` trace of `evalᴱₙ(e)` exists. The proof does not use that `X` is a base data type, so `theorem52` is stated for every `X`; `theorem52_base` is the paper's instance. **"The evaluation" is read as: a safe trace exists.** `Ev` is not proved deterministic here (its rules are syntax- and value-directed, but no functionality theorem is stated), so the statement is existence of an `Ok` trace, and `Ok ⊆ Ev`. `S` is `S52` at `erasing = false` for `evalₙ` and at `erasing = true` for `evalᴱₙ` (usage 0 changed as the paper says). Environments are `Env52`: at `evalₙ` every entry, whatever its usage; at `evalᴱₙ` the entries of usage 1 and ω. The fundamental property is the induction on `Tl` and `Rt` (`s52_tl_step`, `s52_rt_step`, evalₙ: the usage vector is an arbitrary one of the context's length, only its length is read, and premises are judged in the conclusion's own environment) and on `Er` (`s52_er_step`: environments restricted along `SubU`, as theorem4e), inside a strong induction on the budget (`s52_closed_all`); reflect uses the outer hypothesis at the decoded budget `m < n`. The case lemmas are stated once at `chkf dec encTy cap erasing` and instantiated by one generator (`rec-term`, s52assembly) for all three inductions. **Deviation, no definition changed: regularity.** `Tl.zBcons`, unlike `Rt.rBcons`, records no formation of its motive `P`, and S's head-of-list clause substitutes a label into `P`, which needs `P` formed. The `Tl` induction therefore carries `Reg G A` (A formed, or a branch-list type whose motive is formed under a label) as a hypothesis on the conclusion type, and each rule passes it to its premises' types (`reg_lam`, `reg_subst1`, `reg_lift2`, `reg_stepTy`, `reg_leafTy`, `reg_nodeTy`, `reg_gTy`, `reg_hTy`, `reg_chkT`, `reg_negT`, `reg_baseCode`; skeleton typing only). `Reg` holds at the roots of use (a type-level argument's type is formed by the rule's own premise `hA`). `abort` is vacuous (its premise's `S(0)` is empty), `H₁` and `H` are vacuous by `s52_no_contradiction` / `s52_no_refutation` (Corollary 3.7 at every size, no footprint), as the paper says |
| Lemma 6.4 (meta content): the default model is sound at a fixed budget, without induction on budgets; Corollary 6.5 (meta content) | `riDfltD`, `riDflt_laws`, `riDflt`, `riDflt_pr`, `riDflt_lf`, `riDflt_pf`, `riDflt_phcons`, `F_refl_dfl`, `F_h1_dfl`, `lemma64_step`, `lemma64_gen`, `lemma64`, `lemma64_theta`, `den_conchk_ri`, `cor65_dflt`, `cor65_concrete`, `lemma64_concrete`, `riDflt_ne_std`, `dflt_differs_std_reflect` (dflt64) | proved | Hypothesis: `CheckSpec` only; `lemma64_concrete` and `cor65_concrete` have none (`Check decCert`). **Statement.** `riDflt` is the R-interpretation with `print` and the leaf label the identity and `riPf` constantly false (`reflect_X r e ↦ dflt_X` whatever `r` is, `H₁ … ↦ 0`; `abort` is the standard clause). `lemma64`: for every runtime derivation `D ⊢ t :^us A` in a well-formed context and every budget `n`, `Sound_ri … riDflt n D us t A` (for all `η ⊨ₖ (D, us)`, `k ≤ n`: `⟦t⟧ⁿη ∈ Vⁿₖ(A)η`). `lemma64_theta` is the paper's form: `Θₙ ⊢ t :¹ A` gives `⟦t⟧ ∈ Vⁿₙ(A)` at the token environment. `cor65_dflt`: from `Θₙ ⊢ t :¹ Con′_ω`, `chk′ c ⌜0⌝ = ff` for every `c` with `lblOk c` (`IsSyn c`, which is the `Syn` clause of `V`, as `Con_λ` is stated in R4 §6.3). `cor37_refutation` gives the same for every code without a derivation; the derivation form is what Int-6.4 internalizes. **No induction on budgets (the point of Lemma 6.4).** `lemma64_step` takes no `OuterIH` argument: `F_refl_dfl` and `F_h1_dfl` never reach the phantom-free branch (`riPf` is false), where `refl_pf_ri` and `h1_core_ri` read `OuterIH_ri n`; they use `refl_ph_ri` / `PhCons` instead. `lemma64` is therefore an induction on the derivation only, a copy of `lemma36_step_ri` with those two case terms replaced and nothing else changed. **What is used instead of the induction:** Corollary 3.7 at every size (`phcons_of_spec`: BF₀, BF₁), which the paper proves in PA (Lemma 6.3); here it is a meta theorem, and its own proof (`cor37_*`) is the generic Lemma 3.6 at the standard model, which does the outer induction. That is not circular for Int-6.4: BF₀ and BF₁ are the closed bounded facts, proved in PA without budget induction. **Reflect at `D ≠ 0`:** `V_dflt_ri` (`dflt_D ∈ V(D)`); at `D = 0`: PhCons excludes it (`refl_ph_ri`). **The default model is not the standard one:** `riDflt_ne_std` (`riPf` differs); `dflt_differs_std_reflect` (a reflect at `Nat` whose certificate is accepted and decodes to `succ zero` is `1` at `stdRI` and `dflt_Nat = 0` at `riDflt`; the hypotheses are shown satisfiable in the suite). **Deviation:** none; no existing definition changed. `cor65_dflt` is stated for `lblOk c`, not for every code, because the anchor `aL` of p5.clj quantifies over all codes while R4 §6.3's `Con_λ` quantifies over `IsSyn` codes: `cor37_refutation` closes the gap at the object level, and the E-PA^ω sentence of Int-6.4 is the `IsSyn` one |
| Proposition 4.11 | `prop411`, `prop411_typed`, `prop411_den`, `prop411_nodes` (section4c) | proved | cₙ is the chain of ω-lets `sh4_cn`; label ℓ is 0 (below NL). ⟦cₙ⟧ is the complete binary code `sh4_bush` (2ⁿ internal-node-count as cnodes + 1 = 2ⁿ, since Nat subtraction truncates; `prop411_nodes` is cnodes = 2ⁿ − 1). The certificate-size table is the paper's measurement, not a theorem |
| Proposition 4.12 (1) | `prop412`, `prop412_typed`, `prop412_den`, `prop412_nodes` (section4c) | proved | the motive is Σ(p :ω Syn). 1, which does not depend on the Nat; the step opens the package and rebuilds a node. Part (2), polynomial certificate size, is the paper's citation of T3 and Proposition 4.1 |
| Proposition 4.9 | `sh4_depth0_typed`, `sh4_from_ff`, `sh4_cv_ff` (section4c) | partial | k = 0 of `depthLeq` (tt on a leaf, ff on a node) is a closed budget-0 term of Π(c :ω Syn). Bool. Where that bound has computed to ff, T(ff) ▹ 0 by Hd.tTF, so the evidence inhabits 0. The closed inhabitant of Π(c). T(depthLeq c k) ⊸ T(chk′ c c⊥) ⊸ 0 is not constructed: δ needs a canonical code, so the paper's proof case-splits to depth k (100-way caseLbl, two subcodes), and that derivation was not built |
| §6.2 H_PA; §6.4 step 4 (PA is consistent, relative to the metatheory) | `PT`, `PF`, `PAx`, `PPrv`, `paHolds`, `paPrv_sound`, `pa_consistent` (pa) | proved | de Bruijn variables (A4/A5/E2 without "free for"); DN is sound by stability (`paHolds_stab`), with no classical axiom |
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
  - `Enc46` with large certificates (`LargeCert`, `hbig`): jointly
    satisfiable at a toy checker (`enc46w`); refuted at F7's padded format
    (`enc46f7`); at F7's canonical format Enc46 holds vacuously
    (`enc46_canon`) — check that `check_nest_free`'s reading of Enc46's
    premise (an accepted tree with accepted trees strictly inside) is the
    paper's case 3.
- **Generators** (`tools/*.py`, and the Clojure generators in `unfold`,
  `mono`, `syntactic`, `fundamental`): do they emit the intended statements?
  - The generic chain `ri_*` is generated by `tools/gen_ri_all.sh`, which
    runs `gen_ri.py` and then `ri_edits.py`. Does each hand edit in
    `ri_edits.py` match the phantom model of R4 §4.5?
- **Deviations** above: do any change what the theorems mean?
Proofs need not be read. `bin/test-formal` rebuilds and re-checks everything.

## Consequences

- The metatheory's status line will change from "paper proofs" to naming what
  the kernel has checked.
- The formal suite is slow, since Ansatz elaborates every declaration at load.
  It is kept separate from the fast and extended suites.
