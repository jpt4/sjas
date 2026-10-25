# SJAS development log

## 2026-09-11 — G2 and representations of syntax without arithmetization

Recorded the question and answer on whether Gödel's second incompleteness theorem
can be proved using explicit representations of syntax without reducing to
arithmetization, including finite-set, expression-tree, and concatenation
approaches. The conversation preserves all six literature citations and the
distinctions between completed formalizations, proof sketches, and
undecidability results.

See [the conversation record](docs/log/2026-09-11-g2-without-arithmetization-conversation.md).

## 2026-09-22 — Continuing the first-principles analysis of SJAS program typing

Recovered the question about programs typable with an SJAS as their logic and
the instruction to reason from first principles and the literature. Derived a
bounded-witness lemma and, using Willard's extension theorem, an exact
characterization across the specified additive family of bounded-graph native
function specifications. Applied it to bounded search, native pairing, and
explicit execution-trace certificates. The distinction between a provable
specification, its computable realizer, and a particular type checker's accepted
terms remains explicit. Source theorem statements were checked as PDF images;
the new arguments are paper proofs, not mechanized results.

See [the research continuation](docs/log/2026-09-22-sjas-program-typing-continuation.md).

## 2026-09-22 — Correcting the focus to programs using self-consistency

The user clarified that the central question is what programs gain from access
to the self-consistency statement itself. Recorded explicit uses: eliminating
an impossible error branch, certifying a constant-result optimization of the
contradiction-proof checker, deriving safety from a certified reduction of bad
executions to contradictions, and transporting consistency through a supplied
proof transformation. Checked Willard's actual translated-reflection theorem
and its standard-model versus internal-equivalence distinction against page
images.

See [the self-consistency interface analysis](docs/log/2026-09-22-computational-use-of-self-consistency.md).

## 2026-09-22 — Comparing internal consistency with external verification

Reviewed the literature independently of the repository's proposed interfaces.
Derived an exact implementation whose totality is equivalent to the specified
checker's consistency, then gave a direct proof of the relevant case of
Kreisel's observation: adding true Pi_1 sentences to PA does not enlarge its
extensional class of provably recursive functions. Distinguished native
certificate availability from external program guarantees, and consistency of
the current checker from consistency of a predecessor. Compared the result with
Willard's restricted reflection, foundational proof-carrying code, and CakeML's
externally verified self-compilation. The arguments are paper proofs under
explicit assumptions, not a verified SJAS programming implementation.

See [the internal-versus-external comparison](docs/log/2026-09-22-internal-versus-external-consistency.md).

## 2026-09-23 — The lower boundary of self-justification

Investigated sub-Turing self-justification while separating executable
computation, representation of finite computations, and decidability of
theoremhood. Derived a halting reduction from the representation and
Sigma_1-completeness requirements of Willard's 2011 generic configurations.
Checked Pakhomov's independent Hilbert-style self-consistency theorem and its
mutual interpretability with Robinson R; this gives a weak positive benchmark
but not a decidable proof theory. Distinguished that result from Presburger,
bounded consistency, and typed-code reflection. No least system or natural
decidable self-justifying calculus was established by this review.

See [the lower-boundary analysis](docs/log/2026-09-23-lower-boundary-of-self-justification.md).

## 2026-09-23 — Execution and finite-computation representation

Resumed the interrupted request to distinguish both computational boundaries.
Separated terminating trace checking from unbounded existential descriptions
and execution, gave explicit hypotheses and a proof of halting-complete
theoremhood, and derived the independent diagonal limit on effective sound
totality certification. Located Pakhomov's R interpretation above the universal
finite-computation representation boundary. Added a direct PRA/G2 citation to
Simpson's Theorem 6.6.7 and checked the limitations of Presburger and
automaton-predicate self-verification. Runtime restriction is compatible with
an existing SJAS; a natural decidable instance of the entire self-justifying
proof apparatus remains unestablished by this review. The derivations are
paper proofs, not a programming-language implementation.

See [the two-boundary comparison](docs/log/2026-09-23-two-computational-boundaries.md).

## 2026-09-24 — Standard-model soundness of Willard's constructions

Answered whether the self-justifying systems only prove true sentences.
Derived full external soundness from the true Sigma_1-complete base,
Pi_1 added axioms, sound deduction, and established consistency. Checked that
the self-consistency axiom belongs to the required class. Distinguished this
soundness consequence from the bare definition of self-justification and from
unrestricted internal reflection.

See [the soundness argument](docs/log/2026-09-24-soundness-of-willard-sjas.md).

## 2026-09-24 — Completeness and intended truth

Clarified that syntactic completeness plus consistency need not give truth in
an intended model. Proved that consistency plus completeness for that model
does imply soundness, with syntactic completeness redundant. Distinguished
this condition from the usual semantic completeness of a logical calculus,
and gave a one-atom propositional counterexample. Added the derivation to the
[soundness note](docs/log/2026-09-24-soundness-of-willard-sjas.md#completeness-consistency-and-intended-model-truth).

## 2026-09-24 — Expressing soundness through reflection

Distinguished local and uniform reflection schemas from a single global
reflection formula using a truth predicate. Explained the role of Tarski's
undefinability theorem, restricted partial truth, and consistency as the
False instance of reflection. Recorded the conditional Lob obstruction and
a direct Godel-sentence derivation with its hypotheses. Checked Willard's
translated reflection theorem and the distinction between external and
internal equivalence against the rendered paper pages.

See [the soundness-reflection note](docs/log/2026-09-24-soundness-reflection.md).

## 2026-09-25 — Identifying the missing translation equivalences

Made the soundness-reflection distinction explicit: the relevant equivalence
is between A and the assertion that the original base B has no proof of
not A. Distinguished provable individual instances from the unavailable
whole family. Derived the unprovability of the reverse implication at the
Godel sentence under the stated fixed-point and proof-internalization
hypotheses, and identified its relationship to formalized Sigma_1 completeness.

See [the clarification](docs/log/2026-09-24-soundness-reflection.md#clarification-which-equivalences-fail-internally).

## 2026-09-25 — Godel-sentence independence

Separated unprovability from independence. For the ordinary Godel sentence,
consistency gives unprovability; Sigma_1 soundness or omega-consistency also
excludes its negation. Rosser obtains independence under consistency by
changing the sentence, with the usual arithmetic and representation
hypotheses. Connected this distinction to the sound SJAS instances and the
weaker assumptions needed by the preceding reflection obstruction.

See [the independence clarification](docs/log/2026-09-24-soundness-reflection.md#clarification-independence-of-godel-sentences).

## 2026-09-25 — Native constructive SJAS and program extraction

The user clarified that constructive means a constructive logic suitable for
program extraction. Recovered the earlier inherited-axiom and paired-certificate
constructions, then developed a native intuitionistic Hilbert variant whose
consistency axiom concerns its own full checker. Adapted Willard's finite-model
argument, made the proof-size requirements explicit, and supplied bounded-axiom
realizers and a witness-extraction argument. The result is a paper construction
using the cited bounded arithmetization; implementation and machine-checked
metatheory are not claimed. Existing prototypes and concurrent refinement files
were left untouched.

See [the native constructive construction](docs/log/2026-09-25-native-constructive-sjas.md).

## 2026-09-25 — Explicit realizers for the arithmetic axioms

Expanded the constructive SJAS note with concrete realizers for every axiom
in its illustrative finite arithmetic base, the named-number axioms, and
its self-consistency sentence. Specified the atomic-evidence convention,
bounded witness construction, and quantifier guards. Clarified that constant
negative realizers are valid by the external absence of antecedent evidence;
their program text does not independently establish consistency. Checked the
atomic realizability definition and Willard's addition graph on rendered
source pages. This is a paper clarification, with no checker or extractor
implementation added.

See [the realizer assignments](docs/log/2026-09-25-native-constructive-sjas.md#explicit-arithmetic-realizer-assignments).

## 2026-09-25 — Meaning and informational content of realizers

Clarified why the constant arithmetic realizers carry little information:
the selected interpretation assigns a unit-like value to each true atom,
and the substantive correctness claim is the external realizability
relation. Contrasted two formulas assigned the same candidate program,
explained how existential witnesses and disjunction choices retain
computational content, and distinguished this interpretation from a full
proof term or a native dependent type checker. No new consistency or
implementation claim is made.

See [the explanation of constant realizers](docs/log/2026-09-25-native-constructive-sjas.md#why-a-constant-realizer-can-carry-almost-no-information).

## 2026-09-26 — A PTS presentation of the constructive SJAS

Specified a lambda-P logical-framework signature for the constructive
Hilbert system, with explicit intuitionistic rule constants, arithmetic
axiom declarations, a fully displayed division proof, and its extracted
program. Gave the external proof-correspondence argument and distinguished
pure kernel beta reduction from object-proof normalization and numerical
execution. Preserved the original self-consistency predicate; no internal
translation theorem or self-consistency of a new standalone PTS checker
is claimed. This is a paper specification, leaving concurrent implementation
work untouched.

See [the PTS presentation](docs/log/2026-09-26-pts-presentation-of-constructive-sjas.md).

## 2026-09-26 — PTS computation and the internal consistency bridge

Clarified that opaque native arithmetic in the chosen LF encoding is a
design choice, and that an external proof normalizer need not extend the
kernel. Explained the effect of adding logical or arithmetic rewrites to
definitional equality. Distinguished Con_H from Con_K, gave the explicit
intuitionistic derivation transferring consistency from an internal bridge
restricted to FALSE, and identified the standard-versus-internal quantifier
boundary. Also derived the existing source theory's no-FALSE guarantee for
an interface requiring both PTS and Hilbert certificates.

See the [computation clarification](docs/log/2026-09-26-pts-presentation-of-constructive-sjas.md#clarification-extraction-external-normalization-and-conversion)
and the [consistency clarification](docs/log/2026-09-26-pts-presentation-of-constructive-sjas.md#clarification-exactly-which-consistency-statement-is-available).

## 2026-09-26 — Cirquents, deep inference, and graph deduction for SJAS

Examined cirquent calculi, first-order intuitionistic deep inference, proof
nets, and interaction nets as SJAS components. Gave a conditional extension
of the native finite-model consistency proof to sound finite-support graph
certificates with bounded checking and explicit coding bounds. Distinguished
that positive argument from a completed arithmetization of any particular
graph checker, and separated proof compression, constructive extraction,
execution, and internally certified proof transformations. Checked published
proof-complexity results and Willard's explicit allowance for some pointer
sharing. No implementation, benchmark, or mechanized result is claimed.

See [the alternative deduction methods analysis](docs/log/2026-09-26-alternative-deduction-methods-for-sjas.md).

## 2026-10-20 — Stewart's LtU remarks on SJAS and the Dialectica interpretation

Found the LtU thread the user remembered, on giving a proof system a
consumable budget (node 3076, 2008), and Charles Stewart's remarks there on
Willard's systems and his 2004 Advogato note. Stewart doubted, but did not
prove, that SJAS have a Dialectica interpretation. Assessment: Willard's
tableaux systems are Π₁-axiomatized without induction, so a Shoenfield-style
interpretation into recursor-free typed λ-terms exists and yields linearly
bounded Herbrand terms. What fails is self-justification on the functional
side: the target keeps the cut and proves successor total, which is the
cell where Solovay's theorem is negative. Also covered: why a total
self-interpreter fails there by growth alone. The argument is ours and
unpublished; no formal result is claimed.

See [the note](docs/log/2026-10-20-dialectica-interpretation-of-sjas.md).

## 2026-10-20 to 2026-10-22 — Codex lanes: limit waits, network failures, the S3 lane

Phase 2's Codex lanes run unattended from `code/lcert/test-runs` (gitignored;
`HANDOFF.md` there is the guide).
- **Limit waits (v4.3, 2026-10-20).** At the user's request, the supervisor
  now waits until the reset time that Codex's `/status` reports, read
  through the app server's read-only `account/rateLimits/read`
  (`codex-limits.sh`). It checks the wall clock, so a suspend no longer
  stretches the wait. The fixed 5 h wait it replaced had overshot the reset
  by up to an hour, and a suspend had added 2 h 17 min more.
- **Limits observed.** "Workspace out of credits" means the 5-hour window is
  used up and the workspace has no credits to spend. Three high-effort lanes
  used up the window in 25–60 minutes.
- **Network outage, 10-20 to 10-22.** On 10-20, three attempts failed at once
  on "workspace routing discovery" errors and cost a 5 h back-off. From 10-21
  15:22, both sessions hung on "waiting for network" until a reboot at 10-22
  04:45. The lanes stayed down until 17:38. v4.4 waits for the network
  instead of counting a failure, and stops a session that has shown only
  network errors for 30 minutes.
- **Disk.** Free space fell to 3.0 GB, below the lanes' 4 GB floor. Deleting
  superseded Codex releases (`~/.codex/packages/*/releases`, keeping the
  current one and one prior) freed 9 GB.
- **New lane s3.** It holds ADR-0007's W26–W30 and W31's general part: S3,
  E-PA^ω conservative over PA, by negative translation and HEO. It runs in
  the w10 worktree on branch `p2-s3`, from `p2-w10`'s df08bf9, since S3 needs
  only W16–W17. Only the instance at `conL` waits for W5.
- **Progress meanwhile.** W10, W16 and W17 are complete (cold gates green).
  W3c gate 1 is merged into `adr-0007-phase2` (bb114e1), and gate 2 is under
  way. W3b has 19 definitions left.
- **Tests.** The supervisor's new behaviour has tests: `test-codex-limits.sh`
  (9/9), `test-supervisor-limit-wait.sh` (9/9) and
  `test-supervisor-network.sh` (8/8). Each was run red against the earlier
  version first.

## 2026-10-22 — ADR-0007's work plan reduced to what P5 requires

At the user's request ("make certain all items are actually required, not
process or ceremony"), audited ADR-0007's work items against P5's deduction.
The deduction needs S1, S3, Int-6.1 and Int-6.4 at concrete natural `conL`
and `conPA`, with (A1) and (A3) beneath them.
- **Growth.** The plan grew from 19 items to 37. Apart from W3b and W3c, the
  growth subdivided the same content into gates, each with its own cold run,
  records, merge request and merge. Lane branches all edited the ADR, LOG and
  LESSONS, so every merge conflicted three ways.
- **Dropped.** The optional direct-PA route (W15, W20, W21). It would change
  R4 §6's argument, whereas the fidelity directive keeps the paper's route:
  Int-6.4 in E-PA^ω, then S3.
- **Merged.** The rest became one gate per statement of the paper: 29 open
  items to 15 gates, with no content lost.
- **Process.** One cold run per gate. Only the integrating p2 lane writes the
  ADR, LOG and LESSONS, at merge. Lanes merge only when a consumer needs the
  work.
- **Where.** The decision text is `code/lcert/test-runs/plan-2026-10-22.md`.
  p2 inserts it into ADR-0007 at its next commit. The w3c, s3 and p2 prompts
  now follow it.

## 2026-10-22 — Codex lanes switched to medium effort

- **Why.** On 2026-10-22 three lanes at effort high used up each 5-hour window in 24 minutes, and each window used about 15% of the weekly window. That window resets at 2026-10-29 21:38Z and stood at 31%, so the remaining allowance would have lasted about 4½ windows. That is roughly 2½ hours of three-lane work a week.
- **Decision (user).** "Switch Codex's effort to medium."
- **Change.** The effort field of the p2, w3c and s3 tasks went from high to medium in their queues. The supervisor re-reads its queue between attempts, so the change applies from each lane's next session with no restart. The main lane's x8 was already medium.
- **Watch.** Medium effort may struggle on the hardest proofs. If a lane stalls on them, raise that one task back to high in its queue.

## 2026-10-23 — W3b's loads waited on desktop memory; memory notifier

- **What happened.** p2's W3b test load needs about 6 GB, behind an 8 GiB gate that protects the desktop.
  - The 22-10 23:07 run grew to about 6 GB. Its watchdog stopped it at `decSkDT` once the machine had only 2.4 GB left.
  - Its next waiters never saw 8 GiB free (about 6.8 GB with every lane idle), so each gave up after 90 minutes.
  - Each new waiter cost a Codex session, and `cold-run.hold` paused w3c and s3 meanwhile.
  - The user freed memory at 10:28 and the load started within a minute.
- **Decision (user).** "Codex time is higher priority" than desktop memory. Claude does not order p2 to cut the load's memory.
- **Change.**
  - `test-runs/memory-notify.sh` sends the user a desktop notification when a waiter logs `gate-wait=`, at most once every 30 minutes. Its tests are `test-memory-notify.sh`: 10/10, and 5 fail against a no-op script.
  - p2's prompt has order 6: keep the gate, spend no session cutting memory, and on a timeout start one new waiter and end the session.

## 2026-10-23 — s3 blocked on a W17 presentation gap; resolved as a fidelity repair

- **Gap (s3, 11:17).** W17's `EPAwPrf.pa` admits a PA axiom only when every variable of the finite-type environment is nat (`epawprf.clj:180-183`). `pfToEPAwCtx` has the same restriction, and no constructor weakens the context. So a PA axiom cannot be used under a higher-type variable it does not mention, although F6.2 and P5 §3's E-PA^ω allow it. s3 stopped W26–W31, as its prompt says to do for any W17/F6.2 difference.
- **Resolution (Claude).** This is a technical repair for fidelity to F6.2, not a route choice, so it is within the lanes' authority.
  - s3 repairs W17 on `p2-s3`: a PA axiom then needs only its own free variables to be nat. No new constructor is added.
  - s3 proves the weakening lemma the repair needs, includes W17's tests in the S3 (i) cold run, and files a merge request for the repair early, because Lemma 6.2 consumes W17.
- **Records.** The decision is in `test-runs/s3.blocked.resolved` and `s3-prompt.md`; p2 records it in ADR-0007 at merge.

## 2026-10-23 — Supervisor v4.5: Codex priority for p2

- **Decision (user).** "Prioritize p2, if it is blocking the other lanes." It is:
  - W3b gates W5 and every later gate.
  - The main lane stays paused until W3b's cold run.
  - On 2026-10-23 a green p2 result (`prf_checkNatHistLookup`, 15:39) sat uncommitted for 3 h, because w3c and s3 had used up the 5-hour window by 15:43.
- **Measure.** Each session costs about 9% of a 5-hour window: about 100k tokens of the roughly 1.1M a window allowed that day. p2 drew about 29% of that window.
- **Change.**
  - `supervisor-codex-v4.5.sh` adds `CAP=<percent>`: a capped lane starts no attempt while the 5-hour window is used to CAP% or more. The cap lifts 45 minutes (`CAP_RELEASE`) before the reset, so p2's unused reserve is not wasted.
  - `codex-limits.sh --window 300` reads the window's percentage and reset time below 100% too.
  - w3c and s3 run with `CAP=60`; so will main, once p2 restarts it. p2 is uncapped.
  - The cap is checked only before an attempt, so running sessions finish; by the per-session figure above, the capped lanes overshoot by about two sessions at most.
- **Tests.**
  - `test-supervisor-cap.sh`: 11/11. Against v4.4, 10 fail.
  - `test-codex-limits.sh`: 14/14. The 5 new cases failed before the change.
  - The limit-wait and network tests pass against v4.5.
- **Switch-over.** The three supervisors moved to v4.5 at 17:23, while all three waited for the 18:43 reset; no session was interrupted. HANDOFF's commands now start v4.5 with the caps.

## 2026-10-23 — Claude worked p2's W3b while Codex was limited

- **Request (user).** "Use your own usage, up to 90%, to work on p2 until Codex refreshes."
- **Finding.** p2's green lookup piece (15:39) would have failed a cold run. `prf_checkNatHistLookup` was generated only by p2's dev probe, not by the `p2-pr-gate5` namespace.
- **Change.** 5ecf271 on `adr-0007-phase2` generates it at load. The namespace now requires `p2-pr-defs` and `p2-pr-ho` instead of `p2-pr-mach`.
- **Check.** A dev-only check (`code/lcert/test-runs/p2-w3b-gate5-ns-check.log` in the p2 worktree) loaded the whole namespace and passed the lookup test 3/3 (28 min). The name check passes.
- **Handoff.** p2's red test for the final target stays uncommitted in its worktree. A handoff entry is in `p2.progress`. Claude stopped at 18:22 rather than start a second 28-minute load, which would have delayed p2's 18:44 session.

## 2026-10-24 00:22 — Lanes restarted after a power-off
The machine powered off cleanly at 21:37 on 10-23 (logind "System is powering down", right after xfce4-power-manager's suspend request was refused by polkit; no out-of-memory event) and booted at 23:52. Claude deleted the stale PID files and cold-run.hold per HANDOFF.md, restarted the p2, w3c and s3 supervisors (v4.5, reniced to 19) and the memory notifier at 00:22. p2's step-body green check (started 21:11) was killed by the shutdown; p2 reruns it. 5h window 0%, weekly 28%.

## 2026-10-24 10:04 — Claude takes the p2 lane
Codex reached its 5-hour limit at 09:59 (weekly 56%). The user: "Codex has hit its credit limit; use yours, up to 90% of your 5-hour usage." Claude works p2 (W3b) itself from now on, so that p2 and a Codex session never share the worktree: `supervisor-codex-p2.stop` paused p2's Codex supervisor. The 60% priority cap on w3c and s3 existed only to reserve Codex for p2, so their supervisors were restarted with `CAP=` (no cap). Claude reads its own 5-hour usage from `~/.claude/statusline-input.json`, which the user's statusline script now saves (8% at 10:03).

## 2026-10-24 — Incompleteness Ex Machina analysed; Self-Justification Ex Machina drafted
The user asked for an analysis of Oberhoff's essay *Incompleteness Ex Machina* (2018) and, if possible, a computational exegesis of Willard's self-justification on the same lines. All 9 pages were read as text and as page images; the text layer drops the overbar on the halting lemma's program H̄ (hazard H2).

Finding: every result in Oberhoff's essay except G2 uses only external Σ₁-completeness ("watching"). His G2 step taken "with some faith" is formalized Σ₁-completeness ("knowing that one watches"). Willard's systems keep the first and lack the second, and by Theorem 6 run backwards, a consistent system that proves its own Level(1) consistency cannot prove the second. The essay retells Willard 2001's Assertion ++ as an Oberhoff-style case analysis about a refutation-searching program, prices the faith step with Willard's boundary results (squaring without cuts, successor with cuts), and refactors Oberhoff's Theorem 7 into 7a/7b.

The proposition and the interpretive vocabulary are ours, labelled as such. Willard quotations were checked against page images (TR 93-10 pp. 6–7, 2001 p. 20, 2006a p. 24), and proof statuses come from the codification registry.

See [the analysis note](docs/log/2026-10-24-incompleteness-ex-machina-analysis.md) and the essay, [`prose/self-justification-ex-machina/`](prose/self-justification-ex-machina/self-justification-ex-machina.tex) (13 pp., PDF beside it).

## 2026-10-25 — Self-Justification Ex Machina committed, reviewed, revised twice
- **Request (user).** "Commit this to a new branch; run an adversarial read-only review. Oberhoff's work satisfies the requirements for a paper proof, at the level of granularity necessary for G2; self-justification is a finer-grained concept, and so SJexM must be more technically precise to meet equal proof-status validity."
- **Branch.** `sjexm-essay`, from master, in worktree `.claude/worktrees/sjexm`. The main worktree stays on `f6-p5`. Commits: 84eba77 (draft 1), e4c736e (revision 2, the self-audit), 79be6a7 (revision 3, review fixes). Not pushed. LOG.md stays untracked, as before.
- **Review.** Grok 4.7 through the Cursor CLI, read-only, run against draft 1 with the standard above. Verdict: does not meet it; 15 findings (2 critical). Each finding was checked against the source text. One suggestion was not adopted, and the reason is recorded.
- **What changed.**
  - A Conventions section fixes the configuration, language, consistency level, fixed point, encoding, and relative consistency.
  - Willard's 2001 and 2005 consistency theorems are stated separately.
  - The consistency proof is retold from 2005 Thms 1–2 (Probe).
  - Prop. E now shows that, given Level(1) self-consistency, the faith step is provably equivalent to the Gödel sentence.
  - New section on 2005 Thm 6: the faith step bounded, as a Π\*₁ theorem of PA, switched on by one Σ\*₁ axiom.
  - Lemma dial: Δ₀ cut and Π\*₁/Σ\*₁ lemmas are compatible; Π\*₂/Σ\*₂ lemmas are not (sketch); unrestricted lemmas are not (Solovay, cited).
- **Discrepancy.** 2005 p. 40 (theorems valid for uncompressed encodings) conflicts with the wording of codification obligation O51. Not edited here.
- See the [analysis note](docs/log/2026-10-24-incompleteness-ex-machina-analysis.md), revision sections and disposition table, and the essay (21 pp.). The copies in this worktree match the branch.

## 2026-10-25 — LOG.md tracked; `sjexm-essay` pushed; second review of SJexM
- **Request (user).** "Track LOG.md; push to remote on this branch, then review again."
- **Tracked.** This commit adds LOG.md to `sjexm-essay`. It is the main worktree's file, which was untracked, copied as it stood, plus this entry.
  - Two older, disjoint LOG.md files exist on unmerged branches: `adr-0001-affine-tree-sjas` (2026-07-29 entries) and `adr-0003-alsjas-paper-first` (2026-09-01 to 09-02). Neither is in this file. Merging either branch will conflict on LOG.md.
  - Of the 13 notes under `docs/log/` that LOG.md links, only the SJexM analysis note is tracked on any branch. The other 12 links are dead on this branch.
- **Pushed.** `sjexm-essay` goes to origin (github.com/jpt4/sjas) with this commit.
- **Review round 2.** Grok 4.7 (Cursor CLI, read-only) reviews revision 3 against the same standard. It checks each R1 finding's disposition and then reviews the new material. Results follow in a later entry.

## 2026-10-25 — SJexM round-2 review and revision 4
- **Review.** Grok 4.7 reviewed revision 3, read-only, in about 34 minutes. Verdict: does not yet meet the standard. 16 findings: 1 critical, 8 major, 7 minor. 7 of the 15 round-1 findings were only partly resolved.
- **Critical (R2-01).** Proposition K fed Willard's `Pair` the code of the prenex halting sentence. `Pair` takes the syntactic negation, ⌜¬f⌝ (2005 Eq. 28), whose code is different.
  - The essay now keeps ¬G̈ and the halting sentence **H** apart, and aims steps (2) and (3) at ¬G̈.
  - It leaves open whether Willard's systems fail knowing-that-they-watch at **H** itself.
- **Also changed.**
  - The hypotheses of each step are cited where they are used. (H1) is now Level(0−).
  - Fact FP is split. G̈'s unprovability no longer needs the transposed uniqueness sentence (U); (U\*) is an explicit assumption of Cor L(b) only.
  - The (E) argument is redone from Appendix A.
  - §12's 7b claims are split by system.
  - "Counting" is replaced by Solovay's actual hypotheses.
  - Page numbers follow one convention.
- **Two reviewer claims corrected.** Willard writes number terms with an overline (2005 p. 4, image), and 2001 App B covers tableau trees (p. 52).
- **Found by us.** The fourth step of Willard's 2005 Theorem 6 proof uses Definition 6's q as a proof of ¬℧, but Definition 6 (p. 33, image) has q prove "∃p Prf(⌜℧⌝,p)". The essay notes this and gives a repair.
- Revision 4: 25 pp., clean build. The disposition table and open items are in the [analysis note](docs/log/2026-10-24-incompleteness-ex-machina-analysis.md).

## 2026-10-25 — SJexM round-3 review and revision 5
- **Request (user).** "What is the 'faith step'? Run a third review." The faith step was explained in the conversation. A follow-up question asked why the transcript needs multiplication. The answer given: by Herbrand's theorem, any function a true, Π₁\*-axiomatized U-Grounding theory proves total is bounded linearly, and a transcript must outgrow that. The argument is not yet in the essay.
- **Review.** Grok 4.7 reviewed revision 4, read-only, in about 33 minutes. Verdict: does not yet meet the standard. 10 findings: 0 critical, 8 major, 2 minor.
- **Main change.** Willard's `Pair` admits two readings: (i) the Σ₁\* representative of the negation, as his Theorem 1 proof uses it; (ii) the string ¬Φ, as Eq. 28 writes it.
  - Prop K, the corollaries and §12 are now stated for **N**, the sentence `Pair` pairs with G̈.
  - Under (i) the failure of knowing-that-it-watches is at a Σ₁-class sentence.
  - Under (ii), Willard's consistency proof itself needs a Prenex\* convention that he does not fix. The essay now says so.
- **Also changed.**
  - Fact FP has its full hypotheses and a proper Lemma 7.1B step.
  - (E) is restricted to proofs of G̈, under two stated encoding assumptions.
  - Cor L lists all its assumptions.
  - Cor M's watching step is completed.
  - Willard's Theorem 6, step (31), is now listed as a gap. The repair is a conjecture.
  - The abstract and §13 are cut back to what the body proves.
  - §12's boundaries carry their full hypotheses.
- **Rejected.** The reviewer's claim that §1 misstates 2001's regular consistency. 2001 p. 12 matches §1.
- Revision 5: 27 pp., clean build. The disposition table and open items are in the [analysis note](docs/log/2026-10-24-incompleteness-ex-machina-analysis.md).

## 2026-10-25 — SJexM revision 6: the open items closed
- **Request (user).** "Yes, add the Herbrand argument. That list of open points reads like the entire point of the essay is not yet accomplished — get it done."
- **Closed by fixing Willard's unspecified conventions.**
  - A least-witness substitution formula and a fixed point without the bound h ≤ x remove the transposed uniqueness sentence and the encoding hypothesis.
  - A fixed Prenex\* rewriting makes ¬Φ and its Σ₁\* form share their tableau proofs. A `Pair` that accepts both removes the two-readings problem. The halting sentence **H** is now exactly what `Pair` pairs with G̈.
  - Correctness of Willard's proof predicate is proved from the fixed encoding and his arithmetization.
- **Theorem 6.** Step (31) is repaired within the scope of Willard's Lemma 3 sketch.
- **Added.** Proposition "Linear witnesses", with a model-theoretic proof via the linear closure of an element, and its corollary: Willard's systems cannot prove that multiplication is total, or that every number has a written name. So the uniform faith step fails for both the 2001 and 2005 systems.
- **Also.** Hilbert–Bernays verified. Honesty and the first incompleteness theorem are now proved for IS(A) as well.
- Revision 6: 28 pp., clean build. The status table is in the [analysis note](docs/log/2026-10-24-incompleteness-ex-machina-analysis.md). Round 4 of the review is next.
